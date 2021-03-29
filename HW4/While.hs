-- | A single register imperative language.
module While where


--
-- * Syntax
--

-- Before refactoring:
--
--    int  ::= (any integer)
--
--    expr ::= `R`                  -- load from integer register
--          |  int                  -- integer literal
--          |  expr `+` expr        -- addition expression
--          |  expr `≤` expr        -- less than or equal to
--
--    stmt ::= `R :=` expr          -- set register
--          |  `while` expr stmt    -- while loop
--          |  `begin` stmt* `end`  -- statement block

-- data Expr
--    = Get
--    | Lit Int
--    | Add Expr Expr
--    | LTE Expr Expr 
--   deriving (Eq, Show)

-- data Stmt 
--     = Set Expr
--     | While Expr Expr
--     | Block [Stmt]
--   deriving (Eq, Show)

-- After refactoring to eliminate the possibility of type errors:
--
--    int  ::= (any integer)
--
--    expr ::= `R`                  -- load from integer register
--          |  int                  -- integer literal
--          |  expr `+` expr        -- addition expression
--    
--    test ::= expr `≤` expr        -- less than or equal to
--
--    stmt ::= `R :=` expr          -- set register
--          |  `while` test stmt    -- while loop
--          |  `begin` stmt* `end`  -- statement block

data Expr
   = Get
   | Lit Int
   | Add Expr Expr  
  deriving (Eq, Show)

data Test 
    = LTE Expr Expr 
   deriving (Eq, Show)

data Stmt 
    = Set Expr
    | While Test Expr
    | Block [Stmt]
  deriving (Eq, Show)

-- Example program:
--   begin
--     R := 1
--     while R <= 100
--       R := R + R
--   end
p :: Stmt
p = Block[
        Set (Lit 1), 
        While (LTE Get (Lit 100)) 
              (Set (Add Get Get))
    ]


--
-- * Semantics
--
type Reg = Int

-- Before refactoring:
--   expr: Reg -> Maybe (Either Bool Int) 
--   stmt: Reg -> Maybe Reg

-- expr :: Expr -> Reg -> Maybe (Either Bool Int)
-- expr Get       s = Just (Right s)
-- expr (Lit i)   s = Just (Right i)
-- expr (Add l r) s = case (expr l, expr r) of 
--                     (Just (Right i), Just (Right j)) -> Just (Right (i + j))
--                     _ -> Nothing
-- expr (LTE l r) s = case (expr l, expr r) of 
--                     (Just (Right i), Just (Right j)) -> Just (Right (i <= j))
--                     _ -> Nothing

-- stmt :: Stmt -> Reg -> Maybe Reg


-- After refactoring:
--   expr: Reg -> Int
--   test: Reg -> Bool
--   stmt: Reg -> Reg

-- | Semantic function for expressions
expr :: Expr -> Reg -> Int
expr Get       s = s
expr (Lit i)   s = i
expr (Add l r) s = expr l s + expr r s

-- | Semantic function for tests
test :: Test -> Reg -> Bool
test (LTE l r) s = expr l s <= expr r s

-- | Semantic function for statements
stmt :: Stmt -> Reg -> Reg
stmt (Set e)     s = expr e s
stmt (While c b) s = if test c s then stmt (While c b) (stmt b s) else s
stmt (Block ss)  s = stmt ss s