import PA1Helper

-- Haskell representation of lambda expression
-- In Lambda Lexp Lexp, the first Lexp should always be Atom String
-- data Lexp = Atom String | Lambda Lexp Lexp | Apply Lexp  Lexp

-- Given a filename and function for reducing lambda expressions,
-- reduce all valid lambda expressions in the file and output results.
-- runProgram :: String -> (Lexp -> Lexp) -> IO()

-- This is the identity function for the Lexp datatype, which is
-- used to illustrate pattern matching with the datatype. "_" was
-- used since I did not need to use bound variable. For your code,
-- however, you can replace "_" with an actual variable name so you
-- can use the bound variable. The "@" allows you to retain a variable
-- that represents the entire structure, while pattern matching on
-- components of the structure.
id' :: Lexp -> Lexp
id' v@(Atom _) = v
id' lexp@(Lambda (Atom _) _) = lexp
id' lexp@(Apply _ _) = lexp



beta :: Lexp -> Lexp
-- Case of variable (Atom)
beta v@(Atom _) = v
-- Cases of Lambda
beta lexp@(Lambda x e) = (Lambda x (beta e) )
beta lexp@(Apply (Lambda x e) m )
  | e == x = m
  | otherwise = beta e
-- Case of Function Applicaiton
beta lexp@ (Apply x y) = (Apply (beta x) (beta y))

-- replaceIn:: Lexp -> Lexp -> Lexp -> Lexp
-- Atom(ThingWeNeedToReplace), Atom(ValueToReplaceWith), ExpressionToWorkOn -> Result
-- replaceIn originalVar newName Atom(e)
--   | e == originalVar = newName
--   | otherwise        = e
-- replaceIn originalVar newName e@(Lambda( _ _ ))



alpha ::  Integer -> Lexp -> Lexp
alpha n lexp@(Lambda (Atom x) (Atom e))
  | e == x = Lambda  (Atom ("var" ++ show n)) (Atom("var" ++ show n))
-- alpha n lexp@(Lambda (Atom x) (Lambda y e)) =  Atom x

-- alpha n (Atom v) = Atom v
-- alpha n lexp@(Lambda x e)
--   | e == x = Lambda  (Atom ("var" ++ show n)) (Atom("var" ++ show n))
--   | otherwise = Lambda x (alpha n e)
-- alpha n lexp@(Apply e m)  = (Apply (alpha (n+1) e) m)



eta :: Lexp ->  Lexp
-- Case of variable
eta (Atom v) = Atom v
-- Case of Lambda
eta lexp@(Lambda x (Apply e m))
  | x == m = e
  | otherwise = lexp
eta (Lambda x e) =  (Lambda x (eta e))
-- Case of Application
eta (Apply x y) = Apply (eta x) (eta y)



simplify :: Lexp -> Lexp
simplify e = beta (eta e)

-- Entry point of program
main = do
    putStrLn "Please enter a filename containing lambda expressions:"
    fileName <- getLine
    -- id' simply returns its input, so runProgram will result
    -- in printing each lambda expression twice.
    runProgram fileName simplify
