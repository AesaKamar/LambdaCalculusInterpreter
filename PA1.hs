import PA1Helper
import Data.Unique
import System.IO.Unsafe (unsafePerformIO)
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


replaceIn:: Lexp -> Lexp -> Lexp -> Lexp
replaceIn (Atom thingWeNeedToReplace) valueToReplaceWith expressionToWorkOn@(Atom e)
  | e == thingWeNeedToReplace = valueToReplaceWith
  | otherwise = expressionToWorkOn
replaceIn (Atom thingWeNeedToReplace) valueToReplaceWith expressionToWorkOn@(Lambda (Atom x) e)
  | otherwise = Lambda (Atom x) (replaceIn (Atom thingWeNeedToReplace) valueToReplaceWith e)
replaceIn (Atom thingWeNeedToReplace) valueToReplaceWith expressionToWorkOn@(Apply e1 e2)
  | otherwise = Apply (replaceIn (Atom thingWeNeedToReplace) valueToReplaceWith e1) (replaceIn (Atom thingWeNeedToReplace) valueToReplaceWith e2)



alpha :: Lexp -> IO Lexp
alpha lexp@(Lambda (Atom x) e) = do
    unique <- newUnique
    return (replaceIn (Atom x) (Atom ("var" ++ show (hashUnique unique))) e)
alpha e =  return e


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
simplify e =  beta (eta (unsafePerformIO (alpha e)) )

-- Entry point of program
main = do
    putStrLn "Please enter a filename containing lambda expressions:"
    fileName <- getLine
    -- id' simply returns its input, so runProgram will result
    -- in printing each lambda expression twice.
    runProgram fileName simplify
