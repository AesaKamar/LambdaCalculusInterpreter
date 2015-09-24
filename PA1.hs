-- Programmed by Aesa Kamar and Stephen Burchett

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
  | otherwise = replaceIn x m (beta e)
-- Case of Function Applicaiton
beta lexp@ (Apply x y) = (Apply (beta x) (beta y))



replaceIn:: Lexp -> Lexp -> Lexp -> Lexp
replaceIn (Atom thingWeNeedToReplace) valueToReplaceWith expressionToWorkOn@(Atom e)
  | e == thingWeNeedToReplace = valueToReplaceWith
  | otherwise = Atom e
replaceIn (Atom thingWeNeedToReplace) valueToReplaceWith expressionToWorkOn@(Lambda (Atom x) e)
  = Lambda (Atom x) (replaceIn (Atom thingWeNeedToReplace) valueToReplaceWith e)
replaceIn (Atom thingWeNeedToReplace) valueToReplaceWith expressionToWorkOn@(Apply e1 e2)
  = Apply (replaceIn (Atom thingWeNeedToReplace) valueToReplaceWith e1) (replaceIn (Atom thingWeNeedToReplace) valueToReplaceWith e2)



alpha :: Lexp -> IO Lexp
alpha (Lambda (Atom x) e) = do
    unique <- newUnique
    let newVar = (Atom ("v" ++ show (hashUnique unique)))
    return (Lambda newVar (replaceIn (unsafePerformIO (alpha (Atom x))) newVar e) )
alpha (Lambda x e) =
  return (Lambda (unsafePerformIO (alpha x)) (unsafePerformIO (alpha e)) )
alpha (Atom v) =return (Atom v)
alpha (Apply e1 e2) = return  (Apply (unsafePerformIO (alpha e1) ) (unsafePerformIO (alpha e2)))


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
-- simplify e = beta (eta (unsafePerformIO (alpha e)))
-- simplify e = beta (eta e)

simplify e =
   (\ x n -> iterate beta x !! n) (eta(unsafePerformIO (alpha e) )) 20
-- simplify e = do
--   let moreReducable = True
--   -- If we have pattern (Apply (Lambda x e) m), set moreReducable = True
--
--   let newLexp = beta (eta (unsafePerformIO (alpha e)))
--   if newLexp == e
--     then do
--       moreReducable = False
--     else do
--       return beta newLexp



-- Entry point of program
main = do
    putStrLn "Please enter a filename containing lambda expressions:"
    fileName <- getLine
    -- id' simply returns its input, so runProgram will result
    -- in printing each lambda expression twice.
    runProgram fileName simplify
