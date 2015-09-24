-- Programmed by Aesa Kamar and Stephen Burchett
-- September 2015
--
-- This program takes a Lambda calculus expression and simplifies it ith alpha, eta, and beta reductions

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

-- Beta Handles a single application of the Beta Redux
beta :: Lexp -> Lexp
beta v@(Atom _) = v                                                                                 --Case of expression as v
beta lexp@(Lambda x e) = (Lambda x (beta e) )                                                       --Case of expression as \v.e
beta lexp@(Apply (Lambda x e) m )                                                                   --Case of expression as (\x.E M)
  | e == x = m
  | otherwise = replaceIn x m (beta e)
beta lexp@ (Apply x y) = (Apply (beta x) (beta y))                                                  --Case of expression as v


-- ReplaceIn is a helper function to replace all x's within e with expression m
replaceIn:: Lexp -> Lexp -> Lexp -> Lexp
replaceIn (Atom thingWeNeedToReplace) valueToReplaceWith expressionToWorkOn@(Atom e)                --Case of expression as v
  | e == thingWeNeedToReplace = valueToReplaceWith
  | otherwise = Atom e
replaceIn (Atom thingWeNeedToReplace) valueToReplaceWith expressionToWorkOn@(Lambda (Atom x) e)     --Case of expression as \v.e
  = Lambda (Atom x) (replaceIn (Atom thingWeNeedToReplace) valueToReplaceWith e)
replaceIn (Atom thingWeNeedToReplace) valueToReplaceWith expressionToWorkOn@(Apply e1 e2)           --Case of expression as (e e)
  = Apply (replaceIn (Atom thingWeNeedToReplace) valueToReplaceWith e1) (replaceIn (Atom thingWeNeedToReplace) valueToReplaceWith e2)


-- Alpha necessarily renames bound pairs of variables to fresh names
-- Uses NewUnique to generate a universally unique variable name for alpha conversion
-- unsafePerformIO is a promise to the haskell compiler that this is a safe operation
alpha :: Lexp -> IO Lexp
alpha (Atom v) =return (Atom v)                                                                     --Case of expression as v
alpha (Lambda (Atom x) e) = do                                                                      --Case of expression as \v.e
    unique <- newUnique
    let newVar = (Atom ("v" ++ show (hashUnique unique)))
    return (Lambda newVar (unsafePerformIO (alpha (replaceIn (unsafePerformIO (alpha (Atom x))) newVar e) )))
alpha (Lambda e1 e2) =                                                                                --Case of expression as \e1.e2
  return (Lambda (unsafePerformIO (alpha e1)) (unsafePerformIO (alpha e2)))
alpha (Apply e1 e2) = return  (Apply (unsafePerformIO (alpha e1) ) (unsafePerformIO (alpha e2)))    --Case of expression as (e1 e2)

-- Eta takes a lambda expression and eta converts it down to a simpler form
eta :: Lexp ->  Lexp
eta (Atom v) = Atom v                                                                               --Case of expression as v
eta lexp@(Lambda x (Apply e m))                                                                     --Case of expression as \x.(e1 e2)
  | x == m = e
  | otherwise = lexp
eta (Lambda x e) =  (Lambda x (eta e))                                                              --Case of expression as \v.e
eta (Apply x y) = Apply (eta x) (eta y)                                                             --Case of expression as (e1 e2)

-- Simplify applies alpha renameing, eta conversion, then beta reduction to an expression
simplify :: Lexp -> Lexp
simplify e =
   (\ x n -> iterate beta x !! n) (eta(unsafePerformIO (alpha e) )) 30



-- Entry point of program
main = do
    putStrLn "Please enter a filename containing lambda expressions:"
    fileName <- getLine
    -- id' simply returns its input, so runProgram will result
    -- in printing each lambda expression twice.
    runProgram fileName simplify
