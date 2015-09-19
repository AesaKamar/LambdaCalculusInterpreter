data Expression = Literal String
                | Lambda String Expression
                | Apply Expression Expression
                deriving Show

type Context = [(String, Expression)]

show' :: Expression -> String
show' (Literal a) = a
show' (Lambda x e) = "(λ " ++ x ++ ". " ++ show' e ++ ")"
show' (Apply e1 e2) = "(" ++ show' e1 ++ " " ++ show' e2 ++ ")"

eval :: Context -> Expression -> Expression
eval context e@(Literal a) = maybe e id (lookup a context)
eval context (Lambda x e) = Lambda x (eval context e)
eval context (Apply e1 e2) = apply context (eval context e1) (eval context e2)

apply :: Context -> Expression -> Expression -> Expression
apply context (Lambda x e) e2 = eval ((x, e2):context) e
apply context e1 e2 = Apply e1 e2

parse :: String -> (Expression, String)
parse ('(':'λ':s) = let
    (Literal a, s') = parse (tail s)
    (e, s'') = parse (drop 2 s')
    in (Lambda a e, tail s'')


parse ('(':s) = let
    (e1, s') = parse s
    (e2, s'') = parse (tail s')
    in (Apply e1 e2, tail s'')

parse (c:s) | elem c " .)" = (Literal "", c:s)
            | otherwise    = let ((Literal a), s') = parse s
                             in (Literal (c:a), s')

run :: String -> String
run = show' . eval [] . fst . parse
main = do
  line <- getLine
  putStrLn$ run line
