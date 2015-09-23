import Data.Unique

main = do
    unique <- newUnique
    print $ hashUnique unique

    unique <- newUnique
    print $ hashUnique unique

    unique <- newUnique
    print $ hashUnique unique
