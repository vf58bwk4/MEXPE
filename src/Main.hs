module Main where
import System.Environment
import Data.List
import Data.List.Split

main :: IO ()

myClassify2 classifierDb x =
    case mbClassified of
        Just (prefix,classifier) -> classifier
        Nothing -> "unspecified"
    where
        mbClassified = find (\(prefix,_) ->  prefix `isPrefixOf` x) classifierDb

-- myClassify classifierDb x
--     | isPrefixedBy x "IP GULIEVA A.S." = "food"
--     | isPrefixedBy x "APTEKA" = "health"
--     | otherwise = "unspecified"
--     where isPrefixedBy o p = isPrefixOf p o

myReformat classifierDb = 
    (intercalate "," 
    . map (\ a -> "\"" ++ a ++ "\"")
    . (\(f1:f2:f3:_) -> [ddmm2mmdd f1,f2,f3,classified f2]) 
    -- . map ()
    . splitOn "\t"
    )
    where
        ddmm2mmdd   = intercalate "/" . (\(dd:mm:xs) -> mm:dd:xs) . splitOn "/"
        classified  = myClassify2 classifierDb
    
main = do
    [classifierDdF, inF,outF] <- getArgs

    c <- readFile classifierDdF
    let classifierDd = (map ((\(prefix:classifier:_) -> (prefix,classifier)) . splitOn "\t") . lines) c

    s <- readFile inF
    writeFile outF ((unlines . map (myReformat classifierDd) . lines) s)

    putStrLn "Done."

-- main = do
--   putStrLn "hello world"

-- main = interact id
-- count s = show (length s) ++ "\n"
-- main = interact count
-- main = interact (count . lines)
-- main = interact (unlines . map reverse . lines)

-- main = do
--     [inF,outF] <- getArgs
--     s <- readFile inF
--     writeFile outF s
