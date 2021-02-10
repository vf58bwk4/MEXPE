module Main where
import System.Environment
import Data.List
import Data.List.Split

main :: IO ()

myClassify2 pos
    | otherwise = "unspecified"
    where 

myClassify x
    | isPrefixedBy x "IP GULIEVA A.S." = "food"
    | isPrefixedBy x "APTEKA" = "health"
    | otherwise = "unspecified"
    where isPrefixedBy o p = isPrefixOf p o

myReformat = 
    intercalate ";" . (\(f1:f2:f3:_) -> [ddmm2mmdd f1,f2,f3, myClassify f2]) . splitOn "\t"
    where
        ddmm2mmdd = intercalate "/" . (\(dd:mm:xs) -> mm:dd:xs) . splitOn "/"
    
main = do
    [inF,outF] <- getArgs
    s <- readFile inF
    writeFile outF ((unlines . map myReformat . lines) s)

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
