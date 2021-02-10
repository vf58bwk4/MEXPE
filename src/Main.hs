module Main where
import System.Environment
import Data.List
import Data.List.Split

main :: IO ()

myClassify x
    | isPrefixOf "IP GULIEVA A.S." x = "food"
    | isPrefixOf "APTEKA" x = "health"
    | otherwise = "unspecified"

convertDDMM2MMDD = 
    intercalate "/" . (\(dd:mm:xs) -> mm:dd:xs) . splitOn "/"

myReformat = 
    intercalate ";" . (\(f1:f2:f3:_) -> [convertDDMM2MMDD f1,f2,f3, myClassify f2]) . splitOn "\t"
    
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
