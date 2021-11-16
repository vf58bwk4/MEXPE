module Main where
import System.Environment
import Data.List
import Data.List.Split

main :: IO ()

main = do
    [cDBF, inF,outF] <- getArgs

    c <- readFile cDBF
    s <- readFile inF
    let r = unlines . map (formatReportLine . loadCDB $ c) . lines . skipBOM $ s
    writeFile outF r

    putStrLn "Done."

myClassify2 cDB x =
    case mbClassified of
        Just (_,classifier) -> classifier
        Nothing -> "unspecified"
    where
        mbClassified = find (\ (prefix, _) ->  prefix `isPrefixOf` x) cDB

myClassify3 cDB x =
    case classifiers of
        [] -> "unspecified"
        (classifier:_) -> classifier
    where
        classifiers = [classifier | (prefix,classifier) <- cDB, prefix `isPrefixOf` x]

formatReportLine cDB = 
    intercalate "," 
        . map (\ a -> "\"" ++ a ++ "\"")
        . (\ (f1:f2:f3:_) -> [ddmm2mmdd f1,f2,f3,classified f2])
        -- . splitOn "\t"
        . parseCSVLine
    where
        ddmm2mmdd   = intercalate "/" . (\ (dd:mm:xs) -> mm:dd:xs) . splitOn "/"
        classified  = myClassify3 cDB

loadCDB =
    map parseDbLine . lines
    where 
        parseDbLine = (\ (prefix:classifier:_) -> (prefix,classifier)) . splitOn "\t"

skipBOM ('\xEF':'\xBB':'\xBF':cs) = cs
skipBOM (_:_:_:cs) = cs
skipBOM cs = cs

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

parseCSVLine s = 
    reverse $ parseQuotedFields [] s
    where
        parseQuotedFields acc input = -- returns inverted list!
            case input of
                [] -> acc
                ('"':inputTail) -> 
                    let (left, right) = (\ (l, r) -> (reverse l, r)) $ skipTo [] inputTail in
                    case right of
                        []  -> parseQuotedFields (left:acc) [] -- end of line
                        (',':rightTail) -> parseQuotedFields (left:acc) rightTail -- end of field
                        _ -> error $ "Wrong fields delimiter starting at " ++ right
                _ -> error $ "Field must be enclosed in '\"': " ++ s
        
        skipTo left right = -- returns inverted list!
            case right of
                [] -> error $ "Last field doesn't have closed '\"' " ++ s
                ('"':xs) -> (left, xs) -- normal stop
                (x:xs) -> skipTo (x:left) xs
