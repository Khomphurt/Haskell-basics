-- HC13T1: List Files in Directory
listFiles :: IO [FilePath]
listFiles = listDirectory "."

main1 :: IO ()
main1 = do
    files <- listFiles
    putStrLn "Files in current directory:"
    mapM_ putStrLn files

-- HC13T2: Filter Files by Substring
filterFilesBySubstring :: String -> IO [FilePath]
filterFilesBySubstring sub = do
    files <- listDirectory "."
    return $ filter (isInfixOf sub) files

main2 :: IO ()
main2 = do
    putStrLn "Enter a substring to filter files:"
    sub <- getLine
    filtered <- filterFilesBySubstring sub
    putStrLn "Filtered files:"
    mapM_ putStrLn filtered

-- HC13T3: Sort and Return Filtered Files
sortedFilteredFiles :: String -> IO [FilePath]
sortedFilteredFiles sub = do
    files <- listDirectory "."
    return $ sort $ filter (isInfixOf sub) files

main3 :: IO ()
main3 = do
    putStrLn "Enter a substring to filter and sort files:"
    sub <- getLine
    sorted <- sortedFilteredFiles sub
    putStrLn "Sorted filtered files:"
    mapM_ putStrLn sorted

-- HC13T4 handled in SumNonEmpty module

-- HC13T5 handled in restricted export of SumNonEmpty

-- HC13T6: File Names to Map
fileNamesToMap :: [FilePath] -> Map.Map Int FilePath
fileNamesToMap files = Map.fromList $ zip [1..] files

main6 :: IO ()
main6 = do
    files <- listDirectory "."
    let fileMap = fileNamesToMap files
    putStrLn "File Map (Index -> Name):"
    mapM_ print (Map.toList fileMap)

-- HC13T7: Use Custom Module in Main
main7 :: IO ()
main7 = do
    putStrLn "Enter numbers separated by spaces:"
    nums <- fmap (map read . words) getLine
    print (SN.sumNonEmpty nums)

-- HC13T8: Qualified Imports for Name Conflicts
main8 :: IO ()
main8 = do
    let list1 = DL.sort [3,1,2]
    let dirFn = Dir.listDirectory
    putStrLn $ "Sorted list from Data.List: " ++ show list1
    files <- dirFn "."
    putStrLn "Files using qualified System.Directory:"
    mapM_ putStrLn files

-- HC13T9: Renaming Module Namespace
main9 :: IO ()
main9 = do
    let m = DM.fromList [(1, "a"), (2, "b")]
    print $ DM.lookup 2 m

-- HC13T10: Multi-Module Main Function
searchAndSortFiles :: String -> IO ()
searchAndSortFiles sub = do
    files <- Dir.listDirectory "."
    let result = DL.sort $ DL.filter (isInfixOf sub) files
    mapM_ putStrLn result

main10 :: IO ()
main10 = do
    putStrLn "Enter substring to search and sort files:"
    sub <- getLine
    searchAndSortFiles sub
