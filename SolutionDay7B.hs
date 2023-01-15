import Data.List (foldl', break)
import Text.Pretty.Simple (pPrint) 
-- The only external dependency, only necessary if you want to prettyprint the filesystem.

type Instruction = String
type Name = String
type Data = String
data FSItem = File Name Data | Folder Name [FSItem] deriving (Eq, Show)
data FSCrumb = FSCrumb Name [FSItem] [FSItem] deriving Show
type FSZipper = (FSItem, [FSCrumb])

emptyFS :: FSItem
emptyFS = Folder "" []

main :: IO ()
main = do
    instrs <- lines <$> readFile "Path\\File.txt"
    let (filesystem, _) = topMost $ foldl' reconstruct (emptyFS, []) instrs

        capacity = 70000000
        requiredSpace = 30000000

        (memoryUsed, folderSizes) = scan filesystem
        memoryLeft = capacity - memoryUsed

        toBeDeleted = requiredSpace - memoryLeft

        bigFolders = [f | f <- folderSizes, 
                          f > toBeDeleted]
        
        solution = minimum bigFolders

  --  pPrint filesystem
    print solution

reconstruct :: FSZipper -> Instruction -> FSZipper
reconstruct zipper@(Folder _ items, _) instruction = 
    case words instruction of ["$", "cd", ".."] -> moveTo ".." zipper
                              ["$", "cd", name] | elem (Folder name []) items -> moveTo name zipper
                                                | otherwise -> moveTo name $ create (Folder name []) zipper
                              ["$", "ls"] -> zipper
                              ["dir", name] -> create (Folder name []) zipper
                              [content, name]  -> create (File name content) zipper
                                                      
moveTo :: Name -> FSZipper -> FSZipper
moveTo ".." (item, FSCrumb name ls rs:bs) = (Folder name (ls ++ [item] ++ rs), bs)
moveTo name (Folder folderName items, bs) =
    let (ls, item:rs) = break (nameIs name) items
    in (item, FSCrumb folderName ls rs:bs)

nameIs :: Name -> FSItem -> Bool
nameIs name (Folder folderName _) = name == folderName
nameIs name (File fileName _) = name == fileName

create :: FSItem -> FSZipper -> FSZipper
create item (Folder folderName items, bs) = (Folder folderName (item:items), bs)

topMost :: FSZipper -> FSZipper
topMost (item, []) = (item, [])
topMost zipper = topMost (moveTo ".." zipper)

scan :: FSItem -> (Int, [Int])
scan (File fileName content) = (read content, [])
scan (Folder folderName items) = 
    let (memory, folderSizes) = unzip $ map scan items
        size = sum memory
        folderSizes' = concat folderSizes
    in (size, size : folderSizes')