import Data.List (foldl', break)
import Text.Pretty.Simple (pPrint)

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
    instrs <- lines <$> readFile "C:\\Users\\35850\\Desktop\\input.txt"
    let (filesystem, _) = topMost $ foldl' reconstruct (emptyFS, []) instrs
        solution = sum . filter (<= 100000) . snd $ scan filesystem
    pPrint filesystem
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