module ExIO where

import Prelude hiding
    ( putStr
    , putStrLn
    , getLine
    , interact
    , (>>)
    , (>>=)
    )

import System.IO
    ( hSetEcho,
      hGetEcho,
      stdin,
      stdout
    )

-- read through the whole module first, to get an idea
-- of what's required and to decide where to start

isEOL :: Char -> Bool
isEOL = ('\n' ==)

getLine :: IO String
getLine =
    do c <- getChar
       if isEOL c
           then return ""
           else do
               cs <- getLine
               return (c:cs)

getInt :: IO Int
getInt =
    do l <- getLine
       return (read l :: Int)

getSafeInt :: IO (Maybe Int)
getSafeInt =
    do l <- getLine
       let res = reads l :: [(Int, String)]
       case res of [(x,_)] -> return $ Just x
                   _       -> return Nothing

-- sequencing: first do f ignoring its result, then do g and keep its result
infixl 1 >>

(>>) :: IO a -> IO b -> IO b
ax >> ay =
    do ax
       ay

-- pauses till the user presses any normal key
pause :: IO ()
pause = void $ echoeless getChar

skip :: IO ()
skip = return ()

newline :: IO ()
newline = putChar '\n'

-- define it as a foldr
putStr :: String -> IO ()
putStr = undefined

-- transform f into one "just like f" except that it prints a newline
-- after any side-effects f may had
lnize :: (a -> IO b) -> a -> IO b
lnize f x =
    do x' <- f x
       newline
       return x'

putStrLn :: String -> IO ()
putStrLn = lnize putStr

putCharLn :: Char -> IO ()
putCharLn = lnize putChar

-- reads the entire user input as a single string, transforms it, and prints it
interact :: (String -> String) -> IO ()
interact = undefined

perlineize :: (String -> String) -> (String -> String)
perlineize f = unlines . map f . lines

interactPerLine :: (String -> String) -> IO ()
interactPerLine = interact . perlineize

when :: Bool -> IO () -> IO ()
when b ax = do if b then ax else skip

unless :: Bool -> IO () -> IO ()
unless = when . not

guard :: Bool -> IO ()
guard = undefined

forever :: IO a -> IO b
forever ax =
    do ax
       forever ax

-- transforms the action given to an equivalent one that has no result
void :: IO a -> IO ()
void = (>> skip)

echoeless :: IO a -> IO a
echoeless ax =
    do oldEcho <- hGetEcho stdin
       hSetEcho stdin False
       x <- ax
       hSetEcho stdin oldEcho
       return x

-- Kleisli compositions
infixr 1 >=>, <=<

-- diagrammatic order
(>=>) :: (a -> IO b) -> (b -> IO c) -> (a -> IO c)
(f >=> g) x =
    do x' <- f x
       g x'

-- traditional order
-- comparison of types:
-- (.)   :: (b ->    c) -> (a ->    b) -> a ->    c
-- (<=<) :: (b -> IO c) -> (a -> IO b) -> a -> IO c
(<=<) :: (b -> IO c) -> (a -> IO b) -> (a -> IO c)
(<=<) = flip (>=>)


-- Bind
infixl 1 >>=

(>>=) :: IO a -> (a -> IO b) -> IO b
ax >>= f =
    do x <- ax
       f x


infixl 4 $>, <$

-- make an action that has the side effects of the action on the left
-- but with result the value on the right
($>) :: IO a -> b -> IO b
ax $> y = ax >> return y

-- vice-versa
(<$) :: a -> IO b -> IO a
(<$) = flip ($>)

ap :: IO (a -> b) -> IO a -> IO b
af `ap` ax =
    do f <- af
       iomap f ax

filterIO :: (a -> IO Bool) -> [a] -> IO [a]
filterIO _ [] = return []
filterIO p (x:xs) =
    do b <- p x
       xs' <- filterIO p xs
       return (if b then x:xs' else xs')

iomap :: (a -> b) -> IO a -> IO b
iomap f ax = ax >>= (return . f)

mapIO :: (a -> IO b) -> [a] -> IO [b]
mapIO f = sequenceIO . map f

zipWithIO :: (a -> b -> IO c) -> [a] -> [b] -> IO [c]
zipWithIO = undefined

zipWithIO_ :: (a -> b -> IO c) -> [a] -> [b] -> IO ()
zipWithIO_ = undefined

sequenceIO :: [IO a] -> IO [a]
sequenceIO []       = return []
sequenceIO (ax:axs) = 
    do x <- ax
       xs <- sequenceIO axs
       return (x:xs)

sequenceIO_ :: [IO a] -> IO ()
sequenceIO_ = void . sequenceIO

replicateIO :: Integral i => i -> IO a -> IO [a]
replicateIO = undefined

replicateIO_ :: Integral i => i -> IO a -> IO ()
replicateIO_ = undefined

forIO :: [a] -> (a -> IO b) -> IO [b]
forIO = flip mapIO

forIO_ :: [a] -> (a -> IO b) -> IO ()
forIO_ xs = void . forIO xs

joinIO :: IO (IO a) -> IO a
joinIO = undefined

foldlIO :: (b -> a -> IO b) -> b -> [a] -> IO b
foldlIO = undefined

foldlIO_ :: (b -> a -> IO b) -> b -> [a] -> IO ()
foldlIO_ = undefined
