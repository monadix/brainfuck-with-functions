import System.Environment
import Control.Monad
import Data.List
import Data.Char
import Data.Int
import Data.Bifunctor

emptyTape = (repeat 0, 0, repeat 0)

data Function = Function {actions :: [Action], innerFunctions :: [Function]} deriving Show

data Action = Action Char | Loop [Action] | FunctionCall Int deriving Show

type Tape = ([Int8], Int8, [Int8])

data Runtime = Runtime {tape :: Tape, functions :: [Function]} 

type Arr = (Int8, [Int8])

fmapAction :: ([Action] -> [Action]) -> Action -> Action
fmapAction f (Loop acts) = Loop $ f acts
fmapAction _ act = act

makeFunction :: String -> (Function, String)
makeFunction [] = error "Wrong brackets (function)"
makeFunction ('[':code) = (Function (loop:acts) funcs, code')
    where
        (loop, (Function acts funcs, code')) = fmap makeFunction $ makeLoop code
        makeLoop :: String -> (Action, String)
        makeLoop [] = error "Wrong brackets (loop)"
        makeLoop (sym:code) = case sym of
            '{' -> error "Function definition in a loop"
            '[' -> let (loop, code') = makeLoop code
                   in first (fmapAction (loop:)) $ makeLoop code'
            ']' -> (Loop [], code)
            '$' -> let (n, code') = first length $ span (=='`') code
                   in first (fmapAction (FunctionCall n:)) $ makeLoop code'
            _ -> first (fmapAction (Action sym:)) $ makeLoop code

makeFunction ('$':code) = (Function (FunctionCall funcNum : acts) funcs, code') where
    (funcNum, (Function acts funcs, code')) = bimap length makeFunction $ span (=='`') code

makeFunction (sym:code) = case sym of
    '{' -> let (func, (Function acts funcs, code')) = fmap makeFunction $ makeFunction code
           in (Function acts $ func:funcs, code')
    '}' -> (Function [] [], code)
    _ -> let (Function acts funcs, code') = makeFunction code
         in (Function (Action sym : acts) funcs, code')

callFunction :: Function -> Arr -> IO Arr
callFunction func@(Function acts funcs) (n, xs) = do
    let runtime = Runtime (repeat 0, n, xs ++ repeat 0) $ func:funcs
    (Runtime (_, ptr, right) _) <- interpret runtime acts
    return (ptr, genericTake ptr right)

makeMain :: String -> Function
makeMain = fst . makeFunction . (++"}") . filter ((`any` "><+-.,[]{}$`") . (==))

interpret :: Runtime -> [Action] -> IO Runtime
interpret = foldM doAction where
    doAction :: Runtime -> Action -> IO Runtime
    doAction runtime@(Runtime tape@(left@(l:left'), ptr, right@(r:right')) funcs) action = case action of
        Loop actions ->  while (\(Runtime (_, ptr, _) funcs) -> ptr /= 0) (`interpret` actions) runtime
        FunctionCall n -> do
            let (xs, rightRemaining) = genericSplitAt ptr right
            (k, ys) <- callFunction (funcs !! n) (ptr, xs)
            return $ Runtime (left, k, ys ++ rightRemaining) funcs
        Action act -> case act of
            '>' -> return $ Runtime (ptr:left, r, right') funcs
            '<' -> return $ Runtime (left', l, ptr:right) funcs
            '+' -> return $ Runtime (left, ptr+1, right) funcs
            '-' -> return $ Runtime (left, ptr-1, right) funcs
            '.' -> do
                let ptr' = fromIntegral ptr
                putChar $ chr $ ptr' + if ptr' < 0 then 128 else 0
                return runtime
            ',' -> do
                sym <- getChar
                return $ Runtime (left, fromIntegral $ ord sym, right) funcs
            _ -> error "A cho proiskhodit"

while :: (a -> Bool) -> (a -> IO a) -> a -> IO a
while p f x = if p x
    then do
        x' <- f x
        while p f x'
    else return x

main = do
    [fileName] <- getArgs
    file <- readFile fileName
    let mainF = makeMain file
    callFunction mainF (0, [])