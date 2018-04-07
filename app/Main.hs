{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Monad.State.Strict
import qualified Data.Vector as V
import Control.Lens
import System.Random
import Data.Char

data Direction = L | R | U | D deriving (Eq, Show)
moveDirection :: Direction -> (Int, Int) -> (Int, Int)
moveDirection L (x, y) = (x-1, y)
moveDirection R (x, y) = (x+1, y)
moveDirection U (x, y) = (x, y-1)
moveDirection D (x, y) = (x, y+1)


data InterpreterState = InterpreterState
    {
        _program :: V.Vector (V.Vector Char),
        _dimensions :: (Int, Int),
        _position :: (Int, Int),
        _direction :: Direction,
        _stack :: [Integer],
        _randomDirections :: [Direction],
        _halt :: Bool,
        _output :: [String]
    }
makeLenses ''InterpreterState

type Interpreter a = State InterpreterState a

ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM p x y = p >>= \b -> if b then x else y

discardM :: Monad m => m a -> m ()
discardM = (>> return ())

initialState :: String -> StdGen -> InterpreterState
initialState grid gen = InterpreterState
    {
        _program = p,
        _dimensions = (V.length $ p V.! 0, V.length p),
        _position = (0, 0),
        _direction = R,
        _stack = [],
        _randomDirections = map ([L,R,U,D] !!) $ randomRs (0,3) gen, -- Generate an infinite list of random directions
        _halt = False,
        _output = [] 
    }
    where rows = lines grid
          longest = maximum $ map length rows
          p = V.fromList (map (V.fromListN longest) $ map (++ repeat ' ') rows) -- Pad short rows

getInstrAt :: (Int, Int) -> Interpreter Char
getInstrAt (x, y) = do
        board <- gets _program
        return $ (board V.! y) V.! x

setInstrAt :: (Int, Int) -> Char -> Interpreter ()
setInstrAt (x, y) v = modify (over program $ \b -> b V.// [(y, (b V.! y) V.// [(x, v)])])

getInstr :: Interpreter Char
getInstr = gets _position >>= getInstrAt

pop :: Interpreter Integer
pop = state (\st -> (head $ _stack st, over stack tail st))

stackEmpty :: Interpreter Bool
stackEmpty = gets (null . _stack)

stackSwap :: Interpreter ()
stackSwap = do
            x <- pop
            y <- ifM stackEmpty (return 0) pop
            push x >> push y

push :: Integer -> Interpreter ()
push x = modify $ over stack (x:)

uop :: (Integer -> Integer) -> Interpreter ()
uop f = f <$> pop >>= push
bop :: (Integer -> Integer -> Integer) -> Interpreter ()
bop f = f <$> pop <*> pop >>= push

setDirection :: Direction -> Interpreter ()
setDirection d = modify (set direction d)

setRandomDirection :: Interpreter ()
setRandomDirection = do
        (d:t) <- gets _randomDirections
        modify (set direction d . set randomDirections t)

move :: Interpreter ()
move = do
        (x,y) <- moveDirection <$> gets _direction <*> gets _position
        (w,h) <- gets _dimensions
        let pos = ((x + w) `mod` w, (y + h) `mod` h)
        modify (set position pos)

addOutput :: String -> Interpreter ()
addOutput s = modify (over output (s:))

terminate :: Interpreter ()
terminate = modify (set halt True)

while :: Interpreter Bool -> Interpreter () -> Interpreter ()
while p a = p >>= \b -> when b (a >> while p a)
doWhile :: Interpreter Bool -> Interpreter () -> Interpreter ()
doWhile p a = a >> while p a

putFromStack :: Interpreter ()
putFromStack = do
        let pfi = fromInteger <$> pop
        (y,x,v) <- (,,) <$> pfi <*> pfi <*> pfi
        setInstrAt (x, y) (chr v)

getFromStack :: Interpreter ()
getFromStack = do
        let pfi = fromInteger <$> pop
        (y,x) <- (,) <$> pfi <*> pfi
        push . toInteger . ord =<< getInstrAt (x,y)


singleStep :: Interpreter ()
singleStep = getInstr >>= f
    where
        f c
            | c `elem` ['0'..'9'] = push $ toInteger $ digitToInt c
            | c == '+'  = bop (+)
            | c == '-'  = bop (flip (-))
            | c == '*'  = bop (*)
            | c == '/'  = bop (\a b -> if a == 0 then 0 else b `div` a)
            | c == '%'  = bop (\a b -> if a == 0 then 0 else b `rem` a)
            | c == '!'  = uop (\a -> if a == 0 then 1 else 0)
            | c == '`'  = bop (\a b -> if b > a then 1 else 0)
            | c == '>'  = setDirection R
            | c == '<'  = setDirection L
            | c == '^'  = setDirection U
            | c == 'v'  = setDirection D
            | c == '?'  = setRandomDirection
            | c == '_'  = pop >>= \v -> if v == 0 then setDirection R else setDirection L
            | c == '|'  = pop >>= \v -> if v == 0 then setDirection D else setDirection U
            | c == '"'  = move >> while ((/= '"') <$> getInstr) (getInstr >>= push . toInteger . ord >> move)
            | c == ':'  = ifM stackEmpty (push 0) (pop >>= \v -> push v >> push v)
            | c == '\\' = stackSwap
            | c == '$'  = discardM pop
            | c == '.'  = addOutput . show =<< pop
            | c == ','  = addOutput . (:"") . chr . fromInteger =<< pop
            | c == '#'  = move
            | c == 'p'  = putFromStack
            | c == 'g'  = getFromStack
            | c == '@'  = terminate
            | c == ' '  = return ()
            | otherwise = error "Unrecognised command"


interpret :: StdGen -> String -> String
interpret gen board = concat $ reverse $ _output $ execState a (initialState board gen)
    where
        a = while (gets $ fmap not _halt) (singleStep >> move)

main :: IO ()
main = do
        stdGen <- getStdGen
        putStrLn $ interpret stdGen str
    where
        str = ">25*\"!dlroW olleH\":v\n                v:,_@\n                >  ^"