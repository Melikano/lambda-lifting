module Lib.Parser where

import Lib.Monads
import Control.Applicative

----------------------------------------
-- Basic types and definitions
----------------------------------------
type RowCol = (Int, Int)
type LocatedString = (RowCol, String)
type ErrorMsg = String

-- RowCol -> LocatedString -> [Either ErrorMsg (a, LocatedString)]
newtype Parser a = 
    Parser { unwrapParser ::
                (ReaderT RowCol 
                (StateT LocatedString 
                (ExceptT ErrorMsg [])) a)
        }

instance Functor Parser where
    fmap f (Parser a) = Parser (fmap f a)

instance Applicative Parser where
    pure a = Parser (pure a)
    Parser f <*> Parser a = Parser (f <*> a)

instance Monad Parser where
    return = pure

    Parser ma >>= f = Parser (ma >>= f')
      where 
        f' a = case f a of
            Parser a -> a

instance Alternative Parser where
    empty = Parser empty 

    -- deterministic choice
    Parser f <|> Parser g = firstParse $ Parser (f <|> g)

startCol :: Int
startCol = 1

startRow :: Int
startRow = 1

tabDistance :: Int
tabDistance = 4

startRowCol :: RowCol
startRowCol = (startCol, startRow)

runParser :: 
    Parser a -> 
    String -> 
    [Either ErrorMsg (a, LocatedString)]
runParser (Parser pa) inp = runExceptT $ runStateT (runReaderT pa startRowCol) (startRowCol, inp)

----------------------------------------
-- Wrappers for the monad transformer..
----------------------------------------
askDefRowCol :: Parser RowCol
askDefRowCol = Parser ask

localDefRowCol :: (RowCol -> RowCol) -> Parser a -> Parser a
localDefRowCol f (Parser pa) = Parser $ local f pa

getCurrInput :: Parser LocatedString
getCurrInput = Parser (lift get)

setCurrInput :: LocatedString -> Parser ()
setCurrInput inp = Parser (lift $ set inp)


----------------------------------------
-- Parser utilities
----------------------------------------

{- Get 1 item in the parser
 -}
item :: Parser Char
item = do
    defRowCol <- askDefRowCol
    ((curRowCol), inp) <- getCurrInput
    case inp of
        x:xs -> do
            setCurrInput (newRowCol curRowCol x, xs)
            if onSide defRowCol curRowCol
                then return x
                else empty
        [] -> empty
  where
    newRowCol :: RowCol -> Char -> RowCol
    newRowCol (r,c) ch = case ch of
        '\n' -> (succ r, startCol)
        '\t' -> (r, c + tabDistance )
        _    -> (r, succ c)

    onSide :: RowCol -> RowCol -> Bool
    onSide (dr, dc) (r, c) = c > dc || r == dr
    -- onSide if the column is strictly greater than 
    -- definition.. OR
    -- A special case of starting a new definition on 
    -- a new column i.e., it is the first so the row
    -- is the same...

-----
-- Basic parsing primitives
-----
sat :: (Char -> Bool) -> Parser Char
sat p = item >>= \c -> if p c then return c else empty

char :: Char -> Parser Char
char c = sat (==c)

digit :: Parser Char
digit = sat (\c -> '0' <= c && c <= '9' )

upper :: Parser Char
upper = sat (\c -> 'A' <= c && c <= 'Z')

lower :: Parser Char
lower = sat (\c -> 'a' <= c && c <= 'z')

alpha :: Parser Char
alpha = upper <|> lower 

alphaNum :: Parser Char
alphaNum = alpha <|> digit 

spaces :: Parser ()
spaces = some (sat f) >> return ()
  where
    f a = or $ map ($ a) [(=='\n'), (=='\t'), (==' ')]

string :: String -> Parser String
string str = firstParse (traverse char str)

nat :: Parser Int
nat = do 
    dgts <- firstParse (some digit)
    return $ read dgts

int :: Parser Int
int = do 
    sgn <- (char '-' >> return negate) <|> return id
    nat' <- nat
    return $ sgn nat'

surrounded :: Parser l -> Parser a -> Parser r -> Parser a
surrounded pl pa pr = pl *> pa <* pr

eof :: Parser ()
eof = do
    (_, res) <- getCurrInput
    if null res 
        then return ()
        else empty

-----
-- More involved parsing primitives
-----
firstParse :: Parser a -> Parser a
firstParse (Parser pa) = Parser $ ReaderT $ \r -> StateT $ \s -> 
    ExceptT $ fmap f runExceptT (runStateT (runReaderT pa r) s) 
  where
    f (x:_) = [x]
    f [] = []

catchParserError :: Parser a -> (RowCol -> ErrorMsg) -> Parser a
catchParserError (Parser pa) err = Parser $ ReaderT $ \r -> StateT $ \s@(pos, _) -> 
    let f [] = [Left (err pos)]
        f xs = xs
    in ExceptT $ f $ runExceptT (runStateT (runReaderT pa r) s) 

(<?>) :: Parser a -> ErrorMsg  -> Parser a
(<?>) pa str = catchParserError pa $ \(r,c) -> concat 
    ["Parse error at row " ++ show r ++ " and column " ++ show c ++ ": " ++ str]

-----
-- Basic parsing combinators
-----
chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainl1 pa pop = firstParse (pa >>= f)
  where
    f acc = (do
        op <- pop
        a <- pa
        f (acc `op` a)) <|> return acc

chainr1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainr1 pa pop = firstParse (pa >>= f)
  where
    f a = (do
        op <- pop
        b <- chainr1 pa pop
        return $ a `op` b
        ) <|> return a

offsideSome :: Parser a -> Parser [a]
offsideSome pa = do
    (curPos, _) <- getCurrInput
    localDefRowCol (const curPos) (some pa') 
  where
    -- For new definitions (which are on
    -- the same column), we need to set the
    -- row for which this definition is on.
    -- pa' :: Parser a -> Parser a
    pa' = do
        (dr, dc) <- askDefRowCol
        --  We trade these lines 
        --  because testing if it is not null
        --  will never run the parser on the input... 
        --  which this helps for error messages in 
        --  A2 so it never runs the parser at the 
        --  end of a file incorrectly saying
        --  it is a parse error, and can 
        --  correctly tell the user they are missing
        --  a fun keyword..
        -- ((r,c), _) <- getCurrInput
        -- if c == dc
        --
        ((r,c), inp) <- getCurrInput
        if c == dc && not (null inp)
        --
            then localDefRowCol (const (r,dc)) pa
            else empty

offsideMany :: Parser a -> Parser [a]
offsideMany pa = offsideSome pa <|> return []
