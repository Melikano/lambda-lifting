module ASTParse where

import Lib.Parser
import Lib.Monads
import AST

import Data.List
import Control.Applicative 
import Control.Arrow (first, second)

{- A parser for the AST.
 -
 - Either call: 
 -      ``unsafeParseProg inputString``
 - or
 -      ``parseProg inputString``
 -
 - to parse a program.
 -
 - Known issues:
 -      - It is a bit slow on large inputs...
 -
 - This is provided FOR CONVENIENCE ONLY and may or may not have bugs
 - in it. 
 -}

unsafeParseProg :: String -> Prog String String
unsafeParseProg inp = case parseProg inp of
    Right prog -> prog
    Left err -> error err

parseProg :: String -> Either String (Prog String String)
parseProg inp = Prog <$> parseFuns inp

---------------------------------------------------------

junk :: Parser ()
junk = (localDefRowCol (const (-1,-1)) 
    $ firstParse 
    $ many (comment <|> multilineComment <|> spaces)) >> return ()

comment :: Parser ()
comment = (string "--" >> many (sat (/='\n')) >> return ())

multilineComment :: Parser ()
multilineComment = (string "{-" >> many (sat (const True)) >> string "-}" >> return ())

token :: Parser a -> Parser a
token pa = pa <* junk

symbol :: String -> Parser String
symbol str = string str <* junk

natural :: Parser Int
natural = nat <* junk

keywords :: [String]
keywords = ["let", "in", "fun", "if", "then", "else", "end", "not"]

identifier :: Parser String
identifier = token $ do
    c <- lower
    cs <- firstParse (many (alphaNum <|> char '_'))
    let ident = c:cs
    if ident `notElem` keywords
        then return ident
        else empty

parseFuns :: String -> Either String [Fun String String]
parseFuns inp = case runParser pa inp of
    [Right (vals, (_, ""))] -> return vals
    [Right (_, (loc, rst))]   -> Left $ concat
        [ "Could not parse input at " 
        , "row " 
        , show (fst loc)
        , " and column " 
        , show (snd loc)
        , " with inputs:\n" 
        , rst
        ]
    [Left err]         -> Left $ err
    []                 -> Left $ "Invalid parse"
    as                 -> Left $ "An unknown error occured. Here are all the parse attempts: " ++ show as
  where
    pa = junk >> offsideMany fun


fun :: Parser (Fun String String)
fun = do
    symbol "fun" <?> "Expected 'fun' keyword at start of function declaration."
    fname <- identifier <?> "Expected function identifier after 'fun' keyword."
    symbol "(" <?> concat 
        [ "Expected '(' after function identifier before arguments. "
        , "Perhaps you meant to write: ``"
        , "fun " ++ fname ++ "(<arguments>) = <function body>``"
        ]
    args <- (do
        arg <- identifier
        args <- firstParse (many (symbol "," >> identifier))
        return $ arg:args
        ) <|> return []
    symbol ")" <?> "Expected ')' after function arguments before function definition."
    symbol "=" <?> "Expected '=' sign after arguments."
    expr' <- expr
    return $ Fun (fname, args, expr')


expr :: Parser (Exp String String)
expr = chainl1 expr0 ((symbol "+" >> pure ADD) <|> (symbol "-" >> pure SUB))

expr0 :: Parser (Exp String String)
expr0 = chainl1 expr1 ((symbol "*" >> pure MUL) <|> (symbol "/" >> pure DIV))

expr1 :: Parser (Exp String String)
expr1 = firstParse $ (CONST <$> natural)
    <|> varfun
    <|> cond
    <|> letbind
    <|> neg
    <|> surrounded (symbol "(") expr (symbol ")")

neg :: Parser (Exp String String)
neg = do
    symbol "-"
    expr' <- expr
    return $ NEG expr'


cond :: Parser (Exp String String)
cond = do
    symbol "if"
    bexpr' <- bexpr 
    symbol "then" <?> "Expected 'then'."
    expr' <- expr
    symbol "else" <?> "Expected 'else'."
    expr'' <- expr
    return $ COND bexpr' expr' expr''

data AppParse 
    = VarParse
    | ArgedFunParse [Exp String String]

varfun :: Parser (Exp String String)
varfun = do
    f' <- fmap f $ do 
        ident <- identifier
        -- A very cheap hack to help give informative error messages for some cases when you 
        -- forget that function application isn't space...
        idents' <- many (identifier <|> (show <$> natural) <|> (show <$> (symbol "-" >> natural )))
        if null idents' 
            then return ident
            else Parser $ lift $ lift $ throwError $ concat
                [ "Illegal expression: ``"
                , intercalate " " $ ident : idents'
                , "``. Perhaps you meant to use function application, so write: "
                , ident ++ "(" ++ intercalate "," idents' ++ ")"
                ]
    argparse <- argparse
    return $ f' argparse
  where
    f name VarParse = VAR name
    f name (ArgedFunParse args) = APP name args

argparse :: Parser AppParse
argparse = 
    (symbol "(" >> argedfunparse)
    <|> varparse
  where
    argedfunparse = (do
            expr' <- expr
            exprs' <- many $ do 
                symbol "," 
                expr <?> "Expected comma seperated arguments in function call."
            symbol ")"
            return $ ArgedFunParse $ expr' : exprs'
        )
        <|> (symbol ")" >> return (ArgedFunParse []))
    varparse = return VarParse
    
letbind :: Parser (Exp String String)
letbind = do
    symbol "let"
    funs <- offsideSome fun 
    symbol "in" <?> "Expected 'in' keyword to seperate let declarations and body"
    expr' <- expr
    symbol "end" <?> "Expected 'end' keyword after function let statement"
    return $ LET funs expr'

bexpr :: Parser (BExp String String)
bexpr = chainr1 bterm0 (symbol "||" >> pure OR)
  where
    bterm0 :: Parser (BExp String String)
    bterm0 = chainr1 bterm1 (symbol "&&" >> pure AND)

    bterm1 :: Parser (BExp String String)
    bterm1 = 
        (symbol "not" >> surrounded (symbol "(") bexpr (symbol ")"))
        <|> (do l <- expr ; _ <- symbol "<" ; r <- expr ; return $ Lt l r)
        <|> (do l <- expr ; _ <- symbol ">" ; r <- expr ; return $ Gt l r)
        <|> (do l <- expr ; _ <- symbol "==" ; r <- expr ; return $ Eq l r)
        <|> surrounded (symbol "(") bexpr (symbol ")")

