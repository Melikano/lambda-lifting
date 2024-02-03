{- Functions related to alpha renaming..
 - You should modify this file!
 -
 - See Section 2 of ./lambda-lift.pdf for more information..
 -}
module AlphaRename where

import           AST
import           Lib.Monads

import           ASTParse
import           ExamplePrograms

-- lift throwError (from Lib.Monads) in order to lift it to StateT
throwError' :: String -> StateT s (ExceptT String Identity) a
throwError' e = lift (throwError e)

-- my states are of type (Int, [(String, String)]) in which the Int is the counter which
-- counts the names in lookup table and [(String, String)] is the lookup table itself
-- I combined StateT with ExceptT in order to be able to throw errors
-- my error would be of type String so I used ExceptT String Identity
-- a is the type of the output in the state monad
type Renamer a = StateT (Int, LookupTable) (ExceptT String Identity) a
type LookupTable = [(String, String)]


-- run the state machine with (0, [functionNames]) as the initial state
-- I initialize my lookup table with outer functions names because in the test cases
-- then I extract the result of a computation that is wrapped in the Except monad
rename :: Prog String String -> Either String (Prog String String)
rename (Prog funcs) =
    case
            runExcept
                (evalStateT
                    (mapState renameFun funcs)
                    (0, map (\(Fun (name, _, _)) -> (name, name)) funcs)
                )
        of
            Left  error -> Left error
            Right funs  -> Right (Prog funs)

-- renameFunName is used to rename the function names in Let construct
renameFunName :: String -> Renamer String
renameFunName name = do
    -- get current state (counter and lookup table)
    (counter, names) <- get

    -- add one to counter (since we are adding a name to lookup table)
    let newCounter = counter + 1
    -- create new name by adding __ and counter as postfix
    let newName    = name ++ "__" ++ show counter
    -- setting the new state (newConter and lookup table with the new name added)
    set (newCounter, (name, newName) : names)

    -- return new name
    return newName

-- renameFun is used to rename function defintions (the Fun construct) 
renameFun :: Fun String String -> Renamer (Fun String String)
renameFun (Fun (name, args, body)) = do
    -- get current state (counter and lookup table)
    (counter, names) <- get
    -- this is because I add all names of functions to lookup table before
    -- that is because a function can be called inside a function that has been defined before that function in the program
    -- so by adding all names, I can figure out if they really exist or not
    -- and throw exception only if they don't

    -- lookup function name (it definitely exists in the lookup table)
    let newName = case lookup name names of
            Nothing   -> name
            (Just nm) -> nm
    -- rename args
    newArgs <- mapState
        (\arg -> do
            (counter, names) <- get
            let cntr   = counter + 1
            let newArg = arg ++ "__" ++ show counter
            set (cntr, (arg, newArg) : names)
            return newArg
        )
        args
    -- recursively rename body
    newBody <- renameExp body
    -- reset the lookup table to original
    modify (mapSnd (const names))
    -- return renamed fun
    return (Fun (newName, newArgs, newBody))

renameExp :: Exp String String -> Renamer (Exp String String)
renameExp (VAR x) = do
    -- get lookup table
    (_, names) <- get
    -- then lookup the variable name
    -- if the name of the variable is not added before, then this is an issue
    -- so I throw exception
    case lookup x names of
        Nothing         -> throwError' ("Could not lookup variable: " ++ x)
        (Just new_name) -> return (VAR new_name)
-- const doesn't need any renaming!
renameExp (CONST x) = do
    return (CONST x)
-- for COND, I rename ifExp which is a boolean exp using renameBExp
-- and rename thenExp and elseExp using renameExp
-- return COND with renamed expressions
renameExp (COND ifExp thenExp elseExp) = do
    newIfExp   <- renameBExp ifExp
    newThenExp <- renameExp thenExp
    newElseExp <- renameExp elseExp
    return (COND newIfExp newThenExp newElseExp)
-- for NEG, I just rename it's only expression and return it wrapped with NEG
renameExp (NEG exp) = do
    newExp <- renameExp exp
    return (NEG newExp)
-- for APP, I first lookup the name of the function which is applied and rename the expression recursively
-- then return APP with retrieved name and renamed expressions
renameExp (APP funcName exprs) = do
    -- get lookup table
    (_, names) <- get
    -- then lookup the function name
    -- if the name of the function is not added before, then this is an issue
    -- so I throw exception
    fn         <- case lookup funcName names of
        Nothing -> throwError' ("Could not lookup variable: " ++ funcName)
        (Just new_name) -> return new_name
    newExprs <- mapState renameExp exprs
    return (APP fn newExprs)
-- for LET, I add names of the functions defined in let to the lookup table
-- that is because a function can be called inside a function that has been defined before that function in the program
-- so by adding all names, I can figure out if they really exist or not
-- and throw exception only if they don't
-- then I rename functions and expression and return let with renamed funcs and expression
renameExp (LET funcs expr) = do
    newFunsNames <- mapState (\(Fun (name, _, _)) -> renameFunName name) funcs
    newFuns      <- mapState renameFun funcs
    newExpr      <- renameExp expr
    return (LET newFuns newExpr)
-- renameExp is similar for ADD, SUB, MUL and DIV
-- I just recursively rename their expressions, and return the new ADD/SUB/MUL/DIV with renamed expressions
renameExp (ADD left right) = do
    newLeft  <- renameExp left
    newRight <- renameExp right
    return (ADD newLeft newRight)
renameExp (SUB left right) = do
    newLeft  <- renameExp left
    newRight <- renameExp right
    return (SUB newLeft newRight)
renameExp (MUL left right) = do
    newLeft  <- renameExp left
    newRight <- renameExp right
    return (MUL newLeft newRight)
renameExp (DIV left right) = do
    newLeft  <- renameExp left
    newRight <- renameExp right
    return (DIV newLeft newRight)

-- for boolean expressions, I just called renameExp/renameBExp for their inside (boolean)expressions
renameBExp :: BExp String String -> Renamer (BExp String String)
renameBExp (Lt left right) = do
    newLeft  <- renameExp left
    newRight <- renameExp right
    return (Lt newLeft newRight)
renameBExp (Gt left right) = do
    newLeft  <- renameExp left
    newRight <- renameExp right
    return (Gt newLeft newRight)
renameBExp (Eq left right) = do
    newLeft  <- renameExp left
    newRight <- renameExp right
    return (Eq newLeft newRight)
renameBExp (AND left right) = do
    newLeft  <- renameBExp left
    newRight <- renameBExp right
    return (AND newLeft newRight)
renameBExp (OR left right) = do
    newLeft  <- renameBExp left
    newRight <- renameBExp right
    return (OR newLeft newRight)
renameBExp (NOT exp) = do
    newExp <- renameBExp exp
    return (NOT newExp)

-- mapState takes a function of type f::a -> StateT s m b and a list of [a] and applies f on each element of the list and returns StateT s m [b]
mapState :: Monad m => (a -> StateT s m b) -> [a] -> StateT s m [b]
mapState f []       = return []
mapState f (x : xs) = do
    y  <- f x
    ys <- mapState f xs
    return (y : ys)

-- map first and map second functions
mapFst :: (a -> b) -> (a, c) -> (b, c)
mapFst f (x, y) = (f x, y)
mapSnd :: (a -> b) -> (c, a) -> (c, b)
mapSnd f (x, y) = (x, f y)
