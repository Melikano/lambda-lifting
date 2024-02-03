{- Functions related to making the call graph..
 - You should modify this file!
 -
 - See Section 3 and the start of Section 4 of ./lambda-lift.pdf for more information..
 -}
module CallGraph where

import           AST
import           ASTParse
import           AlphaRename
import           ExamplePrograms
import           Lib.Monads

-- function to push something to a stack
push :: a -> [a] -> [a]
push x xs = x : xs

-- function to pop the head of stack and return it along with the new stack
pop :: [a] -> Maybe (a, [a])
pop (x : xs) = Just (x, xs)
pop []       = Nothing

-- function that removes redundant elements of a list and returns a list with unique elements
unique :: Eq a => [a] -> [a]
unique [] = []
unique (x : xs) | x `elem` xs = unique xs
                | otherwise   = x : unique xs

-- my state is of type [String] (lets say call stack)
type CallGraphCreator a = State CallStack a
type FreeVars = [String]
type Args = [String]
type CallingFuncs = [String]
type CallStack = [String]
-- CallGraphNode -> (function name, arguments, freeVariables, functions that this function calls)
type CallGraphNode = (String, Args, FreeVars, CallingFuncs)
-- just list of callGraphNodes
type CallGraph = [CallGraphNode]

-- run state machine with [] as initial state and get the output (evalState)
createCallGraph :: Prog String String -> [CallGraph]
createCallGraph (Prog funcs) = evalState (mapState funCallGraph funcs) []


funCallGraph :: Fun String String -> CallGraphCreator CallGraph
funCallGraph (Fun (name, args, body)) = do
    -- get current state (stack)
    stack <- get
    -- keep length of the stack as the starting point of the current function
    -- we need to pop stack to this point in order to build call graph of this function
    let start = length stack
    -- get callGraph and free variables of the body
    (bodyGraph, freeVars) <- expCallGraph body
    -- get updated stack
    stack                 <- get
    -- create graph for current function using starting point and updated stack
    -- pop all function related to this function and set the new stack
    let (newStack, callingFuncs) = createGraph start stack
    set newStack
    -- add callGraph of this function to callGraph of the body
    return ((name, args, unique freeVars, callingFuncs) : bodyGraph)

expCallGraph :: Exp String String -> CallGraphCreator (CallGraph, FreeVars)
-- call graph for VAR x is empty list and free variable is x
expCallGraph (VAR x) = do
    return ([], [x])
-- call graph for CONST x is empty list and there is no free variables
expCallGraph (CONST x) = do
    return ([], [])
expCallGraph (APP f exps) = do
    -- get stack
    stack <- get
    -- push f to call stack
    set (push f stack)
    -- get callGraphs and free variables of expressions
    expsRes <- mapState expCallGraph exps
    -- append call graphs and free variables of all expressions
    return (foldr (\(a, b) (c, d) -> (a ++ c, b ++ d)) ([], []) expsRes)
expCallGraph (LET funcs exp) = do
    --get callGraphs of funcs
    funcsGraphs             <- concatState (mapState funCallGraph funcs)
    -- get call graph and free variables of the expression
    (expGraph, expFreeVars) <- expCallGraph exp
    -- append call graph of expression and functions together
    return (expGraph ++ funcsGraphs, expFreeVars)

-- for the rest I just got call graphs and free variables of the expressions and appended them together
expCallGraph (ADD left right) = do
    (leftCallGraph , leftFreeVars ) <- expCallGraph left
    (rightCallGraph, rightFreeVars) <- expCallGraph right
    return (leftCallGraph ++ rightCallGraph, leftFreeVars ++ rightFreeVars)
expCallGraph (SUB left right) = do
    (leftCallGraph , leftFreeVars ) <- expCallGraph left
    (rightCallGraph, rightFreeVars) <- expCallGraph right
    return (leftCallGraph ++ rightCallGraph, leftFreeVars ++ rightFreeVars)
expCallGraph (MUL left right) = do
    (leftCallGraph , leftFreeVars ) <- expCallGraph left
    (rightCallGraph, rightFreeVars) <- expCallGraph right
    return (leftCallGraph ++ rightCallGraph, leftFreeVars ++ rightFreeVars)
expCallGraph (DIV left right) = do
    (leftCallGraph , leftFreeVars ) <- expCallGraph left
    (rightCallGraph, rightFreeVars) <- expCallGraph right
    return (leftCallGraph ++ rightCallGraph, leftFreeVars ++ rightFreeVars)
expCallGraph (NEG exp) = do
    expCallGraph exp
expCallGraph (COND ifExp thenExp elseExp) = do
    (leftCallGraph , leftFreeVars ) <- expCallGraph thenExp
    (rightCallGraph, rightFreeVars) <- expCallGraph elseExp
    return (leftCallGraph ++ rightCallGraph, leftFreeVars ++ rightFreeVars)

bExpCallGraph :: BExp String String -> CallGraphCreator (CallGraph, FreeVars)
bExpCallGraph (Lt left right) = do
    (leftCallGraph , leftFreeVars ) <- expCallGraph left
    (rightCallGraph, rightFreeVars) <- expCallGraph right
    return (leftCallGraph ++ rightCallGraph, leftFreeVars ++ rightFreeVars)
bExpCallGraph (Gt left right) = do
    (leftCallGraph , leftFreeVars ) <- expCallGraph left
    (rightCallGraph, rightFreeVars) <- expCallGraph right
    return (leftCallGraph ++ rightCallGraph, leftFreeVars ++ rightFreeVars)
bExpCallGraph (Eq left right) = do
    (leftCallGraph , leftFreeVars ) <- expCallGraph left
    (rightCallGraph, rightFreeVars) <- expCallGraph right
    return (leftCallGraph ++ rightCallGraph, leftFreeVars ++ rightFreeVars)
bExpCallGraph (AND left right) = do
    (leftCallGraph , leftFreeVars ) <- bExpCallGraph left
    (rightCallGraph, rightFreeVars) <- bExpCallGraph right
    return (leftCallGraph ++ rightCallGraph, leftFreeVars ++ rightFreeVars)
bExpCallGraph (OR left right) = do
    (leftCallGraph , leftFreeVars ) <- bExpCallGraph left
    (rightCallGraph, rightFreeVars) <- bExpCallGraph right
    return (leftCallGraph ++ rightCallGraph, leftFreeVars ++ rightFreeVars)
bExpCallGraph (NOT bexp) = do
    bExpCallGraph bexp

-- this function gets an integer (n) and pops from the stack until the length of the stack becomes n
-- it returns poped items as the called functions
createGraph :: Int -> CallStack -> (CallStack, CallingFuncs)
createGraph start stack
    | length stack == start = (stack, [])
    | otherwise = case pop stack of
        Just (f, fs) -> (stack, f : callingFuncs)
            where (stack, callingFuncs) = createGraph start fs
        Nothing -> ([], [])

-- takes something of type State s [[a]], concats the output and returns a State s [a]
concatState :: State s [[a]] -> State s [a]
concatState (StateT f) = StateT
    (\s -> case f s of
        Identity (a', s') -> Identity (concat a', s')
    )