{- Functions related to lambda lifting
 - You should modify this file!
 -
 - See Section 4 of ./lambda-lift.pdf for more information..
 -}
module LambdaLift where

import           Lib.Monads

import           AST
import           ASTParse
import           AlphaRename
import           CallGraph
import           ExamplePrograms

-- this function searches a function name in the calling functions of a call graph,
-- if it finds it, returns Just that node and else it returns Nothing
searchInCallGraph :: String -> CallGraph -> Maybe CallGraphNode
searchInCallGraph f [] = Nothing
searchInCallGraph f ((g, args, frees, cfs) : gs)
    | f == g    = Just (g, args, frees, cfs)
    | otherwise = searchInCallGraph f gs

-- takes a call graph and and update it by adding the (freeVars - arguments) of the calling functions
-- V'free(f) = Vfree(f) union (Vfree(g)/Vargs(g)) (g is a calling function)
updateFrees :: CallGraph -> FreeVars -> CallingFuncs -> FreeVars
updateFrees cg frees fs = frees ++ filter (`notElem` frees) newFrees  where
    newFrees = foldr
        (\f vs -> case searchInCallGraph f cg of
            Nothing -> vs
            Just (_, args, freeVars, _) ->
                -- Vfree(f) ++ Vfree(g)/Vargs(g)
                vs ++ filter (`notElem` args) freeVars
        )
        []
        fs

-- takes a list of tuples in which the fst is a call graph node and snd shows that if that node's free variable can be updated any more or not
-- when all the snds are true then we should stop updating 
updateFreeVars :: [(CallGraphNode, Bool)] -> CallGraph
updateFreeVars cgb
    -- if all true then return cg
    | all snd cgb = cg
    -- other wise call updateFreeVars recursively
    | otherwise = updateFreeVars
        (map
            (\((funcName, args, frees, callingFuncs), status) ->
                -- update free vars using updateFrees helper function
                let newFrees = unique $ updateFrees cg frees callingFuncs
                in  ( (funcName, args, newFrees, callingFuncs)
                    -- check if frees are equal to newFrees
                    , frees == newFrees
                    )
            )

            cgb
        )
    where cg = map fst cgb
-- map updateFreeVars for all nodes (all functions)
update :: CallGraph -> CallGraph
update cg = updateFreeVars $ map (\c -> (c, False)) cg

-- lambda lift function. first rename the program then create call graph and then lift it
lambdaLift :: Prog String String -> Either String (Prog String String)
lambdaLift prog = case rename prog of
    Left  error       -> Left error
    Right renamedProg -> Right
        -- apply liftFun on all call graphs along with their renamed version
        (Prog (concat $ zipWith liftFun callGraphs renamedFuncs))
      where
        Prog renamedFuncs = renamedProg
        callGraphs        = map update $ createCallGraph renamedProg

-- generate list of functions (bring all to the top level)
liftFun :: CallGraph -> Fun String String -> [Fun String String]
liftFun cg (Fun (f, args, body)) = case searchInCallGraph f cg of
    Nothing -> []
    Just (_, args, frees, _) ->
        -- lift body and create function with lifted body and (args + free variables) 
        Fun (f, args ++ filter (`notElem` args) frees, liftedBody)
        -- add to other lifted funcs
            : liftedBodyFuncs
    where (liftedBody, liftedBodyFuncs) = liftExp cg body

liftExp
    :: CallGraph
    -> Exp String String
    -> (Exp String String, [Fun String String])
-- for APP, lift expressions recursively and add free variables of the f and return it
-- along with the lifted functions in the expressions
liftExp cg (APP f exps) =
    (APP f (liftedExps ++ filteredFrees), concat liftedExpsFuncs)
  where
    (liftedExps, liftedExpsFuncs) = unzip (map (liftExp cg) exps)
    filteredFrees                 = case searchInCallGraph f cg of
        Nothing                  -> []
        Just (_, args, frees, _) -> map VAR (filter (`notElem` args) frees)

-- from this point to the end I just did the almost same thing for all
-- lift everything and pass up!
liftExp cg (LET funcs exp) = (liftedExp, liftedExpFuncs ++ liftedFuncs)  where
    (liftedExp, liftedExpFuncs) = liftExp cg exp
    liftedFuncs                 = concatMap (liftFun cg) funcs
liftExp cg (ADD left right) =
    (ADD liftedLeft liftedRight, liftedLeftFuncs ++ liftedRightFuncs)
  where
    (liftedLeft , liftedLeftFuncs ) = liftExp cg left
    (liftedRight, liftedRightFuncs) = liftExp cg right
liftExp cg (SUB left right) =
    (SUB liftedLeft liftedRight, liftedLeftFuncs ++ liftedRightFuncs)
  where
    (liftedLeft , liftedLeftFuncs ) = liftExp cg left
    (liftedRight, liftedRightFuncs) = liftExp cg right
liftExp cg (MUL left right) =
    (MUL liftedLeft liftedRight, liftedLeftFuncs ++ liftedRightFuncs)
  where
    (liftedLeft , liftedLeftFuncs ) = liftExp cg left
    (liftedRight, liftedRightFuncs) = liftExp cg right
liftExp cg (DIV left right) =
    (DIV liftedLeft liftedRight, liftedLeftFuncs ++ liftedRightFuncs)
  where
    (liftedLeft , liftedLeftFuncs ) = liftExp cg left
    (liftedRight, liftedRightFuncs) = liftExp cg right
liftExp cg (COND ifExp thenExp elseExp) =
    ( COND liftedIf liftedThen liftedElse
    , liftedIfFuncs ++ liftedThenFuncs ++ liftedElseFuncs
    )
  where
    (liftedIf  , liftedIfFuncs  ) = liftBExp cg ifExp
    (liftedThen, liftedThenFuncs) = liftExp cg thenExp
    (liftedElse, liftedElseFuncs) = liftExp cg elseExp
liftExp cg (NEG exp) = (NEG liftedExp, liftedExpFuncs)
    where (liftedExp, liftedExpFuncs) = liftExp cg exp
-- no functions in Var and Const, so just return them and [] as lifted functions
liftExp _ (VAR   x) = (VAR x, [])
liftExp _ (CONST x) = (CONST x, [])

-- lift everything in the (boolean) expressions and pass them up
liftBExp
    :: CallGraph
    -> BExp String String
    -> (BExp String String, [Fun String String])
liftBExp cg (AND left right) =
    (AND liftedLeft liftedRight, liftedLeftFuncs ++ liftedRightFuncs)
  where
    (liftedLeft , liftedLeftFuncs ) = liftBExp cg left
    (liftedRight, liftedRightFuncs) = liftBExp cg right
liftBExp cg (OR left right) =
    (OR liftedLeft liftedRight, liftedLeftFuncs ++ liftedRightFuncs)
  where
    (liftedLeft , liftedLeftFuncs ) = liftBExp cg left
    (liftedRight, liftedRightFuncs) = liftBExp cg right
liftBExp cg (Lt left right) =
    (Lt liftedLeft liftedRight, liftedLeftFuncs ++ liftedRightFuncs)
  where
    (liftedLeft , liftedLeftFuncs ) = liftExp cg left
    (liftedRight, liftedRightFuncs) = liftExp cg right
liftBExp cg (Gt left right) =
    (Gt liftedLeft liftedRight, liftedLeftFuncs ++ liftedRightFuncs)
  where
    (liftedLeft , liftedLeftFuncs ) = liftExp cg left
    (liftedRight, liftedRightFuncs) = liftExp cg right
liftBExp cg (Eq left right) =
    (Eq liftedLeft liftedRight, liftedLeftFuncs ++ liftedRightFuncs)
  where
    (liftedLeft , liftedLeftFuncs ) = liftExp cg left
    (liftedRight, liftedRightFuncs) = liftExp cg right
liftBExp cg (NOT bexp) = (NOT liftedBExp, liftedBExpFuns)
    where (liftedBExp, liftedBExpFuns) = liftBExp cg bexp
