module A2 where
import           AST
{- It might be easier to leave these imports here,
 - so when you type ``ghci A2``, it will automatically
 - load all functions of all files imported in this file.
 - This makes it easier to play with things in your code 
 - and see what's going on!
 -}
import           ASTParse
import           ExamplePrograms

import           AlphaRename
import           CallGraph
import           LambdaLift


-- | You should implement this!
-- Only this function will be autograded...  So, it is very 
-- important that YOU DO NOT CHANGE the type signature nor 
-- the name of this function...
runLambdaLift
   ::
    -- | Program as input
      Prog String String
   ->
    -- | Either an error message as output,
    -- OR a lambda lifted program as output.
    -- See ./lambda-lift.pdf for more details
      Either String (Prog String String)
runLambdaLift = lambdaLift