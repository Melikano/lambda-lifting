{- | A collection of common monads and monad transformers used in Haskell including:
 -   - State / StateT monad                              [discussed in class]
 -   - Except / ExceptT (Either monad transformer) monad [not covered in class]
 -   - Reader / ReaderT monad                            [not covered in class]
 -   - Identity monad                                    [not covered in class, but should be straightforward]
 -
 -   - Alternative instances for monads                  [not covered in class, but needed for the parser]
 -
 -
 - Further optional reading:
 -  - This link is the (probably) most commonly used Haskell library for monad transformers.
 -    The code presented should like quite similar (we are missing a few functions here however) 
 -    to what is presented in that library.
 -          https://hackage.haskell.org/package/transformers
 -
 -  - A very comprehensive document on type checking, type classes, monads, monad transformers. 
 -    However, this paper is quite old, and the notation (function names) are quite different now. 
 -          https://web.cecs.pdx.edu/~mpj/pubs/springschool95.pdf
 -
 - Things NOT included in this file (for those with prior Haskell experience):
 -   - Monad classes using functional dependencies: We're trying to stay away
 -     from using GHC extensions with i.e., we are sticking with regular
 -     old Haskell 2010. Besides, understanding monad transformers is a
 -     stepping stone to understanding monad classes using functional
 -     dependencies (and monad classes using functional dependencies
 -     have their own shortcomings as well!)
 -}
module Lib.Monads where


import Control.Applicative

----------------------
-- 'StateT' monad
-- This is the State monad, BUT it lets you combine it with
-- the effects of any OTHER monad... This is useful to combine
-- with the Either monad provided in the Haskell standard libraries
-- (Prelude!)
----------------------
-- | This is the 'StateT' type
-- Here is an explanation of the type variables:
--      - s: is the state of the computation
--      - m: is the monad for which this is combined with
--      - a: is the generic output type of the function
-- An explanation of (s -> m (a, s)). This type is saying that
-- if you give it a state, it will return (a, s) in the monad m i.e.,
-- it will give you a value ``a" and a new state ``s" all in the monad m.
newtype StateT s m a = StateT (s -> m (a, s))

-- | 'runStateT'
-- Given a (StateT s m a), and a state s, this will run
-- the (StateT s m a) with the given state s, and output
-- m (s, a) where m is the inner monad.
runStateT :: Monad m => StateT s m a -> s -> m (a,s)
runStateT (StateT f) s = f s 

-- | 'evalStateT'
-- Similar to 'runStateT', but only returns the output value
evalStateT :: Monad m => StateT s m a -> s -> m a
evalStateT st = fmap fst . runStateT st

-- | 'execStateT'
-- Similar to runStateT, but only returns the output state
execStateT :: Monad m => StateT s m a -> s -> m s
execStateT st = fmap snd . runStateT st

-- | 'set'
-- Given a new state ``s", this function will
-- set the state to be ``s" for all computations
-- after this call
set :: Monad m => s -> StateT s m ()
set s' = StateT (\s -> return ((), s'))
-- Alternative implementation:
-- set s = modify (const s)

-- | 'get'
-- This will return the current state
get :: Monad m => StateT s m s
get = StateT (\s -> return (s , s))
-- Alternative implementation
-- get = gets id

-- | 'gets'
-- Given a function f, this will return the current
-- state AFTER applying f to the state
gets :: Monad m => (s -> s') -> StateT s m s'
gets f = StateT (\s -> return (f s, s))

-- | 'modify'
-- Given a function ``f :: s -> s", this will
-- apply ``f" to the current state, and modify 
-- the state for the computations after this call.
modify :: Monad m => (s -> s) -> StateT s m ()
modify f = StateT (\s -> return ((), f s))

-- | 'state'
-- This constructs a state monad computation from a pure state computation
state :: 
    Monad m =>
    (s -> (a, s)) ->
    StateT s m a
state f = StateT (return . f)

-- | This is a type alias for the StateT monad
-- to give us the original state monad that 
-- is NOT combined with any other monad i.e., this is
-- just the plain State monad
type State s a = StateT s Identity a

-- | 'runState'
-- Similar to runStateT
runState :: State s a -> s -> (a,s)
runState st = runIdentity . runStateT st

-- | 'evalState'
-- Similar to evalStateT
evalState :: State s a -> s -> a
evalState st = runIdentity . evalStateT st

-- | 'execState'
-- Similar to execStateT
execState :: State s a -> s -> s
execState st = runIdentity . execStateT st

-- Functor instance (don't worry about this too much)
-- didn't go through this in class
instance Monad m => Functor (StateT s m) where
    fmap f (StateT g) = StateT $ \s -> do
        (a, s') <- g s
        return (f a, s')

-- Applicative instance (don't worry about this too much)
-- didn't go through this in class
instance Monad m => Applicative (StateT s m) where
    pure a = StateT $ \s -> pure (a,s) 

    StateT f <*> StateT a = StateT $ \s -> do
        (f', s') <- f s
        (a', s'') <- a s' 
        pure (f' a', s'')


instance Monad m => Monad (StateT s m) where
    -- return :: a -> (StateT s m) a
    return = pure
    -- (>>=) :: (StateT s m) a -> (a -> (StateT s m) b) -> (StateT s m) b
    StateT f >>= g = StateT (\s -> do
            (a, s')  <- f s 
            let StateT f' = g a 
            (b, s'') <- f' s'
            return (b, s'')
        )

-- This instance tells us how to take ANY monad and LIFT it up to the
-- StateT monad.. In other words, this tell us how to combine any monad
-- with the StateT monad..
instance MonadTrans (StateT s) where 
    -- lift :: m a -> StateT m a
    lift ma = StateT $ \s -> do
            a <- ma
            return (a, s)


----------------------
-- 'MonadTrans'
----------------------
-- This is the type class to 'lift' a monad to another monad..
-- 'lift' is used to take a monad and bring the monad up to another monad
-- transformer.
class MonadTrans t where
    lift :: Monad m => m a -> t m a

----------------------
-- 'Identity' monad
-- Generally speaking, this monad is useful when used with
-- monad transformers (e.g., StateT) when we just want to
-- use the monad WITHOUT combining another monad.. See
-- StateT for an example
----------------------
newtype Identity a = Identity a 

-- | 'runIdentity'
-- This will take a value outside of the identity..
runIdentity :: Identity a -> a
runIdentity (Identity a) =  a

instance Functor Identity where
    fmap f (Identity n) = Identity $ f n

instance Applicative Identity where
    pure = Identity
    Identity f <*> n = fmap f n

instance Monad Identity where
    return = pure
    Identity n >>= f = f n

--------------------------------------------
-- 'ReaderT' monad (not discussed in tutorial)
-- This is the same as the 'State' monad, but
-- the environment cannot be modified.
--------------------------------------------
newtype ReaderT r m a = ReaderT { runReaderT :: r -> m a }

type Reader r a = ReaderT r Identity a

runReader :: Reader r a -> r -> a
runReader env = runIdentity . runReaderT env

instance Monad m => Functor (ReaderT r m) where
    fmap f (ReaderT g) = ReaderT (fmap f . g)

instance Monad m => Applicative (ReaderT r m) where
    pure n = ReaderT $ \r -> pure n

    f <*> n = ReaderT $ \r -> do
        f' <- runReaderT f r
        n' <- runReaderT n r
        pure $ f' n'
                                
instance Monad m => Monad (ReaderT r m) where
    return = pure

    n >>= f = ReaderT $ \r -> do 
        n' <- runReaderT n r
        runReaderT (f n') r

instance MonadTrans (ReaderT r) where
    lift = ReaderT . const

asks :: Monad m => 
    (r -> r) -> 
    ReaderT r m r
asks f = ReaderT (\r -> pure (f r))

ask :: Monad m => ReaderT r m r
ask = asks Prelude.id

local :: 
    -- to modify local env
    (r -> r) -> 
    -- to run with modified env
    ReaderT r m a ->
    -- regular env..
    ReaderT r m a 
local f (ReaderT g) = ReaderT $
    \r -> g (f r)

---------------------------------
-- 'ExceptT' (not discussed in tutorial)
-- This is a wrapper around the Either monad
-- which allows one to combine the Either monad
-- with other monads
---------------------------------
newtype ExceptT e m a = ExceptT { runExceptT :: m (Either e a) }

type Except e a = ExceptT e Identity a

runExcept :: Except e a -> Either e a
runExcept = runIdentity . runExceptT

instance Monad m => Functor (ExceptT e m) where
    fmap f (ExceptT mv) = ExceptT $ do
        v <- mv
        return $ f <$> v

instance Monad m => Applicative (ExceptT e m) where
    pure = ExceptT . pure . Right 

    ExceptT f <*> ExceptT a = ExceptT $ do
        f' <- f
        a' <- a
        pure (f'<*> a')
instance Monad m => Monad (ExceptT e m) where
    return = pure

    ExceptT n >>= k = ExceptT $ do 
        n' <- n
        case n' of
            Left e -> return (Left e)
            Right a -> runExceptT (k a)

instance MonadTrans (ExceptT e) where
    lift m = ExceptT (Right <$> m)

throwError :: Monad m => e -> ExceptT e m a
throwError = ExceptT . return . Left

catchError :: Monad m => ExceptT e m a -> (e -> ExceptT e m a) -> ExceptT e m a
catchError (ExceptT a) f = ExceptT $ do
    a' <- a
    case a' of
        Left e -> runExceptT (f e)
        Right n -> return (Right n)

------------------------
-- 'WriterT' monad (not covered in class or tutorial)
------------------------
-- Generally speaking, this is used to ``logging" extra
-- information when doing a computation with the tell method
-- in the do block..
--
-- Also useful for code generation.

newtype WriterT w m a = WriterT { runWriterT :: m (a, w) }

type Writer w a = WriterT w Identity a

runWriter :: Writer w a -> (a, w)
runWriter = runIdentity . runWriterT

instance Monad m => Functor (WriterT w m) where
    fmap f (WriterT n) = WriterT $ do 
                            (a, w) <- n
                            return (f a, w)

instance (Monad m, Monoid w) => Applicative (WriterT w m) where
    pure a = WriterT $ pure (a, mempty)

    f <*> n = WriterT $ do
                (f', w) <- runWriterT f
                (a, w') <- runWriterT n
                return (f' a, w `mappend` w')

instance (Monad m, Monoid w) => Monad (WriterT w m) where
    return = pure

    n >>= f = WriterT $ do
                (a, w) <- runWriterT n
                (b, w') <- runWriterT $ f a
                return (b, w `mappend` w')

instance Monoid w => MonadTrans (WriterT w) where
    lift ma = WriterT $ do
        a <- ma
        return (a, mempty)

tell :: (Monad m, Monoid w) => w -> WriterT w m ()
tell w = WriterT $ pure ((), w)

------------------------
-- 'Alternative' instances
------------------------
instance (Monad m, Alternative m) => Alternative (ReaderT r m) where
    empty = ReaderT (const empty)
    ReaderT f <|> ReaderT g = ReaderT $ \r -> f r <|> g r

instance (Monad m, Alternative m) => Alternative (StateT r m) where
    empty = StateT $ \s -> empty
    StateT f <|> StateT g = StateT $ \s -> f s <|> g s

instance (Monad m, Alternative m) => Alternative (ExceptT err m) where
    empty = ExceptT empty
    ExceptT a <|> ExceptT b = ExceptT $ a <|> b
