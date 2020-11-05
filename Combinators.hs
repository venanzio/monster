module Combinators where
 
import Control.Monad
import MonStreams
import Operations

-- Takes two monadic streams, and combines the monadic actions, returning the elements from the second stream
--  this can be used to serialise two IO monsters for example (the actions in each stream are both run at each step)
infixr 5 >>>
(>>>) :: Monad m => MonStr m a -> MonStr m b -> MonStr m b
mas >>> mbs = MCons $ do (a, as) <- unwrapMS mas
                         fmap (\(e,s) -> (e, as >>> s)) (unwrapMS mbs)
                                
-- (&&&) :: Monad m => MonStr m a -> MonStr
    
-- Takes a monadic action and joins it with each action in the given monster, executing the new monadic action second
--  throws away the return value of the given monadic action
infixr 5 |:>
(|:>) :: Monad m => m a -> MonStr m b -> MonStr m b
ma |:> mas = MCons . join $ (\(a,s) -> fmap (\_ -> (a, ma |:> s)) ma) <$> unwrapMS mas

-- Takes a monadic action and joins it with each action in the given monster, executing the new monadic action first
--  throws away the return value of the given monadic action
infixr 5 >:|
(>:|) :: Monad m => m a -> MonStr m b -> MonStr m b
ma >:| mas = MCons . join $ (\_ -> fmap (\(a, s) -> (a, ma >:| s)) (unwrapMS mas)) <$> ma


-- Takes a monadic action and joins it with each action in the given monster, executing the new monadic action second
--  modifies the result value with the function inside the new monadic action
infixr 5 $:>
($:>) :: Monad m => m (a -> b) -> MonStr m a -> MonStr m b
mf $:> mas = MCons . join $ (\(a,s) -> fmap (\f -> (f a, mf $:> s)) mf) <$> unwrapMS mas

-- Takes a monadic action and joins it with each action in the given monster, executing the new monadic action first
--  modifies the result value with the function inside the new monadic action
infixr 5 >:$
(>:$) :: Monad m => m (a -> b) -> MonStr m a -> MonStr m b
mf >:$ mas = MCons . join $ (\f -> fmap (\(a, s) -> (f a, mf >:$ s)) (unwrapMS mas)) <$> mf