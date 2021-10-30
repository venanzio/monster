module FurtherTypeClasses where

import Control.Comonad
import Data.Bifunctor
import Data.Functor.Rep -- requires adjunctions-4.4 from Hackage
import Data.Distributive

import MonadicStreams

-- * More typeclasses for monadic streams using comonadic, distributive and representable functors.

instance Comonad w => Comonad (MonStr w) where
  extract = extract . head
  duplicate ms = MCons $ fmap (\(h,t) -> (ms, duplicate t)) (uncons ms)
  

instance Distributive m => Distributive (MonStr m) where
  -- distribute :: Functor f => f (MonStr m a) -> MonStr m (f a)
  distribute fma = MCons $ fmap (\fp -> (fmap fst fp, (distribute . fmap snd) fp)) 
                                (distribute (fmap uncons fma))


infixr 5 :<
data NonEmpty a = Last a | a :< (NonEmpty a) deriving Show
                
-- This is proven correct
instance Representable m => Representable (MonStr m) where
  type Rep (MonStr m) = NonEmpty (Rep m)
  -- tabulate :: (NonEmpty (Rep m) -> a) -> MonStr m a
  tabulate f = MCons $ tabulate (\k -> (f (Last k), tabulate (f . (k :<))))
         
  -- index :: MonStr m a -> NonEmpty (Rep m) -> a
  index ms k = case k of
                  Last k  -> index (head ms) k
                  k :< ks -> index (index (tail ms) k) ks


instance (Representable m, Applicative m) => Monad (MonStr m) where
  ma >>= f = bindRep ma f
