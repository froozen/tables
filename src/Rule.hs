{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances #-}
{- |
Module: Rule

Rules are a State RuleState monad below a few monad transformers.

ReaderT allows acces to the original plain supplied to the rule.

MaybeT allows for failures and makes it easy to implement the rejection functions.
-}

module Rule
    ( Rule
    , RuleState(..)
    , applyRule
    , liftR
    , guardR
    ) where

import Control.Monad
import Control.Applicative (Applicative, Alternative)
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Trans.Maybe
import Data.Monoid

-- | Type used with State in Rule
data RuleState = RuleState {
      stored :: String
    , result :: String
    } deriving (Show, Eq)

-- | Representation of a Rule
newtype Rule a = Rule
    { runRule :: MaybeT (ReaderT String (State RuleState)) a }
    deriving (Monad, MonadState RuleState, MonadReader String,
              MonadPlus, Functor, Applicative, Alternative)

-- I made Rule an instance of Monoid, so I can use it
-- with a Writer.
instance Monoid (Rule ()) where
    mempty  = liftR id
    mappend = (>>)

-- | Apply a rule to a String
applyRule :: Rule a -> String -> Maybe String
applyRule r s = evalState (runReaderT (runMaybeT . runRule $ r >> extract) s) $ RuleState s s
    where extract = gets stored

-- | Lift a (String -> String) function to a Rule
liftR :: (String -> String) -> Rule ()
liftR f = modify $ \s -> s { result = f. result $ s }

-- | The equivalent of guard
guardR :: (RuleState -> Bool) -> Rule ()
guardR r = guard =<< (gets r)
