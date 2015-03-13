{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Rule
    ( Rule
    , Memory(..)
    , applyRule
    , liftR
    , liftMemR
    , guardR
    , saveMem
    ) where

import Control.Arrow
import Control.Category
import Data.Monoid
-- prevent clashes with Control.Category
import Prelude hiding ((.), id)

-- | Representation of a Rule
type Rule = Kleisli Maybe (String, Memory) (String, Memory)

-- | Representation of the memory
data Memory = Memory {
      origin :: String
    , stored :: String
    } deriving (Show, Eq)

-- I made Rule an instance of Monoid, so I can use it
-- with a Writer.
instance Monoid Rule where
    mempty        = liftR $ id
    a `mappend` b = a >>> b

-- | Apply a rule to a String
applyRule :: Rule -> String -> Maybe String
applyRule r = runKleisli $ initiate >>> r >>> takeFirst
    where initiate  = arr $ \s -> (s, Memory s s)
          takeFirst = arr fst

-- | Lift a (String -> String) function to a Rule
liftR :: (String -> String) -> Rule
liftR f = first $ Kleisli (Just . f)

-- | Lift a ((String, Memory) -> (String, Memory)) function to a Rule
liftMemR :: ((String, Memory) -> (String, Memory)) -> Rule
liftMemR = arr

-- | The equivalent of guard
guardR :: (String -> Bool) -> Rule
guardR p = Kleisli $ f
    where f (s, mem) = if p s
                       then Just (s, mem)
                       else Nothing

-- | Save the String into memory
saveMem :: Rule
saveMem = liftMemR $ \(a, mem) -> (origin mem, mem { stored = a })
