
{-# LANGUAGE GADTs, ScopedTypeVariables, DataKinds, FlexibleContexts, Rank2Types, ConstraintKinds, TypeFamilies, MultiParamTypeClasses, FlexibleInstances, NoMonomorphismRestriction #-}

module Lib where

import Data.Dependent.Map (DMap,DSum)
import qualified Data.Dependent.Map as DMap
import Data.GADT.Compare (GCompare)
import Reflex hiding (combineDyn)
import qualified Reflex as Reflex 
import Reflex.Dom hiding (combineDyn)
import GHC.Exts (Constraint)
import Control.Monad.Identity (Identity)
import qualified GHCJS.DOM.EventM as J
import Data.IORef
import Control.Monad.Trans
import Control.Monad (join)


-------  reflex missings --------------

type Morph t m a = Dynamic t (m a) -> m (Event t a)

mapMorph  :: (MonadHold t m, Reflex t) => Morph t m (Event t b) -> (a -> m (Event t b)) -> Dynamic t a -> m (Event t b)
mapMorph dyn f d = mapDyn f d >>= dyn >>= joinE

mapMorph' dyn f d = mapDyn f d >>= dyn 

joinE :: (Reflex t, MonadHold t f) => Event t (Event t a) -> f (Event t a)
joinE = fmap switch . hold never

pick :: (GCompare k, Reflex t) => k a -> Event t (DMap k Identity) -> Event t a
pick x r = select (fan r) x -- shouldn't we share fan r ?

gateWith f = attachWithMaybe $ \allow a -> if f allow then Just a else Nothing

pair x = leftmost . (: [x])
--------- buggy namings, wait for Dynamic functor instance ---------------

combineDynWith = Reflex.combineDyn
combineDyn = combineDynWith (,)

------------- Spider synonims

type ES = Event Spider
type DS = Dynamic Spider
type BS = Behavior Spider

-------------- Dom + spider synonyms

type MS = MonadWidget Spider
type Plug a = ES (DMap a Identity)

-- specialized mapMorph for the Dom host 
domMorph ::     MonadWidget t m 
                => (a -> m (Event t b))  -- widget rewriter
                -> Dynamic t a           -- driver for rewritings
                -> m (Event t b)         -- signal the happened rewriting
domMorph = mapMorph dyn
domMorph' = mapMorph' dyn

ifMorph :: MS m => DS Bool -> m (ES a) -> m (ES a)
ifMorph x d = domMorph f x where
    f False = return never
    f True = d
-------------- create a Plug ----------
mergeDSums :: GCompare a => [DSum a ES] -> Plug a
mergeDSums = merge . DMap.fromList


