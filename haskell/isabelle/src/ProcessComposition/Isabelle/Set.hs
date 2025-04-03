{-# LANGUAGE EmptyDataDecls, RankNTypes, ScopedTypeVariables #-}

module ProcessComposition.Isabelle.Set(Set(..), image, filtera) where {

import Prelude ((==), (/=), (<), (<=), (>=), (>), (+), (-), (*), (/), (**),
  (>>=), (>>), (=<<), (&&), (||), (^), (^^), (.), ($), ($!), (++), (!!), Eq,
  error, id, return, not, fst, snd, map, filter, concat, concatMap, reverse,
  zip, null, takeWhile, dropWhile, all, any, Integer, negate, abs, divMod,
  String, Bool(True, False), Maybe(Nothing, Just));
import Data.Bits ((.&.), (.|.), (.^.));
import qualified Prelude;
import qualified Data.Bits;

data Set a = Set [a] | Coset [a] deriving (Prelude.Read, Prelude.Show);

image :: forall a b. (a -> b) -> Set a -> Set b;
image f (Set xs) = Set (map f xs);

filtera :: forall a. (a -> Bool) -> Set a -> Set a;
filtera p (Set xs) = Set (filter p xs);

}
