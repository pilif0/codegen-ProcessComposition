{-# LANGUAGE EmptyDataDecls, RankNTypes, ScopedTypeVariables #-}

module ProcessComposition.Isabelle.Groups_List(sum_list) where {

import Prelude ((==), (/=), (<), (<=), (>=), (>), (+), (-), (*), (/), (**),
  (>>=), (>>), (=<<), (&&), (||), (^), (^^), (.), ($), ($!), (++), (!!), Eq,
  error, id, return, not, fst, snd, map, filter, concat, concatMap, reverse,
  zip, null, takeWhile, dropWhile, all, any, Integer, negate, abs, divMod,
  String, Bool(True, False), Maybe(Nothing, Just));
import Data.Bits ((.&.), (.|.), (.^.));
import qualified Prelude;
import qualified Data.Bits;
import qualified ProcessComposition.Isabelle.List;
import qualified ProcessComposition.Isabelle.Arith;

sum_list ::
  forall a. (ProcessComposition.Isabelle.Arith.Monoid_add a) => [a] -> a;
sum_list xs =
  ProcessComposition.Isabelle.List.foldr ProcessComposition.Isabelle.Arith.plus
    xs ProcessComposition.Isabelle.Arith.zero;

}
