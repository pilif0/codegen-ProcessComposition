{-# LANGUAGE EmptyDataDecls, RankNTypes, ScopedTypeVariables #-}

module ProcessComposition.Isabelle.Showa(shows_string) where {

import Prelude ((==), (/=), (<), (<=), (>=), (>), (+), (-), (*), (/), (**),
  (>>=), (>>), (=<<), (&&), (||), (^), (^^), (.), ($), ($!), (++), (!!), Eq,
  error, id, return, not, fst, snd, map, filter, concat, concatMap, reverse,
  zip, null, takeWhile, dropWhile, all, any, Integer, negate, abs, divMod,
  String, Bool(True, False), Maybe(Nothing, Just));
import Data.Bits ((.&.), (.|.), (.^.));
import qualified Prelude;
import qualified Data.Bits;
import qualified Str_Literal;
import qualified ProcessComposition.Isabelle.Str;

shows_string ::
  [ProcessComposition.Isabelle.Str.Char] ->
    [ProcessComposition.Isabelle.Str.Char] ->
      [ProcessComposition.Isabelle.Str.Char];
shows_string = (\ a b -> a ++ b);

}
