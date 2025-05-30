{-# LANGUAGE EmptyDataDecls, RankNTypes, ScopedTypeVariables #-}

module ProcessComposition.Isabelle.GCD(gcd_int) where {

import Prelude ((==), (/=), (<), (<=), (>=), (>), (+), (-), (*), (/), (**),
  (>>=), (>>), (=<<), (&&), (||), (^), (^^), (.), ($), ($!), (++), (!!), Eq,
  error, id, return, not, fst, snd, map, filter, concat, concatMap, reverse,
  zip, null, takeWhile, dropWhile, all, any, Integer, negate, abs, divMod,
  String, Bool(True, False), Maybe(Nothing, Just));
import Data.Bits ((.&.), (.|.), (.^.));
import qualified Prelude;
import qualified Data.Bits;
import qualified Str_Literal;
import qualified ProcessComposition.Isabelle.Arith;

gcd_int ::
  ProcessComposition.Isabelle.Arith.Int ->
    ProcessComposition.Isabelle.Arith.Int ->
      ProcessComposition.Isabelle.Arith.Int;
gcd_int (ProcessComposition.Isabelle.Arith.Int_of_integer x)
  (ProcessComposition.Isabelle.Arith.Int_of_integer y) =
  ProcessComposition.Isabelle.Arith.Int_of_integer (Prelude.gcd x y);

}
