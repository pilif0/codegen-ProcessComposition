{-# LANGUAGE EmptyDataDecls, RankNTypes, ScopedTypeVariables #-}

module
  ProcessComposition.Isabelle.Arith(Nat(..), one_nat, less_nat, less_eq_nat,
                                     plus_nat)
  where {

import Prelude ((==), (/=), (<), (<=), (>=), (>), (+), (-), (*), (/), (**),
  (>>=), (>>), (=<<), (&&), (||), (^), (^^), (.), ($), ($!), (++), (!!), Eq,
  error, id, return, not, fst, snd, map, filter, concat, concatMap, reverse,
  zip, null, takeWhile, dropWhile, all, any, Integer, negate, abs, divMod,
  String, Bool(True, False), Maybe(Nothing, Just));
import qualified Prelude;

data Nat = Zero_nat | Suc Nat deriving (Prelude.Read, Prelude.Show);

one_nat :: Nat;
one_nat = Suc Zero_nat;

less_nat :: Nat -> Nat -> Bool;
less_nat m (Suc n) = less_eq_nat m n;
less_nat n Zero_nat = False;

less_eq_nat :: Nat -> Nat -> Bool;
less_eq_nat (Suc m) n = less_nat m n;
less_eq_nat Zero_nat n = True;

plus_nat :: Nat -> Nat -> Nat;
plus_nat (Suc m) n = plus_nat m (Suc n);
plus_nat Zero_nat n = n;

}
