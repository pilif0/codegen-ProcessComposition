{-# LANGUAGE EmptyDataDecls, RankNTypes, ScopedTypeVariables #-}

module
  ProcessComposition.Isabelle.Arith(Nat(..), Num(..), integer_of_nat, plus_nat,
                                     one_nat, suc, less_nat, zero_nat,
                                     equal_nat)
  where {

import Prelude ((==), (/=), (<), (<=), (>=), (>), (+), (-), (*), (/), (**),
  (>>=), (>>), (=<<), (&&), (||), (^), (^^), (.), ($), ($!), (++), (!!), Eq,
  error, id, return, not, fst, snd, map, filter, concat, concatMap, reverse,
  zip, null, takeWhile, dropWhile, all, any, Integer, negate, abs, divMod,
  String, Bool(True, False), Maybe(Nothing, Just));
import qualified Prelude;

newtype Nat = Nat Integer deriving (Prelude.Read, Prelude.Show);

data Num = One | Bit0 Num | Bit1 Num deriving (Prelude.Read, Prelude.Show);

integer_of_nat :: Nat -> Integer;
integer_of_nat (Nat x) = x;

plus_nat :: Nat -> Nat -> Nat;
plus_nat m n = Nat (integer_of_nat m + integer_of_nat n);

one_nat :: Nat;
one_nat = Nat (1 :: Integer);

suc :: Nat -> Nat;
suc n = plus_nat n one_nat;

less_nat :: Nat -> Nat -> Bool;
less_nat m n = integer_of_nat m < integer_of_nat n;

zero_nat :: Nat;
zero_nat = Nat (0 :: Integer);

equal_nat :: Nat -> Nat -> Bool;
equal_nat m n = integer_of_nat m == integer_of_nat n;

}
