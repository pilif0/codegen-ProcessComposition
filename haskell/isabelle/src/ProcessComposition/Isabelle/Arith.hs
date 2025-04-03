{-# LANGUAGE EmptyDataDecls, RankNTypes, ScopedTypeVariables #-}

module
  ProcessComposition.Isabelle.Arith(Nat(..), integer_of_nat, plus_nat, Plus(..),
                                     zero_nat, Zero(..), Semigroup_add(..),
                                     Monoid_add(..), One(..), Numeral(..),
                                     Times(..), Power(..), Semigroup_mult(..),
                                     Ab_semigroup_add(..), Semiring(..),
                                     Mult_zero(..), Comm_monoid_add(..),
                                     Semiring_0(..), Monoid_mult(..),
                                     Semiring_numeral(..), Zero_neq_one(..),
                                     Semiring_1(..), Int(..), Num(..), one_nat,
                                     suc, minus_nat, equal_nat, power, one_int,
                                     integer_of_int, less_int, less_nat,
                                     numeral, nat_of_integer, divmod_nat,
                                     plus_int, zero_int, divmod_integer, of_nat,
                                     equal_int, times_int, times_nat,
                                     uminus_int, divide_integer, divide_int)
  where {

import Prelude ((==), (/=), (<), (<=), (>=), (>), (+), (-), (*), (/), (**),
  (>>=), (>>), (=<<), (&&), (||), (^), (^^), (.), ($), ($!), (++), (!!), Eq,
  error, id, return, not, fst, snd, map, filter, concat, concatMap, reverse,
  zip, null, takeWhile, dropWhile, all, any, Integer, negate, abs, divMod,
  String, Bool(True, False), Maybe(Nothing, Just));
import Data.Bits ((.&.), (.|.), (.^.));
import qualified Prelude;
import qualified Data.Bits;
import qualified ProcessComposition.Isabelle.Product_Type;
import qualified ProcessComposition.Isabelle.HOL;
import qualified ProcessComposition.Isabelle.Orderings;

newtype Nat = Nat Integer deriving (Prelude.Read, Prelude.Show);

integer_of_nat :: Nat -> Integer;
integer_of_nat (Nat x) = x;

plus_nat :: Nat -> Nat -> Nat;
plus_nat m n = Nat (integer_of_nat m + integer_of_nat n);

class Plus a where {
  plus :: a -> a -> a;
};

instance Plus Nat where {
  plus = plus_nat;
};

zero_nat :: Nat;
zero_nat = Nat (0 :: Integer);

class Zero a where {
  zero :: a;
};

instance Zero Nat where {
  zero = zero_nat;
};

class (Plus a) => Semigroup_add a where {
};

class (Semigroup_add a, Zero a) => Monoid_add a where {
};

instance Semigroup_add Nat where {
};

instance Monoid_add Nat where {
};

instance ProcessComposition.Isabelle.Orderings.Ord Integer where {
  less_eq = (\ a b -> a <= b);
  less = (\ a b -> a < b);
};

class One a where {
  one :: a;
};

class (One a, Semigroup_add a) => Numeral a where {
};

class Times a where {
  times :: a -> a -> a;
};

class (One a, Times a) => Power a where {
};

class (Times a) => Semigroup_mult a where {
};

class (Semigroup_add a) => Ab_semigroup_add a where {
};

class (Ab_semigroup_add a, Semigroup_mult a) => Semiring a where {
};

class (Times a, Zero a) => Mult_zero a where {
};

class (Ab_semigroup_add a, Monoid_add a) => Comm_monoid_add a where {
};

class (Comm_monoid_add a, Mult_zero a, Semiring a) => Semiring_0 a where {
};

class (Semigroup_mult a, Power a) => Monoid_mult a where {
};

class (Monoid_mult a, Numeral a, Semiring a) => Semiring_numeral a where {
};

class (One a, Zero a) => Zero_neq_one a where {
};

class (Semiring_numeral a, Semiring_0 a, Zero_neq_one a) => Semiring_1 a where {
};

newtype Int = Int_of_integer Integer deriving (Prelude.Read, Prelude.Show);

data Num = One | Bit0 Num | Bit1 Num deriving (Prelude.Read, Prelude.Show);

one_nat :: Nat;
one_nat = Nat (1 :: Integer);

suc :: Nat -> Nat;
suc n = plus_nat n one_nat;

minus_nat :: Nat -> Nat -> Nat;
minus_nat m n =
  Nat (ProcessComposition.Isabelle.Orderings.max (0 :: Integer)
        (integer_of_nat m - integer_of_nat n));

equal_nat :: Nat -> Nat -> Bool;
equal_nat m n = integer_of_nat m == integer_of_nat n;

power :: forall a. (Power a) => a -> Nat -> a;
power a n =
  (if equal_nat n zero_nat then one
    else times a (power a (minus_nat n one_nat)));

one_int :: Int;
one_int = Int_of_integer (1 :: Integer);

integer_of_int :: Int -> Integer;
integer_of_int (Int_of_integer k) = k;

less_int :: Int -> Int -> Bool;
less_int k l = integer_of_int k < integer_of_int l;

less_nat :: Nat -> Nat -> Bool;
less_nat m n = integer_of_nat m < integer_of_nat n;

numeral :: forall a. (Numeral a) => Num -> a;
numeral (Bit1 n) = let {
                     m = numeral n;
                   } in plus (plus m m) one;
numeral (Bit0 n) = let {
                     m = numeral n;
                   } in plus m m;
numeral One = one;

nat_of_integer :: Integer -> Nat;
nat_of_integer k =
  Nat (ProcessComposition.Isabelle.Orderings.max (0 :: Integer) k);

divmod_nat :: Nat -> Nat -> (Nat, Nat);
divmod_nat m n =
  let {
    k = integer_of_nat m;
    l = integer_of_nat n;
  } in ProcessComposition.Isabelle.Product_Type.map_prod nat_of_integer
         nat_of_integer
         (if k == (0 :: Integer) then ((0 :: Integer), (0 :: Integer))
           else (if l == (0 :: Integer) then ((0 :: Integer), k)
                  else divMod (abs k) (abs l)));

plus_int :: Int -> Int -> Int;
plus_int k l = Int_of_integer (integer_of_int k + integer_of_int l);

zero_int :: Int;
zero_int = Int_of_integer (0 :: Integer);

divmod_integer :: Integer -> Integer -> (Integer, Integer);
divmod_integer k l =
  (if k == (0 :: Integer) then ((0 :: Integer), (0 :: Integer))
    else (if (0 :: Integer) < l
           then (if (0 :: Integer) < k then divMod (abs k) (abs l)
                  else (case divMod (abs k) (abs l) of {
                         (r, s) ->
                           (if s == (0 :: Integer)
                             then (negate r, (0 :: Integer))
                             else (negate r - (1 :: Integer), l - s));
                       }))
           else (if l == (0 :: Integer) then ((0 :: Integer), k)
                  else ProcessComposition.Isabelle.Product_Type.apsnd negate
                         (if k < (0 :: Integer) then divMod (abs k) (abs l)
                           else (case divMod (abs k) (abs l) of {
                                  (r, s) ->
                                    (if s == (0 :: Integer)
                                      then (negate r, (0 :: Integer))
                                      else (negate r - (1 :: Integer),
     negate l - s));
                                })))));

of_nat :: forall a. (Semiring_1 a) => Nat -> a;
of_nat n =
  (if equal_nat n zero_nat then zero
    else (case divmod_nat n (nat_of_integer (2 :: Integer)) of {
           (m, q) -> let {
                       ma = times (numeral (Bit0 One)) (of_nat m);
                     } in (if equal_nat q zero_nat then ma else plus ma one);
         }));

equal_int :: Int -> Int -> Bool;
equal_int k l = integer_of_int k == integer_of_int l;

times_int :: Int -> Int -> Int;
times_int k l = Int_of_integer (integer_of_int k * integer_of_int l);

times_nat :: Nat -> Nat -> Nat;
times_nat m n = Nat (integer_of_nat m * integer_of_nat n);

uminus_int :: Int -> Int;
uminus_int k = Int_of_integer (negate (integer_of_int k));

divide_integer :: Integer -> Integer -> Integer;
divide_integer k l = fst (divmod_integer k l);

divide_int :: Int -> Int -> Int;
divide_int k l =
  Int_of_integer (divide_integer (integer_of_int k) (integer_of_int l));

}
