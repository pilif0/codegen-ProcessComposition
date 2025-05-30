{-# LANGUAGE EmptyDataDecls, RankNTypes, ScopedTypeVariables #-}

module
  ProcessComposition.Isabelle.List(upt, foldl, foldr, member, insert, remdups,
                                    removeAll, replicate, gen_length,
                                    map_filter, size_list)
  where {

import Prelude ((==), (/=), (<), (<=), (>=), (>), (+), (-), (*), (/), (**),
  (>>=), (>>), (=<<), (&&), (||), (^), (^^), (.), ($), ($!), (++), (!!), Eq,
  error, id, return, not, fst, snd, map, filter, concat, concatMap, reverse,
  zip, null, takeWhile, dropWhile, all, any, Integer, negate, abs, divMod,
  String, Bool(True, False), Maybe(Nothing, Just));
import Data.Bits ((.&.), (.|.), (.^.));
import qualified Prelude;
import qualified Data.Bits;
import qualified Str_Literal;
import qualified ProcessComposition.Isabelle.Option;
import qualified ProcessComposition.Isabelle.Arith;

upt ::
  ProcessComposition.Isabelle.Arith.Nat ->
    ProcessComposition.Isabelle.Arith.Nat ->
      [ProcessComposition.Isabelle.Arith.Nat];
upt i j =
  (if ProcessComposition.Isabelle.Arith.less_nat i j
    then i : upt (ProcessComposition.Isabelle.Arith.suc i) j else []);

foldl :: forall a b. (a -> b -> a) -> a -> [b] -> a;
foldl f a [] = a;
foldl f a (x : xs) = foldl f (f a x) xs;

foldr :: forall a b. (a -> b -> b) -> [a] -> b -> b;
foldr f [] = id;
foldr f (x : xs) = f x . foldr f xs;

member :: forall a. (Eq a) => [a] -> a -> Bool;
member [] y = False;
member (x : xs) y = x == y || member xs y;

insert :: forall a. (Eq a) => a -> [a] -> [a];
insert x xs = (if member xs x then xs else x : xs);

remdups :: forall a. (Eq a) => [a] -> [a];
remdups [] = [];
remdups (x : xs) = (if member xs x then remdups xs else x : remdups xs);

removeAll :: forall a. (Eq a) => a -> [a] -> [a];
removeAll x [] = [];
removeAll x (y : xs) = (if x == y then removeAll x xs else y : removeAll x xs);

replicate :: forall a. ProcessComposition.Isabelle.Arith.Nat -> a -> [a];
replicate n x =
  (if ProcessComposition.Isabelle.Arith.equal_nat n
        ProcessComposition.Isabelle.Arith.zero_nat
    then []
    else x : replicate
               (ProcessComposition.Isabelle.Arith.minus_nat n
                 ProcessComposition.Isabelle.Arith.one_nat)
               x);

gen_length ::
  forall a.
    ProcessComposition.Isabelle.Arith.Nat ->
      [a] -> ProcessComposition.Isabelle.Arith.Nat;
gen_length n (x : xs) = gen_length (ProcessComposition.Isabelle.Arith.suc n) xs;
gen_length n [] = n;

map_filter :: forall a b. (a -> Maybe b) -> [a] -> [b];
map_filter f [] = [];
map_filter f (x : xs) = (case f x of {
                          Nothing -> map_filter f xs;
                          Just y -> y : map_filter f xs;
                        });

size_list :: forall a. [a] -> ProcessComposition.Isabelle.Arith.Nat;
size_list = gen_length ProcessComposition.Isabelle.Arith.zero_nat;

}
