{-# LANGUAGE EmptyDataDecls, RankNTypes, ScopedTypeVariables #-}

module
  ProcessComposition.Isabelle.Show_Instances(string_of_digit, showsp_nat,
      shows_prec_nat)
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
import qualified ProcessComposition.Isabelle.Showa;
import qualified ProcessComposition.Isabelle.Str;
import qualified ProcessComposition.Isabelle.Arith;

string_of_digit ::
  ProcessComposition.Isabelle.Arith.Nat ->
    [ProcessComposition.Isabelle.Str.Char];
string_of_digit n =
  (if ProcessComposition.Isabelle.Arith.equal_nat n
        ProcessComposition.Isabelle.Arith.zero_nat
    then [ProcessComposition.Isabelle.Str.Char False False False False True True
            False False]
    else (if ProcessComposition.Isabelle.Arith.equal_nat n
               ProcessComposition.Isabelle.Arith.one_nat
           then [ProcessComposition.Isabelle.Str.Char True False False False
                   True True False False]
           else (if ProcessComposition.Isabelle.Arith.equal_nat n
                      (ProcessComposition.Isabelle.Arith.nat_of_integer
                        (2 :: Integer))
                  then [ProcessComposition.Isabelle.Str.Char False True False
                          False True True False False]
                  else (if ProcessComposition.Isabelle.Arith.equal_nat n
                             (ProcessComposition.Isabelle.Arith.nat_of_integer
                               (3 :: Integer))
                         then [ProcessComposition.Isabelle.Str.Char True True
                                 False False True True False False]
                         else (if ProcessComposition.Isabelle.Arith.equal_nat n
                                    (ProcessComposition.Isabelle.Arith.nat_of_integer
                                      (4 :: Integer))
                                then [ProcessComposition.Isabelle.Str.Char False
False True False True True False False]
                                else (if ProcessComposition.Isabelle.Arith.equal_nat
   n (ProcessComposition.Isabelle.Arith.nat_of_integer (5 :: Integer))
                                       then [ProcessComposition.Isabelle.Str.Char
       True False True False True True False False]
                                       else (if ProcessComposition.Isabelle.Arith.equal_nat
          n (ProcessComposition.Isabelle.Arith.nat_of_integer (6 :: Integer))
      then [ProcessComposition.Isabelle.Str.Char False True True False True True
              False False]
      else (if ProcessComposition.Isabelle.Arith.equal_nat n
                 (ProcessComposition.Isabelle.Arith.nat_of_integer
                   (7 :: Integer))
             then [ProcessComposition.Isabelle.Str.Char True True True False
                     True True False False]
             else (if ProcessComposition.Isabelle.Arith.equal_nat n
                        (ProcessComposition.Isabelle.Arith.nat_of_integer
                          (8 :: Integer))
                    then [ProcessComposition.Isabelle.Str.Char False False False
                            True True True False False]
                    else [ProcessComposition.Isabelle.Str.Char True False False
                            True True True False False])))))))));

showsp_nat ::
  ProcessComposition.Isabelle.Arith.Nat ->
    ProcessComposition.Isabelle.Arith.Nat ->
      [ProcessComposition.Isabelle.Str.Char] ->
        [ProcessComposition.Isabelle.Str.Char];
showsp_nat p n =
  (if ProcessComposition.Isabelle.Arith.less_nat n
        (ProcessComposition.Isabelle.Arith.nat_of_integer (10 :: Integer))
    then ProcessComposition.Isabelle.Showa.shows_string (string_of_digit n)
    else showsp_nat p
           (ProcessComposition.Isabelle.Arith.divide_nat n
             (ProcessComposition.Isabelle.Arith.nat_of_integer
               (10 :: Integer))) .
           ProcessComposition.Isabelle.Showa.shows_string
             (string_of_digit
               (ProcessComposition.Isabelle.Arith.modulo_nat n
                 (ProcessComposition.Isabelle.Arith.nat_of_integer
                   (10 :: Integer)))));

shows_prec_nat ::
  ProcessComposition.Isabelle.Arith.Nat ->
    ProcessComposition.Isabelle.Arith.Nat ->
      [ProcessComposition.Isabelle.Str.Char] ->
        [ProcessComposition.Isabelle.Str.Char];
shows_prec_nat = showsp_nat;

}
