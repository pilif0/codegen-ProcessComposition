{-# LANGUAGE EmptyDataDecls, RankNTypes, ScopedTypeVariables #-}

module ProcessComposition.Isabelle.Str(Char(..), integer_of_char, implode)
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
import qualified ProcessComposition.Isabelle.Arith;

instance ProcessComposition.Isabelle.Arith.Plus String where {
  plus = (\ a b -> a ++ b);
};

instance ProcessComposition.Isabelle.Arith.Zero String where {
  zero = "";
};

instance ProcessComposition.Isabelle.Arith.Semigroup_add String where {
};

instance ProcessComposition.Isabelle.Arith.Monoid_add String where {
};

data Char = Char Bool Bool Bool Bool Bool Bool Bool Bool
  deriving (Prelude.Read, Prelude.Show);

integer_of_char :: Char -> Integer;
integer_of_char (Char b0 b1 b2 b3 b4 b5 b6 b7) =
  ((((((ProcessComposition.Isabelle.Arith.of_bool b7 * (2 :: Integer) +
         ProcessComposition.Isabelle.Arith.of_bool b6) *
         (2 :: Integer) +
        ProcessComposition.Isabelle.Arith.of_bool b5) *
        (2 :: Integer) +
       ProcessComposition.Isabelle.Arith.of_bool b4) *
       (2 :: Integer) +
      ProcessComposition.Isabelle.Arith.of_bool b3) *
      (2 :: Integer) +
     ProcessComposition.Isabelle.Arith.of_bool b2) *
     (2 :: Integer) +
    ProcessComposition.Isabelle.Arith.of_bool b1) *
    (2 :: Integer) +
    ProcessComposition.Isabelle.Arith.of_bool b0;

implode :: [Char] -> String;
implode cs = Str_Literal.literalOfAsciis (map integer_of_char cs);

}
