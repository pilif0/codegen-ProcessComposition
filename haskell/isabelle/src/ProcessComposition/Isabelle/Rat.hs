{-# LANGUAGE EmptyDataDecls, RankNTypes, ScopedTypeVariables #-}

module
  ProcessComposition.Isabelle.Rat(Rat(..), one_rat, quotient_of, normalize,
                                   plus_rat, zero_rat, times_rat, of_int,
                                   equal_rat, divide_rat)
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
import qualified ProcessComposition.Isabelle.GCD;
import qualified ProcessComposition.Isabelle.HOL;
import qualified ProcessComposition.Isabelle.Arith;

newtype Rat = Frct
  (ProcessComposition.Isabelle.Arith.Int, ProcessComposition.Isabelle.Arith.Int)
  deriving (Prelude.Read, Prelude.Show);

one_rat :: Rat;
one_rat =
  Frct (ProcessComposition.Isabelle.Arith.one_int,
         ProcessComposition.Isabelle.Arith.one_int);

instance ProcessComposition.Isabelle.Arith.One Rat where {
  one = one_rat;
};

quotient_of ::
  Rat ->
    (ProcessComposition.Isabelle.Arith.Int,
      ProcessComposition.Isabelle.Arith.Int);
quotient_of (Frct x) = x;

normalize ::
  (ProcessComposition.Isabelle.Arith.Int,
    ProcessComposition.Isabelle.Arith.Int) ->
    (ProcessComposition.Isabelle.Arith.Int,
      ProcessComposition.Isabelle.Arith.Int);
normalize p =
  (if ProcessComposition.Isabelle.Arith.less_int
        ProcessComposition.Isabelle.Arith.zero_int (snd p)
    then let {
           a = ProcessComposition.Isabelle.GCD.gcd_int (fst p) (snd p);
         } in (ProcessComposition.Isabelle.Arith.divide_int (fst p) a,
                ProcessComposition.Isabelle.Arith.divide_int (snd p) a)
    else (if ProcessComposition.Isabelle.Arith.equal_int (snd p)
               ProcessComposition.Isabelle.Arith.zero_int
           then (ProcessComposition.Isabelle.Arith.zero_int,
                  ProcessComposition.Isabelle.Arith.one_int)
           else let {
                  a = ProcessComposition.Isabelle.Arith.uminus_int
                        (ProcessComposition.Isabelle.GCD.gcd_int (fst p)
                          (snd p));
                } in (ProcessComposition.Isabelle.Arith.divide_int (fst p) a,
                       ProcessComposition.Isabelle.Arith.divide_int (snd p)
                         a)));

plus_rat :: Rat -> Rat -> Rat;
plus_rat p q =
  Frct (case quotient_of p of {
         (a, c) ->
           (case quotient_of q of {
             (b, d) ->
               normalize
                 (ProcessComposition.Isabelle.Arith.plus_int
                    (ProcessComposition.Isabelle.Arith.times_int a d)
                    (ProcessComposition.Isabelle.Arith.times_int b c),
                   ProcessComposition.Isabelle.Arith.times_int c d);
           });
       });

instance ProcessComposition.Isabelle.Arith.Plus Rat where {
  plus = plus_rat;
};

zero_rat :: Rat;
zero_rat =
  Frct (ProcessComposition.Isabelle.Arith.zero_int,
         ProcessComposition.Isabelle.Arith.one_int);

instance ProcessComposition.Isabelle.Arith.Zero Rat where {
  zero = zero_rat;
};

instance ProcessComposition.Isabelle.Arith.Semigroup_add Rat where {
};

instance ProcessComposition.Isabelle.Arith.Numeral Rat where {
};

times_rat :: Rat -> Rat -> Rat;
times_rat p q =
  Frct (case quotient_of p of {
         (a, c) ->
           (case quotient_of q of {
             (b, d) ->
               normalize
                 (ProcessComposition.Isabelle.Arith.times_int a b,
                   ProcessComposition.Isabelle.Arith.times_int c d);
           });
       });

instance ProcessComposition.Isabelle.Arith.Times Rat where {
  times = times_rat;
};

instance ProcessComposition.Isabelle.Arith.Power Rat where {
};

instance ProcessComposition.Isabelle.Arith.Ab_semigroup_add Rat where {
};

instance ProcessComposition.Isabelle.Arith.Semigroup_mult Rat where {
};

instance ProcessComposition.Isabelle.Arith.Semiring Rat where {
};

instance ProcessComposition.Isabelle.Arith.Mult_zero Rat where {
};

instance ProcessComposition.Isabelle.Arith.Monoid_add Rat where {
};

instance ProcessComposition.Isabelle.Arith.Comm_monoid_add Rat where {
};

instance ProcessComposition.Isabelle.Arith.Semiring_0 Rat where {
};

instance ProcessComposition.Isabelle.Arith.Monoid_mult Rat where {
};

instance ProcessComposition.Isabelle.Arith.Semiring_numeral Rat where {
};

instance ProcessComposition.Isabelle.Arith.Zero_neq_one Rat where {
};

instance ProcessComposition.Isabelle.Arith.Semiring_1 Rat where {
};

of_int :: ProcessComposition.Isabelle.Arith.Int -> Rat;
of_int a = Frct (a, ProcessComposition.Isabelle.Arith.one_int);

equal_rat :: Rat -> Rat -> Bool;
equal_rat a b = quotient_of a == quotient_of b;

divide_rat :: Rat -> Rat -> Rat;
divide_rat p q =
  Frct (case quotient_of p of {
         (a, c) ->
           (case quotient_of q of {
             (b, d) ->
               normalize
                 (ProcessComposition.Isabelle.Arith.times_int a d,
                   ProcessComposition.Isabelle.Arith.times_int c b);
           });
       });

}
