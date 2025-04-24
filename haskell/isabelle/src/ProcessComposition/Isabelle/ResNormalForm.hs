{-# LANGUAGE EmptyDataDecls, RankNTypes, ScopedTypeVariables #-}

module ProcessComposition.Isabelle.ResNormalForm(normalised) where {

import Prelude ((==), (/=), (<), (<=), (>=), (>), (+), (-), (*), (/), (**),
  (>>=), (>>), (=<<), (&&), (||), (^), (^^), (.), ($), ($!), (++), (!!), Eq,
  error, id, return, not, fst, snd, map, filter, concat, concatMap, reverse,
  zip, null, takeWhile, dropWhile, all, any, Integer, negate, abs, divMod,
  String, Bool(True, False), Maybe(Nothing, Just));
import Data.Bits ((.&.), (.|.), (.^.));
import qualified Prelude;
import qualified Data.Bits;
import qualified Str_Literal;
import qualified ProcessComposition.Isabelle.List;
import qualified ProcessComposition.Isabelle.ResTerm;
import qualified ProcessComposition.Isabelle.Arith;

normalised ::
  forall a b. ProcessComposition.Isabelle.ResTerm.Res_term a b -> Bool;
normalised ProcessComposition.Isabelle.ResTerm.Empty = True;
normalised ProcessComposition.Isabelle.ResTerm.Anything = True;
normalised (ProcessComposition.Isabelle.ResTerm.Res x) = True;
normalised (ProcessComposition.Isabelle.ResTerm.Copyable x) = True;
normalised (ProcessComposition.Isabelle.ResTerm.Parallel xs) =
  all normalised xs &&
    all (\ x -> not (ProcessComposition.Isabelle.ResTerm.is_Empty x)) xs &&
      all (\ x -> not (ProcessComposition.Isabelle.ResTerm.is_Parallel x)) xs &&
        ProcessComposition.Isabelle.Arith.less_nat
          ProcessComposition.Isabelle.Arith.one_nat
          (ProcessComposition.Isabelle.List.size_list xs);
normalised (ProcessComposition.Isabelle.ResTerm.NonD x y) =
  normalised x && normalised y;
normalised (ProcessComposition.Isabelle.ResTerm.Executable x y) =
  normalised x && normalised y;
normalised (ProcessComposition.Isabelle.ResTerm.Repeatable x y) =
  normalised x && normalised y;

}
