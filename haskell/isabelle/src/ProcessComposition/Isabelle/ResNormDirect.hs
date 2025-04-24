{-# LANGUAGE EmptyDataDecls, RankNTypes, ScopedTypeVariables #-}

module ProcessComposition.Isabelle.ResNormDirect(normal_dir) where {

import Prelude ((==), (/=), (<), (<=), (>=), (>), (+), (-), (*), (/), (**),
  (>>=), (>>), (=<<), (&&), (||), (^), (^^), (.), ($), ($!), (++), (!!), Eq,
  error, id, return, not, fst, snd, map, filter, concat, concatMap, reverse,
  zip, null, takeWhile, dropWhile, all, any, Integer, negate, abs, divMod,
  String, Bool(True, False), Maybe(Nothing, Just));
import Data.Bits ((.&.), (.|.), (.^.));
import qualified Prelude;
import qualified Data.Bits;
import qualified Str_Literal;
import qualified ProcessComposition.Isabelle.ResTerm;

normal_dir ::
  forall a b.
    ProcessComposition.Isabelle.ResTerm.Res_term a b ->
      ProcessComposition.Isabelle.ResTerm.Res_term a b;
normal_dir ProcessComposition.Isabelle.ResTerm.Empty =
  ProcessComposition.Isabelle.ResTerm.Empty;
normal_dir ProcessComposition.Isabelle.ResTerm.Anything =
  ProcessComposition.Isabelle.ResTerm.Anything;
normal_dir (ProcessComposition.Isabelle.ResTerm.Res x) =
  ProcessComposition.Isabelle.ResTerm.Res x;
normal_dir (ProcessComposition.Isabelle.ResTerm.Copyable x) =
  ProcessComposition.Isabelle.ResTerm.Copyable x;
normal_dir (ProcessComposition.Isabelle.ResTerm.Parallel xs) =
  ProcessComposition.Isabelle.ResTerm.parallelise
    (ProcessComposition.Isabelle.ResTerm.merge_all_parallel
      (ProcessComposition.Isabelle.ResTerm.remove_all_empty
        (map normal_dir xs)));
normal_dir (ProcessComposition.Isabelle.ResTerm.NonD x y) =
  ProcessComposition.Isabelle.ResTerm.NonD (normal_dir x) (normal_dir y);
normal_dir (ProcessComposition.Isabelle.ResTerm.Executable x y) =
  ProcessComposition.Isabelle.ResTerm.Executable (normal_dir x) (normal_dir y);
normal_dir (ProcessComposition.Isabelle.ResTerm.Repeatable x y) =
  ProcessComposition.Isabelle.ResTerm.Repeatable (normal_dir x) (normal_dir y);

}
