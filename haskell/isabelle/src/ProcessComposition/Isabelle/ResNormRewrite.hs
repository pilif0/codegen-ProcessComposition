{-# LANGUAGE EmptyDataDecls, RankNTypes, ScopedTypeVariables #-}

module
  ProcessComposition.Isabelle.ResNormRewrite(merge_one_parallel,
      remove_one_empty, step, normal_rewr)
  where {

import Prelude ((==), (/=), (<), (<=), (>=), (>), (+), (-), (*), (/), (**),
  (>>=), (>>), (=<<), (&&), (||), (^), (^^), (.), ($), ($!), (++), (!!), Eq,
  error, id, return, not, fst, snd, map, filter, concat, concatMap, reverse,
  zip, null, takeWhile, dropWhile, all, any, Integer, negate, abs, divMod,
  String, Bool(True, False), Maybe(Nothing, Just));
import qualified Prelude;
import qualified ProcessComposition.Isabelle.ResNormDirect;
import qualified ProcessComposition.Isabelle.ResNormalForm;
import qualified ProcessComposition.Isabelle.List;
import qualified ProcessComposition.Isabelle.ResTerm;

merge_one_parallel ::
  forall a b.
    [ProcessComposition.Isabelle.ResTerm.Res_term a b] ->
      [ProcessComposition.Isabelle.ResTerm.Res_term a b];
merge_one_parallel [] = [];
merge_one_parallel (ProcessComposition.Isabelle.ResTerm.Parallel x : xs) =
  x ++ xs;
merge_one_parallel (ProcessComposition.Isabelle.ResTerm.Res v : xs) =
  ProcessComposition.Isabelle.ResTerm.Res v : merge_one_parallel xs;
merge_one_parallel (ProcessComposition.Isabelle.ResTerm.Copyable v : xs) =
  ProcessComposition.Isabelle.ResTerm.Copyable v : merge_one_parallel xs;
merge_one_parallel (ProcessComposition.Isabelle.ResTerm.Empty : xs) =
  ProcessComposition.Isabelle.ResTerm.Empty : merge_one_parallel xs;
merge_one_parallel (ProcessComposition.Isabelle.ResTerm.Anything : xs) =
  ProcessComposition.Isabelle.ResTerm.Anything : merge_one_parallel xs;
merge_one_parallel (ProcessComposition.Isabelle.ResTerm.NonD v va : xs) =
  ProcessComposition.Isabelle.ResTerm.NonD v va : merge_one_parallel xs;
merge_one_parallel (ProcessComposition.Isabelle.ResTerm.Executable v va : xs) =
  ProcessComposition.Isabelle.ResTerm.Executable v va : merge_one_parallel xs;
merge_one_parallel (ProcessComposition.Isabelle.ResTerm.Repeatable v va : xs) =
  ProcessComposition.Isabelle.ResTerm.Repeatable v va : merge_one_parallel xs;

remove_one_empty ::
  forall a b.
    [ProcessComposition.Isabelle.ResTerm.Res_term a b] ->
      [ProcessComposition.Isabelle.ResTerm.Res_term a b];
remove_one_empty [] = [];
remove_one_empty (ProcessComposition.Isabelle.ResTerm.Empty : xs) = xs;
remove_one_empty (ProcessComposition.Isabelle.ResTerm.Res v : xs) =
  ProcessComposition.Isabelle.ResTerm.Res v : remove_one_empty xs;
remove_one_empty (ProcessComposition.Isabelle.ResTerm.Copyable v : xs) =
  ProcessComposition.Isabelle.ResTerm.Copyable v : remove_one_empty xs;
remove_one_empty (ProcessComposition.Isabelle.ResTerm.Anything : xs) =
  ProcessComposition.Isabelle.ResTerm.Anything : remove_one_empty xs;
remove_one_empty (ProcessComposition.Isabelle.ResTerm.Parallel v : xs) =
  ProcessComposition.Isabelle.ResTerm.Parallel v : remove_one_empty xs;
remove_one_empty (ProcessComposition.Isabelle.ResTerm.NonD v va : xs) =
  ProcessComposition.Isabelle.ResTerm.NonD v va : remove_one_empty xs;
remove_one_empty (ProcessComposition.Isabelle.ResTerm.Executable v va : xs) =
  ProcessComposition.Isabelle.ResTerm.Executable v va : remove_one_empty xs;
remove_one_empty (ProcessComposition.Isabelle.ResTerm.Repeatable v va : xs) =
  ProcessComposition.Isabelle.ResTerm.Repeatable v va : remove_one_empty xs;

step ::
  forall a b.
    ProcessComposition.Isabelle.ResTerm.Res_term a b ->
      ProcessComposition.Isabelle.ResTerm.Res_term a b;
step ProcessComposition.Isabelle.ResTerm.Empty =
  ProcessComposition.Isabelle.ResTerm.Empty;
step ProcessComposition.Isabelle.ResTerm.Anything =
  ProcessComposition.Isabelle.ResTerm.Anything;
step (ProcessComposition.Isabelle.ResTerm.Res x) =
  ProcessComposition.Isabelle.ResTerm.Res x;
step (ProcessComposition.Isabelle.ResTerm.Copyable x) =
  ProcessComposition.Isabelle.ResTerm.Copyable x;
step (ProcessComposition.Isabelle.ResTerm.NonD x y) =
  (if not (ProcessComposition.Isabelle.ResNormalForm.normalised x)
    then ProcessComposition.Isabelle.ResTerm.NonD (step x) y
    else (if not (ProcessComposition.Isabelle.ResNormalForm.normalised y)
           then ProcessComposition.Isabelle.ResTerm.NonD x (step y)
           else ProcessComposition.Isabelle.ResTerm.NonD x y));
step (ProcessComposition.Isabelle.ResTerm.Executable x y) =
  (if not (ProcessComposition.Isabelle.ResNormalForm.normalised x)
    then ProcessComposition.Isabelle.ResTerm.Executable (step x) y
    else (if not (ProcessComposition.Isabelle.ResNormalForm.normalised y)
           then ProcessComposition.Isabelle.ResTerm.Executable x (step y)
           else ProcessComposition.Isabelle.ResTerm.Executable x y));
step (ProcessComposition.Isabelle.ResTerm.Repeatable x y) =
  (if not (ProcessComposition.Isabelle.ResNormalForm.normalised x)
    then ProcessComposition.Isabelle.ResTerm.Repeatable (step x) y
    else (if not (ProcessComposition.Isabelle.ResNormalForm.normalised y)
           then ProcessComposition.Isabelle.ResTerm.Repeatable x (step y)
           else ProcessComposition.Isabelle.ResTerm.Repeatable x y));
step (ProcessComposition.Isabelle.ResTerm.Parallel xs) =
  (if any (\ x -> not (ProcessComposition.Isabelle.ResNormalForm.normalised x))
        xs
    then ProcessComposition.Isabelle.ResTerm.Parallel (map step xs)
    else (if any ProcessComposition.Isabelle.ResTerm.is_Parallel xs
           then ProcessComposition.Isabelle.ResTerm.Parallel
                  (merge_one_parallel xs)
           else (if any ProcessComposition.Isabelle.ResTerm.is_Empty xs
                  then ProcessComposition.Isabelle.ResTerm.Parallel
                         (remove_one_empty xs)
                  else (case xs of {
                         [] -> ProcessComposition.Isabelle.ResTerm.Empty;
                         [a] -> a;
                         _ : _ : _ ->
                           ProcessComposition.Isabelle.ResTerm.Parallel xs;
                       }))));

normal_rewr ::
  forall a b.
    ProcessComposition.Isabelle.ResTerm.Res_term a b ->
      ProcessComposition.Isabelle.ResTerm.Res_term a b;
normal_rewr x =
  (if ProcessComposition.Isabelle.ResNormalForm.normalised x then x
    else ProcessComposition.Isabelle.ResNormDirect.normal_dir (step x));

}
