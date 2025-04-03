{-# LANGUAGE EmptyDataDecls, RankNTypes, ScopedTypeVariables #-}

module
  ProcessComposition.Isabelle.ResTerm(Res_term(..), equal_res_term, parallelise,
                                       parallel_parts, refine_res_term,
                                       is_Empty, remove_all_empty,
                                       merge_all_parallel, is_Parallel,
                                       map_res_term)
  where {

import Prelude ((==), (/=), (<), (<=), (>=), (>), (+), (-), (*), (/), (**),
  (>>=), (>>), (=<<), (&&), (||), (^), (^^), (.), ($), ($!), (++), (!!), Eq,
  error, id, return, not, fst, snd, map, filter, concat, concatMap, reverse,
  zip, null, takeWhile, dropWhile, all, any, Integer, negate, abs, divMod,
  String, Bool(True, False), Maybe(Nothing, Just));
import Data.Bits ((.&.), (.|.), (.^.));
import qualified Prelude;
import qualified Data.Bits;

data Res_term a b = Res a | Copyable b | Empty | Anything
  | Parallel [Res_term a b] | NonD (Res_term a b) (Res_term a b)
  | Executable (Res_term a b) (Res_term a b)
  | Repeatable (Res_term a b) (Res_term a b)
  deriving (Prelude.Read, Prelude.Show);

instance (Eq a, Eq b) => Eq (Res_term a b) where {
  a == b = equal_res_term a b;
};

equal_res_term ::
  forall a b. (Eq a, Eq b) => Res_term a b -> Res_term a b -> Bool;
equal_res_term (Executable x71 x72) (Repeatable x81 x82) = False;
equal_res_term (Repeatable x81 x82) (Executable x71 x72) = False;
equal_res_term (NonD x61 x62) (Repeatable x81 x82) = False;
equal_res_term (Repeatable x81 x82) (NonD x61 x62) = False;
equal_res_term (NonD x61 x62) (Executable x71 x72) = False;
equal_res_term (Executable x71 x72) (NonD x61 x62) = False;
equal_res_term (Parallel x5) (Repeatable x81 x82) = False;
equal_res_term (Repeatable x81 x82) (Parallel x5) = False;
equal_res_term (Parallel x5) (Executable x71 x72) = False;
equal_res_term (Executable x71 x72) (Parallel x5) = False;
equal_res_term (Parallel x5) (NonD x61 x62) = False;
equal_res_term (NonD x61 x62) (Parallel x5) = False;
equal_res_term Anything (Repeatable x81 x82) = False;
equal_res_term (Repeatable x81 x82) Anything = False;
equal_res_term Anything (Executable x71 x72) = False;
equal_res_term (Executable x71 x72) Anything = False;
equal_res_term Anything (NonD x61 x62) = False;
equal_res_term (NonD x61 x62) Anything = False;
equal_res_term Anything (Parallel x5) = False;
equal_res_term (Parallel x5) Anything = False;
equal_res_term Empty (Repeatable x81 x82) = False;
equal_res_term (Repeatable x81 x82) Empty = False;
equal_res_term Empty (Executable x71 x72) = False;
equal_res_term (Executable x71 x72) Empty = False;
equal_res_term Empty (NonD x61 x62) = False;
equal_res_term (NonD x61 x62) Empty = False;
equal_res_term Empty (Parallel x5) = False;
equal_res_term (Parallel x5) Empty = False;
equal_res_term Empty Anything = False;
equal_res_term Anything Empty = False;
equal_res_term (Copyable x2) (Repeatable x81 x82) = False;
equal_res_term (Repeatable x81 x82) (Copyable x2) = False;
equal_res_term (Copyable x2) (Executable x71 x72) = False;
equal_res_term (Executable x71 x72) (Copyable x2) = False;
equal_res_term (Copyable x2) (NonD x61 x62) = False;
equal_res_term (NonD x61 x62) (Copyable x2) = False;
equal_res_term (Copyable x2) (Parallel x5) = False;
equal_res_term (Parallel x5) (Copyable x2) = False;
equal_res_term (Copyable x2) Anything = False;
equal_res_term Anything (Copyable x2) = False;
equal_res_term (Copyable x2) Empty = False;
equal_res_term Empty (Copyable x2) = False;
equal_res_term (Res x1) (Repeatable x81 x82) = False;
equal_res_term (Repeatable x81 x82) (Res x1) = False;
equal_res_term (Res x1) (Executable x71 x72) = False;
equal_res_term (Executable x71 x72) (Res x1) = False;
equal_res_term (Res x1) (NonD x61 x62) = False;
equal_res_term (NonD x61 x62) (Res x1) = False;
equal_res_term (Res x1) (Parallel x5) = False;
equal_res_term (Parallel x5) (Res x1) = False;
equal_res_term (Res x1) Anything = False;
equal_res_term Anything (Res x1) = False;
equal_res_term (Res x1) Empty = False;
equal_res_term Empty (Res x1) = False;
equal_res_term (Res x1) (Copyable x2) = False;
equal_res_term (Copyable x2) (Res x1) = False;
equal_res_term (Repeatable x81 x82) (Repeatable y81 y82) =
  equal_res_term x81 y81 && equal_res_term x82 y82;
equal_res_term (Executable x71 x72) (Executable y71 y72) =
  equal_res_term x71 y71 && equal_res_term x72 y72;
equal_res_term (NonD x61 x62) (NonD y61 y62) =
  equal_res_term x61 y61 && equal_res_term x62 y62;
equal_res_term (Parallel x5) (Parallel y5) = x5 == y5;
equal_res_term (Copyable x2) (Copyable y2) = x2 == y2;
equal_res_term (Res x1) (Res y1) = x1 == y1;
equal_res_term Anything Anything = True;
equal_res_term Empty Empty = True;

parallelise :: forall a b. [Res_term a b] -> Res_term a b;
parallelise [] = Empty;
parallelise [x] = x;
parallelise (v : vb : vc) = Parallel (v : vb : vc);

parallel_parts :: forall a b. Res_term a b -> [Res_term a b];
parallel_parts Empty = [];
parallel_parts Anything = [Anything];
parallel_parts (Res a) = [Res a];
parallel_parts (Copyable a) = [Copyable a];
parallel_parts (Parallel xs) = concatMap parallel_parts xs;
parallel_parts (NonD a b) = [NonD a b];
parallel_parts (Executable a b) = [Executable a b];
parallel_parts (Repeatable a b) = [Repeatable a b];

refine_res_term ::
  forall a b c d.
    (a -> Res_term b c) -> (d -> c) -> Res_term a d -> Res_term b c;
refine_res_term f g Empty = Empty;
refine_res_term f g Anything = Anything;
refine_res_term f g (Res a) = f a;
refine_res_term f g (Copyable x) = Copyable (g x);
refine_res_term f g (Parallel xs) = Parallel (map (refine_res_term f g) xs);
refine_res_term f g (NonD x y) =
  NonD (refine_res_term f g x) (refine_res_term f g y);
refine_res_term f g (Executable x y) =
  Executable (refine_res_term f g x) (refine_res_term f g y);
refine_res_term f g (Repeatable x y) =
  Repeatable (refine_res_term f g x) (refine_res_term f g y);

is_Empty :: forall a b. Res_term a b -> Bool;
is_Empty (Res x1) = False;
is_Empty (Copyable x2) = False;
is_Empty Empty = True;
is_Empty Anything = False;
is_Empty (Parallel x5) = False;
is_Empty (NonD x61 x62) = False;
is_Empty (Executable x71 x72) = False;
is_Empty (Repeatable x81 x82) = False;

remove_all_empty :: forall a b. [Res_term a b] -> [Res_term a b];
remove_all_empty [] = [];
remove_all_empty (x : xs) =
  (if is_Empty x then remove_all_empty xs else x : remove_all_empty xs);

merge_all_parallel :: forall a b. [Res_term a b] -> [Res_term a b];
merge_all_parallel [] = [];
merge_all_parallel (x : xs) = (case x of {
                                Res _ -> x : merge_all_parallel xs;
                                Copyable _ -> x : merge_all_parallel xs;
                                Empty -> x : merge_all_parallel xs;
                                Anything -> x : merge_all_parallel xs;
                                Parallel y -> y ++ merge_all_parallel xs;
                                NonD _ _ -> x : merge_all_parallel xs;
                                Executable _ _ -> x : merge_all_parallel xs;
                                Repeatable _ _ -> x : merge_all_parallel xs;
                              });

is_Parallel :: forall a b. Res_term a b -> Bool;
is_Parallel (Res x1) = False;
is_Parallel (Copyable x2) = False;
is_Parallel Empty = False;
is_Parallel Anything = False;
is_Parallel (Parallel x5) = True;
is_Parallel (NonD x61 x62) = False;
is_Parallel (Executable x71 x72) = False;
is_Parallel (Repeatable x81 x82) = False;

map_res_term ::
  forall a b c d. (a -> b) -> (c -> d) -> Res_term a c -> Res_term b d;
map_res_term f1 f2 (Res x1) = Res (f1 x1);
map_res_term f1 f2 (Copyable x2) = Copyable (f2 x2);
map_res_term f1 f2 Empty = Empty;
map_res_term f1 f2 Anything = Anything;
map_res_term f1 f2 (Parallel x5) = Parallel (map (map_res_term f1 f2) x5);
map_res_term f1 f2 (NonD x61 x62) =
  NonD (map_res_term f1 f2 x61) (map_res_term f1 f2 x62);
map_res_term f1 f2 (Executable x71 x72) =
  Executable (map_res_term f1 f2 x71) (map_res_term f1 f2 x72);
map_res_term f1 f2 (Repeatable x81 x82) =
  Repeatable (map_res_term f1 f2 x81) (map_res_term f1 f2 x82);

}
