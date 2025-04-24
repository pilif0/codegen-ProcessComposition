{-# LANGUAGE EmptyDataDecls, RankNTypes, ScopedTypeVariables #-}

module
  ProcessComposition.Isabelle.Resource(Resource(..), of_resource,
equal_resource, res, nonD, empty, anything, copyable, resource_par, parallel,
executable, repeatable, parallelise, parallel_parts, refine_resource,
map_resource)
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
import qualified ProcessComposition.Isabelle.ResNormDirect;
import qualified ProcessComposition.Isabelle.ResTerm;

newtype Resource a b = Abs_resource
  (ProcessComposition.Isabelle.ResTerm.Res_term a b)
  deriving (Prelude.Read, Prelude.Show);

of_resource ::
  forall a b. Resource a b -> ProcessComposition.Isabelle.ResTerm.Res_term a b;
of_resource (Abs_resource x) =
  ProcessComposition.Isabelle.ResNormDirect.normal_dir x;

equal_resource ::
  forall a b. (Eq a, Eq b) => Resource a b -> Resource a b -> Bool;
equal_resource a b =
  ProcessComposition.Isabelle.ResTerm.equal_res_term (of_resource a)
    (of_resource b);

instance (Eq a, Eq b) => Eq (Resource a b) where {
  a == b = equal_resource a b;
};

res :: forall a b. a -> Resource a b;
res x = Abs_resource (ProcessComposition.Isabelle.ResTerm.Res x);

nonD :: forall a b. Resource a b -> Resource a b -> Resource a b;
nonD (Abs_resource xa) (Abs_resource x) =
  Abs_resource (ProcessComposition.Isabelle.ResTerm.NonD xa x);

empty :: forall a b. Resource a b;
empty = Abs_resource ProcessComposition.Isabelle.ResTerm.Empty;

anything :: forall a b. Resource a b;
anything = Abs_resource ProcessComposition.Isabelle.ResTerm.Anything;

copyable :: forall a b. a -> Resource b a;
copyable x = Abs_resource (ProcessComposition.Isabelle.ResTerm.Copyable x);

resource_par :: forall a b. Resource a b -> Resource a b -> Resource a b;
resource_par (Abs_resource x) (Abs_resource y) =
  Abs_resource (ProcessComposition.Isabelle.ResTerm.Parallel [x, y]);

parallel :: forall a b. [Resource a b] -> Resource a b;
parallel (x : xs) = resource_par x (parallel xs);
parallel [] = empty;

executable :: forall a b. Resource a b -> Resource a b -> Resource a b;
executable (Abs_resource xa) (Abs_resource x) =
  Abs_resource (ProcessComposition.Isabelle.ResTerm.Executable xa x);

repeatable :: forall a b. Resource a b -> Resource a b -> Resource a b;
repeatable (Abs_resource xa) (Abs_resource x) =
  Abs_resource (ProcessComposition.Isabelle.ResTerm.Repeatable xa x);

parallelise :: forall a b. [Resource a b] -> Resource a b;
parallelise (x : y : zs) = parallel (x : y : zs);
parallelise [x] = x;
parallelise [] = empty;

parallel_parts :: forall a b. Resource a b -> [Resource a b];
parallel_parts (Abs_resource x) =
  map Abs_resource (ProcessComposition.Isabelle.ResTerm.parallel_parts x);

refine_resource ::
  forall a b c d.
    (a -> Resource b c) -> (d -> c) -> Resource a d -> Resource b c;
refine_resource f g (Abs_resource x) =
  Abs_resource
    (ProcessComposition.Isabelle.ResTerm.refine_res_term (of_resource . f) g x);

map_resource ::
  forall a b c d. (a -> b) -> (c -> d) -> Resource a c -> Resource b d;
map_resource f g (Abs_resource x) =
  Abs_resource (ProcessComposition.Isabelle.ResTerm.map_res_term f g x);

}
