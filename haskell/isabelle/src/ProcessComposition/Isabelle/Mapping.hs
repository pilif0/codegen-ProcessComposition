{-# LANGUAGE EmptyDataDecls, RankNTypes, ScopedTypeVariables #-}

module
  ProcessComposition.Isabelle.Mapping(Mapping(..), empty, lookup, update,
                                       defaulta, map_entry, map_values,
                                       map_default)
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
import qualified ProcessComposition.Isabelle.Product_Type;
import qualified ProcessComposition.Isabelle.Option;
import qualified ProcessComposition.Isabelle.AList;
import qualified ProcessComposition.Isabelle.Map;

newtype Mapping a b = Mapping [(a, b)] deriving (Prelude.Read, Prelude.Show);

empty :: forall a b. Mapping a b;
empty = Mapping [];

lookup :: forall a b. (Eq a) => Mapping a b -> a -> Maybe b;
lookup (Mapping xs) = ProcessComposition.Isabelle.Map.map_of xs;

update :: forall a b. (Eq a) => a -> b -> Mapping a b -> Mapping a b;
update k v (Mapping xs) =
  Mapping (ProcessComposition.Isabelle.AList.update k v xs);

defaulta :: forall a b. (Eq a) => a -> b -> Mapping a b -> Mapping a b;
defaulta k v m =
  (if not (ProcessComposition.Isabelle.Option.is_none (lookup m k)) then m
    else update k v m);

map_entry :: forall a b. (Eq a) => a -> (b -> b) -> Mapping a b -> Mapping a b;
map_entry k f m = (case lookup m k of {
                    Nothing -> m;
                    Just v -> update k (f v) m;
                  });

map_values :: forall a b c. (a -> b -> c) -> Mapping a b -> Mapping a c;
map_values f (Mapping xs) = Mapping (map (\ (x, y) -> (x, f x y)) xs);

map_default ::
  forall a b. (Eq a) => a -> b -> (b -> b) -> Mapping a b -> Mapping a b;
map_default k v f m = map_entry k f (defaulta k v m);

}
