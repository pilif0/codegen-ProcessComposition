{-# LANGUAGE EmptyDataDecls, RankNTypes, ScopedTypeVariables #-}

module
  ProcessComposition.Isabelle.Set(Set(..), image, filtera, insert, member,
                                   bot_set)
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
import qualified ProcessComposition.Isabelle.List;

data Set a = Set [a] | Coset [a] deriving (Prelude.Read, Prelude.Show);

image :: forall a b. (a -> b) -> Set a -> Set b;
image f (Set xs) = Set (map f xs);

filtera :: forall a. (a -> Bool) -> Set a -> Set a;
filtera p (Set xs) = Set (filter p xs);

insert :: forall a. (Eq a) => a -> Set a -> Set a;
insert x (Coset xs) = Coset (ProcessComposition.Isabelle.List.removeAll x xs);
insert x (Set xs) = Set (ProcessComposition.Isabelle.List.insert x xs);

member :: forall a. (Eq a) => a -> Set a -> Bool;
member x (Coset xs) = not (ProcessComposition.Isabelle.List.member xs x);
member x (Set xs) = ProcessComposition.Isabelle.List.member xs x;

bot_set :: forall a. Set a;
bot_set = Set [];

}
