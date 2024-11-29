{-# LANGUAGE EmptyDataDecls, RankNTypes, ScopedTypeVariables #-}

module
  ProcessComposition.Isabelle.Port(Port(..), Qualified_port(..), listPorts,
                                    side, index, label, renameQPort,
                                    qualifyQPort, name, port)
  where {

import Prelude ((==), (/=), (<), (<=), (>=), (>), (+), (-), (*), (/), (**),
  (>>=), (>>), (=<<), (&&), (||), (^), (^^), (.), ($), ($!), (++), (!!), Eq,
  error, id, return, not, fst, snd, map, filter, concat, concatMap, reverse,
  zip, null, takeWhile, dropWhile, all, any, Integer, negate, abs, divMod,
  String, Bool(True, False), Maybe(Nothing, Just));
import qualified Prelude;
import qualified ProcessComposition.Isabelle.Product_Type;
import qualified ProcessComposition.Isabelle.List;
import qualified ProcessComposition.Isabelle.Arith;

data Port a b = Port a ProcessComposition.Isabelle.Arith.Nat b
  deriving (Prelude.Read, Prelude.Show);

data Qualified_port a b c = QPort (Port a b) [c]
  deriving (Prelude.Read, Prelude.Show);

listPorts ::
  forall a b. ProcessComposition.Isabelle.Arith.Nat -> a -> [b] -> [Port a b];
listPorts n s asa =
  map (\ (a, b) -> Port s a b)
    (zip (ProcessComposition.Isabelle.List.upt n
           (ProcessComposition.Isabelle.Arith.plus_nat n
             (ProcessComposition.Isabelle.List.size_list asa)))
      asa);

side :: forall a b. Port a b -> a;
side (Port x1 x2 x3) = x1;

index :: forall a b. Port a b -> ProcessComposition.Isabelle.Arith.Nat;
index (Port x1 x2 x3) = x2;

label :: forall a b. Port a b -> b;
label (Port x1 x2 x3) = x3;

renameQPort ::
  forall a b c d. ([a] -> [b]) -> Qualified_port c d a -> Qualified_port c d b;
renameQPort f (QPort port path) = QPort port (f path);

qualifyQPort :: forall a b c. a -> Qualified_port b c a -> Qualified_port b c a;
qualifyQPort x (QPort port path) = QPort port (x : path);

name :: forall a b c. Qualified_port a b c -> [c];
name (QPort x1 x2) = x2;

port :: forall a b c. Qualified_port a b c -> Port a b;
port (QPort x1 x2) = x1;

}
