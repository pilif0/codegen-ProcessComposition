{-# LANGUAGE EmptyDataDecls, RankNTypes, ScopedTypeVariables #-}

module
  ProcessComposition.Isabelle.Port(Port(..), equal_port, Side_in_out(..),
                                    Qualified_port(..), listPorts, side, index,
                                    label, portSetSide, renameQPort,
                                    qualifyQPort, name, port,
                                    equal_qualified_port)
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
import qualified ProcessComposition.Isabelle.List;
import qualified ProcessComposition.Isabelle.Arith;

data Port a b = Port a ProcessComposition.Isabelle.Arith.Nat b
  deriving (Prelude.Read, Prelude.Show);

equal_port :: forall a b. (Eq a, Eq b) => Port a b -> Port a b -> Bool;
equal_port (Port x1 x2 x3) (Port y1 y2 y3) =
  x1 == y1 && ProcessComposition.Isabelle.Arith.equal_nat x2 y2 && x3 == y3;

instance (Eq a, Eq b) => Eq (Port a b) where {
  a == b = equal_port a b;
};

class Side_in_out a where {
  ina :: a;
  out :: a;
};

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

portSetSide :: forall a b c. a -> Port b c -> Port a c;
portSetSide side (Port s i r) = Port side i r;

renameQPort ::
  forall a b c d. ([a] -> [b]) -> Qualified_port c d a -> Qualified_port c d b;
renameQPort f (QPort port path) = QPort port (f path);

qualifyQPort :: forall a b c. a -> Qualified_port b c a -> Qualified_port b c a;
qualifyQPort x (QPort port path) = QPort port (x : path);

name :: forall a b c. Qualified_port a b c -> [c];
name (QPort x1 x2) = x2;

port :: forall a b c. Qualified_port a b c -> Port a b;
port (QPort x1 x2) = x1;

equal_qualified_port ::
  forall a b c.
    (Eq a, Eq b, Eq c) => Qualified_port a b c -> Qualified_port a b c -> Bool;
equal_qualified_port (QPort x1 x2) (QPort y1 y2) = equal_port x1 y1 && x2 == y2;

}
