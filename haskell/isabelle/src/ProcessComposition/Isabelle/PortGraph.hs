{-# LANGUAGE EmptyDataDecls, RankNTypes, ScopedTypeVariables #-}

module
  ProcessComposition.Isabelle.PortGraph(Place(..), equal_place, Edge(..),
 equal_edge, Node(..), equal_node, Port_graph(..), shiftPort, place_name,
 place_port, edge_from, qualifyPlace, edge_to, qualifyEdge, node_ports,
 node_label, node_name, qualifyNode, shiftOpenPlace, shiftOpenInEdge,
 place_open, pg_ports, pg_nodes, pg_edges, qualifyPortGraph,
 disconnectFromPlaces, equal_port_graph)
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
import qualified ProcessComposition.Isabelle.Arith;
import qualified ProcessComposition.Isabelle.Port;

data Place a b c =
  GroundPort (ProcessComposition.Isabelle.Port.Qualified_port a b c)
  | OpenPort (ProcessComposition.Isabelle.Port.Port a b)
  deriving (Prelude.Read, Prelude.Show);

equal_place ::
  forall a b c. (Eq a, Eq b, Eq c) => Place a b c -> Place a b c -> Bool;
equal_place (GroundPort x1) (OpenPort x2) = False;
equal_place (OpenPort x2) (GroundPort x1) = False;
equal_place (OpenPort x2) (OpenPort y2) =
  ProcessComposition.Isabelle.Port.equal_port x2 y2;
equal_place (GroundPort x1) (GroundPort y1) =
  ProcessComposition.Isabelle.Port.equal_qualified_port x1 y1;

data Edge a b c = Edge (Place a b c) (Place a b c)
  deriving (Prelude.Read, Prelude.Show);

equal_edge ::
  forall a b c. (Eq a, Eq b, Eq c) => Edge a b c -> Edge a b c -> Bool;
equal_edge (Edge x1 x2) (Edge y1 y2) = equal_place x1 y1 && equal_place x2 y2;

instance (Eq a, Eq b, Eq c) => Eq (Edge a b c) where {
  a == b = equal_edge a b;
};

data Node a b c d = Node [c] d [ProcessComposition.Isabelle.Port.Port a b]
  deriving (Prelude.Read, Prelude.Show);

equal_node ::
  forall a b c d.
    (Eq a, Eq b, Eq c, Eq d) => Node a b c d -> Node a b c d -> Bool;
equal_node (Node x1 x2 x3) (Node y1 y2 y3) = x1 == y1 && x2 == y2 && x3 == y3;

instance (Eq a, Eq b, Eq c, Eq d) => Eq (Node a b c d) where {
  a == b = equal_node a b;
};

instance (Eq a, Eq b, Eq c) => Eq (Place a b c) where {
  a == b = equal_place a b;
};

data Port_graph a b c d =
  PGraph [Node a b c d] [Edge a b c] [ProcessComposition.Isabelle.Port.Port a b]
  deriving (Prelude.Read, Prelude.Show);

shiftPort ::
  forall a b.
    (a -> ProcessComposition.Isabelle.Arith.Nat) ->
      ProcessComposition.Isabelle.Port.Port a b ->
        ProcessComposition.Isabelle.Port.Port a b;
shiftPort n p =
  ProcessComposition.Isabelle.Port.Port
    (ProcessComposition.Isabelle.Port.side p)
    (ProcessComposition.Isabelle.Arith.plus_nat
      (n (ProcessComposition.Isabelle.Port.side p))
      (ProcessComposition.Isabelle.Port.index p))
    (ProcessComposition.Isabelle.Port.label p);

place_name :: forall a b c. Place a b c -> [c];
place_name (GroundPort qp) = ProcessComposition.Isabelle.Port.name qp;

place_port ::
  forall a b c. Place a b c -> ProcessComposition.Isabelle.Port.Port a b;
place_port (GroundPort qp) = ProcessComposition.Isabelle.Port.port qp;
place_port (OpenPort p) = p;

edge_from :: forall a b c. Edge a b c -> Place a b c;
edge_from (Edge x1 x2) = x1;

qualifyPlace :: forall a b c. a -> Place b c a -> Place b c a;
qualifyPlace x (GroundPort qport) =
  GroundPort (ProcessComposition.Isabelle.Port.qualifyQPort x qport);
qualifyPlace x (OpenPort port) = OpenPort port;

edge_to :: forall a b c. Edge a b c -> Place a b c;
edge_to (Edge x1 x2) = x2;

qualifyEdge :: forall a b c. a -> Edge b c a -> Edge b c a;
qualifyEdge x e =
  Edge (qualifyPlace x (edge_from e)) (qualifyPlace x (edge_to e));

node_ports ::
  forall a b c d. Node a b c d -> [ProcessComposition.Isabelle.Port.Port a b];
node_ports (Node x1 x2 x3) = x3;

node_label :: forall a b c d. Node a b c d -> d;
node_label (Node x1 x2 x3) = x2;

node_name :: forall a b c d. Node a b c d -> [c];
node_name (Node x1 x2 x3) = x1;

qualifyNode :: forall a b c d. a -> Node b c a d -> Node b c a d;
qualifyNode x n = Node (x : node_name n) (node_label n) (node_ports n);

shiftOpenPlace ::
  forall a b c.
    (a -> ProcessComposition.Isabelle.Arith.Nat) -> Place a b c -> Place a b c;
shiftOpenPlace n (GroundPort p) = GroundPort p;
shiftOpenPlace n (OpenPort p) = OpenPort (shiftPort n p);

shiftOpenInEdge ::
  forall a b c.
    (a -> ProcessComposition.Isabelle.Arith.Nat) ->
      (a -> ProcessComposition.Isabelle.Arith.Nat) -> Edge a b c -> Edge a b c;
shiftOpenInEdge m n e =
  Edge (shiftOpenPlace m (edge_from e)) (shiftOpenPlace n (edge_to e));

place_open :: forall a b c. Place a b c -> Bool;
place_open (GroundPort x1) = False;
place_open (OpenPort x2) = True;

pg_ports ::
  forall a b c d.
    Port_graph a b c d -> [ProcessComposition.Isabelle.Port.Port a b];
pg_ports (PGraph x1 x2 x3) = x3;

pg_nodes :: forall a b c d. Port_graph a b c d -> [Node a b c d];
pg_nodes (PGraph x1 x2 x3) = x1;

pg_edges :: forall a b c d. Port_graph a b c d -> [Edge a b c];
pg_edges (PGraph x1 x2 x3) = x2;

qualifyPortGraph ::
  forall a b c d. a -> Port_graph b c a d -> Port_graph b c a d;
qualifyPortGraph x g =
  PGraph (map (qualifyNode x) (pg_nodes g)) (map (qualifyEdge x) (pg_edges g))
    (pg_ports g);

disconnectFromPlaces ::
  forall a b c.
    (Eq a, Eq b, Eq c) => [Place a b c] -> [Edge a b c] -> [Edge a b c];
disconnectFromPlaces places edges =
  filter
    (\ e ->
      not (ProcessComposition.Isabelle.List.member places (edge_from e)) &&
        not (ProcessComposition.Isabelle.List.member places (edge_to e)))
    edges;

equal_port_graph ::
  forall a b c d.
    (Eq a, Eq b, Eq c,
      Eq d) => Port_graph a b c d -> Port_graph a b c d -> Bool;
equal_port_graph (PGraph x1 x2 x3) (PGraph y1 y2 y3) =
  x1 == y1 && x2 == y2 && x3 == y3;

}
