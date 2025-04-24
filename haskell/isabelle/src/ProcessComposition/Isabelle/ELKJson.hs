{-# LANGUAGE EmptyDataDecls, RankNTypes, ScopedTypeVariables #-}

module
  ProcessComposition.Isabelle.ELKJson(Elk_side(..), equal_elk_side,
                                       sideInOutToELK, portSideToString,
                                       groundPlaceToID, openPlaceToID,
                                       placeToID, edgeToJSON, groundPortToJSON,
                                       nodeToJSON, openPortToJSON,
                                       portGraphToJSON)
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
import qualified ProcessComposition.Isabelle.Set;
import qualified ProcessComposition.Isabelle.Show_Instances;
import qualified ProcessComposition.Isabelle.Str;
import qualified ProcessComposition.Isabelle.List;
import qualified ProcessComposition.Isabelle.Nano_JSON;
import qualified ProcessComposition.Isabelle.Arith;
import qualified ProcessComposition.Isabelle.PortGraph;
import qualified ProcessComposition.Isabelle.Port;

data Elk_side = UNDEFINED | NORTH | EAST | SOUTH | WEST
  deriving (Prelude.Read, Prelude.Show);

equal_elk_side :: Elk_side -> Elk_side -> Bool;
equal_elk_side SOUTH WEST = False;
equal_elk_side WEST SOUTH = False;
equal_elk_side EAST WEST = False;
equal_elk_side WEST EAST = False;
equal_elk_side EAST SOUTH = False;
equal_elk_side SOUTH EAST = False;
equal_elk_side NORTH WEST = False;
equal_elk_side WEST NORTH = False;
equal_elk_side NORTH SOUTH = False;
equal_elk_side SOUTH NORTH = False;
equal_elk_side NORTH EAST = False;
equal_elk_side EAST NORTH = False;
equal_elk_side UNDEFINED WEST = False;
equal_elk_side WEST UNDEFINED = False;
equal_elk_side UNDEFINED SOUTH = False;
equal_elk_side SOUTH UNDEFINED = False;
equal_elk_side UNDEFINED EAST = False;
equal_elk_side EAST UNDEFINED = False;
equal_elk_side UNDEFINED NORTH = False;
equal_elk_side NORTH UNDEFINED = False;
equal_elk_side WEST WEST = True;
equal_elk_side SOUTH SOUTH = True;
equal_elk_side EAST EAST = True;
equal_elk_side NORTH NORTH = True;
equal_elk_side UNDEFINED UNDEFINED = True;

instance Eq Elk_side where {
  a == b = equal_elk_side a b;
};

sideInOutToELK ::
  forall a.
    (Eq a, ProcessComposition.Isabelle.Port.Side_in_out a) => a -> Elk_side;
sideInOutToELK x =
  (if x == ProcessComposition.Isabelle.Port.ina then WEST
    else (if x == ProcessComposition.Isabelle.Port.out then EAST
           else UNDEFINED));

portSideToString :: Elk_side -> String;
portSideToString UNDEFINED = "UNDEFINED";
portSideToString NORTH = "NORTH";
portSideToString EAST = "EAST";
portSideToString SOUTH = "SOUTH";
portSideToString WEST = "WEST";

groundPlaceToID ::
  forall a b c.
    ([a] -> String) ->
      (b -> Elk_side) ->
        [a] -> ProcessComposition.Isabelle.PortGraph.Place b c a -> String;
groundPlaceToID pathToString portSideToELK prefix p =
  (pathToString
     (prefix ++ ProcessComposition.Isabelle.PortGraph.place_name p) ++
    portSideToString
      (portSideToELK
        (ProcessComposition.Isabelle.Port.side
          (ProcessComposition.Isabelle.PortGraph.place_port p)))) ++
    ProcessComposition.Isabelle.Str.implode
      (ProcessComposition.Isabelle.Show_Instances.shows_prec_nat
        ProcessComposition.Isabelle.Arith.zero_nat
        (ProcessComposition.Isabelle.Port.index
          (ProcessComposition.Isabelle.PortGraph.place_port p))
        []);

openPlaceToID ::
  forall a b.
    (a -> Elk_side) -> ProcessComposition.Isabelle.Port.Port a b -> String;
openPlaceToID portSideToELK port =
  ("Open" ++
    portSideToString
      (portSideToELK (ProcessComposition.Isabelle.Port.side port))) ++
    ProcessComposition.Isabelle.Str.implode
      (ProcessComposition.Isabelle.Show_Instances.shows_prec_nat
        ProcessComposition.Isabelle.Arith.zero_nat
        (ProcessComposition.Isabelle.Port.index port) []);

placeToID ::
  forall a b c.
    ([a] -> String) ->
      (b -> Elk_side) ->
        [a] -> ProcessComposition.Isabelle.PortGraph.Place b c a -> String;
placeToID pathToString portSideToELK prefix p =
  (case p of {
    ProcessComposition.Isabelle.PortGraph.GroundPort _ ->
      groundPlaceToID pathToString portSideToELK prefix p;
    ProcessComposition.Isabelle.PortGraph.OpenPort a ->
      openPlaceToID portSideToELK a;
  });

edgeToJSON ::
  forall a b c.
    ([a] -> String) ->
      (b -> Elk_side) ->
        [a] ->
          ProcessComposition.Isabelle.PortGraph.Edge b c a ->
            ProcessComposition.Isabelle.Nano_JSON.Json String
              ProcessComposition.Isabelle.Arith.Int;
edgeToJSON pathToString portSideToELK prefix e =
  ProcessComposition.Isabelle.Nano_JSON.OBJECT
    [("id",
       ProcessComposition.Isabelle.Nano_JSON.STRING
         ((placeToID pathToString portSideToELK prefix
             (ProcessComposition.Isabelle.PortGraph.edge_from e) ++
            "-") ++
           placeToID pathToString portSideToELK prefix
             (ProcessComposition.Isabelle.PortGraph.edge_to e))),
      ("sources",
        ProcessComposition.Isabelle.Nano_JSON.ARRAY
          [ProcessComposition.Isabelle.Nano_JSON.STRING
             (placeToID pathToString portSideToELK prefix
               (ProcessComposition.Isabelle.PortGraph.edge_from e))]),
      ("targets",
        ProcessComposition.Isabelle.Nano_JSON.ARRAY
          [ProcessComposition.Isabelle.Nano_JSON.STRING
             (placeToID pathToString portSideToELK prefix
               (ProcessComposition.Isabelle.PortGraph.edge_to e))])];

groundPortToJSON ::
  forall a b c.
    ([a] -> String) ->
      (b -> Elk_side) ->
        (c -> String) ->
          [a] ->
            ProcessComposition.Isabelle.Arith.Nat ->
              ProcessComposition.Isabelle.Port.Port b c ->
                ProcessComposition.Isabelle.Nano_JSON.Json String
                  ProcessComposition.Isabelle.Arith.Int;
groundPortToJSON pathToString portSideToELK portLabelToString prefix maxIdx p =
  ProcessComposition.Isabelle.Nano_JSON.OBJECT
    [("id",
       ProcessComposition.Isabelle.Nano_JSON.STRING
         ((pathToString prefix ++
            portSideToString
              (portSideToELK (ProcessComposition.Isabelle.Port.side p))) ++
           ProcessComposition.Isabelle.Str.implode
             (ProcessComposition.Isabelle.Show_Instances.shows_prec_nat
               ProcessComposition.Isabelle.Arith.zero_nat
               (ProcessComposition.Isabelle.Port.index p) []))),
      ("resource",
        ProcessComposition.Isabelle.Nano_JSON.STRING
          (portLabelToString (ProcessComposition.Isabelle.Port.label p))),
      ("properties",
        ProcessComposition.Isabelle.Nano_JSON.OBJECT
          [("port.side",
             ProcessComposition.Isabelle.Nano_JSON.STRING
               (portSideToString
                 (portSideToELK (ProcessComposition.Isabelle.Port.side p)))),
            ("port.index",
              ProcessComposition.Isabelle.Nano_JSON.NUMBER
                (ProcessComposition.Isabelle.Arith.int_of_nat
                  (if ProcessComposition.Isabelle.Set.member
                        (portSideToELK
                          (ProcessComposition.Isabelle.Port.side p))
                        (ProcessComposition.Isabelle.Set.insert WEST
                          (ProcessComposition.Isabelle.Set.insert SOUTH
                            ProcessComposition.Isabelle.Set.bot_set))
                    then ProcessComposition.Isabelle.Arith.minus_nat maxIdx
                           (ProcessComposition.Isabelle.Port.index p)
                    else ProcessComposition.Isabelle.Port.index p)))]),
      ("width",
        ProcessComposition.Isabelle.Nano_JSON.NUMBER
          (ProcessComposition.Isabelle.Arith.Int_of_integer (10 :: Integer))),
      ("height",
        ProcessComposition.Isabelle.Nano_JSON.NUMBER
          (ProcessComposition.Isabelle.Arith.Int_of_integer (10 :: Integer)))];

nodeToJSON ::
  forall a b c d.
    (Eq c) => ([a] -> String) ->
                (b -> String) ->
                  (c -> Elk_side) ->
                    (d -> String) ->
                      [a] ->
                        ProcessComposition.Isabelle.PortGraph.Node c d a b ->
                          ProcessComposition.Isabelle.Nano_JSON.Json String
                            ProcessComposition.Isabelle.Arith.Int;
nodeToJSON pathToString labelToString portSideToELK portLabelToString prefix n =
  ProcessComposition.Isabelle.Nano_JSON.OBJECT
    [("id",
       ProcessComposition.Isabelle.Nano_JSON.STRING
         (pathToString
           (prefix ++ ProcessComposition.Isabelle.PortGraph.node_name n))),
      ("width",
        ProcessComposition.Isabelle.Nano_JSON.NUMBER
          (ProcessComposition.Isabelle.Arith.Int_of_integer (60 :: Integer))),
      ("height",
        ProcessComposition.Isabelle.Nano_JSON.NUMBER
          (ProcessComposition.Isabelle.Arith.Int_of_integer (60 :: Integer))),
      ("properties",
        ProcessComposition.Isabelle.Nano_JSON.OBJECT
          [("portConstraints",
             ProcessComposition.Isabelle.Nano_JSON.STRING "FIXED_ORDER"),
            ("nodeLabels.placement",
              ProcessComposition.Isabelle.Nano_JSON.STRING
                "[H_CENTER, V_CENTER, INSIDE]")]),
      ("labels",
        ProcessComposition.Isabelle.Nano_JSON.ARRAY
          [ProcessComposition.Isabelle.Nano_JSON.OBJECT
             [("text",
                ProcessComposition.Isabelle.Nano_JSON.STRING
                  (labelToString
                    (ProcessComposition.Isabelle.PortGraph.node_label n))),
               ("id",
                 ProcessComposition.Isabelle.Nano_JSON.STRING
                   (pathToString
                      (prefix ++
                        ProcessComposition.Isabelle.PortGraph.node_name n) ++
                     "_label")),
               ("width",
                 ProcessComposition.Isabelle.Nano_JSON.NUMBER
                   (ProcessComposition.Isabelle.Arith.Int_of_integer
                     (20 :: Integer))),
               ("height",
                 ProcessComposition.Isabelle.Nano_JSON.NUMBER
                   (ProcessComposition.Isabelle.Arith.Int_of_integer
                     (20 :: Integer)))]]),
      ("ports",
        ProcessComposition.Isabelle.Nano_JSON.ARRAY
          (map (\ p ->
                 groundPortToJSON pathToString portSideToELK portLabelToString
                   (prefix ++ ProcessComposition.Isabelle.PortGraph.node_name n)
                   (ProcessComposition.Isabelle.Arith.minus_nat
                     (ProcessComposition.Isabelle.List.size_list
                       (filter
                         (\ x ->
                           ProcessComposition.Isabelle.Port.side x ==
                             ProcessComposition.Isabelle.Port.side p)
                         (ProcessComposition.Isabelle.PortGraph.node_ports n)))
                     ProcessComposition.Isabelle.Arith.one_nat)
                   p)
            (ProcessComposition.Isabelle.PortGraph.node_ports n)))];

openPortToJSON ::
  forall a b.
    (a -> Elk_side) ->
      (b -> String) ->
        ProcessComposition.Isabelle.Arith.Nat ->
          ProcessComposition.Isabelle.Port.Port a b ->
            ProcessComposition.Isabelle.Nano_JSON.Json String
              ProcessComposition.Isabelle.Arith.Int;
openPortToJSON portSideToELK portLabelToString maxIdx p =
  ProcessComposition.Isabelle.Nano_JSON.OBJECT
    [("id",
       ProcessComposition.Isabelle.Nano_JSON.STRING
         (openPlaceToID portSideToELK p)),
      ("resource",
        ProcessComposition.Isabelle.Nano_JSON.STRING
          (portLabelToString (ProcessComposition.Isabelle.Port.label p))),
      ("properties",
        ProcessComposition.Isabelle.Nano_JSON.OBJECT
          [("port.side",
             ProcessComposition.Isabelle.Nano_JSON.STRING
               (portSideToString
                 (portSideToELK (ProcessComposition.Isabelle.Port.side p)))),
            ("port.index",
              ProcessComposition.Isabelle.Nano_JSON.NUMBER
                (ProcessComposition.Isabelle.Arith.int_of_nat
                  (if ProcessComposition.Isabelle.Set.member
                        (portSideToELK
                          (ProcessComposition.Isabelle.Port.side p))
                        (ProcessComposition.Isabelle.Set.insert WEST
                          (ProcessComposition.Isabelle.Set.insert SOUTH
                            ProcessComposition.Isabelle.Set.bot_set))
                    then ProcessComposition.Isabelle.Arith.minus_nat maxIdx
                           (ProcessComposition.Isabelle.Port.index p)
                    else ProcessComposition.Isabelle.Port.index p)))]),
      ("width",
        ProcessComposition.Isabelle.Nano_JSON.NUMBER
          (ProcessComposition.Isabelle.Arith.Int_of_integer (10 :: Integer))),
      ("height",
        ProcessComposition.Isabelle.Nano_JSON.NUMBER
          (ProcessComposition.Isabelle.Arith.Int_of_integer (10 :: Integer)))];

portGraphToJSON ::
  forall a b c d.
    (Eq c) => ([a] -> String) ->
                (b -> String) ->
                  (c -> Elk_side) ->
                    (d -> String) ->
                      [a] ->
                        ProcessComposition.Isabelle.PortGraph.Port_graph c d a
                          b ->
                          ProcessComposition.Isabelle.Nano_JSON.Json String
                            ProcessComposition.Isabelle.Arith.Int;
portGraphToJSON pathToString labelToString portSideToELK portLabelToString
  prefix g =
  ProcessComposition.Isabelle.Nano_JSON.OBJECT
    [("id",
       ProcessComposition.Isabelle.Nano_JSON.STRING
         (pathToString prefix ++ "Root")),
      ("layoutOptions",
        ProcessComposition.Isabelle.Nano_JSON.OBJECT
          [("algorithm",
             ProcessComposition.Isabelle.Nano_JSON.STRING "layered")]),
      ("children",
        ProcessComposition.Isabelle.Nano_JSON.ARRAY
          (map (nodeToJSON pathToString labelToString portSideToELK
                 portLabelToString prefix)
             (ProcessComposition.Isabelle.PortGraph.pg_nodes g) ++
            map (\ p ->
                  openPortToJSON portSideToELK portLabelToString
                    (ProcessComposition.Isabelle.Arith.minus_nat
                      (ProcessComposition.Isabelle.List.size_list
                        (filter
                          (\ x ->
                            ProcessComposition.Isabelle.Port.side x ==
                              ProcessComposition.Isabelle.Port.side p)
                          (ProcessComposition.Isabelle.PortGraph.pg_ports g)))
                      ProcessComposition.Isabelle.Arith.one_nat)
                    p)
              (ProcessComposition.Isabelle.PortGraph.pg_ports g))),
      ("edges",
        ProcessComposition.Isabelle.Nano_JSON.ARRAY
          (map (edgeToJSON pathToString portSideToELK prefix)
            (ProcessComposition.Isabelle.PortGraph.pg_edges g)))];

}
