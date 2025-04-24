{-# LANGUAGE EmptyDataDecls, RankNTypes, ScopedTypeVariables #-}

module
  ProcessComposition.Isabelle.ProcessPortGraph(Node_content(..),
        forgetPortGraph, oncePortGraph, pgConstruct)
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
import qualified ProcessComposition.Isabelle.Port;
import qualified ProcessComposition.Isabelle.Arith;
import qualified ProcessComposition.Isabelle.IdentityPortGraph;
import qualified ProcessComposition.Isabelle.SwapPortGraph;
import qualified ProcessComposition.Isabelle.NodePortGraph;
import qualified ProcessComposition.Isabelle.ForkPortGraph;
import qualified ProcessComposition.Isabelle.EndPortGraph;
import qualified ProcessComposition.Isabelle.Sequence;
import qualified ProcessComposition.Isabelle.Juxtapose;
import qualified ProcessComposition.Isabelle.PortGraph;
import qualified ProcessComposition.Isabelle.Process;
import qualified ProcessComposition.Isabelle.ProcessPaths;
import qualified ProcessComposition.Isabelle.ProcessPort;
import qualified ProcessComposition.Isabelle.Resource;

data Node_content a b = NodePrimitive a b deriving (Prelude.Read, Prelude.Show);

forgetPortGraph ::
  forall a b c d.
    ProcessComposition.Isabelle.Resource.Resource a b ->
      ProcessComposition.Isabelle.PortGraph.Port_graph
        ProcessComposition.Isabelle.ProcessPort.Process_side
        (ProcessComposition.Isabelle.Resource.Resource a b)
        ProcessComposition.Isabelle.ProcessPaths.Process_inner
        (Node_content c d);
forgetPortGraph x =
  ProcessComposition.Isabelle.PortGraph.PGraph []
    (map (\ p ->
           ProcessComposition.Isabelle.PortGraph.Edge
             (ProcessComposition.Isabelle.PortGraph.OpenPort p)
             (ProcessComposition.Isabelle.PortGraph.OpenPort
               (ProcessComposition.Isabelle.Port.Port
                 ProcessComposition.Isabelle.ProcessPort.out_process_side
                 ProcessComposition.Isabelle.Arith.zero_nat
                 ProcessComposition.Isabelle.Resource.anything)))
      (ProcessComposition.Isabelle.ProcessPort.parallelPorts
        ProcessComposition.Isabelle.Arith.zero_nat
        ProcessComposition.Isabelle.ProcessPort.in_process_side x))
    (ProcessComposition.Isabelle.ProcessPort.parallelPorts
       ProcessComposition.Isabelle.Arith.zero_nat
       ProcessComposition.Isabelle.ProcessPort.in_process_side x ++
      [ProcessComposition.Isabelle.Port.Port
         ProcessComposition.Isabelle.ProcessPort.out_process_side
         ProcessComposition.Isabelle.Arith.zero_nat
         ProcessComposition.Isabelle.Resource.anything]);

oncePortGraph ::
  forall a b c d.
    ProcessComposition.Isabelle.Resource.Resource a b ->
      ProcessComposition.Isabelle.Resource.Resource a b ->
        ProcessComposition.Isabelle.PortGraph.Port_graph
          ProcessComposition.Isabelle.ProcessPort.Process_side
          (ProcessComposition.Isabelle.Resource.Resource a b)
          ProcessComposition.Isabelle.ProcessPaths.Process_inner
          (Node_content c d);
oncePortGraph a b =
  ProcessComposition.Isabelle.PortGraph.PGraph []
    [ProcessComposition.Isabelle.PortGraph.Edge
       (ProcessComposition.Isabelle.PortGraph.OpenPort
         (ProcessComposition.Isabelle.Port.Port
           ProcessComposition.Isabelle.ProcessPort.in_process_side
           ProcessComposition.Isabelle.Arith.zero_nat
           (ProcessComposition.Isabelle.Resource.repeatable a b)))
       (ProcessComposition.Isabelle.PortGraph.OpenPort
         (ProcessComposition.Isabelle.Port.Port
           ProcessComposition.Isabelle.ProcessPort.out_process_side
           ProcessComposition.Isabelle.Arith.zero_nat
           (ProcessComposition.Isabelle.Resource.executable a b)))]
    [ProcessComposition.Isabelle.Port.Port
       ProcessComposition.Isabelle.ProcessPort.in_process_side
       ProcessComposition.Isabelle.Arith.zero_nat
       (ProcessComposition.Isabelle.Resource.repeatable a b),
      ProcessComposition.Isabelle.Port.Port
        ProcessComposition.Isabelle.ProcessPort.out_process_side
        ProcessComposition.Isabelle.Arith.zero_nat
        (ProcessComposition.Isabelle.Resource.executable a b)];

pgConstruct ::
  forall a b c d.
    (Eq a,
      Eq b) => ProcessComposition.Isabelle.Process.Process a b c d ->
                 ProcessComposition.Isabelle.PortGraph.Port_graph
                   ProcessComposition.Isabelle.ProcessPort.Process_side
                   (ProcessComposition.Isabelle.Resource.Resource a b)
                   ProcessComposition.Isabelle.ProcessPaths.Process_inner
                   (Node_content c d);
pgConstruct (ProcessComposition.Isabelle.Process.Primitive ins outs l m) =
  ProcessComposition.Isabelle.NodePortGraph.nodePortGraph [] (NodePrimitive l m)
    (ProcessComposition.Isabelle.Resource.parallel_parts ins)
    (ProcessComposition.Isabelle.Resource.parallel_parts outs);
pgConstruct (ProcessComposition.Isabelle.Process.Seq p q) =
  ProcessComposition.Isabelle.Sequence.seqPortGraphs
    (ProcessComposition.Isabelle.PortGraph.qualifyPortGraph
      ProcessComposition.Isabelle.ProcessPaths.SeqL (pgConstruct p))
    (ProcessComposition.Isabelle.PortGraph.qualifyPortGraph
      ProcessComposition.Isabelle.ProcessPaths.SeqR (pgConstruct q));
pgConstruct (ProcessComposition.Isabelle.Process.Par p q) =
  ProcessComposition.Isabelle.Juxtapose.juxtapose
    (ProcessComposition.Isabelle.PortGraph.qualifyPortGraph
      ProcessComposition.Isabelle.ProcessPaths.ParL (pgConstruct p))
    (ProcessComposition.Isabelle.PortGraph.qualifyPortGraph
      ProcessComposition.Isabelle.ProcessPaths.ParR (pgConstruct q));
pgConstruct (ProcessComposition.Isabelle.Process.Identity a) =
  ProcessComposition.Isabelle.IdentityPortGraph.idPortGraph
    (ProcessComposition.Isabelle.Resource.parallel_parts a);
pgConstruct (ProcessComposition.Isabelle.Process.Swap a b) =
  ProcessComposition.Isabelle.SwapPortGraph.swapPortGraph
    (ProcessComposition.Isabelle.Resource.parallel_parts a)
    (ProcessComposition.Isabelle.Resource.parallel_parts b);
pgConstruct (ProcessComposition.Isabelle.Process.Duplicate a) =
  ProcessComposition.Isabelle.ForkPortGraph.forkPortGraph
    (ProcessComposition.Isabelle.Resource.copyable a);
pgConstruct (ProcessComposition.Isabelle.Process.Erase a) =
  ProcessComposition.Isabelle.EndPortGraph.endPortGraph
    [ProcessComposition.Isabelle.Resource.copyable a];
pgConstruct (ProcessComposition.Isabelle.Process.Repeat a b) =
  ProcessComposition.Isabelle.ForkPortGraph.forkPortGraph
    (ProcessComposition.Isabelle.Resource.repeatable a b);
pgConstruct (ProcessComposition.Isabelle.Process.Close a b) =
  ProcessComposition.Isabelle.EndPortGraph.endPortGraph
    [ProcessComposition.Isabelle.Resource.repeatable a b];
pgConstruct (ProcessComposition.Isabelle.Process.Once a b) = oncePortGraph a b;
pgConstruct (ProcessComposition.Isabelle.Process.Forget a) = forgetPortGraph a;

}
