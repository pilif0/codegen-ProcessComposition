{-# LANGUAGE EmptyDataDecls, RankNTypes, ScopedTypeVariables #-}

module
  ProcessComposition.Isabelle.ProcessPGPrint(intercalate, processLabelToString,
      processInnerToString, processPathToString, res_termToString,
      resourceToString, processToELK)
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
import qualified ProcessComposition.Isabelle.Groups_List;
import qualified ProcessComposition.Isabelle.ResTerm;
import qualified ProcessComposition.Isabelle.Str;
import qualified ProcessComposition.Isabelle.ProcessPortGraph;
import qualified ProcessComposition.Isabelle.ProcessPaths;
import qualified ProcessComposition.Isabelle.PortGraph;
import qualified ProcessComposition.Isabelle.Resource;
import qualified ProcessComposition.Isabelle.ELKJson;
import qualified ProcessComposition.Isabelle.Process;
import qualified ProcessComposition.Isabelle.Nano_JSON;
import qualified ProcessComposition.Isabelle.Arith;
import qualified ProcessComposition.Isabelle.ProcessPort;

intercalate :: forall a. a -> [a] -> [a];
intercalate a [] = [];
intercalate a [x] = [x];
intercalate a (x : v : va) = x : a : intercalate a (v : va);

processLabelToString ::
  forall a.
    ProcessComposition.Isabelle.ProcessPortGraph.Node_content String a ->
      String;
processLabelToString
  (ProcessComposition.Isabelle.ProcessPortGraph.NodePrimitive l m) = l;

processInnerToString ::
  ProcessComposition.Isabelle.ProcessPaths.Process_inner -> String;
processInnerToString ProcessComposition.Isabelle.ProcessPaths.SeqL = "SeqL";
processInnerToString ProcessComposition.Isabelle.ProcessPaths.SeqR = "SeqR";
processInnerToString ProcessComposition.Isabelle.ProcessPaths.ParL = "ParL";
processInnerToString ProcessComposition.Isabelle.ProcessPaths.ParR = "ParR";
processInnerToString ProcessComposition.Isabelle.ProcessPaths.OptL = "OptL";
processInnerToString ProcessComposition.Isabelle.ProcessPaths.OptR = "OptR";
processInnerToString ProcessComposition.Isabelle.ProcessPaths.Rep = "Rep";

processPathToString ::
  [ProcessComposition.Isabelle.ProcessPaths.Process_inner] -> String;
processPathToString [] = "";
processPathToString (x : xs) = processInnerToString x ++ processPathToString xs;

res_termToString ::
  ProcessComposition.Isabelle.ResTerm.Res_term String String -> String;
res_termToString ProcessComposition.Isabelle.ResTerm.Empty = "Empty";
res_termToString ProcessComposition.Isabelle.ResTerm.Anything = "Anything";
res_termToString (ProcessComposition.Isabelle.ResTerm.Res a) = "Res " ++ a;
res_termToString (ProcessComposition.Isabelle.ResTerm.Copyable x) =
  "Copyable " ++ x;
res_termToString (ProcessComposition.Isabelle.ResTerm.Parallel xs) =
  ("Parallel [" ++
    ProcessComposition.Isabelle.Groups_List.sum_list
      (intercalate ", " (map res_termToString xs))) ++
    "]";
res_termToString (ProcessComposition.Isabelle.ResTerm.NonD x y) =
  ((("NonD (" ++ res_termToString x) ++ ", ") ++ res_termToString y) ++ ")";
res_termToString (ProcessComposition.Isabelle.ResTerm.Executable x y) =
  ((("Executable (" ++ res_termToString x) ++ ", ") ++ res_termToString y) ++
    ")";
res_termToString (ProcessComposition.Isabelle.ResTerm.Repeatable x y) =
  ((("Repeatable (" ++ res_termToString x) ++ ", ") ++ res_termToString y) ++
    ")";

resourceToString ::
  ProcessComposition.Isabelle.Resource.Resource String String -> String;
resourceToString x =
  res_termToString (ProcessComposition.Isabelle.Resource.of_resource x);

processToELK ::
  ProcessComposition.Isabelle.Process.Process String String String () ->
    ProcessComposition.Isabelle.Nano_JSON.Json String
      ProcessComposition.Isabelle.Arith.Int;
processToELK x =
  ProcessComposition.Isabelle.ELKJson.portGraphToJSON processPathToString
    processLabelToString ProcessComposition.Isabelle.ELKJson.sideInOutToELK
    resourceToString []
    (ProcessComposition.Isabelle.ProcessPortGraph.pgConstruct x);

}
