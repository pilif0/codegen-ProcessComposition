{-# LANGUAGE EmptyDataDecls, RankNTypes, ScopedTypeVariables #-}

module
  ProcessComposition.Isabelle.Process(Process(..), input, output, valid,
                                       primitives, process_subst,
                                       par_process_list, seq_process_list,
                                       process_refineRes, map_process,
                                       equal_process)
  where {

import Prelude ((==), (/=), (<), (<=), (>=), (>), (+), (-), (*), (/), (**),
  (>>=), (>>), (=<<), (&&), (||), (^), (^^), (.), ($), ($!), (++), (!!), Eq,
  error, id, return, not, fst, snd, map, filter, concat, concatMap, reverse,
  zip, null, takeWhile, dropWhile, all, any, Integer, negate, abs, divMod,
  String, Bool(True, False), Maybe(Nothing, Just));
import qualified Prelude;
import qualified ProcessComposition.Isabelle.Resource;

data Process a b c d =
  Primitive (ProcessComposition.Isabelle.Resource.Resource a b)
    (ProcessComposition.Isabelle.Resource.Resource a b) c d
  | Seq (Process a b c d) (Process a b c d)
  | Par (Process a b c d) (Process a b c d)
  | Opt (Process a b c d) (Process a b c d) | Represent (Process a b c d)
  | Identity (ProcessComposition.Isabelle.Resource.Resource a b)
  | Swap (ProcessComposition.Isabelle.Resource.Resource a b)
      (ProcessComposition.Isabelle.Resource.Resource a b)
  | InjectL (ProcessComposition.Isabelle.Resource.Resource a b)
      (ProcessComposition.Isabelle.Resource.Resource a b)
  | InjectR (ProcessComposition.Isabelle.Resource.Resource a b)
      (ProcessComposition.Isabelle.Resource.Resource a b)
  | OptDistrIn (ProcessComposition.Isabelle.Resource.Resource a b)
      (ProcessComposition.Isabelle.Resource.Resource a b)
      (ProcessComposition.Isabelle.Resource.Resource a b)
  | OptDistrOut (ProcessComposition.Isabelle.Resource.Resource a b)
      (ProcessComposition.Isabelle.Resource.Resource a b)
      (ProcessComposition.Isabelle.Resource.Resource a b)
  | Duplicate b | Erase b
  | Apply (ProcessComposition.Isabelle.Resource.Resource a b)
      (ProcessComposition.Isabelle.Resource.Resource a b)
  | Repeat (ProcessComposition.Isabelle.Resource.Resource a b)
      (ProcessComposition.Isabelle.Resource.Resource a b)
  | Close (ProcessComposition.Isabelle.Resource.Resource a b)
      (ProcessComposition.Isabelle.Resource.Resource a b)
  | Once (ProcessComposition.Isabelle.Resource.Resource a b)
      (ProcessComposition.Isabelle.Resource.Resource a b)
  | Forget (ProcessComposition.Isabelle.Resource.Resource a b)
  deriving (Prelude.Read, Prelude.Show);

input ::
  forall a b c d.
    Process a b c d -> ProcessComposition.Isabelle.Resource.Resource a b;
input (Primitive ins outs l m) = ins;
input (Seq p q) = input p;
input (Par p q) =
  ProcessComposition.Isabelle.Resource.resource_par (input p) (input q);
input (Opt p q) = ProcessComposition.Isabelle.Resource.nonD (input p) (input q);
input (Represent p) = ProcessComposition.Isabelle.Resource.empty;
input (Identity a) = a;
input (Swap a b) = ProcessComposition.Isabelle.Resource.resource_par a b;
input (InjectL a b) = a;
input (InjectR a b) = b;
input (OptDistrIn a b c) =
  ProcessComposition.Isabelle.Resource.resource_par a
    (ProcessComposition.Isabelle.Resource.nonD b c);
input (OptDistrOut a b c) =
  ProcessComposition.Isabelle.Resource.nonD
    (ProcessComposition.Isabelle.Resource.resource_par a b)
    (ProcessComposition.Isabelle.Resource.resource_par a c);
input (Duplicate a) = ProcessComposition.Isabelle.Resource.copyable a;
input (Erase a) = ProcessComposition.Isabelle.Resource.copyable a;
input (Apply a b) =
  ProcessComposition.Isabelle.Resource.resource_par a
    (ProcessComposition.Isabelle.Resource.executable a b);
input (Repeat a b) = ProcessComposition.Isabelle.Resource.repeatable a b;
input (Close a b) = ProcessComposition.Isabelle.Resource.repeatable a b;
input (Once a b) = ProcessComposition.Isabelle.Resource.repeatable a b;
input (Forget a) = a;

output ::
  forall a b c d.
    Process a b c d -> ProcessComposition.Isabelle.Resource.Resource a b;
output (Primitive ins outs l m) = outs;
output (Seq p q) = output q;
output (Par p q) =
  ProcessComposition.Isabelle.Resource.resource_par (output p) (output q);
output (Opt p q) = output p;
output (Represent p) =
  ProcessComposition.Isabelle.Resource.repeatable (input p) (output p);
output (Identity a) = a;
output (Swap a b) = ProcessComposition.Isabelle.Resource.resource_par b a;
output (InjectL a b) = ProcessComposition.Isabelle.Resource.nonD a b;
output (InjectR a b) = ProcessComposition.Isabelle.Resource.nonD a b;
output (OptDistrIn a b c) =
  ProcessComposition.Isabelle.Resource.nonD
    (ProcessComposition.Isabelle.Resource.resource_par a b)
    (ProcessComposition.Isabelle.Resource.resource_par a c);
output (OptDistrOut a b c) =
  ProcessComposition.Isabelle.Resource.resource_par a
    (ProcessComposition.Isabelle.Resource.nonD b c);
output (Duplicate a) =
  ProcessComposition.Isabelle.Resource.resource_par
    (ProcessComposition.Isabelle.Resource.copyable a)
    (ProcessComposition.Isabelle.Resource.copyable a);
output (Erase a) = ProcessComposition.Isabelle.Resource.empty;
output (Apply a b) = b;
output (Repeat a b) =
  ProcessComposition.Isabelle.Resource.resource_par
    (ProcessComposition.Isabelle.Resource.repeatable a b)
    (ProcessComposition.Isabelle.Resource.repeatable a b);
output (Close a b) = ProcessComposition.Isabelle.Resource.empty;
output (Once a b) = ProcessComposition.Isabelle.Resource.executable a b;
output (Forget a) = ProcessComposition.Isabelle.Resource.anything;

valid :: forall a b c d. (Eq a, Eq b) => Process a b c d -> Bool;
valid (Primitive ins outs l m) = True;
valid (Seq p q) = output p == input q && valid p && valid q;
valid (Par p q) = valid p && valid q;
valid (Opt p q) = valid p && valid q && output p == output q;
valid (Represent p) = valid p;
valid (Identity a) = True;
valid (Swap a b) = True;
valid (InjectL a b) = True;
valid (InjectR a b) = True;
valid (OptDistrIn a b c) = True;
valid (OptDistrOut a b c) = True;
valid (Duplicate a) = True;
valid (Erase a) = True;
valid (Apply a b) = True;
valid (Repeat a b) = True;
valid (Close a b) = True;
valid (Once a b) = True;
valid (Forget a) = True;

primitives ::
  forall a b c d.
    Process a b c d ->
      [(ProcessComposition.Isabelle.Resource.Resource a b,
         (ProcessComposition.Isabelle.Resource.Resource a b, (c, d)))];
primitives (Primitive ins outs l m) = [(ins, (outs, (l, m)))];
primitives (Seq p q) = primitives p ++ primitives q;
primitives (Par p q) = primitives p ++ primitives q;
primitives (Opt p q) = primitives p ++ primitives q;
primitives (Represent p) = primitives p;
primitives (Identity a) = [];
primitives (Swap a b) = [];
primitives (InjectL a b) = [];
primitives (InjectR a b) = [];
primitives (OptDistrIn a b c) = [];
primitives (OptDistrOut a b c) = [];
primitives (Duplicate a) = [];
primitives (Erase a) = [];
primitives (Apply a b) = [];
primitives (Repeat a b) = [];
primitives (Close a b) = [];
primitives (Once a b) = [];
primitives (Forget a) = [];

process_subst ::
  forall a b c d.
    (ProcessComposition.Isabelle.Resource.Resource a b ->
      ProcessComposition.Isabelle.Resource.Resource a b -> c -> d -> Bool) ->
      (ProcessComposition.Isabelle.Resource.Resource a b ->
        ProcessComposition.Isabelle.Resource.Resource a b ->
          c -> d -> Process a b c d) ->
        Process a b c d -> Process a b c d;
process_subst p f (Primitive a b l m) =
  (if p a b l m then f a b l m else Primitive a b l m);
process_subst p f (Identity a) = Identity a;
process_subst p f (Swap a b) = Swap a b;
process_subst pa f (Seq p q) =
  Seq (process_subst pa f p) (process_subst pa f q);
process_subst pa f (Par p q) =
  Par (process_subst pa f p) (process_subst pa f q);
process_subst pa f (Opt p q) =
  Opt (process_subst pa f p) (process_subst pa f q);
process_subst p f (InjectL a b) = InjectL a b;
process_subst p f (InjectR a b) = InjectR a b;
process_subst p f (OptDistrIn a b c) = OptDistrIn a b c;
process_subst p f (OptDistrOut a b c) = OptDistrOut a b c;
process_subst p f (Duplicate a) = Duplicate a;
process_subst p f (Erase a) = Erase a;
process_subst pa f (Represent p) = Represent (process_subst pa f p);
process_subst p f (Apply a b) = Apply a b;
process_subst p f (Repeat a b) = Repeat a b;
process_subst p f (Close a b) = Close a b;
process_subst p f (Once a b) = Once a b;
process_subst p f (Forget a) = Forget a;

par_process_list :: forall a b c d. [Process a b c d] -> Process a b c d;
par_process_list [] = Identity ProcessComposition.Isabelle.Resource.empty;
par_process_list (x : xs) =
  (if null xs then x else Par x (par_process_list xs));

seq_process_list :: forall a b c d. [Process a b c d] -> Process a b c d;
seq_process_list [] = Identity ProcessComposition.Isabelle.Resource.empty;
seq_process_list (x : xs) =
  (if null xs then x else Seq x (seq_process_list xs));

process_refineRes ::
  forall a b c d e f.
    (a -> ProcessComposition.Isabelle.Resource.Resource b c) ->
      (d -> c) -> Process a d e f -> Process b c e f;
process_refineRes f g (Primitive ins outs l m) =
  Primitive (ProcessComposition.Isabelle.Resource.refine_resource f g ins)
    (ProcessComposition.Isabelle.Resource.refine_resource f g outs) l m;
process_refineRes f g (Identity a) =
  Identity (ProcessComposition.Isabelle.Resource.refine_resource f g a);
process_refineRes f g (Swap a b) =
  Swap (ProcessComposition.Isabelle.Resource.refine_resource f g a)
    (ProcessComposition.Isabelle.Resource.refine_resource f g b);
process_refineRes f g (Seq p q) =
  Seq (process_refineRes f g p) (process_refineRes f g q);
process_refineRes f g (Par p q) =
  Par (process_refineRes f g p) (process_refineRes f g q);
process_refineRes f g (Opt p q) =
  Opt (process_refineRes f g p) (process_refineRes f g q);
process_refineRes f g (InjectL a b) =
  InjectL (ProcessComposition.Isabelle.Resource.refine_resource f g a)
    (ProcessComposition.Isabelle.Resource.refine_resource f g b);
process_refineRes f g (InjectR a b) =
  InjectR (ProcessComposition.Isabelle.Resource.refine_resource f g a)
    (ProcessComposition.Isabelle.Resource.refine_resource f g b);
process_refineRes f g (OptDistrIn a b c) =
  OptDistrIn (ProcessComposition.Isabelle.Resource.refine_resource f g a)
    (ProcessComposition.Isabelle.Resource.refine_resource f g b)
    (ProcessComposition.Isabelle.Resource.refine_resource f g c);
process_refineRes f g (OptDistrOut a b c) =
  OptDistrOut (ProcessComposition.Isabelle.Resource.refine_resource f g a)
    (ProcessComposition.Isabelle.Resource.refine_resource f g b)
    (ProcessComposition.Isabelle.Resource.refine_resource f g c);
process_refineRes f g (Duplicate a) = Duplicate (g a);
process_refineRes f g (Erase a) = Erase (g a);
process_refineRes f g (Represent p) = Represent (process_refineRes f g p);
process_refineRes f g (Apply a b) =
  Apply (ProcessComposition.Isabelle.Resource.refine_resource f g a)
    (ProcessComposition.Isabelle.Resource.refine_resource f g b);
process_refineRes f g (Repeat a b) =
  Repeat (ProcessComposition.Isabelle.Resource.refine_resource f g a)
    (ProcessComposition.Isabelle.Resource.refine_resource f g b);
process_refineRes f g (Close a b) =
  Close (ProcessComposition.Isabelle.Resource.refine_resource f g a)
    (ProcessComposition.Isabelle.Resource.refine_resource f g b);
process_refineRes f g (Once a b) =
  Once (ProcessComposition.Isabelle.Resource.refine_resource f g a)
    (ProcessComposition.Isabelle.Resource.refine_resource f g b);
process_refineRes f g (Forget a) =
  Forget (ProcessComposition.Isabelle.Resource.refine_resource f g a);

map_process ::
  forall a b c d e f g h.
    (a -> b) ->
      (c -> d) -> (e -> f) -> (g -> h) -> Process a c e g -> Process b d f h;
map_process f1 f2 f3 f4 (Primitive x11 x12 x13 x14) =
  Primitive (ProcessComposition.Isabelle.Resource.map_resource f1 f2 x11)
    (ProcessComposition.Isabelle.Resource.map_resource f1 f2 x12) (f3 x13)
    (f4 x14);
map_process f1 f2 f3 f4 (Seq x21 x22) =
  Seq (map_process f1 f2 f3 f4 x21) (map_process f1 f2 f3 f4 x22);
map_process f1 f2 f3 f4 (Par x31 x32) =
  Par (map_process f1 f2 f3 f4 x31) (map_process f1 f2 f3 f4 x32);
map_process f1 f2 f3 f4 (Opt x41 x42) =
  Opt (map_process f1 f2 f3 f4 x41) (map_process f1 f2 f3 f4 x42);
map_process f1 f2 f3 f4 (Represent x5) = Represent (map_process f1 f2 f3 f4 x5);
map_process f1 f2 f3 f4 (Identity x6) =
  Identity (ProcessComposition.Isabelle.Resource.map_resource f1 f2 x6);
map_process f1 f2 f3 f4 (Swap x71 x72) =
  Swap (ProcessComposition.Isabelle.Resource.map_resource f1 f2 x71)
    (ProcessComposition.Isabelle.Resource.map_resource f1 f2 x72);
map_process f1 f2 f3 f4 (InjectL x81 x82) =
  InjectL (ProcessComposition.Isabelle.Resource.map_resource f1 f2 x81)
    (ProcessComposition.Isabelle.Resource.map_resource f1 f2 x82);
map_process f1 f2 f3 f4 (InjectR x91 x92) =
  InjectR (ProcessComposition.Isabelle.Resource.map_resource f1 f2 x91)
    (ProcessComposition.Isabelle.Resource.map_resource f1 f2 x92);
map_process f1 f2 f3 f4 (OptDistrIn x101 x102 x103) =
  OptDistrIn (ProcessComposition.Isabelle.Resource.map_resource f1 f2 x101)
    (ProcessComposition.Isabelle.Resource.map_resource f1 f2 x102)
    (ProcessComposition.Isabelle.Resource.map_resource f1 f2 x103);
map_process f1 f2 f3 f4 (OptDistrOut x111 x112 x113) =
  OptDistrOut (ProcessComposition.Isabelle.Resource.map_resource f1 f2 x111)
    (ProcessComposition.Isabelle.Resource.map_resource f1 f2 x112)
    (ProcessComposition.Isabelle.Resource.map_resource f1 f2 x113);
map_process f1 f2 f3 f4 (Duplicate x12a) = Duplicate (f2 x12a);
map_process f1 f2 f3 f4 (Erase x13a) = Erase (f2 x13a);
map_process f1 f2 f3 f4 (Apply x141 x142) =
  Apply (ProcessComposition.Isabelle.Resource.map_resource f1 f2 x141)
    (ProcessComposition.Isabelle.Resource.map_resource f1 f2 x142);
map_process f1 f2 f3 f4 (Repeat x151 x152) =
  Repeat (ProcessComposition.Isabelle.Resource.map_resource f1 f2 x151)
    (ProcessComposition.Isabelle.Resource.map_resource f1 f2 x152);
map_process f1 f2 f3 f4 (Close x161 x162) =
  Close (ProcessComposition.Isabelle.Resource.map_resource f1 f2 x161)
    (ProcessComposition.Isabelle.Resource.map_resource f1 f2 x162);
map_process f1 f2 f3 f4 (Once x171 x172) =
  Once (ProcessComposition.Isabelle.Resource.map_resource f1 f2 x171)
    (ProcessComposition.Isabelle.Resource.map_resource f1 f2 x172);
map_process f1 f2 f3 f4 (Forget x18) =
  Forget (ProcessComposition.Isabelle.Resource.map_resource f1 f2 x18);

equal_process ::
  forall a b c d.
    (Eq a, Eq b, Eq c, Eq d) => Process a b c d -> Process a b c d -> Bool;
equal_process (Once x171 x172) (Forget x18) = False;
equal_process (Forget x18) (Once x171 x172) = False;
equal_process (Close x161 x162) (Forget x18) = False;
equal_process (Forget x18) (Close x161 x162) = False;
equal_process (Close x161 x162) (Once x171 x172) = False;
equal_process (Once x171 x172) (Close x161 x162) = False;
equal_process (Repeat x151 x152) (Forget x18) = False;
equal_process (Forget x18) (Repeat x151 x152) = False;
equal_process (Repeat x151 x152) (Once x171 x172) = False;
equal_process (Once x171 x172) (Repeat x151 x152) = False;
equal_process (Repeat x151 x152) (Close x161 x162) = False;
equal_process (Close x161 x162) (Repeat x151 x152) = False;
equal_process (Apply x141 x142) (Forget x18) = False;
equal_process (Forget x18) (Apply x141 x142) = False;
equal_process (Apply x141 x142) (Once x171 x172) = False;
equal_process (Once x171 x172) (Apply x141 x142) = False;
equal_process (Apply x141 x142) (Close x161 x162) = False;
equal_process (Close x161 x162) (Apply x141 x142) = False;
equal_process (Apply x141 x142) (Repeat x151 x152) = False;
equal_process (Repeat x151 x152) (Apply x141 x142) = False;
equal_process (Erase x13a) (Forget x18) = False;
equal_process (Forget x18) (Erase x13a) = False;
equal_process (Erase x13a) (Once x171 x172) = False;
equal_process (Once x171 x172) (Erase x13a) = False;
equal_process (Erase x13a) (Close x161 x162) = False;
equal_process (Close x161 x162) (Erase x13a) = False;
equal_process (Erase x13a) (Repeat x151 x152) = False;
equal_process (Repeat x151 x152) (Erase x13a) = False;
equal_process (Erase x13a) (Apply x141 x142) = False;
equal_process (Apply x141 x142) (Erase x13a) = False;
equal_process (Duplicate x12a) (Forget x18) = False;
equal_process (Forget x18) (Duplicate x12a) = False;
equal_process (Duplicate x12a) (Once x171 x172) = False;
equal_process (Once x171 x172) (Duplicate x12a) = False;
equal_process (Duplicate x12a) (Close x161 x162) = False;
equal_process (Close x161 x162) (Duplicate x12a) = False;
equal_process (Duplicate x12a) (Repeat x151 x152) = False;
equal_process (Repeat x151 x152) (Duplicate x12a) = False;
equal_process (Duplicate x12a) (Apply x141 x142) = False;
equal_process (Apply x141 x142) (Duplicate x12a) = False;
equal_process (Duplicate x12a) (Erase x13a) = False;
equal_process (Erase x13a) (Duplicate x12a) = False;
equal_process (OptDistrOut x111 x112 x113) (Forget x18) = False;
equal_process (Forget x18) (OptDistrOut x111 x112 x113) = False;
equal_process (OptDistrOut x111 x112 x113) (Once x171 x172) = False;
equal_process (Once x171 x172) (OptDistrOut x111 x112 x113) = False;
equal_process (OptDistrOut x111 x112 x113) (Close x161 x162) = False;
equal_process (Close x161 x162) (OptDistrOut x111 x112 x113) = False;
equal_process (OptDistrOut x111 x112 x113) (Repeat x151 x152) = False;
equal_process (Repeat x151 x152) (OptDistrOut x111 x112 x113) = False;
equal_process (OptDistrOut x111 x112 x113) (Apply x141 x142) = False;
equal_process (Apply x141 x142) (OptDistrOut x111 x112 x113) = False;
equal_process (OptDistrOut x111 x112 x113) (Erase x13a) = False;
equal_process (Erase x13a) (OptDistrOut x111 x112 x113) = False;
equal_process (OptDistrOut x111 x112 x113) (Duplicate x12a) = False;
equal_process (Duplicate x12a) (OptDistrOut x111 x112 x113) = False;
equal_process (OptDistrIn x101 x102 x103) (Forget x18) = False;
equal_process (Forget x18) (OptDistrIn x101 x102 x103) = False;
equal_process (OptDistrIn x101 x102 x103) (Once x171 x172) = False;
equal_process (Once x171 x172) (OptDistrIn x101 x102 x103) = False;
equal_process (OptDistrIn x101 x102 x103) (Close x161 x162) = False;
equal_process (Close x161 x162) (OptDistrIn x101 x102 x103) = False;
equal_process (OptDistrIn x101 x102 x103) (Repeat x151 x152) = False;
equal_process (Repeat x151 x152) (OptDistrIn x101 x102 x103) = False;
equal_process (OptDistrIn x101 x102 x103) (Apply x141 x142) = False;
equal_process (Apply x141 x142) (OptDistrIn x101 x102 x103) = False;
equal_process (OptDistrIn x101 x102 x103) (Erase x13a) = False;
equal_process (Erase x13a) (OptDistrIn x101 x102 x103) = False;
equal_process (OptDistrIn x101 x102 x103) (Duplicate x12a) = False;
equal_process (Duplicate x12a) (OptDistrIn x101 x102 x103) = False;
equal_process (OptDistrIn x101 x102 x103) (OptDistrOut x111 x112 x113) = False;
equal_process (OptDistrOut x111 x112 x113) (OptDistrIn x101 x102 x103) = False;
equal_process (InjectR x91 x92) (Forget x18) = False;
equal_process (Forget x18) (InjectR x91 x92) = False;
equal_process (InjectR x91 x92) (Once x171 x172) = False;
equal_process (Once x171 x172) (InjectR x91 x92) = False;
equal_process (InjectR x91 x92) (Close x161 x162) = False;
equal_process (Close x161 x162) (InjectR x91 x92) = False;
equal_process (InjectR x91 x92) (Repeat x151 x152) = False;
equal_process (Repeat x151 x152) (InjectR x91 x92) = False;
equal_process (InjectR x91 x92) (Apply x141 x142) = False;
equal_process (Apply x141 x142) (InjectR x91 x92) = False;
equal_process (InjectR x91 x92) (Erase x13a) = False;
equal_process (Erase x13a) (InjectR x91 x92) = False;
equal_process (InjectR x91 x92) (Duplicate x12a) = False;
equal_process (Duplicate x12a) (InjectR x91 x92) = False;
equal_process (InjectR x91 x92) (OptDistrOut x111 x112 x113) = False;
equal_process (OptDistrOut x111 x112 x113) (InjectR x91 x92) = False;
equal_process (InjectR x91 x92) (OptDistrIn x101 x102 x103) = False;
equal_process (OptDistrIn x101 x102 x103) (InjectR x91 x92) = False;
equal_process (InjectL x81 x82) (Forget x18) = False;
equal_process (Forget x18) (InjectL x81 x82) = False;
equal_process (InjectL x81 x82) (Once x171 x172) = False;
equal_process (Once x171 x172) (InjectL x81 x82) = False;
equal_process (InjectL x81 x82) (Close x161 x162) = False;
equal_process (Close x161 x162) (InjectL x81 x82) = False;
equal_process (InjectL x81 x82) (Repeat x151 x152) = False;
equal_process (Repeat x151 x152) (InjectL x81 x82) = False;
equal_process (InjectL x81 x82) (Apply x141 x142) = False;
equal_process (Apply x141 x142) (InjectL x81 x82) = False;
equal_process (InjectL x81 x82) (Erase x13a) = False;
equal_process (Erase x13a) (InjectL x81 x82) = False;
equal_process (InjectL x81 x82) (Duplicate x12a) = False;
equal_process (Duplicate x12a) (InjectL x81 x82) = False;
equal_process (InjectL x81 x82) (OptDistrOut x111 x112 x113) = False;
equal_process (OptDistrOut x111 x112 x113) (InjectL x81 x82) = False;
equal_process (InjectL x81 x82) (OptDistrIn x101 x102 x103) = False;
equal_process (OptDistrIn x101 x102 x103) (InjectL x81 x82) = False;
equal_process (InjectL x81 x82) (InjectR x91 x92) = False;
equal_process (InjectR x91 x92) (InjectL x81 x82) = False;
equal_process (Swap x71 x72) (Forget x18) = False;
equal_process (Forget x18) (Swap x71 x72) = False;
equal_process (Swap x71 x72) (Once x171 x172) = False;
equal_process (Once x171 x172) (Swap x71 x72) = False;
equal_process (Swap x71 x72) (Close x161 x162) = False;
equal_process (Close x161 x162) (Swap x71 x72) = False;
equal_process (Swap x71 x72) (Repeat x151 x152) = False;
equal_process (Repeat x151 x152) (Swap x71 x72) = False;
equal_process (Swap x71 x72) (Apply x141 x142) = False;
equal_process (Apply x141 x142) (Swap x71 x72) = False;
equal_process (Swap x71 x72) (Erase x13a) = False;
equal_process (Erase x13a) (Swap x71 x72) = False;
equal_process (Swap x71 x72) (Duplicate x12a) = False;
equal_process (Duplicate x12a) (Swap x71 x72) = False;
equal_process (Swap x71 x72) (OptDistrOut x111 x112 x113) = False;
equal_process (OptDistrOut x111 x112 x113) (Swap x71 x72) = False;
equal_process (Swap x71 x72) (OptDistrIn x101 x102 x103) = False;
equal_process (OptDistrIn x101 x102 x103) (Swap x71 x72) = False;
equal_process (Swap x71 x72) (InjectR x91 x92) = False;
equal_process (InjectR x91 x92) (Swap x71 x72) = False;
equal_process (Swap x71 x72) (InjectL x81 x82) = False;
equal_process (InjectL x81 x82) (Swap x71 x72) = False;
equal_process (Identity x6) (Forget x18) = False;
equal_process (Forget x18) (Identity x6) = False;
equal_process (Identity x6) (Once x171 x172) = False;
equal_process (Once x171 x172) (Identity x6) = False;
equal_process (Identity x6) (Close x161 x162) = False;
equal_process (Close x161 x162) (Identity x6) = False;
equal_process (Identity x6) (Repeat x151 x152) = False;
equal_process (Repeat x151 x152) (Identity x6) = False;
equal_process (Identity x6) (Apply x141 x142) = False;
equal_process (Apply x141 x142) (Identity x6) = False;
equal_process (Identity x6) (Erase x13a) = False;
equal_process (Erase x13a) (Identity x6) = False;
equal_process (Identity x6) (Duplicate x12a) = False;
equal_process (Duplicate x12a) (Identity x6) = False;
equal_process (Identity x6) (OptDistrOut x111 x112 x113) = False;
equal_process (OptDistrOut x111 x112 x113) (Identity x6) = False;
equal_process (Identity x6) (OptDistrIn x101 x102 x103) = False;
equal_process (OptDistrIn x101 x102 x103) (Identity x6) = False;
equal_process (Identity x6) (InjectR x91 x92) = False;
equal_process (InjectR x91 x92) (Identity x6) = False;
equal_process (Identity x6) (InjectL x81 x82) = False;
equal_process (InjectL x81 x82) (Identity x6) = False;
equal_process (Identity x6) (Swap x71 x72) = False;
equal_process (Swap x71 x72) (Identity x6) = False;
equal_process (Represent x5) (Forget x18) = False;
equal_process (Forget x18) (Represent x5) = False;
equal_process (Represent x5) (Once x171 x172) = False;
equal_process (Once x171 x172) (Represent x5) = False;
equal_process (Represent x5) (Close x161 x162) = False;
equal_process (Close x161 x162) (Represent x5) = False;
equal_process (Represent x5) (Repeat x151 x152) = False;
equal_process (Repeat x151 x152) (Represent x5) = False;
equal_process (Represent x5) (Apply x141 x142) = False;
equal_process (Apply x141 x142) (Represent x5) = False;
equal_process (Represent x5) (Erase x13a) = False;
equal_process (Erase x13a) (Represent x5) = False;
equal_process (Represent x5) (Duplicate x12a) = False;
equal_process (Duplicate x12a) (Represent x5) = False;
equal_process (Represent x5) (OptDistrOut x111 x112 x113) = False;
equal_process (OptDistrOut x111 x112 x113) (Represent x5) = False;
equal_process (Represent x5) (OptDistrIn x101 x102 x103) = False;
equal_process (OptDistrIn x101 x102 x103) (Represent x5) = False;
equal_process (Represent x5) (InjectR x91 x92) = False;
equal_process (InjectR x91 x92) (Represent x5) = False;
equal_process (Represent x5) (InjectL x81 x82) = False;
equal_process (InjectL x81 x82) (Represent x5) = False;
equal_process (Represent x5) (Swap x71 x72) = False;
equal_process (Swap x71 x72) (Represent x5) = False;
equal_process (Represent x5) (Identity x6) = False;
equal_process (Identity x6) (Represent x5) = False;
equal_process (Opt x41 x42) (Forget x18) = False;
equal_process (Forget x18) (Opt x41 x42) = False;
equal_process (Opt x41 x42) (Once x171 x172) = False;
equal_process (Once x171 x172) (Opt x41 x42) = False;
equal_process (Opt x41 x42) (Close x161 x162) = False;
equal_process (Close x161 x162) (Opt x41 x42) = False;
equal_process (Opt x41 x42) (Repeat x151 x152) = False;
equal_process (Repeat x151 x152) (Opt x41 x42) = False;
equal_process (Opt x41 x42) (Apply x141 x142) = False;
equal_process (Apply x141 x142) (Opt x41 x42) = False;
equal_process (Opt x41 x42) (Erase x13a) = False;
equal_process (Erase x13a) (Opt x41 x42) = False;
equal_process (Opt x41 x42) (Duplicate x12a) = False;
equal_process (Duplicate x12a) (Opt x41 x42) = False;
equal_process (Opt x41 x42) (OptDistrOut x111 x112 x113) = False;
equal_process (OptDistrOut x111 x112 x113) (Opt x41 x42) = False;
equal_process (Opt x41 x42) (OptDistrIn x101 x102 x103) = False;
equal_process (OptDistrIn x101 x102 x103) (Opt x41 x42) = False;
equal_process (Opt x41 x42) (InjectR x91 x92) = False;
equal_process (InjectR x91 x92) (Opt x41 x42) = False;
equal_process (Opt x41 x42) (InjectL x81 x82) = False;
equal_process (InjectL x81 x82) (Opt x41 x42) = False;
equal_process (Opt x41 x42) (Swap x71 x72) = False;
equal_process (Swap x71 x72) (Opt x41 x42) = False;
equal_process (Opt x41 x42) (Identity x6) = False;
equal_process (Identity x6) (Opt x41 x42) = False;
equal_process (Opt x41 x42) (Represent x5) = False;
equal_process (Represent x5) (Opt x41 x42) = False;
equal_process (Par x31 x32) (Forget x18) = False;
equal_process (Forget x18) (Par x31 x32) = False;
equal_process (Par x31 x32) (Once x171 x172) = False;
equal_process (Once x171 x172) (Par x31 x32) = False;
equal_process (Par x31 x32) (Close x161 x162) = False;
equal_process (Close x161 x162) (Par x31 x32) = False;
equal_process (Par x31 x32) (Repeat x151 x152) = False;
equal_process (Repeat x151 x152) (Par x31 x32) = False;
equal_process (Par x31 x32) (Apply x141 x142) = False;
equal_process (Apply x141 x142) (Par x31 x32) = False;
equal_process (Par x31 x32) (Erase x13a) = False;
equal_process (Erase x13a) (Par x31 x32) = False;
equal_process (Par x31 x32) (Duplicate x12a) = False;
equal_process (Duplicate x12a) (Par x31 x32) = False;
equal_process (Par x31 x32) (OptDistrOut x111 x112 x113) = False;
equal_process (OptDistrOut x111 x112 x113) (Par x31 x32) = False;
equal_process (Par x31 x32) (OptDistrIn x101 x102 x103) = False;
equal_process (OptDistrIn x101 x102 x103) (Par x31 x32) = False;
equal_process (Par x31 x32) (InjectR x91 x92) = False;
equal_process (InjectR x91 x92) (Par x31 x32) = False;
equal_process (Par x31 x32) (InjectL x81 x82) = False;
equal_process (InjectL x81 x82) (Par x31 x32) = False;
equal_process (Par x31 x32) (Swap x71 x72) = False;
equal_process (Swap x71 x72) (Par x31 x32) = False;
equal_process (Par x31 x32) (Identity x6) = False;
equal_process (Identity x6) (Par x31 x32) = False;
equal_process (Par x31 x32) (Represent x5) = False;
equal_process (Represent x5) (Par x31 x32) = False;
equal_process (Par x31 x32) (Opt x41 x42) = False;
equal_process (Opt x41 x42) (Par x31 x32) = False;
equal_process (Seq x21 x22) (Forget x18) = False;
equal_process (Forget x18) (Seq x21 x22) = False;
equal_process (Seq x21 x22) (Once x171 x172) = False;
equal_process (Once x171 x172) (Seq x21 x22) = False;
equal_process (Seq x21 x22) (Close x161 x162) = False;
equal_process (Close x161 x162) (Seq x21 x22) = False;
equal_process (Seq x21 x22) (Repeat x151 x152) = False;
equal_process (Repeat x151 x152) (Seq x21 x22) = False;
equal_process (Seq x21 x22) (Apply x141 x142) = False;
equal_process (Apply x141 x142) (Seq x21 x22) = False;
equal_process (Seq x21 x22) (Erase x13a) = False;
equal_process (Erase x13a) (Seq x21 x22) = False;
equal_process (Seq x21 x22) (Duplicate x12a) = False;
equal_process (Duplicate x12a) (Seq x21 x22) = False;
equal_process (Seq x21 x22) (OptDistrOut x111 x112 x113) = False;
equal_process (OptDistrOut x111 x112 x113) (Seq x21 x22) = False;
equal_process (Seq x21 x22) (OptDistrIn x101 x102 x103) = False;
equal_process (OptDistrIn x101 x102 x103) (Seq x21 x22) = False;
equal_process (Seq x21 x22) (InjectR x91 x92) = False;
equal_process (InjectR x91 x92) (Seq x21 x22) = False;
equal_process (Seq x21 x22) (InjectL x81 x82) = False;
equal_process (InjectL x81 x82) (Seq x21 x22) = False;
equal_process (Seq x21 x22) (Swap x71 x72) = False;
equal_process (Swap x71 x72) (Seq x21 x22) = False;
equal_process (Seq x21 x22) (Identity x6) = False;
equal_process (Identity x6) (Seq x21 x22) = False;
equal_process (Seq x21 x22) (Represent x5) = False;
equal_process (Represent x5) (Seq x21 x22) = False;
equal_process (Seq x21 x22) (Opt x41 x42) = False;
equal_process (Opt x41 x42) (Seq x21 x22) = False;
equal_process (Seq x21 x22) (Par x31 x32) = False;
equal_process (Par x31 x32) (Seq x21 x22) = False;
equal_process (Primitive x11 x12 x13 x14) (Forget x18) = False;
equal_process (Forget x18) (Primitive x11 x12 x13 x14) = False;
equal_process (Primitive x11 x12 x13 x14) (Once x171 x172) = False;
equal_process (Once x171 x172) (Primitive x11 x12 x13 x14) = False;
equal_process (Primitive x11 x12 x13 x14) (Close x161 x162) = False;
equal_process (Close x161 x162) (Primitive x11 x12 x13 x14) = False;
equal_process (Primitive x11 x12 x13 x14) (Repeat x151 x152) = False;
equal_process (Repeat x151 x152) (Primitive x11 x12 x13 x14) = False;
equal_process (Primitive x11 x12 x13 x14) (Apply x141 x142) = False;
equal_process (Apply x141 x142) (Primitive x11 x12 x13 x14) = False;
equal_process (Primitive x11 x12 x13 x14) (Erase x13a) = False;
equal_process (Erase x13a) (Primitive x11 x12 x13 x14) = False;
equal_process (Primitive x11 x12 x13 x14) (Duplicate x12a) = False;
equal_process (Duplicate x12a) (Primitive x11 x12 x13 x14) = False;
equal_process (Primitive x11 x12 x13 x14) (OptDistrOut x111 x112 x113) = False;
equal_process (OptDistrOut x111 x112 x113) (Primitive x11 x12 x13 x14) = False;
equal_process (Primitive x11 x12 x13 x14) (OptDistrIn x101 x102 x103) = False;
equal_process (OptDistrIn x101 x102 x103) (Primitive x11 x12 x13 x14) = False;
equal_process (Primitive x11 x12 x13 x14) (InjectR x91 x92) = False;
equal_process (InjectR x91 x92) (Primitive x11 x12 x13 x14) = False;
equal_process (Primitive x11 x12 x13 x14) (InjectL x81 x82) = False;
equal_process (InjectL x81 x82) (Primitive x11 x12 x13 x14) = False;
equal_process (Primitive x11 x12 x13 x14) (Swap x71 x72) = False;
equal_process (Swap x71 x72) (Primitive x11 x12 x13 x14) = False;
equal_process (Primitive x11 x12 x13 x14) (Identity x6) = False;
equal_process (Identity x6) (Primitive x11 x12 x13 x14) = False;
equal_process (Primitive x11 x12 x13 x14) (Represent x5) = False;
equal_process (Represent x5) (Primitive x11 x12 x13 x14) = False;
equal_process (Primitive x11 x12 x13 x14) (Opt x41 x42) = False;
equal_process (Opt x41 x42) (Primitive x11 x12 x13 x14) = False;
equal_process (Primitive x11 x12 x13 x14) (Par x31 x32) = False;
equal_process (Par x31 x32) (Primitive x11 x12 x13 x14) = False;
equal_process (Primitive x11 x12 x13 x14) (Seq x21 x22) = False;
equal_process (Seq x21 x22) (Primitive x11 x12 x13 x14) = False;
equal_process (Forget x18) (Forget y18) = x18 == y18;
equal_process (Once x171 x172) (Once y171 y172) = x171 == y171 && x172 == y172;
equal_process (Close x161 x162) (Close y161 y162) =
  x161 == y161 && x162 == y162;
equal_process (Repeat x151 x152) (Repeat y151 y152) =
  x151 == y151 && x152 == y152;
equal_process (Apply x141 x142) (Apply y141 y142) =
  x141 == y141 && x142 == y142;
equal_process (Erase x13a) (Erase y13a) = x13a == y13a;
equal_process (Duplicate x12a) (Duplicate y12a) = x12a == y12a;
equal_process (OptDistrOut x111 x112 x113) (OptDistrOut y111 y112 y113) =
  x111 == y111 && x112 == y112 && x113 == y113;
equal_process (OptDistrIn x101 x102 x103) (OptDistrIn y101 y102 y103) =
  x101 == y101 && x102 == y102 && x103 == y103;
equal_process (InjectR x91 x92) (InjectR y91 y92) = x91 == y91 && x92 == y92;
equal_process (InjectL x81 x82) (InjectL y81 y82) = x81 == y81 && x82 == y82;
equal_process (Swap x71 x72) (Swap y71 y72) = x71 == y71 && x72 == y72;
equal_process (Identity x6) (Identity y6) = x6 == y6;
equal_process (Represent x5) (Represent y5) = equal_process x5 y5;
equal_process (Opt x41 x42) (Opt y41 y42) =
  equal_process x41 y41 && equal_process x42 y42;
equal_process (Par x31 x32) (Par y31 y32) =
  equal_process x31 y31 && equal_process x32 y32;
equal_process (Seq x21 x22) (Seq y21 y22) =
  equal_process x21 y21 && equal_process x22 y22;
equal_process (Primitive x11 x12 x13 x14) (Primitive y11 y12 y13 y14) =
  x11 == y11 && x12 == y12 && x13 == y13 && x14 == y14;

}
