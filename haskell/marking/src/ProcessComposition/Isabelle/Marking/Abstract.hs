{-# LANGUAGE EmptyDataDecls, RankNTypes, ScopedTypeVariables #-}

module
  ProcessComposition.Isabelle.Marking.Abstract(Cres(..), equal_cres,
        equal_cresa, Lres(..), markAll, case_lres, equal_lres, collectSubs,
        submitMarks, releaseMarks, markingProcess, equal_lresa)
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
import qualified ProcessComposition.Isabelle.Resource;
import qualified ProcessComposition.Isabelle.Process;

data Cres = Instructions deriving (Prelude.Read, Prelude.Show);

equal_cres :: Cres -> Cres -> Bool;
equal_cres Instructions y = True;

equal_cresa :: Cres -> Cres -> Bool;
equal_cresa = equal_cres;

instance Eq Cres where {
  a == b = equal_cresa a b;
};

data Lres = Students | Submissions | Marks | MarksSubmitted | MarksReleased
  deriving (Prelude.Read, Prelude.Show);

markAll ::
  forall a. ProcessComposition.Isabelle.Process.Process Lres a String ();
markAll =
  ProcessComposition.Isabelle.Process.Primitive
    (ProcessComposition.Isabelle.Resource.res Submissions)
    (ProcessComposition.Isabelle.Resource.res Marks) "Mark Submissions" ();

case_lres :: forall a. a -> a -> a -> a -> a -> Lres -> a;
case_lres f1 f2 f3 f4 f5 MarksReleased = f5;
case_lres f1 f2 f3 f4 f5 MarksSubmitted = f4;
case_lres f1 f2 f3 f4 f5 Marks = f3;
case_lres f1 f2 f3 f4 f5 Submissions = f2;
case_lres f1 f2 f3 f4 f5 Students = f1;

equal_lres :: Lres -> Lres -> Bool;
equal_lres MarksReleased y = case_lres False False False False True y;
equal_lres MarksSubmitted y = case_lres False False False True False y;
equal_lres Marks y = case_lres False False True False False y;
equal_lres Submissions y = case_lres False True False False False y;
equal_lres Students y = case_lres True False False False False y;

collectSubs :: ProcessComposition.Isabelle.Process.Process Lres Cres String ();
collectSubs =
  ProcessComposition.Isabelle.Process.Primitive
    (ProcessComposition.Isabelle.Resource.resource_par
      (ProcessComposition.Isabelle.Resource.copyable Instructions)
      (ProcessComposition.Isabelle.Resource.res Students))
    (ProcessComposition.Isabelle.Resource.res Submissions) "Collect Submissions"
    ();

submitMarks ::
  forall a. ProcessComposition.Isabelle.Process.Process Lres a String ();
submitMarks =
  ProcessComposition.Isabelle.Process.Primitive
    (ProcessComposition.Isabelle.Resource.res Marks)
    (ProcessComposition.Isabelle.Resource.res MarksSubmitted) "Submit Marks" ();

releaseMarks ::
  forall a. ProcessComposition.Isabelle.Process.Process Lres a String ();
releaseMarks =
  ProcessComposition.Isabelle.Process.Primitive
    (ProcessComposition.Isabelle.Resource.res MarksSubmitted)
    (ProcessComposition.Isabelle.Resource.res MarksReleased) "Release Marks" ();

markingProcess ::
  ProcessComposition.Isabelle.Process.Process Lres Cres String ();
markingProcess =
  ProcessComposition.Isabelle.Process.seq_process_list
    [collectSubs, markAll, submitMarks, releaseMarks];

equal_lresa :: Lres -> Lres -> Bool;
equal_lresa = equal_lres;

}
