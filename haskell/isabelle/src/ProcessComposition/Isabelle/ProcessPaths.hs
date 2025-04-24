{-# LANGUAGE EmptyDataDecls, RankNTypes, ScopedTypeVariables #-}

module
  ProcessComposition.Isabelle.ProcessPaths(Process_inner(..),
    equal_process_inner, subprocessStep, subprocess)
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
import qualified ProcessComposition.Isabelle.Option;
import qualified ProcessComposition.Isabelle.List;
import qualified ProcessComposition.Isabelle.Process;

data Process_inner = SeqL | SeqR | ParL | ParR | OptL | OptR | Rep
  deriving (Prelude.Read, Prelude.Show);

equal_process_inner :: Process_inner -> Process_inner -> Bool;
equal_process_inner OptR Rep = False;
equal_process_inner Rep OptR = False;
equal_process_inner OptL Rep = False;
equal_process_inner Rep OptL = False;
equal_process_inner OptL OptR = False;
equal_process_inner OptR OptL = False;
equal_process_inner ParR Rep = False;
equal_process_inner Rep ParR = False;
equal_process_inner ParR OptR = False;
equal_process_inner OptR ParR = False;
equal_process_inner ParR OptL = False;
equal_process_inner OptL ParR = False;
equal_process_inner ParL Rep = False;
equal_process_inner Rep ParL = False;
equal_process_inner ParL OptR = False;
equal_process_inner OptR ParL = False;
equal_process_inner ParL OptL = False;
equal_process_inner OptL ParL = False;
equal_process_inner ParL ParR = False;
equal_process_inner ParR ParL = False;
equal_process_inner SeqR Rep = False;
equal_process_inner Rep SeqR = False;
equal_process_inner SeqR OptR = False;
equal_process_inner OptR SeqR = False;
equal_process_inner SeqR OptL = False;
equal_process_inner OptL SeqR = False;
equal_process_inner SeqR ParR = False;
equal_process_inner ParR SeqR = False;
equal_process_inner SeqR ParL = False;
equal_process_inner ParL SeqR = False;
equal_process_inner SeqL Rep = False;
equal_process_inner Rep SeqL = False;
equal_process_inner SeqL OptR = False;
equal_process_inner OptR SeqL = False;
equal_process_inner SeqL OptL = False;
equal_process_inner OptL SeqL = False;
equal_process_inner SeqL ParR = False;
equal_process_inner ParR SeqL = False;
equal_process_inner SeqL ParL = False;
equal_process_inner ParL SeqL = False;
equal_process_inner SeqL SeqR = False;
equal_process_inner SeqR SeqL = False;
equal_process_inner Rep Rep = True;
equal_process_inner OptR OptR = True;
equal_process_inner OptL OptL = True;
equal_process_inner ParR ParR = True;
equal_process_inner ParL ParL = True;
equal_process_inner SeqR SeqR = True;
equal_process_inner SeqL SeqL = True;

instance Eq Process_inner where {
  a == b = equal_process_inner a b;
};

subprocessStep ::
  forall a b c d.
    Process_inner ->
      ProcessComposition.Isabelle.Process.Process a b c d ->
        Maybe (ProcessComposition.Isabelle.Process.Process a b c d);
subprocessStep SeqL p =
  (case p of {
    ProcessComposition.Isabelle.Process.Primitive _ _ _ _ -> Nothing;
    ProcessComposition.Isabelle.Process.Seq x _ -> Just x;
    ProcessComposition.Isabelle.Process.Par _ _ -> Nothing;
    ProcessComposition.Isabelle.Process.Opt _ _ -> Nothing;
    ProcessComposition.Isabelle.Process.Represent _ -> Nothing;
    ProcessComposition.Isabelle.Process.Identity _ -> Nothing;
    ProcessComposition.Isabelle.Process.Swap _ _ -> Nothing;
    ProcessComposition.Isabelle.Process.InjectL _ _ -> Nothing;
    ProcessComposition.Isabelle.Process.InjectR _ _ -> Nothing;
    ProcessComposition.Isabelle.Process.OptDistrIn _ _ _ -> Nothing;
    ProcessComposition.Isabelle.Process.OptDistrOut _ _ _ -> Nothing;
    ProcessComposition.Isabelle.Process.Duplicate _ -> Nothing;
    ProcessComposition.Isabelle.Process.Erase _ -> Nothing;
    ProcessComposition.Isabelle.Process.Apply _ _ -> Nothing;
    ProcessComposition.Isabelle.Process.Repeat _ _ -> Nothing;
    ProcessComposition.Isabelle.Process.Close _ _ -> Nothing;
    ProcessComposition.Isabelle.Process.Once _ _ -> Nothing;
    ProcessComposition.Isabelle.Process.Forget _ -> Nothing;
  });
subprocessStep SeqR p =
  (case p of {
    ProcessComposition.Isabelle.Process.Primitive _ _ _ _ -> Nothing;
    ProcessComposition.Isabelle.Process.Seq _ a -> Just a;
    ProcessComposition.Isabelle.Process.Par _ _ -> Nothing;
    ProcessComposition.Isabelle.Process.Opt _ _ -> Nothing;
    ProcessComposition.Isabelle.Process.Represent _ -> Nothing;
    ProcessComposition.Isabelle.Process.Identity _ -> Nothing;
    ProcessComposition.Isabelle.Process.Swap _ _ -> Nothing;
    ProcessComposition.Isabelle.Process.InjectL _ _ -> Nothing;
    ProcessComposition.Isabelle.Process.InjectR _ _ -> Nothing;
    ProcessComposition.Isabelle.Process.OptDistrIn _ _ _ -> Nothing;
    ProcessComposition.Isabelle.Process.OptDistrOut _ _ _ -> Nothing;
    ProcessComposition.Isabelle.Process.Duplicate _ -> Nothing;
    ProcessComposition.Isabelle.Process.Erase _ -> Nothing;
    ProcessComposition.Isabelle.Process.Apply _ _ -> Nothing;
    ProcessComposition.Isabelle.Process.Repeat _ _ -> Nothing;
    ProcessComposition.Isabelle.Process.Close _ _ -> Nothing;
    ProcessComposition.Isabelle.Process.Once _ _ -> Nothing;
    ProcessComposition.Isabelle.Process.Forget _ -> Nothing;
  });
subprocessStep ParL p =
  (case p of {
    ProcessComposition.Isabelle.Process.Primitive _ _ _ _ -> Nothing;
    ProcessComposition.Isabelle.Process.Seq _ _ -> Nothing;
    ProcessComposition.Isabelle.Process.Par x _ -> Just x;
    ProcessComposition.Isabelle.Process.Opt _ _ -> Nothing;
    ProcessComposition.Isabelle.Process.Represent _ -> Nothing;
    ProcessComposition.Isabelle.Process.Identity _ -> Nothing;
    ProcessComposition.Isabelle.Process.Swap _ _ -> Nothing;
    ProcessComposition.Isabelle.Process.InjectL _ _ -> Nothing;
    ProcessComposition.Isabelle.Process.InjectR _ _ -> Nothing;
    ProcessComposition.Isabelle.Process.OptDistrIn _ _ _ -> Nothing;
    ProcessComposition.Isabelle.Process.OptDistrOut _ _ _ -> Nothing;
    ProcessComposition.Isabelle.Process.Duplicate _ -> Nothing;
    ProcessComposition.Isabelle.Process.Erase _ -> Nothing;
    ProcessComposition.Isabelle.Process.Apply _ _ -> Nothing;
    ProcessComposition.Isabelle.Process.Repeat _ _ -> Nothing;
    ProcessComposition.Isabelle.Process.Close _ _ -> Nothing;
    ProcessComposition.Isabelle.Process.Once _ _ -> Nothing;
    ProcessComposition.Isabelle.Process.Forget _ -> Nothing;
  });
subprocessStep ParR p =
  (case p of {
    ProcessComposition.Isabelle.Process.Primitive _ _ _ _ -> Nothing;
    ProcessComposition.Isabelle.Process.Seq _ _ -> Nothing;
    ProcessComposition.Isabelle.Process.Par _ a -> Just a;
    ProcessComposition.Isabelle.Process.Opt _ _ -> Nothing;
    ProcessComposition.Isabelle.Process.Represent _ -> Nothing;
    ProcessComposition.Isabelle.Process.Identity _ -> Nothing;
    ProcessComposition.Isabelle.Process.Swap _ _ -> Nothing;
    ProcessComposition.Isabelle.Process.InjectL _ _ -> Nothing;
    ProcessComposition.Isabelle.Process.InjectR _ _ -> Nothing;
    ProcessComposition.Isabelle.Process.OptDistrIn _ _ _ -> Nothing;
    ProcessComposition.Isabelle.Process.OptDistrOut _ _ _ -> Nothing;
    ProcessComposition.Isabelle.Process.Duplicate _ -> Nothing;
    ProcessComposition.Isabelle.Process.Erase _ -> Nothing;
    ProcessComposition.Isabelle.Process.Apply _ _ -> Nothing;
    ProcessComposition.Isabelle.Process.Repeat _ _ -> Nothing;
    ProcessComposition.Isabelle.Process.Close _ _ -> Nothing;
    ProcessComposition.Isabelle.Process.Once _ _ -> Nothing;
    ProcessComposition.Isabelle.Process.Forget _ -> Nothing;
  });
subprocessStep OptL p =
  (case p of {
    ProcessComposition.Isabelle.Process.Primitive _ _ _ _ -> Nothing;
    ProcessComposition.Isabelle.Process.Seq _ _ -> Nothing;
    ProcessComposition.Isabelle.Process.Par _ _ -> Nothing;
    ProcessComposition.Isabelle.Process.Opt x _ -> Just x;
    ProcessComposition.Isabelle.Process.Represent _ -> Nothing;
    ProcessComposition.Isabelle.Process.Identity _ -> Nothing;
    ProcessComposition.Isabelle.Process.Swap _ _ -> Nothing;
    ProcessComposition.Isabelle.Process.InjectL _ _ -> Nothing;
    ProcessComposition.Isabelle.Process.InjectR _ _ -> Nothing;
    ProcessComposition.Isabelle.Process.OptDistrIn _ _ _ -> Nothing;
    ProcessComposition.Isabelle.Process.OptDistrOut _ _ _ -> Nothing;
    ProcessComposition.Isabelle.Process.Duplicate _ -> Nothing;
    ProcessComposition.Isabelle.Process.Erase _ -> Nothing;
    ProcessComposition.Isabelle.Process.Apply _ _ -> Nothing;
    ProcessComposition.Isabelle.Process.Repeat _ _ -> Nothing;
    ProcessComposition.Isabelle.Process.Close _ _ -> Nothing;
    ProcessComposition.Isabelle.Process.Once _ _ -> Nothing;
    ProcessComposition.Isabelle.Process.Forget _ -> Nothing;
  });
subprocessStep OptR p =
  (case p of {
    ProcessComposition.Isabelle.Process.Primitive _ _ _ _ -> Nothing;
    ProcessComposition.Isabelle.Process.Seq _ _ -> Nothing;
    ProcessComposition.Isabelle.Process.Par _ _ -> Nothing;
    ProcessComposition.Isabelle.Process.Opt _ a -> Just a;
    ProcessComposition.Isabelle.Process.Represent _ -> Nothing;
    ProcessComposition.Isabelle.Process.Identity _ -> Nothing;
    ProcessComposition.Isabelle.Process.Swap _ _ -> Nothing;
    ProcessComposition.Isabelle.Process.InjectL _ _ -> Nothing;
    ProcessComposition.Isabelle.Process.InjectR _ _ -> Nothing;
    ProcessComposition.Isabelle.Process.OptDistrIn _ _ _ -> Nothing;
    ProcessComposition.Isabelle.Process.OptDistrOut _ _ _ -> Nothing;
    ProcessComposition.Isabelle.Process.Duplicate _ -> Nothing;
    ProcessComposition.Isabelle.Process.Erase _ -> Nothing;
    ProcessComposition.Isabelle.Process.Apply _ _ -> Nothing;
    ProcessComposition.Isabelle.Process.Repeat _ _ -> Nothing;
    ProcessComposition.Isabelle.Process.Close _ _ -> Nothing;
    ProcessComposition.Isabelle.Process.Once _ _ -> Nothing;
    ProcessComposition.Isabelle.Process.Forget _ -> Nothing;
  });
subprocessStep Rep p =
  (case p of {
    ProcessComposition.Isabelle.Process.Primitive _ _ _ _ -> Nothing;
    ProcessComposition.Isabelle.Process.Seq _ _ -> Nothing;
    ProcessComposition.Isabelle.Process.Par _ _ -> Nothing;
    ProcessComposition.Isabelle.Process.Opt _ _ -> Nothing;
    ProcessComposition.Isabelle.Process.Represent a -> Just a;
    ProcessComposition.Isabelle.Process.Identity _ -> Nothing;
    ProcessComposition.Isabelle.Process.Swap _ _ -> Nothing;
    ProcessComposition.Isabelle.Process.InjectL _ _ -> Nothing;
    ProcessComposition.Isabelle.Process.InjectR _ _ -> Nothing;
    ProcessComposition.Isabelle.Process.OptDistrIn _ _ _ -> Nothing;
    ProcessComposition.Isabelle.Process.OptDistrOut _ _ _ -> Nothing;
    ProcessComposition.Isabelle.Process.Duplicate _ -> Nothing;
    ProcessComposition.Isabelle.Process.Erase _ -> Nothing;
    ProcessComposition.Isabelle.Process.Apply _ _ -> Nothing;
    ProcessComposition.Isabelle.Process.Repeat _ _ -> Nothing;
    ProcessComposition.Isabelle.Process.Close _ _ -> Nothing;
    ProcessComposition.Isabelle.Process.Once _ _ -> Nothing;
    ProcessComposition.Isabelle.Process.Forget _ -> Nothing;
  });

subprocess ::
  forall a b c d.
    [Process_inner] ->
      ProcessComposition.Isabelle.Process.Process a b c d ->
        Maybe (ProcessComposition.Isabelle.Process.Process a b c d);
subprocess p x =
  ProcessComposition.Isabelle.List.foldl
    (\ a b -> ProcessComposition.Isabelle.Option.bind a (subprocessStep b))
    (Just x) p;

}
