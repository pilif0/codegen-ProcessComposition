theory CodeExport
  imports
    ProcessComposition.Process
    ProcessComposition.ProcessPort
    "HOL-Library.Code_Target_Numeral"
begin

section\<open>Code Export\<close>

text\<open>Use the direct variant to implement resource normalisation\<close>
lemmas [code_unfold] = normal_rewr_is_normal_dir

text\<open>Export Haskell code\<close>
export_code open
  (* Utilities from general library *)
  remdups image Set.filter
  "HOL.equal :: nat \<Rightarrow> nat \<Rightarrow> bool" "(+) :: nat \<Rightarrow> nat \<Rightarrow> nat" "0 :: nat" "1 :: nat"
  (* Resource terms *)
  normal_dir normal_rewr
  "HOL.equal :: ('s, 'a, 'n) qualified_port \<Rightarrow> ('s, 'a, 'n) qualified_port \<Rightarrow> bool"
  (* Resources *)
  Empty Anything Res Copyable Parallel NonD Executable Repeatable
  of_resource map_resource resource_par parallel_parts parallelise refine_resource
  "HOL.equal :: ('a :: equal, 'b :: equal) resource \<Rightarrow>  ('a, 'b) resource \<Rightarrow> bool"
  (* Processes *)
  input "output" valid primitives
  map_process process_refineRes process_subst
  par_process_list seq_process_list
  "HOL.equal :: ('a :: equal, 'b :: equal, 'l, 'm) process \<Rightarrow>  ('a, 'b, 'l, 'm) process \<Rightarrow> bool"
  (* Ports *)
  port.side port.index port.label listPorts
  qualified_port.port qualified_port.name renameQPort qualifyQPort
  "HOL.equal :: ('s, 'a, 'n) qualified_port \<Rightarrow> ('s, 'a, 'n) qualified_port \<Rightarrow> bool"
  (* Process paths *)
  subprocess "HOL.equal :: process_inner \<Rightarrow> process_inner \<Rightarrow> bool"
  (* Process ports *)
  parallelPorts process_side.In process_side.Out
  "HOL.equal :: process_side \<Rightarrow> process_side \<Rightarrow> bool"
  in Haskell file_prefix "haskell/isabelle/src" (root: ProcessComposition.Isabelle string_classes)

end
