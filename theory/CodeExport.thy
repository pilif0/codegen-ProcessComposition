theory CodeExport
  imports
    ProcessComposition.Process
    ProcessComposition.ProcessPort
begin

section\<open>Code Export\<close>

text\<open>Use the direct variant to implement resource normalisation\<close>
lemmas [code_unfold] = normal_rewr_is_normal_dir

text\<open>Export Haskell code\<close>
export_code open
  (* Utilities from general library *)
  remdups image Set.filter
  (* Resource terms *)
  normal_dir normal_rewr
  (* Resources *)
  Empty Anything Res Copyable Parallel NonD Executable Repeatable
  of_resource map_resource resource_par parallel_parts parallelise refine_resource
  (* Processes *)
  input "output" valid primitives
  map_process process_refineRes process_subst
  par_process_list seq_process_list
  (* Process paths and ports *)
  port.side port.index port.label listPorts
  qualified_port.port qualified_port.name renameQPort qualifyQPort
  subprocess
  parallelPorts process_side.In process_side.Out
  in Haskell file_prefix "haskell/isabelle/src" (root: ProcessComposition.Isabelle string_classes)

end
