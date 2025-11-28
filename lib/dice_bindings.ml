open Core
module CG = DiceLib.CoreGrammar
module Comp = DiceLib.Compiler
module VS = DiceLib.VarState
module BD = DiceLib.Bdd
module WM = DiceLib.Wmc
module LP = DiceLib.LogProbability
module BG = Bignum
module BU = DiceLib.BddUtil

(* Expression table: handle -> expr *)
let expr_tbl : (int, CG.expr) Hashtbl.Poly.t = Hashtbl.Poly.create ()
let next_expr_id = ref 0

let store_expr (e : CG.expr) : int =
  let id = !next_expr_id in
  incr next_expr_id;
  Hashtbl.set expr_tbl ~key:id ~data:e;
  id

let get_expr id = Hashtbl.find_exn expr_tbl id

(* Function table: handle -> func *)
let func_tbl : (int, CG.func) Hashtbl.Poly.t = Hashtbl.Poly.create ()
let next_func_id = ref 0

let store_func (f : CG.func) : int =
  let id = !next_func_id in
  incr next_func_id;
  Hashtbl.set func_tbl ~key:id ~data:f;
  id

let get_func id = Hashtbl.find_exn func_tbl id

(* Compiled program table *)
let prog_tbl : (int, Comp.compiled_program) Hashtbl.Poly.t = Hashtbl.Poly.create ()
let next_prog_id = ref 0

let store_prog (p : Comp.compiled_program) : int =
  let id = !next_prog_id in
  incr next_prog_id;
  Hashtbl.set prog_tbl ~key:id ~data:p;
  id

let get_prog id = Hashtbl.find_exn prog_tbl id

(* Ident table: name -> expr_id *)
let ident_tbl : (string, int) Hashtbl.Poly.t = Hashtbl.Poly.create ()

(* helpers to parse CSV strings passed from FFI *)
let parse_int_csv (s:string) : int list =
  let s = String.strip s in
  if String.length s = 0 then [] else
  String.split s ~on:',' |> List.map ~f:(fun t -> Int.of_string (String.strip t))

let parse_str_csv (s:string) : string list =
  let s = String.strip s in
  if String.length s = 0 then [] else
  String.split s ~on:',' |> List.map ~f:String.strip

(* --- Constructors exposed to FFI --- *)
let mk_ident (name:string) : int =
  match Hashtbl.find ident_tbl name with
  | Some id -> id
  | None ->
    let id = store_expr (CG.Ident name) in
    Hashtbl.set ident_tbl ~key:name ~data:id;
    id

let mk_flip (p:float) : int =
  store_expr (CG.Flip (BG.of_float_decimal p))

let mk_and (id1:int) (id2:int) : int =
  let e1 = get_expr id1 in
  let e2 = get_expr id2 in
  store_expr (CG.And (e1, e2))

let mk_or (id1:int) (id2:int) : int =
  let e1 = get_expr id1 in
  let e2 = get_expr id2 in
  store_expr (CG.Or (e1, e2))

let mk_not (id:int) : int =
  let e = get_expr id in
  store_expr (CG.Not e)

let mk_tup (id1:int) (id2:int) : int =
  let e1 = get_expr id1 in
  let e2 = get_expr id2 in
  store_expr (CG.Tup (e1, e2))

let mk_fst (id:int) : int =
  let e = get_expr id in
  store_expr (CG.Fst e)

let mk_snd (id:int) : int =
  let e = get_expr id in
  store_expr (CG.Snd e)

let mk_let (name:string) (rhs_id:int) (body_id:int) : int =
  let rhs = get_expr rhs_id in
  let body = get_expr body_id in
  store_expr (CG.Let (name, rhs, body))

let mk_func (name:string) (args_csv:string) (body_id:int) : int =
  let arg_names = parse_str_csv args_csv in
  let args = List.map arg_names ~f:(fun n -> (n, CG.TBool)) in
  let body = get_expr body_id in
  store_func { CG.name = name; CG.args = args; CG.body = body }

let mk_func_call (name:string) (args_csv:string) : int =
  let arg_ids = parse_int_csv args_csv in
  let args = List.map arg_ids ~f:get_expr in
  store_expr (CG.FuncCall (name, args))

(* Build and compile program. func_ids_csv is a CSV of function handles (ints). *)
(* Build and compile program from a list of function handles (int list) and a body id.
   This avoids passing CSV strings across the FFI. *)
let build_and_compile_program (func_ids:int list) (body_id:int) : int =
  let funcs = List.map func_ids ~f:(fun id -> get_func id) in
  let body = get_expr body_id in
  let prog = { CG.functions = funcs; CG.body = body } in
  let cprog = Comp.compile_program prog ~eager_eval:false in
  store_prog cprog

(* Evaluate boolean function by program id, function name and CSV arg indices.
   Returns float probability. *)
(* Evaluate boolean function by program id, function name and a list of
   argument indices (int list). Using int list avoids string parsing on the
   FFI boundary. *)
let eval_bool_prog (prog_id:int) (fname:string) (arg_indices:int list) : float =
  let cprog = get_prog prog_id in
  let arg_indices = arg_indices in
  (* collect top-level leaves *)
  let top_leaves = VS.collect_leaves cprog.body.state in
  let compiled = Hashtbl.Poly.find_exn cprog.ctx.funcs fname in
  (* build actuals: wrap bddptr as Leaf so types match bddptr btree *)
  let actuals = List.map arg_indices ~f:(fun i -> VS.Leaf (List.nth_exn top_leaves i)) in

  let zipped = List.zip_exn compiled.args actuals in
  let final_state =
    List.fold zipped ~init:compiled.body.state ~f:(fun acc (ph, act) ->
      VS.subst_state cprog.ctx.man ph act acc)
  in

  let final_z_local =
    List.fold zipped ~init:compiled.body.z ~f:(fun acc (ph, act) ->
      VS.extract_leaf (VS.subst_state cprog.ctx.man ph act (VS.Leaf acc)))
  in
  let final_z = BD.bdd_and cprog.ctx.man cprog.body.z final_z_local in

  let denom = WM.wmc ~collect_cache:true ~wmc_type:0 cprog.ctx.man cprog.body.z cprog.ctx.weights in
  let cache = WM.last_cache () in
  let leaf = VS.extract_leaf final_state in
  let leaf_with_constraints = BD.bdd_and cprog.ctx.man leaf final_z in
  let numer =
    match cache with
    | Some c ->
      (match Hashtbl.Poly.find c leaf_with_constraints with
       | Some v ->
         printf "[dice] WMC cache hit (eval_bool_prog numerator)\n%!";
         v
       | None -> WM.wmc ~wmc_type:0 cprog.ctx.man leaf_with_constraints cprog.ctx.weights)
    | None -> WM.wmc ~wmc_type:0 cprog.ctx.man leaf_with_constraints cprog.ctx.weights
  in
  let res = LP.rat_div_and_conv numer denom in
  BG.to_float res

let eval_distributions (body_handle:int) : float list =
  let body_expr = get_expr body_handle in
  let program = { CG.functions = []; CG.body = body_expr } in

  Printf.printf "[dice] Compiling program for eval_distributions\n%!";
  let compiled_prog = Comp.compile_program program ~eager_eval:false in
  Printf.printf "[dice] Compiled program for eval_distributions\n%!";
  
  let man = compiled_prog.ctx.man in
  let weights = compiled_prog.ctx.weights in
  let z = compiled_prog.body.z in 
  let prob_denominator = WM.wmc ~collect_cache:true ~wmc_type:0 man z weights in
  let cache = WM.last_cache () in
  let leaves = VS.collect_leaves compiled_prog.body.state in

  List.map leaves ~f:(fun leaf_bdd ->
    let leaf_with_constraints = BD.bdd_and man leaf_bdd z in
    (* let leaf_with_constraints = leaf_bdd in *)
    let prob_numerator =
      match cache with
      | Some c ->
        (match Hashtbl.Poly.find c leaf_with_constraints with
         | Some v ->
           printf "[dice] WMC cache hit (eval_distributions numerator)\n%!";
           v
         | None -> WM.wmc ~wmc_type:0 man leaf_with_constraints weights)
      | None -> WM.wmc ~wmc_type:0 man leaf_with_constraints weights
    in
    let p = LP.rat_div_and_conv prob_numerator prob_denominator in
    Printf.printf "[dice] Distribution leaf WMC:probability=%f\n%!"
      (BG.to_float p);
    BG.to_float p
  )

let dump_compiled_bdds (compiled_prog: Comp.compiled_program) : unit =
  let man = compiled_prog.ctx.man in
  let name_map = compiled_prog.ctx.name_map in
  let leaves = VS.collect_leaves compiled_prog.body.state in
  List.iteri leaves ~f:(fun idx leaf_bdd ->
    Format.printf "Leaf %d BDD:\n%!" idx;
    BU.dump_dot man name_map leaf_bdd;
    Format.printf "\n%!") ;
  Format.printf "Constraint Z:\n%!";
  BU.dump_dot man name_map compiled_prog.body.z;
  Format.printf "\n%!"

let dump_bdds_for_body (body_handle:int) : unit =
  let body_expr = get_expr body_handle in
  let program = { CG.functions = []; CG.body = body_expr } in
  let compiled_prog = Comp.compile_program program ~eager_eval:false in
  dump_compiled_bdds compiled_prog

let dump_bdds_for_prog (prog_id:int) : unit =
  let compiled_prog = get_prog prog_id in
  dump_compiled_bdds compiled_prog

(* Register for C callbacks *)
let () =
  Callback.register "mk_ident" mk_ident;
  Callback.register "mk_flip" mk_flip;
  Callback.register "mk_and" mk_and;
  Callback.register "mk_or" mk_or;
  Callback.register "mk_not" mk_not;
  Callback.register "mk_tup" mk_tup;
  Callback.register "mk_fst" mk_fst;
  Callback.register "mk_snd" mk_snd;
  Callback.register "mk_let" mk_let;
  Callback.register "mk_func" mk_func;
  Callback.register "mk_func_call" mk_func_call;
  Callback.register "build_and_compile_program" build_and_compile_program;
  Callback.register "eval_bool_prog" eval_bool_prog;
  Callback.register "eval_distributions" eval_distributions;
  Callback.register "dump_bdds_for_body" dump_bdds_for_body;
  Callback.register "dump_bdds_for_prog" dump_bdds_for_prog