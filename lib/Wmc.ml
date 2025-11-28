open Core
open Bdd

module VS = VarState

(** map from variable index to (low weight, high weight) *)
type weight = (label, (Bignum.t*Bignum.t)) Core.Hashtbl.Poly.t

let last_cache_ref : (bddptr, Bignum.t) Hashtbl.Poly.t option ref = ref None

let last_cache () = !last_cache_ref

let hashtable_to_float (low, high) = 
  (Bignum.to_float low, Bignum.to_float high) 

let hashtable_to_log (low, high) = 
  (LogProbability.make low, LogProbability.make high)





(** Perform a weighted model count of the BDD `bdd` with weight function `w` *)
let wmc ?(collect_cache=false) ~wmc_type mgr (bdd : bddptr) (w: weight) =
  (* internal memoized recursive weighted model count *)
  Printf.printf "[dice] WMC called with wmc_type=%d\n%!" wmc_type;
  let rec wmc_rec bdd w cache addop multop one zero =
    if bdd_is_true mgr bdd then one
    else if bdd_is_false mgr bdd then zero
    else match Hashtbl.Poly.find cache bdd with
      | Some v -> v
      | _ ->
        (* compute weight of children *)
        let (thn, els) = (bdd_high mgr bdd, bdd_low mgr bdd) in
        let thnw = wmc_rec thn w cache addop multop one zero and
          elsw = wmc_rec els w cache addop multop one zero in
        (* compute new weight, add to cache *)
        let (loww, highw) = try Hashtbl.Poly.find_exn w (bdd_topvar mgr bdd)
          with _ -> failwith (Format.sprintf "Could not find variable %d" (Bdd.int_of_label (bdd_topvar mgr bdd)))in
        let new_weight = (addop (multop highw thnw) (multop loww elsw)) in
        Hashtbl.Poly.add_exn cache ~key:bdd ~data:new_weight;
        new_weight in
  let store_cache convert cache_tbl =
    if collect_cache then
      last_cache_ref := Some (Hashtbl.Poly.map cache_tbl ~f:convert)
    else
      last_cache_ref := None
  in
  if wmc_type = 2 then
    let cache = Hashtbl.Poly.create () in
  let res = wmc_rec bdd (Hashtbl.Poly.map ~f:hashtable_to_float w) cache (+.) ( *. ) 1. 0. in
    store_cache (fun v -> Bignum.of_float_decimal v) cache;
    Bignum.of_float_decimal res
  else if wmc_type = 1 then
    let cache = Hashtbl.Poly.create () in
    let res = wmc_rec bdd w cache Bignum.(+) Bignum.( * ) Bignum.one Bignum.zero in
    store_cache (fun v -> v) cache;
    res
  else
    let cache = Hashtbl.Poly.create () in
  let res = wmc_rec bdd
    (Hashtbl.Poly.map ~f:hashtable_to_log (Hashtbl.Poly.map ~f:hashtable_to_float w))
        cache (LogProbability.add) (LogProbability.mult) (LogProbability.make 1.) (LogProbability.make 0.) in
    store_cache (fun v -> Bignum.of_float_decimal (LogProbability.conv v)) cache;
    Bignum.of_float_decimal (LogProbability.conv res)
      
     

