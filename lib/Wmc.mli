
type weight = (Bdd.label, (Bignum.t*Bignum.t)) Core.Hashtbl.Poly.t

(** Performs a weighted model count of the BDD with the supplied weight function. When [collect_cache]
	is [true], the per-node cache from the traversal is retained and can be retrieved via [last_cache].
	If [dump_dot] is enabled (or the [DICE_DUMP_DOT] env var is set), the BDD is exported as a dot graph
	before counting; the output path can be provided via [dump_dot_file] or [DICE_DUMP_DOT_FILE],
	otherwise it defaults to [bdd_dump_<pid>_<timestamp>.dot]. Use [name_map] for readable labels. *)
val wmc :
	?collect_cache:bool ->
	wmc_type: int ->
	Bdd.manager ->
	Bdd.bddptr ->
	weight ->
	Bignum.t

(** Returns the cache from the most recent call to [wmc ~collect_cache:true], if available. *)
val last_cache : unit -> (Bdd.bddptr, Bignum.t) Core.Hashtbl.Poly.t option
