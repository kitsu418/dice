/* C wrapper for calling registered OCaml functions from Rust/C.
         Provides:
         - ocaml_runtime_init(): idempotent OCaml runtime init
         - build_and_compile_program_c(func_ids, len, body_id) -> int
         - eval_bool_prog_c(prog_id, fname, arg_indices, len) -> double

         This file assumes corresponding OCaml functions have been registered
         via Callback.register with names:
                 "build_and_compile_program" : int list -> int -> int
                 "eval_bool_prog" : int -> string -> int list -> float

         The implementation marshals C int arrays into OCaml int lists and
         calls the named OCaml callbacks.
*/

#include <caml/alloc.h>
#include <caml/callback.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <stdio.h>

static int ocaml_initialized = 0;

/* Initialize OCaml runtime once. Safe to call multiple times. */
void ocaml_runtime_init() {
  if (ocaml_initialized)
    return;
  static char *caml_argv[] = {NULL};
  caml_startup(caml_argv);
  ocaml_initialized = 1;
}

/* Helper: construct an OCaml int list from a C int array (length len).
         Returns an OCaml value (list). Uses CAML macros for GC safety. */
static value make_int_list(const int *arr, int len) {
  CAMLparam0();
  CAMLlocal2(head, tail);
  tail = Val_emptylist;
  for (int i = len - 1; i >= 0; --i) {
    head = caml_alloc(2, 0);
    Store_field(head, 0, Val_int(arr[i]));
    Store_field(head, 1, tail);
    tail = head;
  }
  CAMLreturn(tail);
}

/* Build and compile a program from an array of function handles and a body id.
         Returns the program id as int (OCaml int). */
int build_and_compile_program_c(const int *func_ids, int len, int body_id) {
  ocaml_runtime_init();
  CAMLparam0();
  CAMLlocal2(v_func_ids, v_body_id);

  v_func_ids = make_int_list(func_ids, len);
  v_body_id = Val_int(body_id);

  const value *v_cb = caml_named_value("build_and_compile_program");
  if (v_cb == NULL)
    caml_failwith("build_and_compile_program callback not found");

  value v_res = caml_callback2(*v_cb, v_func_ids, v_body_id);
  int res = Int_val(v_res);
  CAMLreturnT(int, res);
}

/* Evaluate a boolean function by program id, function name and argument
   indices. Returns a double probability. */
double eval_bool_prog_c(int prog_id, const char *fname, const int *arg_indices,
                        int len) {
  ocaml_runtime_init();
  CAMLparam0();
  CAMLlocal3(v_prog_id, v_fname, v_arg_list);

  v_prog_id = Val_int(prog_id);
  v_fname = caml_copy_string(fname);
  v_arg_list = make_int_list(arg_indices, len);

  const value *v_cb = caml_named_value("eval_bool_prog");
  if (v_cb == NULL)
    caml_failwith("eval_bool_prog callback not found");

  value v_res = caml_callback3(*v_cb, v_prog_id, v_fname, v_arg_list);
  double res = Double_val(v_res);
  CAMLreturnT(double, res);
}

/* --- Thin wrappers for expression constructors registered in OCaml --- */

int mk_ident_c(const char *name) {
  ocaml_runtime_init();
  CAMLparam0();
  CAMLlocal1(v_name);

  v_name = caml_copy_string(name);
  const value *v_cb = caml_named_value("mk_ident");
  if (v_cb == NULL)
    caml_failwith("mk_ident callback not found");
  value v_res = caml_callback(*v_cb, v_name);
  int res = Int_val(v_res);
  CAMLreturnT(int, res);
}

int mk_flip_c(double p) {
  ocaml_runtime_init();
  CAMLparam0();
  CAMLlocal1(v_p);

  v_p = caml_copy_double(p);
  const value *v_cb = caml_named_value("mk_flip");
  if (v_cb == NULL)
    caml_failwith("mk_flip callback not found");
  value v_res = caml_callback(*v_cb, v_p);
  int res = Int_val(v_res);
  CAMLreturnT(int, res);
}

int mk_and_c(int id1, int id2) {
  ocaml_runtime_init();
  CAMLparam0();
  CAMLlocal2(v1, v2);

  v1 = Val_int(id1);
  v2 = Val_int(id2);
  const value *v_cb = caml_named_value("mk_and");
  if (v_cb == NULL)
    caml_failwith("mk_and callback not found");
  value v_res = caml_callback2(*v_cb, v1, v2);
  int res = Int_val(v_res);
  CAMLreturnT(int, res);
}

int mk_or_c(int id1, int id2) {
  ocaml_runtime_init();
  CAMLparam0();
  CAMLlocal2(v1, v2);

  v1 = Val_int(id1);
  v2 = Val_int(id2);
  const value *v_cb = caml_named_value("mk_or");
  if (v_cb == NULL)
    caml_failwith("mk_or callback not found");
  value v_res = caml_callback2(*v_cb, v1, v2);
  int res = Int_val(v_res);
  CAMLreturnT(int, res);
}

int mk_not_c(int id) {
  ocaml_runtime_init();
  CAMLparam0();
  CAMLlocal1(v1);

  v1 = Val_int(id);
  const value *v_cb = caml_named_value("mk_not");
  if (v_cb == NULL)
    caml_failwith("mk_not callback not found");
  value v_res = caml_callback(*v_cb, v1);
  int res = Int_val(v_res);
  CAMLreturnT(int, res);
}

int mk_tup_c(int id1, int id2) {
  ocaml_runtime_init();
  CAMLparam0();
  CAMLlocal2(v1, v2);

  v1 = Val_int(id1);
  v2 = Val_int(id2);
  const value *v_cb = caml_named_value("mk_tup");
  if (v_cb == NULL)
    caml_failwith("mk_tup callback not found");
  value v_res = caml_callback2(*v_cb, v1, v2);
  int res = Int_val(v_res);
  CAMLreturnT(int, res);
}

int mk_fst_c(int id) {
  ocaml_runtime_init();
  CAMLparam0();
  CAMLlocal1(v1);

  v1 = Val_int(id);
  const value *v_cb = caml_named_value("mk_fst");
  if (v_cb == NULL)
    caml_failwith("mk_fst callback not found");
  value v_res = caml_callback(*v_cb, v1);
  int res = Int_val(v_res);
  CAMLreturnT(int, res);
}

int mk_snd_c(int id) {
  ocaml_runtime_init();
  CAMLparam0();
  CAMLlocal1(v1);

  v1 = Val_int(id);
  const value *v_cb = caml_named_value("mk_snd");
  if (v_cb == NULL)
    caml_failwith("mk_snd callback not found");
  value v_res = caml_callback(*v_cb, v1);
  int res = Int_val(v_res);
  CAMLreturnT(int, res);
}

int mk_let_c(const char *name, int rhs_id, int body_id) {
  ocaml_runtime_init();
  CAMLparam0();
  CAMLlocal2(v_name, v_rhs);

  v_name = caml_copy_string(name);
  v_rhs = Val_int(rhs_id);
  const value *v_cb = caml_named_value("mk_let");
  if (v_cb == NULL)
    caml_failwith("mk_let callback not found");
  value v_res = caml_callback3(*v_cb, v_name, v_rhs, Val_int(body_id));
  int res = Int_val(v_res);
  CAMLreturnT(int, res);
}

int mk_func_c(const char *name, const char *args_csv, int body_id) {
  ocaml_runtime_init();
  CAMLparam0();
  CAMLlocal2(v_name, v_args);

  v_name = caml_copy_string(name);
  v_args = caml_copy_string(args_csv);
  const value *v_cb = caml_named_value("mk_func");
  if (v_cb == NULL)
    caml_failwith("mk_func callback not found");
  value v_res = caml_callback3(*v_cb, v_name, v_args, Val_int(body_id));
  int res = Int_val(v_res);
  CAMLreturnT(int, res);
}
