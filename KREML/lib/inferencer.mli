type error = Typing.error
module R :
  sig
    type 'a t
    val bind : 'a t -> f:('a -> 'b t) -> 'b t
    val return : 'a -> 'a t
    val fail : error -> 'a t
    val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
    val ( >>| ) : 'a t -> ('a -> 'b) -> 'b t
    module Syntax :
      sig
        val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
        val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t
      end
    module RMap :
      sig
        val fold_left :
          (int, 'a, Base.Int.comparator_witness) Base.Map.t ->
          init:'b t -> f:(int -> 'a -> 'b -> 'b t) -> 'b t
      end
    val fresh : int t
    val run : 'a t -> ('a, error) result
  end
type fresh = int
module Type :
  sig
    type t = Typing.typ
    val occurs_in : fresh -> t -> bool
    val free_vars : t -> (fresh, Base.Int.comparator_witness) Base.Set.t
  end
module Subst :
  sig
    type t
    val empty : t
    val singleton : fresh -> Typing.typ -> t R.t
    val find_exn : fresh -> t -> Typing.typ
    val find : fresh -> t -> Typing.typ option
    val apply : t -> Typing.typ -> Typing.typ
    val unify : Typing.typ -> Typing.typ -> t R.t
    val compose : t -> t -> t R.t
    val compose_all : t list -> t R.t
    val remove : t -> fresh -> t
  end
module VarSet :
  sig
    val fold_right :
      ('a -> 'b -> 'a R.t) -> 'a R.t -> ('b, 'c) Base.Set.t -> 'a R.t
  end
module Scheme :
  sig
    type t = Typing.scheme
    val occurs_in : fresh -> (fresh, 'a) Base.Set.t * Typing.typ -> bool
    val free_vars :
      (fresh, Base.Int.comparator_witness) Base.Set.t * Typing.typ ->
      (fresh, Base.Int.comparator_witness) Base.Set.t
    val apply :
      Subst.t ->
      (fresh, 'a) Base.Set.t * Typing.typ ->
      (fresh, 'a) Base.Set.t * Typing.typ
  end
module TypeEnv :
  sig
    type t =
        (string, Typing.scheme, Base.String.comparator_witness) Base.Map.t
    val extend :
      ('a, 'b, 'c) Base.Map.t -> 'a -> 'b -> ('a, 'b, 'c) Base.Map.t
    val empty : (string, 'a, Base.String.comparator_witness) Base.Map.t
    val free_vars : t -> (fresh, Base.Int.comparator_witness) Base.Set.t
    val apply :
      Subst.t ->
      ('a, (fresh, 'b) Base.Set.t * Typing.typ, 'c) Base.Map.t ->
      ('a, (fresh, 'b) Base.Set.t * Typing.typ, 'c) Base.Map.t
    val find_exn :
      'a -> ('a, equal:(string -> string -> bool) -> 'b, 'c) Base.Map.t -> 'b
  end
val unify : Typing.typ -> Typing.typ -> Subst.t R.t
val fresh_var : Typing.typ R.t
val fresh_eq_var : Typing.typ R.t
val instantiate : Typing.scheme -> Typing.typ R.t
val generalize : TypeEnv.t -> Typing.typ -> Typing.scheme
val lookup_env :
  string ->
  (string, Typing.scheme, 'a) Base.Map.t -> (Subst.t * Typing.typ) R.t
val find_identifiers :
  Ast.expr -> (string, Base.String.comparator_witness) Base.Set.t
val is_syntactically_value : Ast.expr -> bool
val infer : TypeEnv.t -> Ast.binding list -> (Subst.t * Typing.typ) R.t
val run_inference : Ast.binding list -> (Typing.typ, error) result
val parse_and_inference : string -> unit
