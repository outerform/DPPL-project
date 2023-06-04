(* module Core

   Core typechecking and evaluation functions
*)

open Syntax
open Support.Error

val typeof : context ->lockset-> term -> ty
val subtype : context -> ty -> ty -> bool
type store
val emptystore : store
val shiftstore : int -> store -> store 
type threads
val emptythreads: threads
val eval : context -> store -> threads -> term -> term * store * threads
val evalbinding : context -> store -> threads -> binding -> binding * store * threads
val tyeqv : context -> ty -> ty -> bool
val simplifyty : context -> ty -> ty
