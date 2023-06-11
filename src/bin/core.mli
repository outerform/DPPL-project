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
type thctx
val emptythctx:thctx
val eval : context -> store -> threads -> thctx -> term -> term * store * threads * thctx
val evalbinding : context -> store -> threads -> thctx -> binding -> binding * store * threads * thctx
val tyeqv : context -> ty -> ty -> bool
val simplifyty : context -> ty -> ty
