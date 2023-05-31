(* module Syntax: syntax trees and associated support functions *)

open Support.Pervasive
open Support.Error

(* Data type definitions *)

type mutexset
val appendmutex : string -> mutexset -> mutexset
val newmutexset : string -> mutexset
val emptymutexset: mutexset
val mapmutexset : (string -> 'a) -> mutexset -> 'a list
val intermutexset : mutexset -> mutexset -> mutexset
val unionmutexset : mutexset -> mutexset -> mutexset
val foldmutexset : (string -> 'a -> 'a) -> mutexset -> 'a -> 'a
val submutexset : mutexset -> mutexset -> bool
val mutexsetequal : mutexset -> mutexset -> bool
val existmutex : string -> mutexset -> bool

type ty =
    TyBot
  | TyTop
  | TyId of string
  | TyVar of int * int
  | TyArr of ty * ty
  | TyRecord of (string * ty) list
  | TyVariant of (string * ty) list
  | TyRef of ty
  | TyString
  | TyUnit
  | TyBool
  | TySource of ty
  | TySink of ty
  | TyFloat
  | TyNat
  | TyThread of ty
  | TyMutex of mutexset

type term =
    TmVar of info * int * int
  | TmAbs of info * string * ty * term
  | TmApp of info * term * term
  | TmAscribe of info * term * ty
  | TmString of info * string
  | TmUnit of info
  | TmLoc of info * int
  | TmRef of info * term
  | TmDeref of info * term 
  | TmAssign of info * term * term
  | TmCase of info * term * (string * (string * term)) list
  | TmTag of info * string * term * ty
  | TmLet of info * string * term * term
  | TmFix of info * term
  | TmTrue of info
  | TmFalse of info
  | TmIf of info * term * term * term
  | TmFloat of info * float
  | TmTimesfloat of info * term * term
  | TmRecord of info * (string * term) list
  | TmProj of info * term * string
  | TmZero of info
  | TmSucc of info * term
  | TmPred of info * term
  | TmIsZero of info * term
  | TmInert of info * ty
  | TmThread of info * term
  | TmMutex of info * mutexset
  | TmFork of info * term
  | TmWait of info * term
  | TmAcquire of info * term * term
  | TmRefMutex of info * term * term

type binding =
    NameBind 
  | VarBind of ty
  | TmAbbBind of term * (ty option)
  | TyVarBind
  | TyAbbBind of ty

type command =
    Import of string
  | Eval of info * term
  | Bind of info * string * binding

(* Contexts *)
type context
val emptycontext : context 
val ctxlength : context -> int
val addbinding : context -> string -> binding -> context
val addname: context -> string -> context
val index2name : info -> context -> int -> string
val getbinding : info -> context -> int -> binding
val name2index : info -> context -> string -> int
val isnamebound : context -> string -> bool
val getTypeFromContext : info -> context -> int -> ty

(* Shifting and substitution *)
val termShift: int -> term -> term
val termSubstTop: term -> term -> term
val typeShift : int -> ty -> ty
val typeSubstTop: ty -> ty -> ty
val tytermSubstTop: ty -> term -> term

(* Printing *)
val printtm: context -> term -> unit
val printtm_ATerm: bool -> context -> term -> unit
val printty : context -> ty -> unit
val prbinding : context -> binding -> unit

(* Misc *)
val tmInfo: term -> info

