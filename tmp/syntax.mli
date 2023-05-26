(* module Syntax: syntax trees and associated support functions *)

open Support.Pervasive
open Support.Error

(* lockset *)
type lockset
val appendlock : string -> lockset -> lockset
val newlockset : string -> lockset
val emptylockset: lockset
val maplockset : (string -> 'a) -> lockset -> 'a list
val interlockset : lockset -> lockset -> lockset
val unionlockset : lockset -> lockset -> lockset
val foldlockset : (string -> 'a -> 'a) -> lockset -> 'a -> 'a
val sublockset : lockset -> lockset -> bool
val locksetequal : lockset -> lockset -> bool
val existlock : string -> lockset -> bool

(* Data type definitions *)
type ty =
    TyBot
  | TyTop
  | TyId of string
  | TyVar of int * int
  | TyArr of ty * ty * lockset
  | TyRecord of (string * ty) list
  | TyVariant of (string * ty) list
  | TyRef of ty * lockset
  | TyString
  | TyUnit
  | TyBool
  | TySource of ty
  | TySink of ty
  | TyFloat
  | TyNat
  | TyThread of ty
  | TyLock of lockset

type term =
    TmVar of info * int * int
  | TmAbs of info * string * ty * term * lockset
  | TmApp of info * term * term
  | TmAscribe of info * term * ty
  | TmString of info * string
  | TmUnit of info
  | TmLoc of info * int
  | TmRef of info * term * lockset
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
  | TmThread of info * Thread.t * term Event.channel
  | TmLock of info * lockset
  | TmFork of info * term
  | TmWait of info * term
  | TmTid of info
  | TmSync of info * term * term

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

