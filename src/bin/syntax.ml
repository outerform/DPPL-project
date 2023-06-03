open Format
open Support.Error
open Support.Pervasive

(* ---------------------------------------------------------------------- *)
(* Datatypes *)

module StringSet = Set.Make(String)
type lockset = StringSet.t
let appendlock = StringSet.add
let emptylockset = StringSet.empty
let maxlock = StringSet.max_elt
let newlockset x = StringSet.singleton x
let printlockset s =
  let s = StringSet.elements s in
  print_string ("<" ^ List.hd s);
  List.iter (fun a -> print_string ("," ^ a)) (List.tl s);
  print_string ">";;
let maplockset f ls = List.map f (StringSet.elements ls)
let interlockset = StringSet.inter
let unionlockset = StringSet.union
let foldlockset f ls i = StringSet.fold f ls i
let sublockset = StringSet.subset
let locksetequal = StringSet.equal
let existlock = StringSet.mem
let sizelockset = StringSet.cardinal
let equallockset = StringSet.equal

type ty =
    TyBot
  | TyTop
  | TyId of string
  | TyVar of int * int
  | TyArr of lockset * string option * ty * ty
  | TyRecord of (string * ty) list
  | TyVariant of (string * ty) list
  | TyRef of lockset * ty
  | TyString
  | TyUnit
  | TyBool
  | TySource of lockset * ty
  | TySink of lockset * ty
  | TyFloat
  | TyNat
  (* New type *)
  (* | TyRefMutex of string * ty *)
  | TyThread of ty
  | TyMutex of string
  (* | TySourceMutex of string * ty
  | TySinkMutex of string * ty *)

type term =
    TmVar of info * int * int
  | TmAbs of info * lockset * string * ty * term
  | TmApp of info * term * term
  | TmAscribe of info * term * ty
  | TmString of info * string
  | TmUnit of info
  | TmLoc of info * int
  | TmRef of info * lockset * term
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
  (* | TmThread of info * Thread.t * term Event.channel *)
  | TmThread of info * term
  (* | TmFork of info * term *)
  | TmWait of info * term
  | TmTid of info
  | TmMutex of info * string
  | TmAcquire of info * term * term
  (* | TmRefMutex of info * string * term *)


type binding =
    NameBind 
  | VarBind of ty
  | TmAbbBind of term * (ty option)
  | TyVarBind
  | TyAbbBind of ty

type context = (string * binding) list

type command =
    Import of string
  | Eval of info * term
  | Bind of info * string * binding

(* ---------------------------------------------------------------------- *)
(* Context management *)

let emptycontext = []

let ctxlength ctx = List.length ctx

let addbinding ctx x bind = (x,bind)::ctx

let addname ctx x = addbinding ctx x NameBind

let rec isnamebound ctx x =
  match ctx with
      [] -> false
    | (y,_)::rest ->
        if y=x then true
        else isnamebound rest x

let rec pickfreshname ctx x =
  if isnamebound ctx x then pickfreshname ctx (x^"'")
  else ((x,NameBind)::ctx), x

let index2name fi ctx x =
  try
    let (xn,_) = List.nth ctx x in
    xn
  with Failure _ ->
    let msg =
      Printf.sprintf "Variable lookup failure: offset: %d, ctx size: %d" in
    error fi (msg x (List.length ctx))

let rec name2index fi ctx x =
  match ctx with
      [] -> error fi ("Identifier " ^ x ^ " is unbound")
    | (y,_)::rest ->
        if y=x then 0
        else 1 + (name2index fi rest x)

(* ---------------------------------------------------------------------- *)
(* Shifting *)

let tymap onvar c tyT = 
  let rec walk c tyT = match tyT with
    TyBot -> TyBot
  | TyArr(lst,l1,tyT1,tyT2) -> TyArr(lst,l1,walk c tyT1,walk c tyT2)
  | TyTop -> TyTop
  | TyString -> TyString
  | TyVariant(fieldtys) -> TyVariant(List.map (fun (li,tyTi) -> (li, walk c tyTi)) fieldtys)
  | TyId(b) as tyT -> tyT
  | TyRecord(fieldtys) -> TyRecord(List.map (fun (li,tyTi) -> (li, walk c tyTi)) fieldtys)
  | TyBool -> TyBool
  | TyFloat -> TyFloat
  | TyUnit -> TyUnit
  | TyRef(lst,tyT1) -> TyRef(lst, walk c tyT1)
  | TySource(lst, tyT1) -> TySource(lst, walk c tyT1)
  | TySink(lst, tyT1) -> TySink(lst, walk c tyT1)
  | TyVar(x,n) -> onvar c x n
  | TyNat -> TyNat
  | TyThread(tyT1) -> TyThread(walk c tyT1)
  | TyMutex(_) as tyT -> tyT
  (* | TyRefMutex(l1,tyT1) -> TyRefMutex(l1,walk c tyT1)
  | TySourceMutex(l1,tyT1) -> TySourceMutex(l1,walk c tyT1)
  | TySinkMutex(l1,tyT1) -> TySinkMutex(l1,walk c tyT1) *)
  in walk c tyT

let tmmap onvar ontype c t = 
  let rec walk c t = match t with
    TmVar(fi,x,n) -> onvar fi c x n
  | TmAbs(lst,fi,x,tyT1,t2) -> TmAbs(lst, fi,x,ontype c tyT1,walk (c+1) t2)
  | TmApp(fi,t1,t2) -> TmApp(fi,walk c t1,walk c t2)
  | TmAscribe(fi,t1,tyT1) -> TmAscribe(fi,walk c t1,ontype c tyT1)
  | TmString _ as t -> t
  | TmUnit(fi) as t -> t
  | TmLoc(fi,l) as t -> t
  | TmRef(fi,lst,t1) -> TmRef(fi,lst,walk c t1)
  | TmDeref(fi,t1) -> TmDeref(fi,walk c t1)
  | TmAssign(fi,t1,t2) -> TmAssign(fi,walk c t1,walk c t2)
  | TmTag(fi,l,t1,tyT) -> TmTag(fi, l, walk c t1, ontype c tyT)
  | TmCase(fi,t,cases) ->
      TmCase(fi, walk c t,
             List.map (fun (li,(xi,ti)) -> (li, (xi,walk (c+1) ti)))
               cases)
  | TmLet(fi,x,t1,t2) -> TmLet(fi,x,walk c t1,walk (c+1) t2)
  | TmFix(fi,t1) -> TmFix(fi,walk c t1)
  | TmFloat _ as t -> t
  | TmTimesfloat(fi,t1,t2) -> TmTimesfloat(fi, walk c t1, walk c t2)
  | TmTrue(fi) as t -> t
  | TmFalse(fi) as t -> t
  | TmIf(fi,t1,t2,t3) -> TmIf(fi,walk c t1,walk c t2,walk c t3)
  | TmProj(fi,t1,l) -> TmProj(fi,walk c t1,l)
  | TmRecord(fi,fields) -> TmRecord(fi,List.map (fun (li,ti) ->
                                               (li,walk c ti))
                                    fields)
  | TmZero(fi)      -> TmZero(fi)
  | TmSucc(fi,t1)   -> TmSucc(fi, walk c t1)
  | TmPred(fi,t1)   -> TmPred(fi, walk c t1)
  | TmIsZero(fi,t1) -> TmIsZero(fi, walk c t1)
  | TmInert(fi,tyT) -> TmInert(fi,ontype c tyT)
  | TmThread(fi,t1) -> TmThread(fi,walk c t1)
  | TmWait(fi,t1) -> TmWait(fi,walk c t1)
  | TmTid(fi) as t -> t
  | TmAcquire(fi,t1,t2) -> TmAcquire(fi, walk c t1, walk c t2)
  | TmMutex(fi,_) as t -> t
  (* | TmRefMutex(fi,l1, t1) -> TmRefMutex(fi, l1, walk c t1) *)
  in walk c t

let typeShiftAbove d c tyT =
  tymap
    (fun c x n -> if x>=c then TyVar(x+d,n+d) else TyVar(x,n+d))
    c tyT

let termShiftAbove d c t =
  tmmap
    (fun fi c x n -> if x>=c then TmVar(fi,x+d,n+d) 
                     else TmVar(fi,x,n+d))
    (typeShiftAbove d)
    c t

let termShift d t = termShiftAbove d 0 t

let typeShift d tyT = typeShiftAbove d 0 tyT

let bindingshift d bind =
  match bind with
    NameBind -> NameBind
  | VarBind(tyT) -> VarBind(typeShift d tyT)
  | TmAbbBind(t,tyT_opt) ->
     let tyT_opt' = match tyT_opt with
                      None->None
                    | Some(tyT) -> Some(typeShift d tyT) in
     TmAbbBind(termShift d t, tyT_opt')
  | TyVarBind -> TyVarBind
  | TyAbbBind(tyT) -> TyAbbBind(typeShift d tyT)

(* ---------------------------------------------------------------------- *)
(* Substitution *)

let termSubst j s t =
  tmmap
    (fun fi j x n -> if x=j then termShift j s else TmVar(fi,x,n))
    (fun j tyT -> tyT)
    j t

let termSubstTop s t = 
  termShift (-1) (termSubst 0 (termShift 1 s) t)

let typeSubst tyS j tyT =
  tymap
    (fun j x n -> if x=j then (typeShift j tyS) else (TyVar(x,n)))
    j tyT

let typeSubstTop tyS tyT = 
  typeShift (-1) (typeSubst (typeShift 1 tyS) 0 tyT)

let rec tytermSubst tyS j t =
  tmmap (fun fi c x n -> TmVar(fi,x,n))
        (fun j tyT -> typeSubst tyS j tyT) j t

let tytermSubstTop tyS t = 
  termShift (-1) (tytermSubst (typeShift 1 tyS) 0 t)

(* ---------------------------------------------------------------------- *)
(* Context management (continued) *)

let rec getbinding fi ctx i =
  try
    let (_,bind) = List.nth ctx i in
    bindingshift (i+1) bind 
  with Failure _ ->
    let msg =
      Printf.sprintf "Variable lookup failure: offset: %d, ctx size: %d" in
    error fi (msg i (List.length ctx))
 let getTypeFromContext fi ctx i =
   match getbinding fi ctx i with
         VarBind(tyT) -> tyT
     | TmAbbBind(_,Some(tyT)) -> tyT
     | TmAbbBind(_,None) -> error fi ("No type recorded for variable "
                                        ^ (index2name fi ctx i))
     | _ -> error fi 
       ("getTypeFromContext: Wrong kind of binding for variable " 
         ^ (index2name fi ctx i)) 
(* ---------------------------------------------------------------------- *)
(* Extracting file info *)

let tmInfo t = match t with
    TmVar(fi,_,_) -> fi
  | TmAbs(fi,_,_,_,_) -> fi
  | TmApp(fi, _, _) -> fi
  | TmAscribe(fi,_,_) -> fi
  | TmString(fi,_) -> fi
  | TmUnit(fi) -> fi
  | TmLoc(fi,_) -> fi
  | TmRef(fi,_,_) -> fi
  | TmDeref(fi,_) -> fi
  | TmAssign(fi,_,_) -> fi
  | TmTag(fi,_,_,_) -> fi
  | TmCase(fi,_,_) -> fi
  | TmLet(fi,_,_,_) -> fi
  | TmFix(fi,_) -> fi
  | TmTrue(fi) -> fi
  | TmFalse(fi) -> fi
  | TmIf(fi,_,_,_) -> fi
  | TmFloat(fi,_) -> fi
  | TmTimesfloat(fi,_,_) -> fi
  | TmProj(fi,_,_) -> fi
  | TmRecord(fi,_) -> fi
  | TmZero(fi) -> fi
  | TmSucc(fi,_) -> fi
  | TmPred(fi,_) -> fi
  | TmIsZero(fi,_) -> fi
  | TmInert(fi,_) -> fi
  | TmThread(fi,_) -> fi
  (* | TmLock(fi,_) -> fi *)
  (* | TmFork(fi,_) -> fi *)
  | TmWait(fi,_) -> fi
  | TmTid(fi) -> fi
  (* | TmRefMutex(fi,_,_) -> fi *)
  (* | TmSync(fi,_,_) -> fi *)
  | TmMutex(fi,_) -> fi
  | TmAcquire(fi,_,_) -> fi

(* ---------------------------------------------------------------------- *)
(* Printing *)

(* The printing functions call these utility functions to insert grouping
  information and line-breaking hints for the pretty-printing library:
     obox   Open a "box" whose contents will be indented by two spaces if
            the whole box cannot fit on the current line
     obox0  Same but indent continuation lines to the same column as the
            beginning of the box rather than 2 more columns to the right
     cbox   Close the current box
     break  Insert a breakpoint indicating where the line maybe broken if
            necessary.
  See the documentation for the Format module in the OCaml library for
  more details. 
*)

let obox0() = open_hvbox 0
let obox() = open_hvbox 2
let cbox() = close_box()
let break() = print_break 0 0

let small t = 
  match t with
    TmVar(_,_,_) -> true
  | _ -> false

let rec printty_Type outer ctx tyT = match tyT with
    (* TyRef(tyT) -> pr "Ref "; printty_AType false ctx tyT *)
  (* | TySource(tyT) -> pr "Source "; printty_AType false ctx tyT *)
  (* | TySink(tyT) -> pr "Sink "; printty_AType false ctx tyT *)
  | TyMutex(lk) -> pr ("Mutex<"^lk^">")
  | TyRef(lk,tyT) -> pr "Ref"; if sizelockset lk > 0 then printlockset lk; printty_AType false ctx tyT
  | TySource(lk,tyT) -> pr ("Source"); if sizelockset lk > 0 then printlockset lk; printty_AType false ctx tyT
  | TySink(lk,tyT) -> pr "Sink";if sizelockset lk > 0 then printlockset lk; printty_AType false ctx tyT
  | TyThread(tyT) -> pr "Thread "; printty_AType false ctx tyT
  | tyT -> printty_ArrowType outer ctx tyT



and printty_ArrowType outer ctx  tyT = match tyT with 
    TyArr(lst,l1,tyT1,tyT2) ->
      obox0(); 
      if sizelockset lst > 0 then printlockset lst;
      (match l1 with
      Some(x) -> pr x
     | None -> ());
      printty_AType false ctx tyT1;
      if outer then pr " ";
      pr "->";
      if outer then print_space() else break();
      printty_ArrowType outer ctx tyT2;
      cbox()
  | tyT -> printty_AType outer ctx tyT

and printty_AType outer ctx tyT = match tyT with
    TyBot -> pr "Bot"
  | TyTop -> pr "Top"
  | TyString -> pr "String"
  | TyUnit -> pr "Unit"
  | TyVariant(fields) ->
        let pf i (li,tyTi) =
          if (li <> ((string_of_int i))) then (pr li; pr ":"); 
          printty_Type false ctx tyTi 
        in let rec p i l = match l with
            [] -> ()
          | [f] -> pf i f
          | f::rest ->
              pf i f; pr","; if outer then print_space() else break(); 
              p (i+1) rest
        in pr "<"; open_hovbox 0; p 1 fields; pr ">"; cbox()
  | TyBool -> pr "Bool"
  | TyId(b) -> pr b
  | TyRecord(fields) ->
        let pf i (li,tyTi) =
          if (li <> ((string_of_int i))) then (pr li; pr ":"); 
          printty_Type false ctx tyTi 
        in let rec p i l = match l with 
            [] -> ()
          | [f] -> pf i f
          | f::rest ->
              pf i f; pr","; if outer then print_space() else break(); 
              p (i+1) rest
        in pr "{"; open_hovbox 0; p 1 fields; pr "}"; cbox()
  | TyFloat -> pr "Float"
  | TyVar(x,n) ->
      if ctxlength ctx = n then
        pr (index2name dummyinfo ctx x)
      else
        pr ("[bad index: " ^ (string_of_int x) ^ "/" ^ (string_of_int n)
            ^ " in {"
            ^ (List.fold_left (fun s (x,_) -> s ^ " " ^ x) "" ctx)
            ^ " }]")
  | TyNat -> pr "Nat"
  (* | TyLock(a) -> pr ("Lock"); printlockset a *)
  | tyT -> pr "("; printty_Type outer ctx tyT; pr ")"

let printty ctx tyT = printty_Type true ctx tyT 

let rec printtm_Term outer ctx t = match t with
    TmAbs(fi,lst,x,tyT1,t2) ->
      (let (ctx',x') = (pickfreshname ctx x) in
         obox(); pr "lambda ";
         if sizelockset lst > 0 then printlockset lst;
         pr x'; pr ":"; printty_Type false ctx tyT1; pr ".";
         if (small t2) && not outer then break() else print_space();
         printtm_Term outer ctx' t2;
         cbox())
  | TmAssign(fi, t1, t2) ->
       obox();
       printtm_AppTerm false ctx t1;
       pr " := ";
       printtm_AppTerm false ctx t2;
       cbox()
  | TmCase(_, t, cases) ->
      obox();
      pr "case "; printtm_Term false ctx t; pr " of";
      print_space();
      let pc (li,(xi,ti)) = let (ctx',xi') = (pickfreshname ctx xi) in
                              pr "<"; pr li; pr "="; pr xi'; pr ">==>"; 
                              printtm_Term false ctx' ti 
      in let rec p l = match l with 
            [] -> ()
          | [c] -> pc c
          | c::rest -> pc c; print_space(); pr "| "; p rest
      in p cases;
      cbox()
  | TmLet(fi, x, t1, t2) ->
       obox0();
       pr "let "; pr x; pr " = "; 
       printtm_Term false ctx t1;
       print_space(); pr "in"; print_space();
       printtm_Term false (addname ctx x) t2;
       cbox()
  | TmFix(fi, t1) ->
       obox();
       pr "fix "; 
       printtm_Term false ctx t1;
       cbox()
  | TmIf(fi, t1, t2, t3) ->
       obox0();
       pr "if ";
       printtm_Term false ctx t1;
       print_space();
       pr "then ";
       printtm_Term false ctx t2;
       print_space();
       pr "else ";
       printtm_Term false ctx t3;
       cbox()
  (* add *)
  |TmAcquire(fi, l1, t2) ->
        obox();
        pr "acquire ";
        printtm_Term false ctx l1;
        print_space();
        pr "in ";
        printtm_Term false ctx t2;
        cbox()
  | t -> printtm_AppTerm outer ctx t

and printtm_AppTerm outer ctx t = match t with
    TmApp(fi, t1, t2) ->
      obox0();
      printtm_AppTerm false ctx t1;
      print_space();
      printtm_ATerm false ctx t2;
      cbox()
  | TmDeref(fi, t1) ->
       obox();
       pr "!";
       printtm_ATerm false ctx t1;
       cbox()
  | TmTimesfloat(_,t1,t2) ->
       pr "timesfloat "; printtm_ATerm false ctx t2; 
       pr " "; printtm_ATerm false ctx t2
  | TmPred(_,t1) ->
       pr "pred "; printtm_ATerm false ctx t1
  | TmIsZero(_,t1) ->
       pr "iszero "; printtm_ATerm false ctx t1
  (* | TmRefMutex(fi, t1, t2) ->
       obox();
       pr "ref ";
       (* printtm_ATerm false ctx t1; *)
       pr t1;
       print_space();
       printtm_AppTerm false ctx t2;
       cbox() *)
  | TmRef(fi, lst, t1) ->
        obox();
        pr "ref ";
        if sizelockset lst > 0 then printlockset lst;
        printtm_ATerm false ctx t1;
        cbox()
  | TmWait(fi, t1) ->
      obox();
      pr "wait ";
      printtm_PathTerm false ctx t1;
      cbox()
  | t -> printtm_PathTerm outer ctx t

and printtm_AscribeTerm outer ctx t = match t with
    TmAscribe(_,t1,tyT1) ->
      obox0();
      printtm_AppTerm false ctx t1;
      print_space(); pr "as ";
      printty_Type false ctx tyT1;
      cbox()
  | t -> printtm_ATerm outer ctx t

and printtm_PathTerm outer ctx t = match t with
    TmProj(_, t1, l) ->
      printtm_ATerm false ctx t1; pr "."; pr l
  | t -> printtm_AscribeTerm outer ctx t

and printtm_ATerm outer ctx t = match t with
    TmVar(fi,x,n) ->
      if ctxlength ctx = n then
        pr (index2name fi ctx x)
      else
        pr ("[bad index: " ^ (string_of_int x) ^ "/" ^ (string_of_int n)
            ^ " in {"
            ^ (List.fold_left (fun s (x,_) -> s ^ " " ^ x) "" ctx)
            ^ " }]")
  | TmString(_,s) -> pr ("\"" ^ s ^ "\"")
  | TmUnit(_) -> pr "unit"
  | TmLoc(fi, l) ->
       pr "<loc #"; print_int l;pr">"
  | TmTag(fi, l, t, tyT) ->
      obox();
      pr "<"; pr l; pr "="; printtm_Term false ctx t; pr ">";
      print_space();
      pr "as "; printty_Type outer ctx tyT;
      cbox();
  | TmTrue(_) -> pr "true"
  | TmFalse(_) -> pr "false"
  | TmFloat(_,s) -> pr (string_of_float s)
  | TmRecord(fi, fields) ->
       let pf i (li,ti) =
         if (li <> ((string_of_int i))) then (pr li; pr "="); 
         printtm_Term false ctx ti 
       in let rec p i l = match l with
           [] -> ()
         | [f] -> pf i f
         | f::rest ->
             pf i f; pr","; if outer then print_space() else break(); 
             p (i+1) rest
       in pr "{"; open_hovbox 0; p 1 fields; pr "}"; cbox()
  | TmZero(fi) ->
       pr "0"
  | TmSucc(_,t1) ->
     let rec f n t = match t with
         TmZero(_) -> pr (string_of_int n)
       | TmSucc(_,s) -> f (n+1) s
       | _ -> (pr "(succ "; printtm_ATerm false ctx t1; pr ")")
     in f 1 t1
  | TmInert(_,tyT) -> pr "inert["; printty_Type false ctx tyT; pr "]"
  (* add *)
  | TmThread(_,t) ->
      obox0();
      print_string ("thread<");printtm_Term false ctx t; print_string(">");
      cbox()
  | TmMutex(_,t) ->
      pr ("lock"); pr t;
  | t -> pr "("; printtm_Term outer ctx t; pr ")"

let printtm ctx t = printtm_Term true ctx t 

let prbinding ctx b = match b with
    NameBind -> ()
  | VarBind(tyT) -> pr ": "; printty ctx tyT
  | TmAbbBind(t,tyT) -> pr "= "; printtm ctx t
  | TyVarBind -> ()
  | TyAbbBind(tyT) -> pr "= "; printty ctx tyT 


