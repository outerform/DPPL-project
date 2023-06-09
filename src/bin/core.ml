open Format
open Syntax
open Support.Error
open Support.Pervasive

(* ------------------------   EVALUATION  ------------------------ *)

let rec isnumericval ctx t = match t with
    TmZero(_) -> true
  | TmSucc(_,t1) -> isnumericval ctx t1
  | _ -> false

let rec isval ctx t = match t with
    TmString _  -> true
  | TmUnit(_)  -> true
  | TmLoc(_,_) -> true
  | TmTag(_,_,t1,_) -> isval ctx t1
  | TmTrue(_)  -> true
  | TmFalse(_) -> true
  | TmFloat _  -> true
  | t when isnumericval ctx t  -> true
  | TmAbs(_,_,_,_,_) -> true
  | TmRecord(_,fields) -> List.for_all (fun (l,ti) -> isval ctx ti) fields
  | TmThreadLoc(_,_) -> true
  | TmMutex(_,_) -> true
  | _ -> false

type store = term list  
let emptystore = []
let extendstore store v = (List.length store, List.append store [v])
let lookuploc store l = List.nth store l
let updatestore store n v =
  let rec f s = match s with 
      (0, _::rest) -> v::rest
    | (n, v'::rest) -> v' :: (f (n-1,rest))
    | _ -> error dummyinfo "updatestore: bad index"
  in
    f (n,store)
let shiftstore i store = List.map (fun t -> termShift i t) store 

type threads = term list  
let emptythreads = []
let threadsnumber threads = List.length threads
let extendthreads threads t = (List.length threads, List.append threads [t])
let lookupthreads threads l = List.nth threads l
let updatethreads threads n t =
  let rec f s = match s with 
      (0, _::rest) -> t::rest
    | (n, t'::rest) -> t' :: (f (n-1,rest))
    | _ -> error dummyinfo "updatethreads: bad index"
  in
    f (n,threads)

type thctx = context list  
let emptythctx = []
let thctxnumber thctx = List.length thctx
let extendthctx thctx t = (List.length thctx, List.append thctx [t])
let lookupthctx thctx l = List.nth thctx l
let updatethctx thctx n t =
  let rec f s = match s with 
      (0, _::rest) -> t::rest
    | (n, t'::rest) -> t' :: (f (n-1,rest))
    | _ -> error dummyinfo "updatethctx: bad index"
  in
    f (n,thctx)

exception NoRuleApplies

let rec eval1 ctx store threads thctx t = match t with
    TmApp(fi,TmAbs(_,_,_,_,t12),v2) when isval ctx v2 ->
      termSubstTop v2 t12, store, threads, thctx
  | TmApp(fi,v1,t2) when isval ctx v1 ->
      let t2',store', threads', thctx' = eval1 ctx store threads thctx t2 in
      TmApp(fi, v1, t2'), store', threads', thctx'
  | TmApp(fi,t1,t2) ->
      let t1',store', threads', thctx' = eval1 ctx store threads thctx t1 in
      TmApp(fi, t1', t2), store', threads', thctx'
  | TmAscribe(_,v1,_) when isval ctx v1 ->
      v1, store, threads, thctx
  | TmAscribe(fi,t1,tyT) ->
      let t1',store', threads', thctx' = eval1 ctx store threads thctx t1 in
      TmAscribe(fi,t1',tyT), store', threads', thctx'
  | TmRef(fi,lst,t1) ->
      if not (isval ctx t1) then
        let (t1',store', threads', thctx') = eval1 ctx store threads thctx t1
        in (TmRef(fi,lst,t1'), store', threads', thctx')
      else
        let (l,store') = extendstore store t1 in
        (TmLoc(dummyinfo,l), store', threads, thctx)
  | TmDeref(fi,t1) ->
      if not (isval ctx t1) then
        let (t1',store', threads', thctx') = eval1 ctx store threads thctx t1
        in (TmDeref(fi,t1'), store', threads', thctx')
      else (match t1 with
            TmLoc(_,l) -> (lookuploc store l, store, threads, thctx)
          | _ -> raise NoRuleApplies)
  | TmAssign(fi,t1,t2) ->
      if not (isval ctx t1) then
        let (t1',store', threads', thctx') = eval1 ctx store threads thctx t1
        in (TmAssign(fi,t1',t2), store', threads', thctx')
      else if not (isval ctx t2) then
        let (t2',store', threads', thctx') = eval1 ctx store threads thctx t2
        in (TmAssign(fi,t1,t2'), store', threads', thctx')
      else (match t1 with
            TmLoc(_,l) -> (TmUnit(dummyinfo), updatestore store l t2, threads, thctx)
          | _ -> raise NoRuleApplies)
  | TmTag(fi,l,t1,tyT) ->
      let t1',store', threads', thctx' = eval1 ctx store threads thctx t1 in
      TmTag(fi, l, t1',tyT), store', threads', thctx'
  | TmCase(fi,TmTag(_,li,v11,_),branches) when isval ctx v11->
      (try 
         let (x,body) = List.assoc li branches in
         termSubstTop v11 body, store, threads, thctx
       with Not_found -> raise NoRuleApplies)
  | TmCase(fi,t1,branches) ->
      let t1',store', threads', thctx' = eval1 ctx store threads thctx t1 in
      TmCase(fi, t1', branches), store', threads', thctx'
  | TmLet(fi,x,v1,t2) when isval ctx v1 ->
      termSubstTop v1 t2, store, threads, thctx
  | TmLet(fi,x,t1,t2) ->
      let t1',store', threads', thctx' = eval1 ctx store threads thctx t1 in
      TmLet(fi, x, t1', t2), store', threads', thctx'
  | TmFix(fi,v1) as t when isval ctx v1 ->
      (match v1 with
         TmAbs(_,_,_,_,t12) -> termSubstTop t t12, store, threads, thctx
       | _ -> raise NoRuleApplies)
  | TmFix(fi,t1) ->
      let t1',store', threads', thctx' = eval1 ctx store threads thctx t1
      in TmFix(fi,t1'), store', threads', thctx'
  | TmIf(_,TmTrue(_),t2,t3) ->
      t2, store, threads, thctx
  | TmIf(_,TmFalse(_),t2,t3) ->
      t3, store, threads, thctx
  | TmIf(fi,t1,t2,t3) ->
      let t1',store',threads',thctx' = eval1 ctx store threads thctx t1 in
      TmIf(fi, t1', t2, t3), store', threads', thctx'
  | TmTimesfloat(fi,TmFloat(_,f1),TmFloat(_,f2)) ->
      TmFloat(fi, f1 *. f2), store, threads, thctx
  | TmTimesfloat(fi,(TmFloat(_,f1) as t1),t2) ->
      let t2',store', threads', thctx' = eval1 ctx store threads thctx t2 in
      TmTimesfloat(fi,t1,t2') , store', threads', thctx'
  | TmTimesfloat(fi,t1,t2) ->
      let t1',store', threads', thctx' = eval1 ctx store threads thctx t1 in
      TmTimesfloat(fi,t1',t2) , store', threads', thctx'
  | TmVar(fi,n,_) ->
      (match getbinding fi ctx n with
          TmAbbBind(t,_) -> t,store,threads,thctx
        | _ -> raise NoRuleApplies)
  | TmRecord(fi,fields) ->
      let rec evalafield l = match l with 
        [] -> raise NoRuleApplies
      | (l,vi)::rest when isval ctx vi -> 
          let rest',store',threads',thctx' = evalafield rest in
          (l,vi)::rest', store', threads', thctx'
      | (l,ti)::rest -> 
          let ti',store',threads',thctx' = eval1 ctx store threads thctx ti in
          (l, ti')::rest, store', threads', thctx'
      in let fields',store',threads',thctx' = evalafield fields in
      TmRecord(fi, fields'), store', threads', thctx'
  | TmProj(fi, TmRecord(_, fields), l) ->
      (try List.assoc l fields, store, threads, thctx
       with Not_found -> raise NoRuleApplies)
  | TmProj(fi, t1, l) ->
      let t1',store',threads',thctx' = eval1 ctx store threads thctx t1 in
      TmProj(fi, t1', l), store', threads, thctx
  | TmSucc(fi,t1) ->
      let t1',store',threads',thctx' = eval1 ctx store threads thctx t1 in
      TmSucc(fi, t1'), store', threads', thctx'
  | TmPred(_,TmZero(_)) ->
      TmZero(dummyinfo), store, threads, thctx
  | TmPred(_,TmSucc(_,nv1)) when (isnumericval ctx nv1) ->
      nv1, store, threads, thctx
  | TmPred(fi,t1) ->
      let t1',store',threads',thctx' = eval1 ctx store threads thctx t1 in
      TmPred(fi, t1'), store', threads', thctx'
  | TmIsZero(_,TmZero(_)) ->
      TmTrue(dummyinfo), store, threads, thctx
  | TmIsZero(_,TmSucc(_,nv1)) when (isnumericval ctx nv1) ->
      TmFalse(dummyinfo), store, threads, thctx
  | TmIsZero(fi,t1) ->
      let t1',store', threads', thctx' = eval1 ctx store threads thctx t1 in
      TmIsZero(fi, t1'), store', threads', thctx'
  | TmThread(fi,t1) ->
      let (l,threads') = extendthreads threads t1 in
      let (_,thctx') = extendthctx thctx ctx in
      TmThreadLoc(dummyinfo, l), store, threads', thctx'
  | TmWait(fi,t1) ->
      if not (isval ctx t1) then
        let (t1',store', threads', thctx') = eval1 ctx store threads thctx t1
        in (TmWait(fi,t1'), store', threads', thctx')
      else (match t1 with
        TmThreadLoc(_,l) -> (
          let t2 = lookupthreads threads l in
          let ctx2 = lookupthctx thctx l in
            if isval ctx2 t2 then
              t2, store, threads, thctx
            else
              let t2' ,store', threads', thctx' = eval1 ctx2 store threads thctx t2
              in TmWait(fi,t1), store', updatethreads threads' l t2', thctx')
        | _ -> raise NoRuleApplies)
  | TmAcquire(_,TmMutex(_,_),v2) when isval ctx v2 ->
      v2, store, threads, thctx
  | TmAcquire(fi,v1,t2) when isval ctx v1 ->
      let t2',store',threads',thctx' = eval1 ctx store threads thctx t2 in
      TmAcquire(fi, v1, t2'), store', threads', thctx'
  | TmAcquire(fi,t1,t2) ->
      let t1',store',threads',thctx' = eval1 ctx store threads thctx t1 in
      TmAcquire(fi, t1', t2), store', threads', thctx'
  | _ -> 
      raise NoRuleApplies

(* random permutation list from 0 to n-1 *)
let random_permutation n =
  let rec aux acc remaining =
    match remaining with
    | [] -> acc
    | _ ->
      let rand_index = Random.int (List.length remaining) in
      let selected_element = List.nth remaining rand_index in
      let new_remaining = List.filter (fun x -> x <> selected_element) remaining in
      aux (selected_element :: acc) new_remaining
  in
  let initial_list = List.init n (fun x -> x) in
  aux [] initial_list

let rec evalp store threads thctx p =
  let random_list = random_permutation (threadsnumber threads) in
    let rec f lst = (
      let l = List.hd lst in
      let ctx = lookupthctx thctx l in
      let t = lookupthreads threads l in
      if (not (isval ctx t)) then
        try let t', store', threads', thctx' = eval1 ctx store threads thctx t
        in false, t', store', (updatethreads threads' l t'), thctx'
        with NoRuleApplies -> f (List.tl lst)
      else if l = p then
        try let t', store', threads', thctx' = eval1 ctx store threads thctx t
        in false, t', store', (updatethreads threads' l t'), thctx'
        with NoRuleApplies -> true, t, store, threads, thctx
      else 
        f (List.tl lst))
  in f random_list
let shiftstore i store = List.map (fun t -> termShift i t) store 

let rec evalrec store threads thctx p =
  let fl, t, store', threads', thctx' = evalp store threads thctx p in
  if fl then t, store, threads, thctx
  else evalrec store' threads' thctx' p

let eval ctx store threads thctx t =
  let p, threads' = extendthreads threads t in
  let _, thctx' = extendthctx thctx ctx in
  evalrec store threads' thctx' p

(* ------------------------   SUBTYPING  ------------------------ *)

let evalbinding ctx store threads thctx b = match b with
    TmAbbBind(t,tyT) ->
      let t',store',threads',thctx' = eval ctx store threads thctx t
      in TmAbbBind(t',tyT), store',threads',thctx'
  | bind -> bind,store,threads,thctx

let istyabb ctx i = 
  match getbinding dummyinfo ctx i with
    TyAbbBind(tyT) -> true
  | _ -> false

let gettyabb ctx i = 
  match getbinding dummyinfo ctx i with
    TyAbbBind(tyT) -> tyT
  | _ -> raise NoRuleApplies

let rec computety ctx tyT = match tyT with
    TyVar(i,_) when istyabb ctx i -> gettyabb ctx i
  | _ -> raise NoRuleApplies

let rec simplifyty ctx tyT =
  try
    let tyT' = computety ctx tyT in
    simplifyty ctx tyT' 
  with NoRuleApplies -> tyT

let rec tyeqv ctx tyS tyT =
  let tyS = simplifyty ctx tyS in
  let tyT = simplifyty ctx tyT in
  match (tyS,tyT) with
    (TyVariant(fields1),TyVariant(fields2)) ->
       (List.length fields1 = List.length fields2)
       && List.for_all2
            (fun (li1,tyTi1) (li2,tyTi2) ->
               (li1=li2) && tyeqv ctx tyTi1 tyTi2)
            fields1 fields2
  | (TyBot,TyBot) -> true
  | (TyId(b1),TyId(b2)) -> b1=b2
  | (TyString,TyString) -> true
  | (TyFloat,TyFloat) -> true
  | (TyArr(l1,al1,tyS1,tyS2),TyArr(l2,al2,tyT1,tyT2)) ->
       equallockset l1 l2 && al1 = al2 && (tyeqv ctx tyS1 tyT1) && (tyeqv ctx tyS2 tyT2)
  | (TyUnit,TyUnit) -> true
  (* | (TyRef(tyT1),TyRef(tyT2)) -> tyeqv ctx tyT1 tyT2
  | (TySource(tyT1),TySource(tyT2)) -> tyeqv ctx tyT1 tyT2
  | (TySink(tyT1),TySink(tyT2)) -> tyeqv ctx tyT1 tyT2 *)
  | (TyTop,TyTop) -> true
  | (TyBool,TyBool) -> true
  | (TyNat,TyNat) -> true
  | (TyRecord(fields1),TyRecord(fields2)) -> 
       List.length fields1 = List.length fields2
       &&                                         
       List.for_all 
         (fun (li2,tyTi2) ->
            try let (tyTi1) = List.assoc li2 fields1 in
                tyeqv ctx tyTi1 tyTi2
            with Not_found -> false)
         fields2
  | (TyVar(i,_), _) when istyabb ctx i ->
      tyeqv ctx (gettyabb ctx i) tyT
  | (_, TyVar(i,_)) when istyabb ctx i ->
      tyeqv ctx tyS (gettyabb ctx i)
  | (TyVar(i,_),TyVar(j,_)) -> i=j
(* new *)
  | (TyMutex(li1), TyMutex(li2)) -> li1 = li2
  | (TyRef(li1,tyT1), TyRef(li2,tyT2)) -> equallockset li1 li2 && tyeqv ctx tyT1 tyT2
  | (TyThread(tyT1,fpid1), TyThread(tyT2,fpid2)) -> tyeqv ctx tyT1 tyT2 && fpid1 = fpid2
  | (TySource(li1,tyT1), TySource(li2,tyT2)) -> equallockset li1 li2 && tyeqv ctx tyT1 tyT2
  | (TySink(li1,tyT1), TySink(li2,tyT2)) -> equallockset li1 li2 && tyeqv ctx tyT1 tyT2
  | _ -> false

let rec subtype ctx tyS tyT =
   tyeqv ctx tyS tyT ||
   let tyS = simplifyty ctx tyS in
   let tyT = simplifyty ctx tyT in
   match (tyS,tyT) with
     (TyBot,_) -> 
       true
   | (_,TyTop) -> 
       true
   | (TyArr(l1,al1,tyS1,tyS2),TyArr(l2,al2,tyT1,tyT2)) ->
       sublockset l1 l2 && 
       ((al1 = None) || (al2 != None && al1 >= al2))
       && (subtype ctx tyT1 tyS1) && (subtype ctx tyS2 tyT2)
   (* | (TyRef(tyT1),TyRef(tyT2)) ->
       subtype ctx tyT1 tyT2 && subtype ctx tyT2 tyT1
   | (TyRef(tyT1),TySource(tyT2)) ->
       subtype ctx tyT1 tyT2
   | (TySource(tyT1),TySource(tyT2)) ->
       subtype ctx tyT1 tyT2
   | (TyRef(tyT1),TySink(tyT2)) ->
       subtype ctx tyT2 tyT1
   | (TySink(tyT1),TySink(tyT2)) ->
       subtype ctx tyT2 tyT1 *)
   | (TyVariant(fS), TyVariant(fT)) ->
       List.for_all
         (fun (li,tySi) -> 
            try let tyTi = List.assoc li fT in
                subtype ctx tySi tyTi
            with Not_found -> false)
         fS
   | (TyRecord(fS), TyRecord(fT)) ->
       List.for_all
         (fun (li,tyTi) -> 
            try let tySi = List.assoc li fS in
                subtype ctx tySi tyTi
            with Not_found -> false)
         fT
    (* add *)
   (* | (TyRefMutex(l1,tyT1), ty2) ->
        (match ty2 with
        | TyRefMutex(l2,tyT2) -> l1 = l2 && subtype ctx tyT1 tyT2 && subtype ctx tyT2 tyT1
        | TyRef(tyT2) -> subtype ctx tyT1 tyT2 && subtype ctx tyT2 tyT1
        | TySourceMutex(l2,tyT2) -> l1 = l2 && subtype ctx tyT1 tyT2
        | TySource(tyT2) -> subtype ctx tyT1 tyT2
        | TySinkMutex(l2,tyT2) -> l1 = l2 && subtype ctx tyT2 tyT1
        | TySink(tyT2) -> subtype ctx tyT2 tyT1
        | _ -> false)
  | (ty1, TySourceMutex(l2,tyT2)) ->
        (match ty1 with
        | TySourceMutex(l1,tyT1) -> l1 = l2 && subtype ctx tyT1 tyT2
        | TyRef(tyT1) -> subtype ctx tyT1 tyT2
        | TySource(tyT1) -> subtype ctx tyT1 tyT2
        | _ -> false)
  | (TySinkMutex(l1,tyT1), ty2) ->
        (match ty2 with
        | TySinkMutex(l2,tyT2) -> l1 = l2 && subtype ctx tyT2 tyT1
        | TyRef(tyT2) -> subtype ctx tyT2 tyT1
        | TySink(tyT2) -> subtype ctx tyT2 tyT1
        | _ -> false) *)
  | (TyRef(l1,tyT1),TyRef(l2,tyT2)) ->
      sublockset l1 l2 && subtype ctx tyT1 tyT2 && subtype ctx tyT2 tyT1
  | (TyRef(l1,tyT1),TySource(l2,tyT2)) ->
      sublockset l1 l2 && subtype ctx tyT1 tyT2
  | (TySource(l1,tyT1),TySource(l2,tyT2)) ->
      sublockset l1 l2 && subtype ctx tyT1 tyT2
  | (TyRef(l1,tyT1),TySink(l2,tyT2)) ->
      sublockset l1 l2 && subtype ctx tyT2 tyT1
  | (TySink(l1,tyT1),TySink(l2, tyT2)) ->
      sublockset l1 l2 && subtype ctx tyT2 tyT1
  | (TyThread(tyT1,fpid1), TyThread(tyT2,fpid2)) ->
      fpid1 = fpid2 && subtype ctx tyT1 tyT2
  | (_,_) -> 
       false
let minalock (ml1:string option) (ml2:string option) = 
  match (ml1,ml2) with
  | (None,_) -> ml2
  | (_,None) -> ml1
  | _ -> min ml1 ml2

let rec join ctx tyS tyT =
  if subtype ctx tyS tyT then tyT else 
  if subtype ctx tyT tyS then tyS else
  let tyS = simplifyty ctx tyS in
  let tyT = simplifyty ctx tyT in
  match (tyS,tyT) with
    (TyArr(l1,al1,tyS1,tyS2),TyArr(l2,al2,tyT1,tyT2)) ->
      TyArr(unionlockset l1 l2, minalock al1 al2, meet ctx tyS1 tyT1, join ctx tyS2 tyT2)
  (* between Ref, sink and source *)
  (* | (TyRef(tyT1),TyRef(tyT2)) ->
      if subtype ctx tyT1 tyT2 && subtype ctx tyT2 tyT1 
        then TyRef(tyT1)
        else (* Warning: this is incomplete... *)
             TySource(join ctx tyT1 tyT2)
  | (TySource(tyT1),TySource(tyT2)) ->
      TySource(join ctx tyT1 tyT2)
  | (TyRef(tyT1),TySource(tyT2)) ->
      TySource(join ctx tyT1 tyT2)
  | (TySource(tyT1),TyRef(tyT2)) ->
      TySource(join ctx tyT1 tyT2)
  | (TySink(tyT1),TySink(tyT2)) ->
      TySink(meet ctx tyT1 tyT2)
  | (TyRef(tyT1),TySink(tyT2)) ->
      TySink(meet ctx tyT1 tyT2)
  | (TySink(tyT1),TyRef(tyT2)) ->
      TySink(meet ctx tyT1 tyT2) *)
  (*  *)
  | (TyRecord(fS), TyRecord(fT)) ->
      let labelsS = List.map (fun (li,_) -> li) fS in
      let labelsT = List.map (fun (li,_) -> li) fT in
      let commonLabels = 
        List.find_all (fun l -> List.mem l labelsT) labelsS in
      let commonFields = 
        List.map (fun li -> 
                    let tySi = List.assoc li fS in
                    let tyTi = List.assoc li fT in
                    (li, join ctx tySi tyTi))
                 commonLabels in
      TyRecord(commonFields)
  (* New *)
  (* RefMutex*)
  (* | (TyRefMutex(li1,tyT1),ty2) ->
      (match ty2 with
      | TyRefMutex(li2,tyT2) when li1 = li2 -> 
          if subtype ctx tyT1 tyT2 && subtype ctx tyT2 tyT1 then TyRefMutex(li1,tyT1) 
          else (* Warning: this is incomplete... *)
             TySource(join ctx tyT1 tyT2)
      | TySourceMutex(li2,tyT2) when li1 = li2 -> 
          TySourceMutex(li1,join ctx tyT1 tyT2)
      | TySinkMutex(li2,tyT2) when li1 = li2 -> 
          TySinkMutex(li1,meet ctx tyT1 tyT2)
      | TyRef(tyT2)  ->
          if subtype ctx tyT1 tyT2 && subtype ctx tyT2 tyT1 then TyRefMutex(li1,tyT1) 
          else (* Warning: this is incomplete... *)
             TySourceMutex(li1,join ctx tyT1 tyT2)
      | TySource(tyT2)->
          TySourceMutex(li1,join ctx tyT1 tyT2)
      | TySink(tyT2)->
          TySinkMutex(li1,meet ctx tyT1 tyT2)
      | _ -> TyTop)
  | (ty1,TyRefMutex(li2,tyT2)) ->
      (match ty1 with
      | TyRefMutex(li1,tyT1) when li1 = li2 -> 
          if subtype ctx tyT1 tyT2 && subtype ctx tyT2 tyT1 then TyRefMutex(li1,tyT1) 
          else (* Warning: this is incomplete... *)
             TySource(join ctx tyT1 tyT2)
      | TySourceMutex(li1,tyT1) when li1 = li2 ->
          TySourceMutex(li1,join ctx tyT1 tyT2)
      | TySinkMutex(li1,tyT1) when li1 = li2 ->
          TySinkMutex(li1,meet ctx tyT1 tyT2)
      | TyRef(tyT1)  -> 
          if subtype ctx tyT1 tyT2 && subtype ctx tyT2 tyT1 then TyRefMutex(li2,tyT2) 
          else (* Warning: this is incomplete... *)
             TySourceMutex(li2,join ctx tyT1 tyT2)
      | TySource(tyT1)->
          TySourceMutex(li2,join ctx tyT1 tyT2)
      | TySink(tyT1)->
          TySinkMutex(li2,meet ctx tyT1 tyT2)
      | _ -> TyTop)
  (* SourceMutex *)
  | (TySourceMutex(li1,tyT1),ty2) -> 
      (match ty2 with
      (* | TyRefMutex(li2,tyT2) when li1 = li2 -> 
          TySourceMutex(li1,join ctx tyT1 tyT2) *)
      | TySourceMutex(li2,tyT2) when li1 = li2 -> 
          TySourceMutex(li1,join ctx tyT1 tyT2)
      | TyRef(tyT2)  -> 
          TySourceMutex(li1,join ctx tyT1 tyT2)
      | TySource(tyT2)->
          TySourceMutex(li1,join ctx tyT1 tyT2)
      | _ -> TyTop)
  | (ty1,TySourceMutex(li2,tyT2)) -> 
      (match ty1 with
      (* | TyRefMutex(li1,tyT1) when li1 = li2 -> 
          TySourceMutex(li1,join ctx tyT1 tyT2) *)
      | TySourceMutex(li1,tyT1) when li1 = li2 -> 
          TySourceMutex(li1,join ctx tyT1 tyT2)
      | TyRef(tyT1)  -> 
          TySourceMutex(li2,join ctx tyT1 tyT2)
      | TySource(tyT1)->
          TySourceMutex(li2,join ctx tyT1 tyT2)
      | _ -> TyTop)
  (* SinkMutex *)
  | (TySinkMutex(li1,tyT1),ty2) ->
      (match ty2 with
      (* | TyRefMutex(li2,tyT2) when li1 = li2 -> 
          TySinkMutex(li1,meet ctx tyT1 tyT2) *)
      | TySinkMutex(li2,tyT2) when li1 = li2 -> 
          TySinkMutex(li1,meet ctx tyT1 tyT2)
      | TyRef(tyT2)  -> 
          TySinkMutex(li1,meet ctx tyT1 tyT2)
      | TySink(tyT2)->
          TySinkMutex(li1,meet ctx tyT1 tyT2)
      | _ -> TyTop)
  | (ty1,TySinkMutex(li2,tyT2)) ->
      (match ty1 with
      (* | TyRefMutex(li1,tyT1) when li1 = li2 -> 
          TySinkMutex(li1,meet ctx tyT1 tyT2) *)
      | TySinkMutex(li1,tyT1) when li1 = li2 -> 
          TySinkMutex(li1,meet ctx tyT1 tyT2)
      | TyRef(tyT1)  -> 
          TySinkMutex(li2,meet ctx tyT1 tyT2)
      | TySink(tyT1)->
          TySinkMutex(li2,meet ctx tyT1 tyT2)
      | _ -> TyTop)
  End of RefMutex *)
| (TyRef(l1,tyT1),TyRef(l2,tyT2)) ->
    if subtype ctx tyT1 tyT2 && subtype ctx tyT2 tyT1 
      then TyRef(unionlockset l1 l2,tyT1)
      else (* Warning: this is incomplete... *)
           TySource(unionlockset l1 l2,join ctx tyT1 tyT2)
| (TySource(l1,tyT1),TySource(l2,tyT2)) ->
    TySource(unionlockset l1 l2,join ctx tyT1 tyT2)
| (TyRef(l1,tyT1),TySource(l2,tyT2)) ->
    TySource(unionlockset l1 l2,join ctx tyT1 tyT2)
| (TySource(l1,tyT1),TyRef(l2,tyT2)) ->
    TySource(unionlockset l1 l2,join ctx tyT1 tyT2)
| (TySink(l1,tyT1),TySink(l2,tyT2)) ->
    TySink(unionlockset l1 l2,meet ctx tyT1 tyT2)
| (TyRef(l1,tyT1),TySink(l2,tyT2)) ->
    TySink(unionlockset l1 l2,meet ctx tyT1 tyT2)
| (TySink(l1,tyT1),TyRef(l2,tyT2)) ->
    TySink(unionlockset l1 l2,meet ctx tyT1 tyT2)
| (TyThread(tyT1,fpid1),TyThread(tyT2,fpid2)) ->
    if fpid1 = fpid2 then TyThread(join ctx tyT1 tyT2,fpid1) else TyTop
  | _ -> 
      TyTop

and meet ctx tyS tyT =
  if subtype ctx tyS tyT then tyS else 
  if subtype ctx tyT tyS then tyT else 
  let tyS = simplifyty ctx tyS in
  let tyT = simplifyty ctx tyT in
  match (tyS,tyT) with
    (TyArr(l1,al1,tyS1,tyS2),TyArr(l2,al2,tyT1,tyT2)) ->
      TyArr(unionlockset l1 l2 , (if al1 = None || al2 = None then None else max al1 al2),join ctx tyS1 tyT1, meet ctx tyS2 tyT2)
  (* | (TyRef(tyT1),TyRef(tyT2)) ->
      if subtype ctx tyT1 tyT2 && subtype ctx tyT2 tyT1 
        then TyRef(tyT1)
        else (* Warning: this is incomplete... *)
             (* TySource(meet ctx tyT1 tyT2) *)
            TyBot
  | (TySource(tyT1),TySource(tyT2)) ->
      TySource(meet ctx tyT1 tyT2)
  (* | (TyRef(tyT1),TySource(tyT2)) ->
      TySource(meet ctx tyT1 tyT2)
  | (TySource(tyT1),TyRef(tyT2)) ->
      TySource(meet ctx tyT1 tyT2) *)
  | (TyRef(tyT1),TySource(tyT2)) ->
    if subtype ctx tyT1 tyT2 then TyRef(tyT1)
    else TyBot
  | (TySource(tyT1),TyRef(tyT2)) ->
    if subtype ctx tyT2 tyT1 then TyRef(tyT2)
    else TyBot
  | (TySink(tyT1),TySink(tyT2)) ->
      TySink(join ctx tyT1 tyT2)
  (* | (TyRef(tyT1),TySink(tyT2)) ->
      TySink(join ctx tyT1 tyT2)
  | (TySink(tyT1),TyRef(tyT2)) ->
      TySink(join ctx tyT1 tyT2) *)
  | (TyRef(tyT1),TySink(tyT2)) ->
    if subtype ctx tyT2 tyT1 then TyRef(tyT1)
    else TyBot
  | (TySink(tyT1),TyRef(tyT2)) ->
    if subtype ctx tyT1 tyT2 then TyRef(tyT2)
    else TyBot *)
  | (TyRecord(fS), TyRecord(fT)) ->
      let labelsS = List.map (fun (li,_) -> li) fS in
      let labelsT = List.map (fun (li,_) -> li) fT in
      let allLabels = 
        List.append
          labelsS 
          (List.find_all 
            (fun l -> not (List.mem l labelsS)) labelsT) in
      let allFields = 
        List.map (fun li -> 
                    if List.mem li allLabels then
                      let tySi = List.assoc li fS in
                      let tyTi = List.assoc li fT in
                      (li, meet ctx tySi tyTi)
                    else if List.mem li labelsS then
                      (li, List.assoc li fS)
                    else
                      (li, List.assoc li fT))
                 allLabels in
      TyRecord(allFields)
  (* new *)
  (* RefMutex *)
  (* | (TyRefMutex(li1,tyT1),ty2) ->
      (match ty2 with
      | TyRefMutex(li2,tyT2) when li1 = li2 -> 
        if subtype ctx tyT1 tyT2 then TyRefMutex(li1,tyT1)
        else TyBot
      | TySourceMutex(li2,tyT2) when li1 = li2 -> 
        if subtype ctx tyT1 tyT2 then TyRefMutex(li1,tyT1)
        else TyBot
      | TySinkMutex(li2,tyT2) when li1 = li2 -> 
        if subtype ctx tyT2 tyT1 then TyRefMutex(li1,tyT1)
        else TyBot
      | TyRef(tyT2) -> 
        if subtype ctx tyT1 tyT2 && subtype ctx tyT2 tyT1 
          then TyRef(tyT1)
          else TyBot
      | TySource(tyT2) -> 
        if subtype ctx tyT1 tyT2 then TyRef(tyT1)
        else TyBot
      | TySink(tyT2) -> 
        if subtype ctx tyT2 tyT1 then TyRef(tyT1)
        else TyBot
      | _ -> TyTop)
  | (ty1,TyRefMutex(li2,tyT2))->
    (match ty1 with
    | TySourceMutex(li1,tyT1) when li1 = li2 -> 
      if subtype ctx tyT2 tyT1 then TyRefMutex(li2,tyT2)
      else TyBot
    | TySinkMutex(li1,tyT1) when li1 = li2 ->
      if subtype ctx tyT1 tyT2 then TyRefMutex(li2,tyT2)
      else TyBot
    | TyRef(tyT1) ->
      if subtype ctx tyT1 tyT2 && subtype ctx tyT2 tyT1 
        then TyRef(tyT2)
        else TyBot
    | TySource(tyT1) ->
      if subtype ctx tyT2 tyT1 then TyRef(tyT2)
      else TyBot
    | TySink(tyT1) ->
      if subtype ctx tyT1 tyT2 then TyRef(tyT2)
      else TyBot
    | _ -> TyTop)
  (* SourceMutex *)
  | (TySourceMutex(li1,tyT1),ty2) ->
      (match ty2 with
      | TySourceMutex(li2,tyT2) when li1 = li2 -> 
        TySourceMutex(li1,meet ctx tyT1 tyT2)
      | TyRef(tyT2) -> 
        if subtype ctx tyT2 tyT1 then TyRef(tyT2)
        else TyBot
      | TySource(tyT2) -> 
        TySource(meet ctx tyT1 tyT2)
      | _ -> TyTop)
  | (ty1,TySourceMutex(li2,tyT2))->
    (match ty1 with
    | TySourceMutex(li1,tyT1) when li1 = li2 -> 
      TySourceMutex(li2,meet ctx tyT1 tyT2)
    | TyRef(tyT1) ->
      if subtype ctx tyT1 tyT2 then TyRef(tyT1)
      else TyBot
    | TySource(tyT1) ->
      TySource(meet ctx tyT1 tyT2)
    | _ -> TyTop)
  (* SinkMutex *)
  | (TySinkMutex(li1,tyT1),ty2) ->
      (match ty2 with
      | TySinkMutex(li2,tyT2) when li1 = li2 -> 
        TySinkMutex(li1,join ctx tyT1 tyT2)
      | TyRef(tyT2) -> 
        if subtype ctx tyT1 tyT2 then TyRef(tyT2)
        else TyBot
      | TySink(tyT2) -> 
        TySink(join ctx tyT1 tyT2)
      | _ -> TyTop)
  | (ty1,TySinkMutex(li2,tyT2))->
    (match ty1 with
    | TySinkMutex(li1,tyT1) when li1 = li2 -> 
      TySinkMutex(li2,join ctx tyT1 tyT2)
    | TyRef(tyT1) ->
      if subtype ctx tyT1 tyT2 then TyRef(tyT1)
      else TyBot
    | TySink(tyT1) ->
      TySink(join ctx tyT1 tyT2)
    | _ -> TyTop) *)
  | (TyRef(l1,tyT1),TyRef(l2,tyT2)) ->
      if subtype ctx tyT1 tyT2 && subtype ctx tyT2 tyT1 
        then TyRef(interlockset l1 l2,tyT1)
        else (* Warning: this is incomplete... *)
             (* TySource(meet ctx tyT1 tyT2) *)
            TyBot
  | (TySource(l1,tyT1),TySource(l2,tyT2)) ->
      TySource(interlockset l1 l2,meet ctx tyT1 tyT2)
  (* | (TyRef(tyT1),TySource(tyT2)) ->
      TySource(meet ctx tyT1 tyT2)
  | (TySource(tyT1),TyRef(tyT2)) ->
      TySource(meet ctx tyT1 tyT2) *)
  | (TyRef(l1,tyT1),TySource(l2,tyT2)) ->
    if subtype ctx tyT1 tyT2 then TyRef(interlockset l1 l2,tyT1)
    else TyBot
  | (TySource(l1,tyT1),TyRef(l2,tyT2)) ->
    if subtype ctx tyT2 tyT1 then TyRef(interlockset l1 l2,tyT2)
    else TyBot
  | (TySink(l1,tyT1),TySink(l2,tyT2)) ->
      TySink(interlockset l1 l2,join ctx tyT1 tyT2)
  (* | (TyRef(tyT1),TySink(tyT2)) ->
      TySink(join ctx tyT1 tyT2)
  | (TySink(tyT1),TyRef(tyT2)) ->
      TySink(join ctx tyT1 tyT2) *)
  | (TyRef(l1,tyT1),TySink(l2,tyT2)) ->
    if subtype ctx tyT2 tyT1 then TyRef(interlockset l1 l2,tyT1)
    else TyBot
  | (TySink(l1,tyT1),TyRef(l2,tyT2)) ->
    if subtype ctx tyT1 tyT2 then TyRef(interlockset l1 l2,tyT2)
    else TyBot
  | (TySource(l1,tyT1),TySink(l2,tyT2)) ->
    if subtype ctx tyT2 tyT1 then 
      (* Warning: this is incomplete... *)
      TyRef(interlockset l1 l2,tyT1)
    else TyBot
  | (TySink(l1,tyT1),TySource(l2,tyT2)) ->
    if subtype ctx tyT1 tyT2 then 
      (* Warning: this is incomplete... *)
      TyRef(interlockset l1 l2,tyT2)
    else TyBot
  | (TyThread(tyT1,fpid1),TyThread(tyT2,fpid2)) ->
    if fpid1 = fpid2
      then TyThread(meet ctx tyT1 tyT2,fpid1)
      else TyBot
  | _ -> 
      TyBot

(* ------------------------   TYPING  ------------------------ *)

  (* printtm ctx t; print_string "\n" ; *)
let rec typeof1 (ctx:context) (lst:lockset) (t:term) (pid: threadid) :ty * string option =
  match t with
    TmVar(fi,i,_) -> getTypeFromContext fi ctx i,None
  | TmAbs(_,l1,x,tyT1,t2) ->
      let ctx' = addbinding ctx x (VarBind(tyT1)) in
      (* let lst' = unionlockset l1 lst in *)
      let tyT2,lk = typeof1 ctx' l1 t2 pid in
      TyArr(l1,lk,tyT1, typeShift (-1) tyT2),None
  | TmApp(fi,t1,t2) ->
      let tyT1,al1 = typeof1 ctx lst t1 pid in
      let tyT2,al2 = typeof1 ctx lst t2 pid in
      (* printty ctx tyT1;
      pr "\n";
      printty ctx tyT2;
      pr"\n"; *)
      (match simplifyty ctx tyT1 with
          TyArr(l11,al11,tyT11,tyT12) ->
            if not (sublockset l11 lst) then error fi "lock isn't held" else
            if subtype ctx tyT2 tyT11 then 
              match al11 with
              | None -> tyT12,None
              | Some(lk) -> if sizelockset lst > 0 && maxlock lst >= lk then error fi "risking acquire lock out of order" else tyT12,minalock (minalock al1 al2) al11
            else error fi "parameter type mismatch" 
        | TyBot -> TyBot,minalock al1 al2
        | _ -> error fi "arrow type expected")
  | TmAscribe(fi,t1,tyT) ->
    let tyT1,al1 = typeof1 ctx lst t1 pid in
     if subtype ctx tyT1 tyT then
       tyT,al1
     else
       error fi "body of as-term does not have the expected type"
  | TmString _ -> TyString,None
  | TmUnit(_) -> TyUnit,None
  | TmRef(_,l1,t1) ->
      let tyT1,al1 = typeof1 ctx lst t1 pid in
      TyRef(l1,tyT1),al1
  | TmLoc(fi,_) ->
      error fi "locations are not supposed to occur in source programs!"
  | TmLet(_,x,t1,t2) ->
     let tyT1,al1= typeof1 ctx lst t1 pid in
     let ctx' = addbinding ctx x (VarBind(tyT1)) in
     let tyT2,al2 = typeof1 ctx' lst t2 pid in
     typeShift (-1) tyT2, minalock al1 al2
  | TmTrue(_) -> 
      TyBool,None
  | TmFalse(_) -> 
      TyBool,None
  | TmIf(fi,t1,t2,t3) ->
    let tyT1,al1 = typeof1 ctx lst t1 pid in
    let tyT2,al2 = typeof1 ctx lst t2 pid in
    let tyT3,al3 = typeof1 ctx lst t3 pid in
      if subtype ctx tyT1 TyBool then
        join ctx tyT2 tyT3,minalock (minalock al1 al2) al3
      else error fi "guard of conditional not a boolean"
  | TmRecord(_, fields) ->
      let tyres = List.map (fun (li,ti) -> (li, typeof1 ctx lst ti pid)) fields in
      let fieldtys = 
        List.map (fun (li,(tyi,al)) -> (li,tyi)) tyres in
      let als = List.map (fun (li,(tyi,al)) -> al) tyres in
      TyRecord(fieldtys), List.fold_left minalock None als
  | TmProj(fi, t1, l) ->
    let tyT1,al1 = typeof1 ctx lst t1 pid in
      (match simplifyty ctx tyT1 with
          TyRecord(fieldtys) ->
            (try List.assoc l fieldtys
             with Not_found -> error fi ("label "^l^" not found")),al1
        | TyBot -> TyBot,al1
        | _ -> error fi "Expected record type")
  | TmCase(fi, t, cases) ->
    let tyT,al = typeof1 ctx lst t pid in
      (match simplifyty ctx tyT with
         TyVariant(fieldtys) ->
           List.iter
             (fun (li,(_,_)) ->
                try let _ = List.assoc li fieldtys in ()
                with Not_found -> error fi ("label "^li^" not in type"))
             cases;
           let typeres =
             List.map (fun (li,(xi,ti)) ->
                         let tyTi =
                           try List.assoc li fieldtys
                           with Not_found ->
                             error fi ("label "^li^" not found") in
                         let ctx' = addbinding ctx xi (VarBind(tyTi)) in
                         let tyTi',al = typeof1 ctx' lst ti pid in
                         typeShift (-1) tyTi',al)
                      cases in
            let casetypes = List.map (fun (tyi,al) -> tyi) typeres in
            let als = List.map (fun (tyi,al) -> al) typeres in
           List.fold_left (join ctx) TyBot casetypes, List.fold_left minalock None als
        | TyBot -> TyBot,al
        | _ -> error fi "Expected variant type")
  | TmTag(fi, li, ti, tyT) ->
      (match simplifyty ctx tyT with
          TyVariant(fieldtys) ->
            (try
               let tyTiExpected = List.assoc li fieldtys in
               let tyTi,ali = typeof1 ctx lst ti pid in
               if subtype ctx tyTi tyTiExpected
                 then tyT,ali
                 else error fi "field does not have expected type"
             with Not_found -> error fi ("label "^li^" not found"))
        | _ -> error fi "Annotation is not a variant type")
  | TmFix(fi, t1) ->
      let tyT1,al1 = typeof1 ctx lst t1 pid in
      (match simplifyty ctx tyT1 with
           TyArr(lst,al,tyT11,tyT12) ->
             if al!=None then error fi "fixpoint with lock acquired" else
             if subtype ctx tyT12 tyT11 then tyT12,al1
             else error fi "result of body not compatible with domain"
         | TyBot -> TyBot,al1
         | _ -> error fi "arrow type expected")
  | TmDeref(fi,t1) ->
    let tyT1,al1 = typeof1 ctx lst t1 pid in
      (match simplifyty ctx tyT1 with
          (* TyRef(tyT1) -> tyT1 *)
        | TyBot -> TyBot,al1
        (* | TySource(tyT1) -> tyT1 *)
        (* add *)
        | TyRef(li1,tyT1) -> if sublockset li1 lst then tyT1,al1 else error fi "lock isn't acquired before dereference"
        | TySource(li1,tyT1) -> if sublockset li1 lst then tyT1,al1 else error fi "lock isn't acquired before dereference"
        | _ -> error fi "argument of ! is not a Ref or Source")
  | TmAssign(fi,t1,t2) ->
    let tyT1,al1 = typeof1 ctx lst t1 pid in
    let tyT2,al2 = typeof1 ctx lst t2 pid in
      (match simplifyty ctx tyT1 with
          (* TyRef(tyT1) ->
            if subtype ctx (typeof ctx lst t2) tyT1 then
              TyUnit
            else
              error fi "arguments of := are incompatible" *)
        | TyBot -> TyBot,minalock al1 al2
        (* |TySink(tyT1) ->
            if subtype ctx (typeof ctx lst t2) tyT1 then
              TyUnit
            else
              error fi "arguments of := are incompatible" *)
      (* add *)
        | TyRef(li1, tyT11) ->
            if subtype ctx tyT2 tyT11 then
              if sublockset li1 lst then
                TyUnit,minalock al1 al2
              else 
                error fi "lock isn't acquire before assignment"
            else
              error fi "arguments of := are incompatible"
        | TySink(li1, tyT11) ->
            if subtype ctx tyT2 tyT11 then
              if sublockset li1 lst then
                TyUnit,minalock al1 al2
              else 
                error fi "lock isn't acquire before assignment"
            else
              error fi "arguments of := are incompatible"
        | _ -> error fi "argument of := is not a Ref or Sink")
  | TmFloat _ -> TyFloat,None
  | TmTimesfloat(fi,t1,t2) ->
    let tyT1,al1 = typeof1 ctx lst t1 pid in
    let tyT2,al2 = typeof1 ctx lst t2 pid in
      if subtype ctx tyT1 TyFloat
      && subtype ctx tyT2 TyFloat then TyFloat,minalock al1 al2
      else error fi "argument of timesfloat is not a number"
  | TmInert(_,tyT) ->
      tyT,None
  | TmZero(_) ->
      TyNat,None
  | TmSucc(fi,t1) ->
      let tyT1,al1 = typeof1 ctx lst t1 pid in
      if subtype ctx tyT1 TyNat then TyNat,al1
      else error fi "argument of succ is not a number"
  | TmPred(fi,t1) ->
      let tyT1,al1 = typeof1 ctx lst t1 pid in
      if subtype ctx tyT1 TyNat then TyNat,al1
      else error fi "argument of pred is not a number"
  | TmIsZero(fi,t1) ->
      let tyT1,al1 = typeof1 ctx lst t1 pid in
      if subtype ctx tyT1 TyNat then TyBool,al1
      else error fi "argument of iszero is not a number"
  (* new *)
  | TmThread(_,t1) ->
    let tyT1,al1 = typeof1 ctx lst t1 (newpid()) in
      TyThread(tyT1,pid),al1
  | TmWait(fi,t1) ->
    if sizelockset lst = 0 then
    let tyT1,al1 = typeof1 ctx lst t1 pid in
      (match simplifyty ctx tyT1 with
          TyThread(tyT1,fpid) ->
            if fpid=pid then tyT1,al1
            else error fi "thread waiting for is not created by this thread"
        | TyBot -> TyBot,al1
        | _ -> error fi "argument of wait is not a thread")
    else  error fi "lock should be released before wait"
  | TmMutex(_,l1) ->
      TyMutex(l1),None
  | TmAcquire(fi,t1,t2) ->
    let tyT1,al1 = typeof1 ctx lst t1 pid in
      (match simplifyty ctx tyT1 with
          TyMutex(l1) -> 
            if sizelockset lst = 0 || maxlock lst < l1 then
            let lst' =  appendlock l1 lst in
            let tyT2, _ = typeof1 ctx lst' t2 pid in
             (simplifyty ctx tyT2 ,minalock (Some l1) al1)
            else error fi "lock should be acquired in order"
        | _ -> error fi "argument of acquire is not a mutex")
  | TmTid(_) ->
      TyNat,None
  | TmThreadLoc(fi,_)-> 
     error fi "thread location is not allowed in source code"
  (* | TmRefMutex(_,li,t1) ->
      TyRefMutex(li, typeof ctx lst t1) *)
  
let typeof (ctx:context) (lst:lockset) (t:term) = 
  let ty,_ = typeof1 ctx lst t rootthreadid in
    ty