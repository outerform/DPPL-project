(* Module Main: The main program.  Deals with processing the command
   line, reading files, building and connecting lexers and parsers, etc. 
   
   For most experiments with the implementation, it should not be
   necessary to change this file.
*)

open Format
open Support.Pervasive
open Support.Error
open Syntax
open Core

let searchpath = ref [""]

let argDefs = [
  "-I",
      Arg.String (fun f -> searchpath := f::!searchpath),
      "Append a directory to the search path"]

let parseArgs () =
  let inFile = ref (None : string option) in
  Arg.parse argDefs
     (fun s ->
       match !inFile with
         Some(_) -> err "You must specify exactly one input file"
       | None -> inFile := Some(s))
     "";
  match !inFile with
      None -> err "You must specify an input file"
    | Some(s) -> s

let openfile infile = 
  let rec trynext l = match l with
        [] -> err ("Could not find " ^ infile)
      | (d::rest) -> 
          let name = if d = "" then infile else (d ^ "/" ^ infile) in
          try open_in name
            with Sys_error m -> trynext rest
  in trynext !searchpath

let parseFile inFile =
  let pi = openfile inFile
  in let lexbuf = Lexer.create inFile pi
  in let result =
    try Parser.toplevel Lexer.main lexbuf with Parsing.Parse_error -> 
    error (Lexer.info lexbuf) "Parse error"
in
  Parsing.clear_parser(); close_in pi; result

let alreadyImported = ref ([] : string list)

let checkbinding fi ctx lst b = match b with
    NameBind -> NameBind
  | VarBind(tyT) -> VarBind(tyT)
  | TmAbbBind(t,None) -> TmAbbBind(t, Some(typeof ctx lst t))
  | TmAbbBind(t,Some(tyT)) ->
     let tyT' = typeof ctx lst t in
     if subtype ctx tyT' tyT then TmAbbBind(t,Some(tyT))
     else error fi "Type of binding does not match declared type"
  | TyVarBind -> TyVarBind
  | TyAbbBind(tyT) -> TyAbbBind(tyT)

let prbindingty ctx lst b = match b with
    NameBind -> ()
  | VarBind(tyT) -> pr ": "; printty ctx tyT 
  | TmAbbBind(t, tyT_opt) -> pr ": ";
     (match tyT_opt with
         None -> printty ctx (typeof ctx lst t)
       | Some(tyT) -> printty ctx tyT)
  | TyVarBind -> ()
  | TyAbbBind(tyT) -> pr ":: *"

let rec process_file f (ctx,lst,store,threads,thctx) =
  if List.mem f (!alreadyImported) then
    (ctx,lst,store,threads,thctx)
  else (
    alreadyImported := f :: !alreadyImported;
    let cmds,_ = parseFile f ctx in
    let g (ctx,lst,store,threads,thctx) c =  
      open_hvbox 0;
      let results = process_command (ctx,lst,store,threads,thctx) c in
      print_flush();
      results
    in
      List.fold_left g (ctx,lst,store,threads,thctx) cmds)

and process_command (ctx,lst,store,threads,thctx) cmd = match cmd with
    Import(f) -> 
      process_file f (ctx,lst,store,threads,thctx)
  | Eval(fi,t) -> 
      let tyT = typeof ctx lst t in
      let t',store,threads,thctx = eval ctx store threads thctx t in
      printtm_ATerm true ctx t'; 
      print_break 1 2;
      pr ": ";
      printty ctx tyT;
      force_newline();
      (ctx,lst,store,threads,thctx)
  | Bind(fi,x,bind) -> 
      let bind = checkbinding fi ctx lst bind in
      let bind',store',threads',thctx' = evalbinding ctx store threads thctx bind in
      pr x; pr " "; prbindingty ctx lst bind'; force_newline();
      (addbinding ctx x bind',lst, (shiftstore 1 store'), threads',thctx')
  
let main () = 
  let inFile = parseArgs() in
  let _ = process_file inFile (emptycontext, emptylockset, emptystore, emptythreads, emptythctx) in
  ()

let () = set_max_boxes 1000
let () = set_margin 67
(* below function has problem *)
let res = 
  Printexc.catch (fun () ->   
    try main();0 
    with Exit x -> x) 
  ()
let () = print_flush()
let () = exit res
