/*  
 *  Yacc grammar for the parser.  The files parser.mli and parser.ml
 *  are generated automatically from parser.mly.
 */

%{
open Support.Error
open Support.Pervasive
open Syntax
%}

/* ---------------------------------------------------------------------- */
/* Preliminaries */

/* We first list all the tokens mentioned in the parsing rules
   below.  The names of the tokens are common to the parser and the
   generated lexical analyzer.  Each token is annotated with the type
   of data that it carries; normally, this is just file information
   (which is used by the parser to annotate the abstract syntax trees
   that it constructs), but sometimes -- in the case of identifiers and
   constant values -- more information is provided.
 */

/* Keyword tokens */
%token <Support.Error.info> IMPORT
%token <Support.Error.info> TBOT
%token <Support.Error.info> LAMBDA
%token <Support.Error.info> TTOP
%token <Support.Error.info> AS
%token <Support.Error.info> USTRING
%token <Support.Error.info> UNIT
%token <Support.Error.info> REF
%token <Support.Error.info> UUNIT
%token <Support.Error.info> RREF
%token <Support.Error.info> CASE
%token <Support.Error.info> OF
%token <Support.Error.info> LET
%token <Support.Error.info> IN
%token <Support.Error.info> FIX
%token <Support.Error.info> LETREC
%token <Support.Error.info> IF
%token <Support.Error.info> THEN
%token <Support.Error.info> ELSE
%token <Support.Error.info> TRUE
%token <Support.Error.info> FALSE
%token <Support.Error.info> BOOL
%token <Support.Error.info> TIMESFLOAT
%token <Support.Error.info> SSOURCE
%token <Support.Error.info> SSINK
%token <Support.Error.info> UFLOAT
%token <Support.Error.info> SUCC
%token <Support.Error.info> PRED
%token <Support.Error.info> ISZERO
%token <Support.Error.info> INERT
%token <Support.Error.info> TYPE
%token <Support.Error.info> NAT

/* new keyword tokens */
%token <Support.Error.info> THREAD
%token <Support.Error.info> ACQUIRE
%token <Support.Error.info> FORK
%token <Support.Error.info> WAIT
%token <Support.Error.info> TID
%token <Support.Error.info> MUTEX


/* Identifier and constant value tokens */
%token <string Support.Error.withinfo> UCID  /* uppercase-initial */
%token <string Support.Error.withinfo> LCID  /* lowercase/symbolic-initial */
%token <int Support.Error.withinfo> INTV
%token <float Support.Error.withinfo> FLOATV
%token <string Support.Error.withinfo> STRINGV

/* Symbolic tokens */
%token <Support.Error.info> APOSTROPHE
%token <Support.Error.info> DQUOTE
%token <Support.Error.info> ARROW
%token <Support.Error.info> BANG
%token <Support.Error.info> BARGT
%token <Support.Error.info> BARRCURLY
%token <Support.Error.info> BARRSQUARE
%token <Support.Error.info> COLON
%token <Support.Error.info> COLONCOLON
%token <Support.Error.info> COLONEQ
%token <Support.Error.info> COLONHASH
%token <Support.Error.info> COMMA
%token <Support.Error.info> DARROW
%token <Support.Error.info> DDARROW
%token <Support.Error.info> DOT
%token <Support.Error.info> EOF
%token <Support.Error.info> EQ
%token <Support.Error.info> EQEQ
%token <Support.Error.info> EXISTS
%token <Support.Error.info> GT
%token <Support.Error.info> HASH
%token <Support.Error.info> LCURLY
%token <Support.Error.info> LCURLYBAR
%token <Support.Error.info> LEFTARROW
%token <Support.Error.info> LPAREN
%token <Support.Error.info> LSQUARE
%token <Support.Error.info> LSQUAREBAR
%token <Support.Error.info> LT
%token <Support.Error.info> RCURLY
%token <Support.Error.info> RPAREN
%token <Support.Error.info> RSQUARE
%token <Support.Error.info> SEMI
%token <Support.Error.info> SLASH
%token <Support.Error.info> STAR
%token <Support.Error.info> TRIANGLE
%token <Support.Error.info> USCORE
%token <Support.Error.info> VBAR

/* New tokens */
%token <Support.Error.info> FORK
%token <Support.Error.info> WAIT
%token <Support.Error.info> MUTEX
%token <Support.Error.info> ACQUIRE

%token <Support.Error.info> MMUTEX
%token <Support.Error.info> TTHREAD

/* ---------------------------------------------------------------------- */
/* The starting production of the generated parser is the syntactic class
   toplevel.  The type that is returned when a toplevel is recognized is
     Syntax.context -> (Syntax.command list * Syntax.context) 
   that is, the parser returns to the user program a function that,
   when given a naming context, returns a fully parsed list of
   Syntax.commands and the new naming context that results when
   all the names bound in these commands are defined.

   All of the syntactic productions in the parser follow the same pattern:
   they take a context as argument and return a fully parsed abstract
   syntax tree (and, if they involve any constructs that bind variables
   in some following phrase, a new context).
   
*/

%start toplevel
%type < Syntax.context -> (Syntax.command list * Syntax.context) > toplevel
%%

/* ---------------------------------------------------------------------- */
/* Main body of the parser definition */

/* The top level of a file is a sequence of commands, each terminated
   by a semicolon. */
toplevel :
    EOF
      { fun ctx -> [],ctx }
  | Command SEMI toplevel
      { fun ctx ->
          let cmd,ctx = $1 ctx in
          let cmds,ctx = $3 ctx in
          cmd::cmds,ctx }

/* A top-level command */
Command :
    IMPORT STRINGV { fun ctx -> (Import($2.v)),ctx }
  | Term 
      { fun ctx -> (let t = $1 ctx in Eval(tmInfo t,t)),ctx }
  | LCID Binder
      { fun ctx -> ((Bind($1.i,$1.v,$2 ctx)), addname ctx $1.v) }
  | UCID TyBinder
      { fun ctx -> ((Bind($1.i, $1.v, $2 ctx)), addname ctx $1.v) }

/* Right-hand sides of top-level bindings */
Binder :
    COLON Type
      { fun ctx -> VarBind ($2 ctx)}
  | EQ Term 
      { fun ctx -> TmAbbBind($2 ctx, None) }

/* All type expressions */
Type :
    ArrowType
                { $1 }
  | RREF AType
      { fun ctx -> TyRef(emptylockset,$2 ctx) }
  | SSOURCE AType
      { fun ctx -> TySource(emptylockset,$2 ctx) }
  | SSINK AType
      { fun ctx -> TySink(emptylockset,$2 ctx) }
//   add
  | MUTEX UCID
      { fun ctx -> TyMutex($2.v) }
  | RREF LT MutexFields GT AType
      { fun ctx -> TyRef($3, $5 ctx) }
  | SSOURCE LT MutexFields GT AType
        { fun ctx -> TySource($3, $5 ctx) }
  | SSINK LT MutexFields GT AType
        { fun ctx -> TySink($3, $5 ctx) }
  | THREAD AType INTV
        { fun ctx -> TyThread($2 ctx, $3.v)  }

/* Atomic types are those that never need extra parentheses */
AType :
    LPAREN Type RPAREN  
           { $2 } 
  | TBOT
      { fun ctx -> TyBot }
  | TTOP
      { fun ctx -> TyTop }
  | USTRING
      { fun ctx -> TyString }
  | UUNIT
      { fun ctx -> TyUnit }
  | LT FieldTypes GT
      { fun ctx ->
          TyVariant($2 ctx 1) }
  | BOOL
      { fun ctx -> TyBool }
  | UCID 
      { fun ctx ->
          if isnamebound ctx $1.v then
            TyVar(name2index $1.i ctx $1.v, ctxlength ctx)
          else 
            TyId($1.v) }
  | LCURLY FieldTypes RCURLY
      { fun ctx ->
          TyRecord($2 ctx 1) }
  | UFLOAT
      { fun ctx -> TyFloat }
  | NAT
      { fun ctx -> TyNat }

/* An "arrow type" is a sequence of atomic types separated by
   arrows. */
ArrowType :
    AType ARROW ArrowType
     { fun ctx -> TyArr(emptylockset, None,$1 ctx, $3 ctx) }
  | AType LT MutexFields GT ARROW ArrowType
     { fun ctx -> TyArr($3, None,$1 ctx, $6 ctx) }
  | AType LSQUARE UCID RSQUARE ARROW ArrowType
    { fun ctx -> TyArr(emptylockset, Some($3.v),$1 ctx, $6 ctx) }
  | AType LT MutexFields GT LSQUARE UCID RSQUARE ARROW ArrowType
    { fun ctx -> TyArr($3, Some($6.v),$1 ctx, $9 ctx) }
  | AType
            { $1 }

Term :
    AppTerm
      { $1 }
  | LAMBDA LCID COLON Type DOT Term 
      { fun ctx ->
          let ctx1 = addname ctx $2.v in
          TmAbs($1,emptylockset, $2.v, $4 ctx, $6 ctx1) }
  | LAMBDA USCORE COLON Type DOT Term 
      { fun ctx ->
          let ctx1 = addname ctx "_" in
          TmAbs($1,emptylockset, "_", $4 ctx, $6 ctx1) }
  | AppTerm COLONEQ AppTerm
      { fun ctx -> TmAssign($2, $1 ctx, $3 ctx) }
  | CASE Term OF Cases
      { fun ctx ->
          TmCase($1, $2 ctx, $4 ctx) }
  | LET LCID EQ Term IN Term
      { fun ctx -> TmLet($1, $2.v, $4 ctx, $6 (addname ctx $2.v)) }
  | LET USCORE EQ Term IN Term
      { fun ctx -> TmLet($1, "_", $4 ctx, $6 (addname ctx "_")) }
  | LETREC LCID COLON Type EQ Term IN Term
      { fun ctx -> 
          let ctx1 = addname ctx $2.v in 
          TmLet($1, $2.v, TmFix($1, TmAbs($1,emptylockset, $2.v, $4 ctx, $6 ctx1)),
                $8 ctx1) }
  | IF Term THEN Term ELSE Term
      { fun ctx -> TmIf($1, $2 ctx, $4 ctx, $6 ctx) }
// add
  | ACQUIRE Term IN Term
        { fun ctx -> TmAcquire($1,$2 ctx, $4 ctx) }
    | LAMBDA LT MutexFields GT LCID COLON Type DOT Term 
        { fun ctx ->
            let ctx1 = addname ctx $5.v in
            TmAbs($1, $3, $5.v, $7 ctx, $9 ctx1) }
    | LAMBDA  LT MutexFields GT USCORE COLON Type DOT Term 
        { fun ctx ->
            let ctx1 = addname ctx "_" in
            TmAbs($5, $3, "_", $7 ctx, $9 ctx1) 
        }

AppTerm :
    PathTerm
      { $1 }
  | AppTerm PathTerm
      { fun ctx ->
          let e1 = $1 ctx in
          let e2 = $2 ctx in
          TmApp(tmInfo e1,e1,e2) }
  | REF PathTerm
      { fun ctx -> TmRef($1, emptylockset , $2 ctx) }
  | BANG PathTerm 
      { fun ctx -> TmDeref($1, $2 ctx) }
  | FIX PathTerm
      { fun ctx ->
          TmFix($1, $2 ctx) }
  | TIMESFLOAT PathTerm PathTerm
      { fun ctx -> TmTimesfloat($1, $2 ctx, $3 ctx) }
  | SUCC PathTerm
      { fun ctx -> TmSucc($1, $2 ctx) }
  | PRED PathTerm
      { fun ctx -> TmPred($1, $2 ctx) }
  | ISZERO PathTerm
      { fun ctx -> TmIsZero($1, $2 ctx) }
// add 
  | WAIT PathTerm
        { fun ctx -> TmWait($1,$2 ctx) }
  | REF LT MutexFields GT PathTerm
        { fun ctx -> TmRef($1, $3, $5 ctx) }



AscribeTerm :
    ATerm AS Type
      { fun ctx -> TmAscribe($2, $1 ctx, $3 ctx) }
  | ATerm
      { $1 }

FieldTypes :
    /* empty */
      { fun ctx i -> [] }
  | NEFieldTypes
      { $1 }

NEFieldTypes :
    FieldType
      { fun ctx i -> [$1 ctx i] }
  | FieldType COMMA NEFieldTypes
      { fun ctx i -> ($1 ctx i) :: ($3 ctx (i+1)) }

FieldType :
    LCID COLON Type
      { fun ctx i -> ($1.v, $3 ctx) }
  | Type
      { fun ctx i -> (string_of_int i, $1 ctx) }

PathTerm :
    PathTerm DOT LCID
      { fun ctx ->
          TmProj($2, $1 ctx, $3.v) }
  | PathTerm DOT INTV
      { fun ctx ->
          TmProj($2, $1 ctx, string_of_int $3.v) }
  | AscribeTerm
      { $1 }

TermSeq :
    Term 
      { $1 }
  | Term SEMI TermSeq 
      { fun ctx ->
          TmApp($2, TmAbs($2, emptylockset,"_", TyUnit, $3 (addname ctx "_")), $1 ctx) }

/* Atomic terms are ones that never require extra parentheses */
ATerm :
    LPAREN TermSeq RPAREN  
      { $2 } 
  | LCID 
      { fun ctx ->
          TmVar($1.i, name2index $1.i ctx $1.v, ctxlength ctx) }
  | STRINGV
      { fun ctx -> TmString($1.i, $1.v) }
  | UNIT
      { fun ctx -> TmUnit($1) }
  | LT LCID EQ Term GT AS Type
      { fun ctx ->
          TmTag($1, $2.v, $4 ctx, $7 ctx) }
  | TRUE
      { fun ctx -> TmTrue($1) }
  | FALSE
      { fun ctx -> TmFalse($1) }
  | FLOATV
      { fun ctx -> TmFloat($1.i, $1.v) }
  | LCURLY Fields RCURLY
      { fun ctx ->
          TmRecord($1, $2 ctx 1) }
  | INTV
      { fun ctx ->
          let rec f n = match n with
              0 -> TmZero($1.i)
            | n -> TmSucc($1.i, f (n-1))
          in f $1.v }
  | INERT LSQUARE Type RSQUARE 
      { fun ctx -> TmInert($1, $3 ctx) }
// add rule
  | FORK LCURLY Term RCURLY
        { fun ctx -> TmThread($1,$3 ctx) }
  | MUTEX LT UCID GT
        { fun ctx -> TmMutex($1, $3.v) }
Cases :
    Case
      { fun ctx -> [$1 ctx] }
  | Case VBAR Cases
      { fun ctx -> ($1 ctx) :: ($3 ctx) }

Case :
    LT LCID EQ LCID GT DDARROW AppTerm
      { fun ctx ->
          let ctx1 = addname ctx $4.v in
          ($2.v, ($4.v, $7 ctx1)) }

Fields :
    /* empty */
      { fun ctx i -> [] }
  | NEFields
      { $1 }

NEFields :
    Field
      { fun ctx i -> [$1 ctx i] }
  | Field COMMA NEFields
      { fun ctx i -> ($1 ctx i) :: ($3 ctx (i+1)) }

Field :
    LCID EQ Term
      { fun ctx i -> ($1.v, $3 ctx) }
  | Term
      { fun ctx i -> (string_of_int i, $1 ctx) }

TyBinder :
    /* empty */
      { fun ctx -> TyVarBind }
  | EQ Type
      { fun ctx -> TyAbbBind($2 ctx) }

MutexFields :
    UCID
      { newlockset $1.v }
  | UCID COMMA MutexFields
      { appendlock $1.v $3 }

/*   */
