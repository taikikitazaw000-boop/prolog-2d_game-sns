(*推論*)
module Evaluator = struct
type ast =
  Atom of string
  | Var  of string
  | App  of string * ast list

module P = Printf

(*let rec print_ast ast = match ast with*) 


let rec print_ast ast = match ast with
  | (App (s, hd :: tl)) ->
      (P.printf "App(\"%s\",[" s;
      print_ast hd;
      List.iter (fun x -> (print_string ";" ; print_ast x)) tl;
      print_string "])")
  | (App (s, [])) ->
      P.printf "App(\"%s\",[])" s
  | (Atom s) -> P.printf "Atom \"%s\"" s
  | (Var  s) -> P.printf "Var \"%s\""  s

let print_ast_list lst =
  match lst with
  | (hd :: tl) ->
      (print_string "[";
      print_ast hd;
      List.iter (fun x -> print_string ";" ; print_ast x) tl;
      print_string "]")
  | [] -> print_string "[]"

let sub name term  =
let rec mapVar ast =
  match ast with
  (Atom x) -> Atom(x)
  | (Var n)      -> if n = name then term else Var n
  | (App (n, terms)) -> App (n, List.map mapVar terms)
in mapVar

let mgu (a, b) =
  let rec ut (one,another,unifier) = match (one,another) with
   ([],[]) -> (true,unifier)
 |(term::t1,Var(name)::t2) ->
    let r= fun x->sub name term (unifier x) in
       ut(List.map r t1, List.map r t2,r)
        |(Var(name)::t1,term::t2)->
 let r= fun x-> sub name term (unifier x) in
 ut(List.map r t1, List.map r t2,r)
 |(Atom(n)::t1,Atom(m)::t2)->
 if n = m then ut(t1,t2,unifier)else(false,unifier)
 |(App(n1,xt1)::t1,App(n2,xt2)::t2)->
 if n1=n2 && List.length xt1 = List.length xt2 then
 ut(xt1@t1,xt2@t2,unifier)
 else(false,unifier)
 |(_,_)-> (false,unifier)
  in
  ut ([a], [b], (fun x -> x))

(* ---------- 変数の重複回避のための rename -------------------------------- *)
let rename ver term =
let rec mapVar ast = match ast with
  (Atom x) -> Atom(x)
  | (Var n) -> Var (n ^ "#" ^ ver)
  | (App (n, terms)) -> App (n, List.map mapVar terms)
in mapVar term

(* ---------- 深さ優先の解探索 ------------------------------------------- *)
exception Compiler_error

(* succeed: 最終的に得たい「結果の組み立て方」を渡す想定らしかったので
   とりあえず恒等写像で置いておく *)
let succeed query = (print_ast query; true)

let rec solve (program, question, result, depth) = match question with
  []-> succeed result
 |goal::goals->
 let onestep _ clause = match List.map (rename (string_of_int depth)) clause with
 []->raise Compiler_error
 |head::conds->
 let (unifiable,unifier) = mgu (head,goal) in
 if unifiable then
 solve (program,List.map unifier(conds@goals),
 unifier result,depth+1)
 else true
in List.fold_left onestep true program 
let eval (program,question) = solve(program,[question],question,1)
end

(*字句回析*)
module Lexer = struct
  module P = Printf

  type token =
    | CID of string
    | VID of string
    | NUM of string
    | TO
    | IS
    | QUIT
    | OPEN
    | EOF
    | ONE of char

  exception End_of_system

  let line = ref 1
  let _ISTREAM = ref stdin
  let ch       = ref []

  let get_line_num () = !line

  let read () =
    match !ch with
    | h :: rest -> ch := rest; h
    | []        -> input_char !_ISTREAM

  let unread c = ch := c :: !ch

  let lookahead () =
    try
      let c = read () in
      unread c; c
    with End_of_file ->
      '$'

  let rec integer i =
    let c = lookahead () in
    if c >= '0' && c <= '9' then
      integer (i ^ Char.escaped (read ()))
    else
      i

  and identifier id =
    let c = lookahead () in
    if
      (c >= 'a' && c <= 'z') ||
      (c >= 'A' && c <= 'Z') ||
      (c >= '0' && c <= '9') ||
      c = '_'
    then
      identifier (id ^ Char.escaped (read ()))
    else
      id

  and native_token () =
    let c = lookahead () in
    if c >= 'a' && c <= 'z' then begin
      let id = identifier "" in
      match id with
      | "is"   -> IS
      | "quit" -> QUIT
      | "open" -> OPEN
      | _      -> CID id
    end
    else if c >= 'A' && c <= 'Z' then
      let id = identifier "" in
      VID id
    else if c >= '0' && c <= '9' then
      NUM (integer "")
    else if c = ':' then begin
      ignore (read ());
      if lookahead () = '-' then (ignore (read ()); TO)
      else ONE ':'
    end
    else
      ONE (read ())

  and gettoken () =
    try
      match native_token () with
      | ONE '\n' -> line := !line + 1; gettoken ()
      | ONE ' ' | ONE '\t' -> gettoken ()
      | tk -> tk
    with End_of_file -> EOF

  let print_token = function
    | CID i   -> P.printf "CID(%s)"   i
    | VID i   -> P.printf "VID(%s)"   i
    | NUM i   -> P.printf "NUM(%s)"   i
    | TO      -> P.printf ":-"
    | QUIT    -> P.printf "quit"
    | OPEN    -> P.printf "open"
    | IS      -> P.printf "is"
    | EOF     -> P.printf "eof"
    | ONE c   -> P.printf "ONE(%c)"   c

  let rec run () =
    let rlt = gettoken () in
    match rlt with
    | EOF -> raise End_of_system
    | _         ->
        print_token rlt;
        P.printf "\n%!";
        run ()
end

module Parser = struct
  module L = Lexer
  module E = Evaluator
let prog = ref [[E.Var ""]]
  let tok = ref (L.ONE ' ')

  let getToken () = L.gettoken ()
  let advance () = tok := getToken ()

  exception Syntax_error
  let error () = raise Syntax_error

  let check t =
    match !tok with
    | L.CID _ -> if (t=(L.CID "")) then () else error()
    | L.VID _ -> if (t=(L.VID "")) then () else error()
    | L.NUM _ -> if (t=(L.NUM "")) then () else error()
    | tk -> if (tk=t) then () else error()

  let eat t = check t; advance ()

  let rec clauses () =
    match !tok with
    | L.EOF -> []
    | _ -> let c = clause() in c :: clauses()

  and clause () =
    match !tok with
    | L.ONE '(' -> let c = term() in eat(L.ONE '.');[c]
    | _ -> let p = predicate () in let  o = p :: to_opt () in eat (L.ONE '.') ; o

  and to_opt () =
    match !tok with
    | L.TO -> eat (L.TO); terms () 
    | _ -> []

  and command () =
    match !tok with
    | L.QUIT -> exit 0
    | L.OPEN ->
        ( eat (L.OPEN);
          match !tok with
          | L.CID s ->
                (eat (L.CID "");
                check (L.ONE '.');
                L._ISTREAM := open_in (s ^ ".pl");
                L.line := 1;
                advance (); prog := clauses ();
                close_in (!L._ISTREAM))
          | _ -> error ())
   (* | L.ONE '(' | L.VID _ | L.CID _ | L.NUM _ ->
        (terms (); check(L.ONE '.'))
    | _ -> error ()*)
    |_ -> let t = term() in (check(L.ONE '.'); let _ = E.eval(!prog, t) in ())

  and term () =
    match !tok with
    | L.ONE '(' -> (eat(L.ONE '('); let t = term () in eat (L.ONE ')');t)  (*termは（）に囲われたもの*)
    (*| L.VID s -> (eat(L.VID ""); eat (L.IS); arithmexp ())*)
    | _ -> predicate ()

  and terms () = let t = term () in t :: terms'()
  and terms' () =
    match !tok with
    | L.ONE ',' -> eat (L.ONE ','); let t1 = term () in t1 :: terms' ()
    | _ -> []

  and predicate () =
    match !tok with
    | L.CID s ->
        eat (L.CID "");
        eat (L.ONE '(');
        let a = args () in
        eat (L.ONE ')') ; E.App(s,a)  (*モジュールの外のを使うとき*)
    | _ -> error ()

  and args () = let e = expr () in e :: args'()
  and args' () =
    match !tok with
    | L.ONE ',' -> eat (L.ONE ',');let e = expr () in e :: args' ()
    | _ -> []
  
    and expr () =
    match !tok with
    | L.ONE '(' -> eat (L.ONE '(');let e = expr () in eat (L.ONE ')') ; e  (*ここ変えた*)
    | L.ONE '[' -> eat (L.ONE '[');let l = list () in eat (L.ONE ']') ; l  (*ここも*)
    | L.CID s -> eat (L.CID ""); tail_opt s 
    | L.VID s -> eat (L.VID "") ; E.Var s
    | L.NUM n -> eat (L.NUM "") ; E.Atom n
    | _ -> error ()

  and tail_opt s =
    match !tok with
    | L.ONE '(' -> eat (L.ONE '('); let a = args ()in eat (L.ONE ')') ; E.App (s,a)
    | _ -> E.Atom s

  and list () =
    match !tok with
    | L.ONE ']' -> E.Atom "nil"
    | _ -> let e = expr () in let l = list_opt () in E.App ("cons",[e;l])
    
  and list_opt () =
    match !tok with
    | L.ONE '|' -> eat (L.ONE '|'); id () 
    | L.ONE ',' -> eat (L.ONE ','); list () 
    | _ -> E.Atom "nil"

  and id () =
    match !tok with
    | L.CID s -> eat(L.CID "");E.Atom s
    | L.VID s -> eat(L.VID "");E.Var s
    | L.NUM n -> eat(L.NUM "");E.Atom n
    | _ -> error ()

  (* and arithmexp () = 
    arithmterm (); 
    arithmexp' ()

  and arithmexp' () =
    match !tok with
    | L.ONE '+' -> eat (L.ONE '+'); arithmterm (); arithmexp' ()
    | L.ONE '-' -> eat (L.ONE '-'); arithmterm (); arithmexp' ()
    | _ -> ()

  and arithmterm () = 
    arithmfactor (); 
    arithmterm' ()

  and arithmterm' () =
    match !tok with
    | L.ONE '*' -> eat (L.ONE '*'); arithmfactor (); arithmterm' ()
    | L.ONE '/' -> eat (L.ONE '/'); arithmfactor (); arithmterm' ()
    | _ -> ()

  and arithmfactor () =
    match !tok with
    | L.ONE '(' -> eat (L.ONE '('); arithmexp (); eat (L.ONE ')')
    | L.ONE '-' -> eat (L.ONE '-'); arithmexp ()
    | L.ONE '[' -> eat (L.ONE '['); list (); eat (L.ONE ']')
    | L.CID s -> eat (L.CID s); tail_opt ()
    | L.VID s -> eat (L.VID s)
    | L.NUM n -> eat (L.NUM n)
    | _ -> ()
      *)
end

let rec run () =
  print_string "?-";
  while true do
    flush stdout;
    Lexer._ISTREAM := stdin;
    try
      Parser.advance ();
      Parser.command ();
      print_string "\n?- "
    with
    | Parser.Syntax_error ->
        let line_num = Lexer.get_line_num () in
        Printf.eprintf "Syntax Error at line %d (token: " line_num;
        Lexer.print_token !Parser.tok;
        Printf.eprintf ").\n%!";
        try
          while (Lexer.read ()) <> '\n' do () done;
          Lexer.line := !Lexer.line + 1;
          print_string "?- "
        with End_of_file ->
          print_string "\n"; exit 0
    | Lexer.End_of_system ->
        print_string "\n"; exit 0
    (*| End_of_file ->
        print_string "\n"; exit 0*)
  done
