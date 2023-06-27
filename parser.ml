open MicroCamlTypes
open Utils
open TokenTypes

(* Provided functions - DO NOT MODIFY *)

(* Matches the next token in the list, throwing an error if it doesn't match the given token *)
let match_token (toks: token list) (tok: token) =
  match toks with
  | [] -> raise (InvalidInputException(string_of_token tok))
  | h::t when h = tok -> t
  | h::_ -> raise (InvalidInputException(
      Printf.sprintf "Expected %s from input %s, got %s"
        (string_of_token tok)
        (string_of_list string_of_token toks)
        (string_of_token h)))

(* Matches a sequence of tokens given as the second list in the order in which they appear, throwing an error if they don't match *)
let match_many (toks: token list) (to_match: token list) =
  List.fold_left match_token toks to_match

(* Return the next token in the token list as an option *)
let lookahead (toks: token list) = 
  match toks with
  | [] -> None
  | h::t -> Some h

(* Return the token at the nth index in the token list as an option*)
let rec lookahead_many (toks: token list) (n: int) = 
  match toks, n with
  | h::_, 0 -> Some h
  | _::t, n when n > 0 -> lookahead_many t (n-1)
  | _ -> None

(* Part 2: Parsing expressions *)

let rec parse_expr toks = 
    match lookahead toks with
    | Some Tok_Let -> parse_let_expr toks
    | Some Tok_If -> parse_if_expr toks
    | Some Tok_Fun -> parse_function_expr toks
    | Some Tok_Not | Some Tok_Int(_) | Some Tok_Bool(_) | Some Tok_String(_) | Some Tok_ID(_) | Some Tok_LParen -> parse_or_expr toks
    | _ -> raise (InvalidInputException "Invalid expression")
and parse_let_expr toks = 
    let t1 = match_token toks Tok_Let in
    let (t2, rec_bool) = match lookahead t1 with
        | Some Tok_Rec -> (match_token t1 Tok_Rec, true)
        | _ -> (t1, false)
    in
    let (t3, var) = match lookahead t2 with
        | Some Tok_ID(id) -> (match_token t2 (Tok_ID(id)), id)
        | _ -> raise (InvalidInputException "No Variable")
    in
    let (t4, e1) = parse_expr (match_token t3 Tok_Equal) in
    let (t5, e2) = parse_expr (match_token t4 Tok_In) in 
    (t5, Let(var, rec_bool, e1, e2))
and parse_if_expr toks =
    let (t1, e1) = parse_expr (match_token toks Tok_If) in
    let (t2, e2) = parse_expr (match_token t1 Tok_Then) in
    let (t3, e3) = parse_expr (match_token t2 Tok_Else) in
    (t3, If(e1, e2, e3))
and parse_function_expr toks = 
    let t1 = match_token toks Tok_Fun in
    let (t2, var) = match lookahead t1 with
    | Some Tok_ID(id) -> (match_token t1 (Tok_ID(id)), id)
    | _ -> raise (InvalidInputException "No Variable")
    in
    let (t3, e2) = parse_expr (match_token t2 Tok_Arrow) in
    (t3, Fun(var, e2))
and parse_or_expr toks =
    let (t1, e1) = parse_and_expr toks in
    match lookahead t1 with
        | Some Tok_Or ->
            let (t2, e2) = parse_or_expr (match_token t1 Tok_Or) in
            (t2, Binop(Or, e1, e2))
        | _ -> (t1, e1)
and parse_and_expr toks = 
    let (t1, e1) = parse_equality_expr toks in
    match lookahead t1 with
        | Some Tok_And ->
            let (t2, e2) = parse_and_expr (match_token t1 Tok_And) in
            (t2, Binop(And, e1, e2))
        | _ -> (t1, e1)
and parse_equality_expr toks =
    let (t1, e1) = parse_relational_expr toks in
    match lookahead t1 with
        | Some Tok_Equal | Some Tok_NotEqual ->
            let (t2, op) = parse_equality_op t1 in
            let (t3, e2) = parse_equality_expr t2 in 
            (t3, Binop(op, e1, e2))
        | _ -> (t1, e1)
and parse_equality_op toks = 
    match lookahead toks with
        | Some Tok_Equal -> (match_token toks Tok_Equal, Equal)
        | Some Tok_NotEqual -> (match_token toks Tok_NotEqual, NotEqual)
        | _ -> raise (InvalidInputException "No equality operator")
and parse_relational_expr toks = 
    let (t1, e1) = parse_additive_expr toks in
    match lookahead t1 with
        | Some Tok_Less | Some Tok_Greater | Some Tok_LessEqual | Some Tok_GreaterEqual ->
            let (t2, op) = parse_relational_op t1 in
            let (t3, e2) = parse_relational_expr t2 in
            (t3, Binop(op, e1, e2))
        | _ -> (t1, e1)
and parse_relational_op toks =
    match lookahead toks with
        | Some Tok_Less -> (match_token toks Tok_Less, Less)
        | Some Tok_Greater -> (match_token toks Tok_Greater, Greater)
        | Some Tok_LessEqual -> (match_token toks Tok_LessEqual, LessEqual)
        | Some Tok_GreaterEqual -> (match_token toks Tok_GreaterEqual, GreaterEqual)
        | _ -> raise (InvalidInputException "No relational operator")
and parse_additive_expr toks = 
    let (t1, e1) = parse_mult_expr toks in
    match lookahead t1 with
        | Some Tok_Add | Some Tok_Sub ->
            let (t2, op) = parse_additive_op t1 in
            let (t3, e2) = parse_additive_expr t2 in
            (t3, Binop(op, e1, e2))
        | _ -> (t1, e1)
and parse_additive_op toks = 
    match lookahead toks with
        | Some Tok_Add -> (match_token toks Tok_Add, Add)
        | Some Tok_Sub -> (match_token toks Tok_Sub, Sub)
        | _ -> raise (InvalidInputException "No additive operator")
and parse_mult_expr toks = 
    let (t1, e1) = parse_concat_expr toks in
    match lookahead t1 with
        | Some Tok_Mult | Some Tok_Div ->
            let (t2, op) = parse_mult_op t1 in
            let (t3, e2) = parse_mult_expr t2 in
            (t3, Binop(op, e1, e2))
        | _ -> (t1, e1)
and parse_mult_op toks = 
    match lookahead toks with
        | Some Tok_Mult -> (match_token toks Tok_Mult, Mult)
        | Some Tok_Div -> (match_token toks Tok_Div, Div)
        | _ -> raise (InvalidInputException "No multiplicative operator")
and parse_concat_expr toks =
    let (t1, e1) = parse_unary_expr toks in
    match lookahead t1 with
        | Some Tok_Concat ->
            let (t2, e2) = parse_concat_expr (match_token t1 Tok_Concat) in
            (t2, Binop(Concat, e1, e2))
        | _ -> (t1, e1)
and parse_unary_expr toks = 
    match lookahead toks with
        | Some Tok_Not -> let (t1, e1) = parse_unary_expr (match_token toks Tok_Not) in
            (t1, Not(e1)) 
        | _ -> parse_call_expr toks
and parse_call_expr toks = 
    let (t1, e1) = parse_primary_expr toks in
    match lookahead t1 with    
        | Some Tok_Not | Some Tok_Int(_) | Some Tok_Bool(_) | Some Tok_String(_) | Some Tok_ID(_) | Some Tok_LParen ->
            let (t2, e2) = parse_primary_expr t1 in
            (t2, FunctionCall(e1, e2))
        | _ -> (t1, e1)
and parse_primary_expr toks = 
    match lookahead toks with
        | Some Tok_Int(i) -> (match_token toks (Tok_Int i), Value(Int i))
        | Some Tok_Bool(b) -> (match_token toks (Tok_Bool b), Value(Bool b))
        | Some Tok_String(s) -> (match_token toks (Tok_String s), Value(String s))
        | Some Tok_ID(id) -> (match_token toks (Tok_ID id), ID(id))
        | Some Tok_LParen -> let (t1, e1) = parse_expr (match_token toks Tok_LParen) in
           (match_token t1 Tok_RParen, e1) 
        | _ -> raise (InvalidInputException "Not primary expression")
(* Part 3: Parsing mutop *)

let rec parse_mutop toks =
    match lookahead toks with 
        | Some Tok_DoubleSemi -> (match lookahead (match_token toks Tok_DoubleSemi) with
            | None -> ([], NoOp)
            | Some _ -> raise (InvalidInputException "Token after double semicolon"))
        | Some Tok_Def -> parse_def_mutop toks
        | _ -> parse_expr_mutop toks
and parse_def_mutop toks =
    let (t1, var) = match lookahead (match_token toks Tok_Def) with
        | Some Tok_ID(id) -> (match_many toks [Tok_Def; Tok_ID(id)], id)
        | _ -> raise (InvalidInputException "No ID")
    in
    let (t2, e) = parse_expr (match_token t1 Tok_Equal) in
    (match_token t2 Tok_DoubleSemi, Def(var, e)) 
and parse_expr_mutop toks = 
    let (t1, e) = parse_expr toks in
    (match_token t1 Tok_DoubleSemi, Expr e) 
