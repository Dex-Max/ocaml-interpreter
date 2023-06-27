open MicroCamlTypes
open Utils

exception TypeError of string
exception DeclareError of string
exception DivByZeroError 

(* Provided functions - DO NOT MODIFY *)

(* Adds mapping [x:v] to environment [env] *)
let extend env x v = (x, ref v)::env

(* Returns [v] if [x:v] is a mapping in [env]; uses the
   most recent if multiple mappings for [x] are present *)
let rec lookup env x =
  match env with
  | [] -> raise (DeclareError ("Unbound variable " ^ x))
  | (var, value)::t -> if x = var then !value else lookup t x

(* Creates a placeholder mapping for [x] in [env]; needed
   for handling recursive definitions *)
let extend_tmp env x = (x, ref (Int 0))::env

(* Updates the (most recent) mapping in [env] for [x] to [v] *)
let rec update env x v =
  match env with
  | [] -> raise (DeclareError ("Unbound variable " ^ x))
  | (var, value)::t -> if x = var then (value := v) else update t x v
        
(* Part 1: Evaluating expressions *)

(* Evaluates MicroCaml expression [e] in environment [env],
   returning a value, or throwing an exception on error *)
let rec eval_expr env e = match e with 
    | Value(v) -> v
    | ID(i) -> lookup env i
    | Not(e) -> (match eval_expr env e with 
        | Bool(b) -> Bool(not b)
        | _ -> raise (TypeError "Not a boolean"))
    | Binop(op, x, y) -> (match (op, eval_expr env x, eval_expr env y) with
        | (Add, Int(a), Int(b)) -> Int(a + b)
        | (Sub, Int(a), Int(b)) -> Int(a - b)
        | (Mult, Int(a), Int(b)) -> Int(a * b)
        | (Div, Int(a), Int(b)) -> if b == 0 then raise DivByZeroError else Int(a / b)
        | (Greater, Int(a), Int(b)) -> Bool(a > b)
        | (Less, Int(a), Int(b)) -> Bool(a < b)
        | (GreaterEqual, Int(a), Int(b)) -> Bool(a >= b)
        | (LessEqual, Int(a), Int(b)) -> Bool(a <= b)
        | (Concat, String(s), String(t)) -> String(s ^ t)
        | (Equal, x, y) -> (match (x, y) with
            | (Int(a), Int(b)) -> Bool(a = b)
            | (Bool(a), Bool(b)) -> Bool(a = b)
            | (String(a), String(b)) -> Bool(a = b)
            | _ -> raise (TypeError "Must be same type"))
        | (NotEqual, x, y) -> eval_expr env (Not(Binop(Equal, Value(x), Value(y))))
        | (Or, Bool(a), Bool(b)) -> Bool(a || b)
        | (And, Bool(a), Bool(b)) -> Bool(a && b)
        | _ -> raise (TypeError "Invalid type"))
    | If(e1, e2, e3) -> (match eval_expr env e1 with
        | Bool(b) -> if b then eval_expr env e2 else eval_expr env e3
        | _ -> raise (TypeError "First expression must be boolean"))
    | Let(var, r, e1, e2) ->
        if r then
            let new_env = extend_tmp env var in
            let _ = update new_env var (eval_expr new_env e1) in
            eval_expr new_env e2
        else
            eval_expr (extend env var (eval_expr env e1)) e2

    | Fun(var, e) -> Closure(env, var, e) 
    | FunctionCall(e1, e2) -> (match (eval_expr env e1, eval_expr env e2) with
        | (Closure(a, x, e), v) -> eval_expr (extend a x v) e
        | _ -> raise (TypeError "First expression must evaluate to closure"))
(* Part 2: Evaluating mutop directive *)

(* Evaluates MicroCaml mutop directive [m] in environment [env],
   returning a possibly updated environment paired with
   a value option; throws an exception on error *)
let eval_mutop env m = match m with
    | Def(var, e) -> let new_env = extend_tmp env var in
        let evaluated = eval_expr new_env e in
        let _ = update new_env var evaluated in
        (new_env, Some evaluated) 
    | Expr(e) -> (env, Some (eval_expr env e))
    | NoOp -> (env, None)
