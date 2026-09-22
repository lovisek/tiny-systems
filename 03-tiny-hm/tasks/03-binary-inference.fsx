// ----------------------------------------------------------------------------
// 03 - Type inference for binary operators and conditionals
// ----------------------------------------------------------------------------

// NOTE: Start with some basic expressions from TinyML
// This time, If requires a real Boolean argument and we have
// operators '+' (int -> int -> int) and '=' (int -> int -> bool)
type Expression = 
  | Constant of int
  | Binary of string * Expression * Expression
  | If of Expression * Expression * Expression
  | Variable of string

type Type = 
  | TyVariable of string
  | TyBool 
  | TyNumber 
  | TyList of Type

// ----------------------------------------------------------------------------
// Constraint solving
// ----------------------------------------------------------------------------

let rec occursCheck vcheck ty =
  // DONE: Return true if type 'ty' contains variable 'vcheck'
  match ty with
  | TyBool -> false
  | TyNumber -> false
  | TyList(ty) -> occursCheck vcheck ty
  | TyVariable(vName) -> if vcheck = vName then true else false

let rec substType (subst:Map<string, Type>) ty = 
  // DONE: Apply all the specified substitutions to the type 'ty'
  // (that is, replace all occurrences of 'v' in 'ty' with 'subst.[v]')
  match ty with
  | TyBool -> TyBool
  | TyNumber -> TyNumber
  | TyList(t) -> TyList(substType subst t)
  | TyVariable(vName) -> if subst.ContainsKey vName then subst.[vName] else TyVariable(vName)

let substConstrs (subst:Map<string, Type>) (cs:list<Type * Type>) = 
  // DONE: Apply substitution 'subst' to all types in constraints 'cs'
  List.map (fun (ty1, ty2) -> ((substType subst ty1), (substType subst ty2))) cs
 
 
let rec solve cs =
  match cs with 
  | [] -> []
  | (TyNumber, TyNumber)::cs -> solve cs
  // DONE: Fill in the remaining cases! You can closely follow the
  // example from task 1 - the logic here is exactly the same.
  | (TyList t1, TyList t2)::cs -> 
      solve ((t1, t2)::cs)
  | (TyBool, TyBool)::cs -> solve cs
  | (TyNumber, TyBool)::_
  | (TyBool, TyNumber)::_
  | (TyList _, TyNumber)::_
  | (TyNumber, TyList _)::_
  | (TyList _, TyBool)::_
  | (TyBool, TyList _)::_ ->
      failwith "Cannot be solved"
  | (t, TyVariable v)::cs
  | (TyVariable v, t)::cs ->
      if occursCheck v t then failwith "Cannot be solved (occurs check)"
      let constrs = substConstrs (Map.ofList [(v, t)]) cs
      let subst = solve constrs
      let t = substType (Map.ofList subst) t
      (v, t)::subst

// ----------------------------------------------------------------------------
// Constraint generation & inference
// ----------------------------------------------------------------------------

// Variable context to keep types of declared variables
// (those will typically be TyVariable cases, but don't have to)
type TypingContext = Map<string, Type>

let rec generate (ctx:TypingContext) e = 
  match e with 
  | Constant _ -> 
      // NOTE: If the expression is a constant number, we return
      // its type (number) and generate no further constraints.
      TyNumber, []

  | Binary("+", e1, e2) ->
      // NOTE: Recursively process sub-expressions, collect all the 
      // constraints and ensure the types of 'e1' and 'e2' are 'TyNumber'
      let t1, s1 = generate ctx e1
      let t2, s2 = generate ctx e2
      TyNumber, s1 @ s2 @ [ t1, TyNumber; t2, TyNumber ]

  | Binary("=", e1, e2) ->
      // DONE: Similar to the case for '+' but returns 'TyBool'
      let t1, s1 = generate ctx e1
      let t2, s2 = generate ctx e2
      TyBool, s1 @ s2 @ [ t1, TyNumber; t2, TyNumber ]

  | Binary(op, _, _) ->
      failwithf "Binary operator '%s' not supported." op

  | Variable v -> 
      // DONE: Just get the type of the variable from 'ctx' here.
      ctx.[v], []

  | If(econd, etrue, efalse) ->
      // DONE: Call generate recursively on all three sub-expressions,
      // collect all constraints and add a constraint that (i) the type
      // of 'econd' is 'TyBool' and (ii) types of 'etrue' and 'efalse' match.
      let tcond, scond = generate ctx econd
      let ttrue, strue = generate ctx etrue
      let tfalse, sfalse = generate ctx efalse

      ttrue, scond @ strue @ sfalse @ [ tcond, TyBool; ttrue, tfalse ]


// ----------------------------------------------------------------------------
// Test cases
// ----------------------------------------------------------------------------


// Simple expressions: x = 10 + x
// Assuming x:'a, infers that 'a = int
let e1 = 
  Binary("=",   
    Variable("x"), 
    Binary("+", Constant(10), Variable("x")))

let t1, cs1 = 
  generate (Map.ofList ["x", TyVariable "a"]) e1

solve cs1

// Simple expressions: if x then 2 + 1 else y
// Assuming x:'a, y:'b, infers 'a = bool, 'b = int
let e2 = 
  If(Variable("x"), 
    Binary("+", Constant(2), Constant(1)),
    Variable("y"))

let t2, cs2 = 
  generate (Map.ofList ["x", TyVariable "a"; "y", TyVariable "b"]) e2

solve cs2

// Simple expressions: if x then 2 + 1 else x
// Cannot be solved, because 'x' used as 'int' and 'bool'
let e3 = 
  If(Variable("x"), 
    Binary("+", Constant(2), Constant(1)),
    Variable("x"))

let t3, cs3 = 
  generate (Map.ofList ["x", TyVariable "a"; "y", TyVariable "b"]) e3

solve cs3
