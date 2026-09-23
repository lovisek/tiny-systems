// ----------------------------------------------------------------------------
// Adding simple data types
// ----------------------------------------------------------------------------

type Expression = 
  | Constant of int
  | Binary of string * Expression * Expression
  | If of Expression * Expression * Expression
  | Variable of string
  | Application of Expression * Expression
  | Lambda of string * Expression
  | Let of string * Expression * Expression
  // NOTE: Added two types of expression for working with tuples
  | Tuple of Expression * Expression
  | TupleGet of bool * Expression

type Type = 
  | TyVariable of string
  | TyBool 
  | TyNumber 
  | TyList of Type
  | TyFunction of Type * Type
  // NOTE: Added type for tuples
  | TyTuple of Type * Type

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
  // DONE: Add case for 'TyFunction' (need to check both nested types)
  | TyFunction(ta, tb) -> (occursCheck vcheck ta) || (occursCheck vcheck tb)
  // DONE: Add case for 'TyTuple' (same as 'TyFunction')
  | TyTuple(ta, tb) -> (occursCheck vcheck ta) || (occursCheck vcheck tb)

let rec substType (subst:Map<string, Type>) ty = 
  // DONE: Apply all the specified substitutions to the type 'ty'
  // (that is, replace all occurrences of 'v' in 'ty' with 'subst.[v]')
  match ty with
  | TyBool -> TyBool
  | TyNumber -> TyNumber
  | TyList(t) -> TyList(substType subst t)
  | TyVariable(vName) -> if subst.ContainsKey vName then subst.[vName] else TyVariable(vName)
  // DONE: Add case for 'TyFunction' (need to substitute in both nested types)
  | TyFunction(ta, tb) -> TyFunction((substType subst ta), (substType subst tb))
  // DONE: Add case for 'TyTuple' (same as 'TyFunction')
  | TyTuple(ta, tb) -> TyTuple((substType subst ta), (substType subst tb))

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
  // DONE: Add case matching TyFunction(ta1, tb1) and TyFunction(ta2, tb2)
  // This generates two new constraints, equating the argument/return types.
  | (TyFunction(ta1, tb1), TyFunction(ta2, tb2))::cs ->
      solve ((ta1, ta2)::(tb1, tb2)::cs)
  | (_, TyFunction(ta, tb))::cs
  | (TyFunction(ta, tb), _)::cs -> failwith "Cannot be solved"
  // DONE: Add case for 'TyTuple' (same as 'TyFunction')
  | (TyTuple(ta1, tb1), TyTuple(ta2, tb2))::cs ->
      solve ((ta1, ta2)::(tb1, tb2)::cs)
  | (_, TyTuple(_, _))::cs
  | (TyTuple(_, _), _)::cs -> failwith "Cannot be solved"


// ----------------------------------------------------------------------------
// Constraint generation & inference
// ----------------------------------------------------------------------------

type TypingContext = Map<string, Type>

let newTyVariable = 
  let mutable n = 0
  fun () -> n <- n + 1; TyVariable(sprintf "_a%d" n)

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
  | Binary("*", e1, e2) ->
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
      // (fails for Let x = x in ...)
      if (not (ctx.ContainsKey v)) then failwith "Variable not defined in the given context" else ctx.[v], []
  | If(econd, etrue, efalse) ->
      // DONE: Call generate recursively on all three sub-expressions,
      // collect all constraints and add a constraint that (i) the type
      // of 'econd' is 'TyBool' and (ii) types of 'etrue' and 'efalse' match.
      let tcond, scond = generate ctx econd
      let ttrue, strue = generate ctx etrue
      let tfalse, sfalse = generate ctx efalse

      ttrue, scond @ strue @ sfalse @ [ tcond, TyBool; ttrue, tfalse ]
  | Let(v, e1, e2) ->
      // DONE: Generate type & constraints for 'e1' first and then
      // add the generated type to the typing context for 't2'.
      let t1, s1 = generate ctx e1
      let localCtx = ctx.Add (v, t1)
  
      let t2, s2 = generate localCtx e2

      t2, s1 @ s2
  | Lambda(v, e) ->
      let targ = newTyVariable()
      // DONE: We do not know what the type of the variable 'v' is, so we 
      // generate a new type variable and add that to the 'ctx'. The
      // resulting type will be 'TyFunction' with 'targ' as argument type.
      let newCtx = ctx.Add (v, targ)
      let te, se = generate newCtx e

      TyFunction(targ, te), se
  | Application(e1, e2) -> 
      // DONE: Tricky case! We cannot inspect the generated type of 'e1'
      // to see what the argument/return type of the function is. Instead,
      // we have to generate a new type variable and add a constraint.
      let rarg = newTyVariable()

      let t2, s2 = generate ctx e2
      let t1, s1 = generate ctx e1

      rarg, s1 @ s2 @ [t1, TyFunction(t2, rarg)]

  | Tuple(e1, e2) ->
      // DONE: Easy. The returned type is composed of the types of 'e1' and 'e2'.
      let t1, s1 = generate ctx e1
      let t2, s2 = generate ctx e2
      TyTuple(t1, t2), s1 @ s2

  | TupleGet(b, e) ->
      // DONE: Trickier. The type of 'e' is some tuple, but we do not know what.
      // We need to generate two new type variables and a constraint.
      let arg1 = newTyVariable()
      let arg2 = newTyVariable()

      let t, s = generate ctx e

      if b then (arg1, s @ [t, TyTuple(arg1, arg2)]) else (arg2, s @ [t, TyTuple(arg1, arg2)])

  

// ----------------------------------------------------------------------------
// Putting it together & test cases
// ----------------------------------------------------------------------------

let infer e = 
  let typ, constraints = generate Map.empty e 
  let subst = solve constraints
  let typ = substType (Map.ofList subst) typ
  //subst
  typ

// Basic tuple examples:
// * (2 = 21, 123)
// * (2 = 21, 123)#1
// * (2 = 21, 123)#2
let etup = Tuple(Binary("=", Constant(2), Constant(21)), Constant(123))
etup |> infer
TupleGet(true, etup) |> infer
TupleGet(false, etup) |> infer

// Interesting case with a nested tuple ('a * ('b * 'c) -> 'a * 'b)
// * fun x -> x#1, x#2#1
Lambda("x", Tuple(TupleGet(true, Variable "x"), 
  TupleGet(true, TupleGet(false, Variable "x"))))
|> infer

// Does not type check - 'int' is not a tuple!
// * (1+2)#1
TupleGet(true, Binary("+", Constant 1, Constant 2)) |> infer


// Combining functions and tuples ('b -> (('b -> 'a) -> ('b * 'a)))
// * fun x f -> (x, f x)   
Lambda("x", Lambda("f", 
  Tuple(Variable "x", 
    Application(Variable "f", Variable "x"))))
|> infer
