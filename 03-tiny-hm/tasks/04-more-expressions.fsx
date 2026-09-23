// ----------------------------------------------------------------------------
// Type inference for binary operators and conditionals
// ----------------------------------------------------------------------------

type Expression = 
  | Constant of int
  | Binary of string * Expression * Expression
  | If of Expression * Expression * Expression
  | Variable of string
  // NOTE: Added three more kinds of expression from TinyML
  | Application of Expression * Expression
  | Lambda of string * Expression
  | Let of string * Expression * Expression

type Type = 
  | TyVariable of string
  | TyBool 
  | TyNumber 
  | TyList of Type
  // NOTE: Added type for functions (of single argument)
  | TyFunction of Type * Type

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


// ----------------------------------------------------------------------------
// Constraint generation & inference
// ----------------------------------------------------------------------------

type TypingContext = Map<string, Type>

// NOTE: You will need this helper in checking of Lambda and Application.
// It generates a new type variable each time you call 'newTypeVariable()'
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
  | Binary("=", e1, e2) ->
      // DONE: Similar to the case for '+' but returns 'TyBool'
      let t1, s1 = generate ctx e1
      let t2, s2 = generate ctx e2
      TyBool, s1 @ s2 @ [ t1, TyNumber; t2, TyNumber ]
  | Binary("*", e1, e2) ->
      let t1, s1 = generate ctx e1
      let t2, s2 = generate ctx e2
      TyNumber, s1 @ s2 @ [ t1, TyNumber; t2, TyNumber ]
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
  

// ----------------------------------------------------------------------------
// Putting it together & test cases
// ----------------------------------------------------------------------------

// Run both of the phases and return the resulting type
let infer e = 
  let typ, constraints = generate Map.empty e 
  let subst = solve constraints
  let typ = substType (Map.ofList subst) typ
  //subst
  typ


// NOTE: Using the above, you will end up with ugly random type variable
// names like '_a4' etc. You can improve this by collecting all the type
// variable names that appear in a type and substituting them with a 
// list of nice names. Useful bit of code to generate the substitution is:
//
//   Map.ofList [ for i, n in Seq.indexed ["_a4"; "_a5"] -> 
//     n, string('a' + char i) ]
//
// You would still need to write code to collect all type variables in a type.


// let x = 10 in x = 10
Let("x", Constant 10, Binary("=", Variable "x", Constant 10))
|> infer 

// let f = fun x -> x*2 in (f 20) + (f 1)
Let("f",
  Lambda("x", Binary("*", Variable("x"), Constant(2))),
  Binary("+", 
    Application(Variable("f"), Constant(20)),
    Application(Variable("f"), Constant(1)) 
  ))
|> infer

// fun x f -> f (f x)
Lambda("x", Lambda("f", 
  Application(Variable "f", Application(Variable "f", Variable "x"))))
|> infer

// fun f -> f f 
// This does not type check due to occurs check
Lambda("f", 
  Application(Variable "f", Variable "f"))
|> infer

// fun f -> f 1 + f (2 = 3) 
// This does not type check because argument of 'f' cannot be both 'int' and 'bool'
Lambda("f", 
  Binary("+",
    Application(Variable "f", Constant 1),
    Application(Variable "f", Binary("=", Constant 2, Constant 3))
  ))
|> infer
