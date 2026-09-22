// ----------------------------------------------------------------------------
// 01 - Complete the simple numerical constraint solver
// ----------------------------------------------------------------------------

type Number =
  | Zero
  | Succ of Number
  | Variable of string


// NOTE: The four functions below currently return a wrong 
// result, but one that makes the code run. As you implement
// them (one by one), the tests should graudally start working.


let rec occursCheck (v:string) (n:Number) = 
  // DONE: Check if variable 'v' appears anywhere inside 'n'
  match n with
  | Zero -> false
  | Succ(num) -> occursCheck v num
  | Variable(vName) -> if v = vName then true else false

let rec substitute (v:string) (subst:Number) (n:Number) =
  // DONE: Replace all occurrences of variable 'v' in the
  // number 'n' with the replacement number 'subst'
  match n with
  | Zero -> Zero
  | Succ(num) -> Succ(substitute v subst num)
  | Variable(vName) -> if v = vName then subst else Variable(vName)

let substituteConstraints (v:string) (subst:Number) (constraints:list<Number * Number>) = 
  // DONE: Substitute 'v' for 'subst' (use 'substitute') in 
  // all numbers in all the constraints in 'constraints'
  // HINT: You can use 'List.map' to implement this.
  List.map (fun (num1, num2) -> ((substitute v subst num1), (substitute v subst num2))) constraints

let rec substituteAll (subst:list<string * Number>) (n:Number) =
  // TODO: Perform all substitutions specified  in 'subst' on the number 'n'
  // HINT: You can use 'List.fold' to implement this. Fold has a type:
  //   ('State -> 'T -> 'State) -> 'State -> List<'T> -> 'State
  // In this case, 'State will be the Number on which we want to apply 
  // the substitutions and List<'T> will be a list of substitutions.
  match n with
  | Zero -> Zero
  | Succ(num) -> Succ(substituteAll subst num)
  | Variable(vName) -> 
    List.fold 
      (fun currentResult (currentVName, currentNumber) -> 
        if currentVName = vName then currentNumber else currentResult) 
      Zero 
      subst

let rec solve constraints = 
  match constraints with 
  | [] -> []
  | (Succ n1, Succ n2)::constraints ->
      solve ((n1, n2)::constraints)
  | (Zero, Zero)::constraints -> solve constraints
  | (Succ _, Zero)::_ | (Zero, Succ _)::_ -> 
      failwith "Cannot be solved"
  | (n, Variable v)::constraints 
  | (Variable v, n)::constraints ->
      if occursCheck v n then failwith "Cannot be solved (occurs check)"
      let constraints = substituteConstraints v n constraints
      let subst = solve constraints
      let n = substituteAll subst n
      (v, n)::subst

// Should work: x = Zero
solve 
  [ Succ(Variable "x"), Succ(Zero) ]

// Should faild: S(Z) <> Z
solve 
  [ Succ(Succ(Zero)), Succ(Zero) ]

// Should fail: No 'x' such that S(S(x)) = S(Z)
solve 
  [ Succ(Succ(Variable "x")), Succ(Zero) ]

// Not done: Need to substitute x/Z in S(x)
solve 
  [ Succ(Variable "x"), Succ(Zero)
    Variable "y", Succ(Variable "x") ]

// Not done: Need to substitute z/Z in S(S(z))
solve 
  [ Variable "x", Succ(Succ(Variable "z"))
    Succ(Variable "z"), Succ(Zero) ]

// Not done: Need occurs check
solve
  [ Variable "x", Succ(Variable "x") ]
