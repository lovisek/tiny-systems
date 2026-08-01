// ----------------------------------------------------------------------------
// 05 - Add a simple data type - tuples
// ----------------------------------------------------------------------------

type Value = 
  | ValNum of int 
  | ValClosure of string * Expression * VariableContext
  // NOTE: A tuple value consisting of two other values.
  // (Think about why we have 'Value' here but 'Expression'
  // in the case of 'ValClosure' above!)
  | ValTuple of Value * Value

and Expression = 
  | Constant of int
  | Binary of string * Expression * Expression
  | Variable of string
  | Unary of string * Expression 
  | If of Expression * Expression * Expression
  | Application of Expression * Expression
  | Lambda of string * Expression
  | Let of string * Expression * Expression
  // NOTE: 'Tuple' represents two-element tuple constructor
  // and 'TupleGet' the destructor (accessing a value)
  // Use 'true' for #1 element, 'false' for #2. This is not
  // particularly descriptive, but it works OK enough.
  | Tuple of Expression * Expression
  | TupleGet of bool * Expression

and VariableContext = 
  Map<string, Value>

// ----------------------------------------------------------------------------
// Evaluator
// ----------------------------------------------------------------------------

let rec evaluate (ctx:VariableContext) e =
  match e with 
  | Constant n -> ValNum n
  | Binary(op, e1, e2) ->
      let v1 = evaluate ctx e1
      let v2 = evaluate ctx e2
      match v1, v2 with 
      | ValNum n1, ValNum n2 -> 
          match op with 
          | "+" -> ValNum(n1 + n2)
          | "*" -> ValNum(n1 * n2)
          | _ -> failwith "unsupported binary operator"
      | _ -> failwith "invalid argument of binary operator"
  | Variable(v) ->
      match ctx.TryFind v with 
      | Some res -> res
      | _ -> failwith ("unbound variable: " + v)

  // NOTE: You have the following from before
  | Unary(op, e) ->
      let v = evaluate ctx e
      match v with 
      | ValNum n ->
          match op with
          | "-" -> ValNum(-n)
          | _ -> failwith "unsupported unary operator"

  | If(e1, e2, e3) ->
      let v1 = evaluate ctx e1
      match v1 with 
      | ValNum n1 ->
          match n1 with
              | 1 -> 
                let v2 = evaluate ctx e2
                v2
              | _ -> 
                let v3 = evaluate ctx e3
                v3

  | Lambda(v, e) ->
      ValClosure(v, e, Map.empty)

  | Application(e1, e2) ->
      let v1 = evaluate ctx e1
      let v2 = evaluate ctx e2
      match v1, v2 with
      | ValNum num, ValClosure (var, exp, context) ->
        failwith "Wrong function call"
      | ValClosure (var, exp, context), ValNum num ->
          let context = context.Add(var, v2)
          let final = evaluate context exp
          final

  | Let(v, e1, e2) ->
    let v1 = evaluate ctx e1
    let ctx = ctx.Add(v, v1)
    evaluate ctx e2

  | Tuple(e1, e2) ->
      // TODO: Construct a tuple value here!
      let v1 = evaluate ctx e1
      let v2 = evaluate ctx e2
      ValTuple(v1, v2)

  | TupleGet(b, e) ->
      // TODO: Access #1 or #2 element of a tuple value.
      // (If the argument is not a tuple, this fails.)
      let tuple = evaluate ctx e
      match b, tuple with
        | true, ValTuple(e1, e2) -> 
            e1
        | false, ValTuple(e1, e2) ->
            e2
        | _ -> failwith "Incorrectly defined tuple"

// ----------------------------------------------------------------------------
// Test cases
// ----------------------------------------------------------------------------

// Data types - simple tuple example (using the e#1, e#2 notation for field access)
//   (2*21, 123)#1
//   (2*21, 123)#2
let ed1= 
  TupleGet(true, 
    Tuple(Binary("*", Constant(2), Constant(21)), 
      Constant(123)))
evaluate Map.empty ed1

let ed2 = 
  TupleGet(false, 
    Tuple(Binary("*", Constant(2), Constant(21)), 
      Constant(123)))
evaluate Map.empty ed2

// Data types - trying to get a first element of a value
// that is not a tuple (This makes no sense and should fail)
//   (42)#1
let ed3 = 
  TupleGet(true, Constant(42))
evaluate Map.empty ed3
