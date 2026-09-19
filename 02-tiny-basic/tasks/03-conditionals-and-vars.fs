// ----------------------------------------------------------------------------
// 03 - Add variables, conditionals and integer values
// ----------------------------------------------------------------------------
module TinyBASIC

type Value =
  | StringValue of string
  // NOTE: Added numerical and Boolean values
  | NumberValue of int
  | BoolValue of bool

type Expression = 
  | Const of Value
  // NOTE: Added functions and variables. Functions  are used for both 
  // functions (later) and binary operators (in this step). We use only
  // 'Function("-", [e1; e2])' and 'Function("=", [e1; e2])' in the demo.
  | Function of string * Expression list
  | Variable of string

type Command = 
  | Print of Expression
  | Run 
  | Goto of int
  // NOTE: Assign expression to a given variable and conditional that 
  // runs a given Command only if the expression evaluates to 'BoolValue(true)'
  | Assign of string * Expression
  | If of Expression * Command

type State = 
  { Program : list<int * Command> 
    // DONE: Add variable context to the program state
    VariableContext : Map<string, Value>
  }

// ----------------------------------------------------------------------------
// Utilities
// ----------------------------------------------------------------------------

let printValue value = 
  // DONE: Take 'value' of type 'Value', pattern match on it and print it nicely.
  match value with
  | StringValue str -> printf "%s" str
  // DONE: Add support for printing NumberValue and BoolValue
  | NumberValue num -> printf "%d" num
  | BoolValue bool -> printf "%b" bool

let getLine state line = 
  // DONE: Get a line with a given number from 'state.Program' (this can fail 
  // if the line is not there.) You need this in the 'Goto' command case below.
  let lineCommand = List.filter (fun (num, cmd) -> num = line) state.Program

  match lineCommand with
  | [(line, cmd)] -> (line, cmd)
  | [] -> failwith "Line not found"
  | _ -> failwith "Multiple lines with the same number"

let addLine state (line, cmd) = 
  // DONE: Add a given line to the program state. This should overwrite 
  // a previous line (if there is one with the same number) and also ensure
  // that state.Program is sorted by the line number.
  // HINT: Use List.filter and List.sortBy. Use F# Interactive to test them!
  let newProgram = 
    state.Program
    |> List.filter (fun (lnum, lcmd) -> lnum <> line)
    |> List.append [line, cmd]
    |> List.sortBy (fun (lnum, lcmd) -> lnum)
      
  { state with Program = newProgram }

// ----------------------------------------------------------------------------
// Evaluator
// ----------------------------------------------------------------------------

let rec evalExpression state expr =
  // DONE: Implement evaluation of expressions. The function should take 
  // 'Expression' and return 'Value'. In this step, it is trivial :-)  
  match expr with 
  | Const c -> c

  // DONE: Add support for 'Function' and 'Variable'. For now, handle just the two
  // functions we need, i.e. "-" (takes two numbers & returns a number) and "="
  // (takes two values and returns Boolean). Note that you can test if two
  // F# values are the same using '='. It works on values of type 'Value' too.
  //
  // HINT: You will need to pass the program state to 'evalExpression' 
  // in order to be able to handle variables!
  | Variable v ->
    match state.VariableContext.TryFind v with 
      | Some res -> res
      | _ -> failwith ("unbound variable: " + v)
  | Function (op, exprList) ->
    let values = exprList |> List.map (fun expr -> evalExpression state expr)
      
    match op, values with
    | "-", [NumberValue x; NumberValue y] -> NumberValue(x - y)
    | "=", [x; y] -> BoolValue(x = y)
    | _ -> failwith $"Unknown operator: {op}"

let rec runCommand state (line, cmd) =
  match cmd with 
  | Run ->
      let first = List.head state.Program    
      runCommand state first
  | Print(expr) -> 
    // DONE: Evaluate the expression and print the resulting value here!
    printValue (evalExpression state expr)

    runNextLine state line
  | Goto(line) -> 
    // DONE: Find the right line of the program using 'getLine' and call 
    // 'runCommand' recursively on the found line to evaluate it.
    runCommand state (getLine state line)
  
  // DONE: Implement assignment and conditional. Assignment should run the
  // next line after setting the variable value. 'If' is a bit trickier:
  // * 'L1: IF TRUE THEN GOTO <L2>' will continue evaluating on line 'L2'
  // * 'L1: IF FALSE THEN GOTO <L2>' will continue on line after 'L1'
  // * 'L1: IF TRUE THEN PRINT "HI"' will print HI and continue on line after 'L1'
  //
  // HINT: If <e> evaluates to TRUE, you can call 'runCommand' recursively with
  // the command in the 'THEN' branch and the current line as the line number.
  | Assign(var, expr) ->
    let result = evalExpression state expr
    let newVarCtx = 
      state.VariableContext.Add(var, result)

    runNextLine { state with VariableContext = newVarCtx } line
  | If (expr, currentCmd) ->
    let exprResult = evalExpression state expr
    
    if exprResult = BoolValue(true) 
    then runCommand state (line, currentCmd)
    else runNextLine state line

and runNextLine state line = 
  // DONE: Find a program line with the number greater than 'line' and evalaute
  // it using 'runCommand' (if found) or just return 'state' (if not found).
  let nextLine = List.filter (fun (num, _) -> num > line) state.Program |> List.tryHead
  
  match nextLine with // if Option.isNone nextLine then state else runCommand state nextLine.Value
  | Some next -> runCommand state next
  | None -> state

// ----------------------------------------------------------------------------
// Interactive program editing
// ----------------------------------------------------------------------------

let runInput state (line, cmd) = 
  // DONE: Simulate what happens when the user enters a line of code in the 
  // interactive terminal. If the 'line' number is 'Some ln', we want to 
  // insert the line into the right location of the program (addLine); if it
  // is 'None', then we want to run it immediately. To make sure that 
  // 'runCommand' does not try to run anything afterwards, you can pass 
  // 'System.Int32.MaxValue' as the line number to it (or you could use -1
  // and handle that case specially in 'runNextLine')
  match line with
  | Some num -> addLine state (num, cmd)
  | None -> runCommand state (-1, cmd)

let runInputs state cmds = 
  //DONE: Apply all the specified commands to the program state using 'runInput'.
  //This is a one-liner if you use 'List.fold' which has the following type:
  //  ('State -> 'T -> 'State) -> 'State -> list<'T> -> 'State
  List.fold (fun state input -> runInput state input) state cmds

// ----------------------------------------------------------------------------
// Test cases
// ----------------------------------------------------------------------------

let empty =
      { Program = []
        VariableContext = Map.empty } // DONE: Add empty variables to the initial state!

let testVariables = 
  [ Some 10, Assign("S", Const(StringValue "HELLO WORLD\n")) 
    Some 20, Assign("I", Const(NumberValue 1))
    Some 30, Assign("B", Function("=", [Variable("I"); Const(NumberValue 1)]))
    Some 40, Print(Variable "S") 
    Some 50, Print(Variable "I")
    Some 55, Print(Const(StringValue("\n"))) 
    Some 60, Print(Variable "B")
    Some 65, Print(Const(StringValue("\n")))
    Some 70, Print(Const(StringValue("\n")))
    Some 75, Print(Const(StringValue("\n")))
    Some 80, Print(Const(StringValue("\n")))
    None, Run ]

// NOTE: Simpler test program without 'If" (just variables and '=' function) 
runInputs empty testVariables |> ignore

let helloTen = 
  [ Some 10, Assign("I", Const(NumberValue 10))
    Some 20, If(Function("=", [Variable("I"); Const(NumberValue 1)]), Goto(60))
    Some 30, Print (Const(StringValue "HELLO WORLD\n")) 
    Some 40, Assign("I", Function("-", [ Variable("I"); Const(NumberValue 1) ]))
    Some 50, Goto 20
    Some 60, Print (Const(StringValue "")) 
    None, Run ]

// NOTE: Prints hello world ten times using conditionals
runInputs empty helloTen |> ignore