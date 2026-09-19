// ----------------------------------------------------------------------------
// 04 - Random function and (not quite correct) POKE
// ----------------------------------------------------------------------------
module TinyBASIC

type Value =
  | StringValue of string
  | NumberValue of int
  | BoolValue of bool

type Expression = 
  | Const of Value
  | Function of string * Expression list
  | Variable of string

type Command = 
  | Print of Expression
  | Run 
  | Goto of int
  | Assign of string * Expression
  | If of Expression * Command
  // NOTE: Clear clears the screen and Poke(x, y, e) puts a string 'e' at 
  // the console location (x, y). In C64, the actual POKE writes to a given
  // memory location, but we only use it for screen access here.
  | Clear
  | Poke of Expression * Expression * Expression

type State = 
  { Program : list<int * Command> 
    // DONE: Add variable context to the program state
    VariableContext : Map<string, Value>
    // DONE: You will need to include random number generator in the state!
    RandomGenerator : System.Random
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

// NOTE: Helper function that makes it easier to implement '>' and '<' operators
// (takes a function 'int -> int -> bool' and "lifts" it into 'Value -> Value -> Value')
let binaryRelOp f args = 
  match args with 
  | [NumberValue a; NumberValue b] -> BoolValue(f a b)
  | _ -> failwith "expected two numerical arguments"

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
    | "=", [NumberValue x; NumberValue y] -> BoolValue(x = y)
  // DONE: Add support for 'RND(N)' which returns a random number in range 0..N-1
  // and for binary operators ||, <, > (and the ones you have already, i.e., - and =).
  // To add < and >, you can use the 'binaryRelOp' helper above. You can similarly
  // add helpers for numerical operators and binary Boolean operators to make
  // your code a bit nicer.
    | "RND", [NumberValue limit] -> NumberValue(state.RandomGenerator.Next(0, limit-1))
    | "||", [BoolValue x; BoolValue y] -> BoolValue(x||y)
    | "<", [NumberValue x; NumberValue y] -> binaryRelOp (<) [NumberValue x; NumberValue y]
    | ">", [NumberValue x; NumberValue y] -> binaryRelOp (>) [NumberValue x; NumberValue y]
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
  // DONE: Implement two commands for screen manipulation
  | Clear ->
    System.Console.Clear()
    runNextLine state line
  | Poke (x, y, symbol) -> 
    let eX = evalExpression state x
    let eY = evalExpression state y
    let eSymbol = evalExpression state symbol
    let currX = System.Console.CursorLeft
    let currY = System.Console.CursorTop

    match eX, eY, eSymbol with
    | NumberValue eNX, NumberValue eNY, StringValue eSSymbol ->
      System.Console.CursorLeft <- eNX
      System.Console.CursorTop <- eNY
      System.Console.Write(eSSymbol)
      System.Console.CursorLeft <- currX
      System.Console.CursorTop <- currY
    | _ -> failwith "Incorrect coordinates in Poke command"

    runNextLine state line

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

// NOTE: Writing all the BASIC expressions is quite tedious, so this is a 
// very basic (and terribly elegant) trick to make our task a bit easier.
// We define a couple of shortcuts and custom operators to construct expressions.
// With these, we can write e.g.: 
//  'Function("RND", [Const(NumberValue 100)])' as '"RND" @ [num 100]' or 
//  'Function("-", [Variable("I"); Const(NumberValue 1)])' as 'var "I" .- num 1'
let num v = Const(NumberValue v)
let str v = Const(StringValue v)
let var n = Variable n
let (.||) a b = Function("||", [a; b])
let (.<) a b = Function("<", [a; b])
let (.>) a b = Function(">", [a; b])
let (.-) a b = Function("-", [a; b])
let (.=) a b = Function("=", [a; b])
let (@) s args = Function(s, args)

let empty = { Program = []; VariableContext = Map.empty; RandomGenerator = System.Random() } // DONE: Add random number generator!

// NOTE: Random stars generation. This has hard-coded max width and height (60x20)
// but you could use 'System.Console.WindowWidth'/'Height' here to make it nicer.
let stars = 
  [ Some 10, Clear
    Some 20, Poke("RND" @ [num 60], "RND" @ [num 20], str "*")
    Some 30, Assign("I", num 100)
    Some 40, Poke("RND" @ [num 60], "RND" @ [num 20], str " ")
    Some 50, Assign("I", var "I" .- num 1)
    Some 60, If(var "I" .> num 1, Goto(40)) 
    Some 100, Goto(20)
    None, Run
  ]

// NOTE: Make the cursor invisible to get a nicer stars animation
System.Console.CursorVisible <- false
runInputs empty stars |> ignore

