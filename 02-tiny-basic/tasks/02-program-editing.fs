// ----------------------------------------------------------------------------
// 02 - Implement interactive program editing
// ----------------------------------------------------------------------------
module TinyBASIC

type Value =
  | StringValue of string

type Expression = 
  | Const of Value

type Command = 
  | Print of Expression
  | Run 
  | Goto of int

type State = 
  { Program : list<int * Command> }

// ----------------------------------------------------------------------------
// Utilities
// ----------------------------------------------------------------------------

let printValue value = 
  // DONE: Take 'value' of type 'Value', pattern match on it and print it nicely.
  match value with
  | StringValue str -> printfn "%s" str
            
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

let rec evalExpression expr = 
  // DONE: Implement evaluation of expressions. The function should take 
  // 'Expression' and return 'Value'. In this step, it is trivial :-)  
  match expr with 
  | Const c -> c

let rec runCommand state (line, cmd) =
  match cmd with 
  | Run ->
    let first = List.head state.Program    
    runCommand state first
  | Print(expr) ->
    // DONE: Evaluate the expression and print the resulting value here!
    printValue (evalExpression expr)

    runNextLine state line
  | Goto(line) ->
    // DONE: Find the right line of the program using 'getLine' and call 
    // 'runCommand' recursively on the found line to evaluate it.
    runCommand state (getLine state line)

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

let helloOnce = 
  [ Some 10, Print (Const (StringValue "HELLO WORLD\n")) 
    Some 10, Print (Const (StringValue "HELLO NPRG077\n")) 
    None, Run ]

let helloInf = 
  [ Some 20, Goto 10
    Some 10, Print (Const (StringValue "HELLO WORLD\n")) 
    Some 10, Print (Const (StringValue "HELLO NPRG077\n")) 
    None, Run ]

let empty = { Program = [] }

runInputs empty helloOnce |> ignore
runInputs empty helloInf |> ignore
