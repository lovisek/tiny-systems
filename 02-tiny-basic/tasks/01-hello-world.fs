// ----------------------------------------------------------------------------
// 01 - Add GOTO and better PRINT for infinite loop fun!
// ----------------------------------------------------------------------------

// NOTE: You can run this using 'dotnet run' from the terminal. 
// If you want to run code in a different file, you will need to change
// the 'tinybasic.fsproj' file (which references this source file now).

// NOTE: F# code in projects is generally organized using namespaces and modules.
// Here, we declare module name for the source code in this file.
module TinyBASIC

type Value =
  | StringValue of string

type Expression = 
  | Const of Value

type Command = 
  | Print of Expression
  | Run 
  // NOTE: GOTO specified line number. Note that this is an integer, rather 
  // than an expression, so you cannot calculate line number dynamically. 
  // (But there are tricks to do this by direct memory access on a real C64!)
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
  | Print(expr) ->
      // DONE: Evaluate the expression and print the resulting value here!
      printValue (evalExpression expr)
      
      runNextLine state line
  | Run ->
      let first = List.head state.Program    
      runCommand state first
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
// Test cases
// ----------------------------------------------------------------------------

let helloOnce = 
  { Program = [ 
      10, Print (Const (StringValue "HELLO WORLD\n")) ] }

let helloInf = 
  { Program = [ 
      10, Print (Const (StringValue "HELLO WORLD\n")) 
      20, Goto 10 ] }

// NOTE: First try to get the following to work!
runCommand helloOnce (-1, Run) |> ignore

// NOTE: Then add 'Goto' and get the following to work!
runCommand helloInf (-1, Run) |> ignore