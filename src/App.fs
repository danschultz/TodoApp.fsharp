module TodoApp

open Fable.Core
open Fable.Import
open Elmish
open Elmish.React
open Elmish.Debug
open Elmish.HMR
open Model

let update msg model =
    match msg with
    | Add -> App.addTodo model
    | Remove id -> App.removeTodo model id
    | Toggle id -> App.toggleTodo model id
    | ChangeVisibility filter -> App.changeVisibility model filter
    | UpdateField value -> { model with newTodo = value }

Program.mkSimple App.init update View.root
|> Program.withDebugger
|> Program.withHMR
|> Program.withReact "app"
|> Program.run
