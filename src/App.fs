module TodoApp

open Fable.Core
open Fable.Import
open Elmish
open Elmish.React
open Model

let update msg model =
    match msg with
    | Add -> App.addTodo model
    | Remove id -> App.removeTodo model id
    | Toggle id -> App.toggleTodo model id
    | UpdateField value -> { model with newTodo = value }

Program.mkSimple App.init update View.root
|> Program.withConsoleTrace
|> Program.withReact "app"
|> Program.run
