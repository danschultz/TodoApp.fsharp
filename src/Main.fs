module TodoApp

open Fable.Core
open Fable.Import
open Elmish
open Elmish.React
open Elmish.Debug
open Elmish.HMR
open Model

Program.mkSimple App.init App.update View.root
|> Program.withReact "app"
|> Program.withDebugger
|> Program.withHMR
|> Program.run
