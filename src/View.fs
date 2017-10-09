module View

open Fable.Import
open Fable.Helpers.React.Props
open Elmish.React
open Fable.Core.JsInterop
open Model

module R = Fable.Helpers.React

let [<Literal>] ENTER_KEY = 13.

let internal onEnter msg dispatch =
    function
    | (ev: React.KeyboardEvent) when ev.keyCode = ENTER_KEY ->
        ev.target?value <- ""
        dispatch msg
    | _ -> ()
    |> OnKeyDown

let header model dispatch =
    R.header [] [
        R.h1 [] [ R.str "Todos" ]
        R.input [
            onEnter Add dispatch
            OnChange (fun ev -> !!ev.target?value |> UpdateField |> dispatch)
            Placeholder "New todo"
        ]
    ]

let footer filter dispatch =
    R.footer [] [
        R.a [ Href "#/"
              OnClick (fun _ -> dispatch (ChangeVisibility All)) ]
            [ R.str "All" ]
        R.str " "
        R.a [ Href "#/active"
              OnClick (fun _ -> dispatch (ChangeVisibility Active)) ]
            [ R.str "Active" ]
        R.str " "
        R.a [ Href "#/completed"
              OnClick (fun _ -> dispatch (ChangeVisibility Completed))]
            [ R.str "Completed" ]
    ]

let todo todo dispatch =
    R.div [] [
        R.button [ OnClick (fun ev -> dispatch (Toggle todo.id))] [ R.str "" ]
        R.div [] [ R.str todo.description ]
        R.button [ OnClick (fun ev -> dispatch (Remove todo.id))] [ R.str "Delete" ]
    ]

let todos todos dispatch =
    R.div [] (List.map (fun i -> todo i dispatch) todos)

let root model dispatch =
    R.div [] [
        header model.newTodo dispatch
        todos (App.todos model) dispatch
        footer model.filter dispatch
    ]
