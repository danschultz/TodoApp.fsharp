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
    R.header [ ClassName "header" ] [
        R.h1 [] [ R.str "Todos" ]
        R.input [
            onEnter Add dispatch
            OnChange (fun ev -> !!ev.target?value |> UpdateField |> dispatch)
            Placeholder "New todo"
        ]
    ]

let footer model dispatch =
    let remainingTodos =
        model
        |> App.remainingTodos
        |> List.length
    let completedTodos =
        model
        |> App.completedTodos
        |> List.length

    R.footer [ ClassName "footer" ] [
        R.div [] [ R.str ((string remainingTodos) + " items left") ]
        R.div [] [
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
        R.div [] [
            R.button
                [ ClassName "clear-completed"
                  Hidden (completedTodos = 0)
                  OnClick (fun _ -> dispatch RemoveCompleted) ]
                [ R.str ("Clear completed (" + string completedTodos + ")") ]
        ]
    ]

let viewTodo todo dispatch =
    R.div [ OnDoubleClick (fun ev -> dispatch (BeginEditing todo.id)) ] [
        R.input
            [ Type "checkbox"
              ClassName "toggle"
              Checked todo.completed
              OnClick (fun ev -> dispatch (Toggle todo.id)) ]
        R.div [] [ R.str todo.description ]
        R.button [ OnClick (fun ev -> dispatch (Remove todo.id))] [ R.str "Delete" ]
    ]

let editTodo todo dispatch =
    R.div [] [
        R.input [
            DefaultValue todo.description
            OnInput (fun ev -> dispatch (Edit (todo.id, !!ev.target?value)))
            onEnter (EndEditing todo.id) dispatch
        ]
    ]

let todos todos dispatch =
    let view dispatch todo =
        R.li [ ClassName "todo" ] [
            (if not todo.editing then viewTodo todo dispatch else editTodo todo dispatch)
        ]
    R.ul [ ClassName "todos" ] (List.map (view dispatch) todos)

let root model dispatch =
    R.div [ ClassName "todos-app" ] [
        header model.newTodo dispatch
        todos (App.todos model) dispatch
        footer model dispatch
    ]
