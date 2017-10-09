namespace Model

type Todo =
    { id: int
      description: string
      editing: bool
      completed: bool }

type VisibilityFilter =
    | All
    | Active
    | Completed

type App =
    { todos: Todo list
      nextId: int
      newTodo: string
      filter: VisibilityFilter }

type Cmd =
    | Add
    | Remove of int
    | Toggle of int
    | BeginEditing of int
    | Edit of int * string
    | EndEditing of int
    | ChangeVisibility of VisibilityFilter
    | UpdateField of string

module Todo =
    let create id desc =
        { id = id; description = desc; editing = false; completed = false }

    let toggle todo = { todo with completed = not todo.completed }

    let beginEditing todo = { todo with editing = true }

    let edit desc todo = { todo with description = desc }

    let endEditing todo = { todo with editing = false }

    let isState filter todo =
        match filter with
        | All -> true
        | Completed -> todo.completed
        | Active -> not todo.completed

module App =
    let init() = { todos = []; nextId = 0; newTodo = ""; filter = All }

    let addTodo app =
        { app with
            nextId = app.nextId + 1
            newTodo = ""
            todos = app.todos @ [Todo.create app.nextId app.newTodo] }

    let removeTodo app todoId =
        { app with todos = List.filter (fun t -> t.id <> todoId) app.todos }

    let toggleTodo app todoId =
        let toggle todo = if todo.id = todoId then Todo.toggle todo else todo
        { app with todos = List.map toggle app.todos}

    let beginEditing app todoId =
        let edit todo = if todo.id = todoId then Todo.beginEditing todo else todo
        { app with todos = List.map edit app.todos }

    let edit app todoId desc =
        let edit todo = if todo.id = todoId then Todo.edit desc todo else todo
        { app with todos = List.map edit app.todos }

    let endEditing app todoId =
        let edit todo = if todo.id = todoId then Todo.endEditing todo else todo
        { app with todos = List.map edit app.todos }

    let changeVisibility app filter = { app with filter = filter }

    let todos app = List.filter (Todo.isState app.filter) app.todos

    let remainingTodos app = List.filter (Todo.isState Active) app.todos

    let completedTodos app = List.filter (Todo.isState Completed) app.todos

    let update msg app =
        match msg with
        | Add -> addTodo app
        | Remove id -> removeTodo app id
        | Toggle id -> toggleTodo app id
        | BeginEditing id -> beginEditing app id
        | Edit (id, desc) -> edit app id desc
        | EndEditing id -> endEditing app id
        | ChangeVisibility filter -> changeVisibility app filter
        | UpdateField value -> { app with newTodo = value }
