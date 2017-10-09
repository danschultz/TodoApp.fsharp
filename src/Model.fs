namespace Model

type Todo =
    { id: int
      description: string
      editing: bool
      completed: bool }

type App =
    { todos: Todo list
      nextId: int
      newTodo: string }

type Cmd =
    | Add
    | Remove of int
    | Toggle of int
    | UpdateField of string

module Todo =
    let create id desc =
        { id = id; description = desc; editing = false; completed = false }

    let toggle todo = { todo with completed = not todo.completed }

module App =
    let init() = { todos = []; nextId = 0; newTodo = "" }

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
