module Engine

let mapWidth = 15
let mapHeight = 15

let rnd = System.Random()

type Direction = Up | Right | Down | Left

type LifeState = Alive | Dead

type ControlState = {
    Direction : Direction
}

type Point = int * int

type SnakeBlock = Point * Direction

type Snake = {
    Head : SnakeBlock
    Tail : SnakeBlock list
}

type GameState = {
    Life : LifeState
    Objective : Point
    Snake : Snake
}

type State = GameState * ControlState

let initialState = (
        { Life = Alive
          Objective = (4, 5)
          Snake = { Head = ((5, 2), Direction.Right)
                    Tail = [ ((4, 2), Direction.Right); ((3, 2), Direction.Right); ((2, 2), Direction.Right) ] } },
        { Direction = Right })

let move direction (x, y) =
    match direction with
    | Up -> x, if y > 0 then y - 1 else mapWidth - 1
    | Right -> (if x < mapWidth - 1 then x + 1 else 0), y
    | Down -> x, if y < mapHeight - 1 then y + 1 else 0
    | Left -> (if x > 0 then x - 1 else mapHeight - 1), y

let opposite dir =
    match dir with
    | Up -> Down
    | Right -> Left
    | Down -> Up
    | Left -> Right

let changeDirection newDir (gameState, controlState) =
    let (point, dir) = gameState.Snake.Head

    // Can't turn 180 degrees
    let newDir =
        if move newDir point = fst gameState.Snake.Tail.Head
        then dir
        else newDir

    ({ gameState with Snake = { gameState.Snake with Head = (point, newDir) } },
     { controlState with Direction = newDir })

let advanceSnake snake collected =
    let newHead = (move (snd snake.Head) (fst snake.Head), (snd snake.Head))

    let rec advanceSnakeRec snake nextDirection collected last =
        match snake with
        | (point, dir) :: tail -> (move dir point, nextDirection) :: advanceSnakeRec tail dir collected (point, dir)
        | [] -> if collected
                then [last]
                else []

    { Head = newHead
      Tail = advanceSnakeRec snake.Tail (snd snake.Head) collected snake.Head }

let newObjective snake =
    let randomPos () = (rnd.Next(mapWidth), rnd.Next(mapHeight))

    let rec newObjectiveRec snake newPoint =
        let isTaken = snake.Head :: snake.Tail
                      |> List.exists (fun (p, _) -> p = newPoint)

        if isTaken
        then newObjectiveRec snake (randomPos ())
        else newPoint

    newObjectiveRec snake (randomPos ())

let advanceGameState gameState =
    match gameState.Life with
    | Dead -> gameState
    | Alive ->
        let (headPoint, headDir) = gameState.Snake.Head
        let collided = List.exists (fun (p, _) -> p = (move headDir headPoint)) gameState.Snake.Tail

        if collided
        then { gameState with Life = Dead }
        else
            let collected = gameState.Objective = (fst gameState.Snake.Head)

            { gameState with
                Snake = advanceSnake gameState.Snake collected
                Objective = if collected
                            then newObjective gameState.Snake
                            else gameState.Objective }

let advance (gameState, controlState) = (advanceGameState gameState, controlState)