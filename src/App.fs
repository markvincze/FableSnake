module FableSnake

open Fable.Core
open Fable.Core.JsInterop
open Fable.Import

let blockSize = 20

type Direction = Up | Right | Down | Left

type ControlState = {
    Direction : Direction
}

type Point = int * int

type SnakeBlock = Point * Direction

type Snake = SnakeBlock list

type GameState = {
    Objective : Point
    Snake : Snake
}

type State = GameState * ControlState

let initialState = (
        {
            Objective = (4, 5)
            Snake = [ ((5, 2), Direction.Right); ((4, 2), Direction.Right); ((3, 2), Direction.Right); ((2, 2), Direction.Right) ]
        },
        {
            Direction = Right
        })

let move direction (x, y) =
    match direction with
    | Up -> x, y - 1
    | Right -> x + 1, y
    | Down -> x, y + 1
    | Left -> x - 1, y


let changeDirection newDir (gameState, controlState) =
    let (point, dir) :: tail = gameState.Snake
    
    let newDir =
        if List.forall (fun (p, _) -> p <> (move newDir point)) gameState.Snake
        then newDir
        else dir

    (
        { gameState with Snake = (point, newDir) :: tail },
        { controlState with Direction = newDir }
    )

let advanceSnake snake =
    let rec advanceSnakeRec snake nextDirection =
        match snake with
        | (point, dir) :: tail -> (move dir point, nextDirection) :: advanceSnakeRec tail dir
        | [] -> []
    
    match snake with
    | (_, dir) :: tail-> advanceSnakeRec snake dir
    | [] -> []
    
let advanceGameState gameState = { gameState with Snake = advanceSnake gameState.Snake }

let advance (gameState, controlState) = (advanceGameState gameState, controlState)

let render (ctx : Browser.CanvasRenderingContext2D) (gameState, controlState) =
    ctx.clearRect(0., 0., 800., 600.)
    ctx.fillStyle <- !^"rgb(200,0,0)"
    ctx.fillRect (
        (gameState.Objective |> fst) * blockSize |> float,
        (gameState.Objective |> snd) * blockSize |> float,
        float(blockSize),
        float(blockSize)
    )
    
    let renderSnakeBlock (x, y) =
        ctx.fillStyle <- !^"rgb(0,0,200)"
        ctx.fillRect (
            x * blockSize |> float,
            y * blockSize |> float,
            float(blockSize),
            float(blockSize)
        )
    
    gameState.Snake
    |> List.map fst
    |> List.iter renderSnakeBlock

let mutable state = initialState

let init() =
    Browser.document.addEventListener_keyup(
        fun e -> match int e.keyCode with
                 | 38 -> state <- changeDirection Up state
                 | 39 -> state <- changeDirection Right state
                 | 40 -> state <- changeDirection Down state
                 | 37 -> state <- changeDirection Left state
                 | _ -> ()

                 null
    )

    let canvas = Browser.document.getElementsByTagName_canvas().[0]
    canvas.width <- 800.
    canvas.height <- 600.
    
    let ctx = canvas.getContext_2d()
    render ctx state
    
    let rec update () =
        state <- advance state
        render ctx state
        Browser.window.setTimeout(update, 150) |> ignore
    
    update ()

init()