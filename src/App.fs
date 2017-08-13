module FableSnake

open Fable.Core
open Fable.Core.JsInterop
open Fable.Import
open Engine

let blockSize = 20.
let mapWidthPixel = float(Engine.mapWidth) * blockSize
let mapHeightPixel = float(Engine.mapHeight) * blockSize

let render (ctx : Browser.CanvasRenderingContext2D) (gameState, controlState) =
    ctx.clearRect(0., 0., mapWidthPixel, mapHeightPixel)
    ctx.fillStyle <- !^"rgb(200,0,0)"
    ctx.fillRect (
        float(gameState.Objective |> fst) * blockSize,
        float(gameState.Objective |> snd) * blockSize,
        float(blockSize),
        float(blockSize)
    )

    let renderSnakeBlock (x, y) =
        ctx.fillStyle <- !^"rgb(0,0,200)"
        ctx.fillRect (
            float(x) * blockSize,
            float(y) * blockSize,
            float(blockSize),
            float(blockSize)
        )

    gameState.Snake.Head :: gameState.Snake.Tail
    |> List.map fst
    |> List.iter renderSnakeBlock

    // Render a triangle for the head
    let (x, y), headDir = gameState.Snake.Head
    ctx.fillStyle <- !^"rgb(0, 200, 0)"
    ctx.beginPath()
    match headDir with
    | Up ->
        ctx.moveTo(float(x) * blockSize, (float(y) + 0.5) * blockSize)
        ctx.lineTo((float(x) + 0.5) * blockSize, float(y) * blockSize)
        ctx.lineTo((float(x) + 1.) * blockSize, (float(y) + 0.5) * blockSize)
    | Right ->
        ctx.moveTo((float(x) + 0.5) * blockSize, float(y) * blockSize)
        ctx.lineTo((float(x) + 1.) * blockSize, (float(y) + 0.5) * blockSize)
        ctx.lineTo((float(x) + 0.5) * blockSize, (float(y) + 1.) * blockSize)
    | Down ->
        ctx.moveTo(float(x) * blockSize, (float(y) + 0.5) * blockSize)
        ctx.lineTo((float(x) + 0.5) * blockSize, (float(y) + 1.) * blockSize)
        ctx.lineTo((float(x) + 1.) * blockSize, (float(y) + 0.5) * blockSize)
    | Left ->
        ctx.moveTo((float(x) + 0.5) * blockSize, float(y) * blockSize)
        ctx.lineTo(float(x) * blockSize, (float(y) + 0.5) * blockSize)
        ctx.lineTo((float(x) + 0.5) * blockSize, (float(y) + 1.) * blockSize)
    ctx.fill()

let mutable state = Engine.initialState

let getById<'T when 'T :> Browser.HTMLElement> id =
    Browser.document.getElementById(id) :?> 'T

let restart() =
    state <- Engine.initialState

let init() =
    Browser.document.addEventListener_keyup(
        fun e -> match int e.keyCode with
                 | 38 -> state <- Engine.changeDirection Up state
                 | 39 -> state <- Engine.changeDirection Right state
                 | 40 -> state <- Engine.changeDirection Down state
                 | 37 -> state <- Engine.changeDirection Left state
                 | _ -> ()

                 null
    )

    let buttonNewGame = getById<Browser.HTMLButtonElement>("newGame")
    buttonNewGame.addEventListener_click(fun _ -> restart(); null)

    let canvas = getById<Browser.HTMLCanvasElement>("canvas")
    canvas.width <- mapWidthPixel
    canvas.height <- mapHeightPixel

    let ctx = canvas.getContext_2d()
    render ctx state

    let rec update () =
        state <- Engine.advance state
        render ctx state
        Browser.window.setTimeout(update, 115) |> ignore

    update ()

init()