import Html exposing (..)
import Html.App as Html
import Html.Events exposing (..)
import Keyboard
import Time exposing (..)
import Element exposing (..)
import Collage exposing (..)
import Color exposing (..)
import Text
import Char
import Random exposing (..)

segmentDim = 15
appleRadius = 7.5
(width, height) = (600, 600)

main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

type Direction 
  = Up
  | Down
  | Left
  | Right

type alias Position = (Float, Float)

pos : Float -> Float -> Position
pos = (,)

type alias Snake = 
  { head: Position
  , tail: List Position
  , direction: Direction }

type alias Apple = Maybe Position

type alias Score = Int

type Model 
  = NotStarted
  | Started Snake Apple Score


type Msg 
  = Tick Time
  | KeyPress Keyboard.KeyCode
  | Spawn (Float, Position)

randPos = Random.pair (Random.float 0 1) (Random.float 0 1)

generator: Random.Generator (Float, Position)
generator = Random.pair (Random.float 0 1) randPos


initSnake : Snake
initSnake = 
  let head = (0, 0)
      tail = [1..8] |> List.map (\n -> pos (-n*segmentDim) 0)
  in { head=head, tail=tail, direction=Right }

init : (Model, Cmd Msg)
init = (NotStarted, Cmd.none)


subscriptions : Model -> Sub Msg
subscriptions model = 
  case model of
    NotStarted ->
      Keyboard.presses KeyPress

    Started _ _ _ ->
      Sub.batch
        [ Keyboard.presses KeyPress
        , Time.every (Time.inMilliseconds 50) Tick
        ]


view : Model -> Html Msg
view model = 
  let bg = rect (toFloat width) (toFloat height) |> filled grey
      content =
        case model of
          NotStarted -> 
            [txt "press SPACE to start"]

          Started snake apple score ->
            let head = rect segmentDim segmentDim |> filled green |> move snake.head 
                tail =
                  snake.tail
                  |> List.map (\pos -> 
                    rect segmentDim segmentDim |> filled green |> move pos)
                scoreLbl = txt (toString score)
            in case apple of
                Nothing -> scoreLbl::head::tail
                Just pos ->
                  (circle appleRadius |> filled orange |> move pos)::scoreLbl::head::tail
  in collage width height (bg::content)
     |> Element.toHtml

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case model of
    NotStarted ->
      case msg of
        KeyPress 32 -> 
          (Started initSnake Nothing 0, Cmd.none)
        
        _ -> 
          (model, Cmd.none)

    Started snake apple score ->
      case msg of
        KeyPress keyCode ->
          let newDir = getNewDirection keyCode snake.direction
              newSnake = { snake | direction=newDir }
          in (Started newSnake apple score, Cmd.none)

        Spawn (chance, (randX, randY)) ->
          if chance <= 0.1 then
            let newApple = spawnApple randX randY
            in (Started snake newApple score, Cmd.none)
          else
            (model, Cmd.none)

        Tick _ ->
          let newHead = getNewSegment snake.head snake.direction
              ateApple =
                case apple of
                  Just pos -> isOverlap newHead pos
                  Nothing -> False
              newTail = 
                if ateApple then
                  snake.head::snake.tail
                else
                  snake.head::(List.take (List.length snake.tail-1) snake.tail)
              newSnake = { snake | head=newHead, tail=newTail }
              (newApple, newScore) =
                if ateApple then
                  (Nothing, score+1)
                else 
                  (apple, score)
              gameOver = isGameOver newHead newTail
          in if gameOver then
              (NotStarted, Cmd.none)
             else if newApple == Nothing then
              (Started newSnake newApple newScore, Random.generate Spawn generator)
             else 
              (Started newSnake newApple newScore, Cmd.none)

txt : String -> Form
txt msg =
  msg
  |> Text.fromString
  |> Text.color black
  |> Text.monospace
  |> Element.centered
  |> Collage.toForm

getNewDirection : Char.KeyCode -> Direction -> Direction
getNewDirection keyCode currentDir =
  let (changeableDirs, newDir) =
    case Char.fromCode keyCode of
      'a' -> ([ Up, Down ], Left)
      'w' -> ([ Left, Right ], Up)
      'd' -> ([ Up, Down ], Right)
      's' -> ([ Left, Right ], Down)
      _  -> ([], currentDir)
  in if List.any ((==) currentDir) changeableDirs then newDir else currentDir

getNewSegment : Position -> Direction -> Position
getNewSegment (x, y) direction =
  case direction of
    Up    -> pos x (y+segmentDim)
    Down  -> pos x (y-segmentDim)
    Left  -> pos (x-segmentDim) y
    Right -> pos (x+segmentDim) y

isGameOver : Position -> List Position -> Bool
isGameOver newHead newTail =
  List.any ((==) newHead) newTail   
  || fst newHead > (width / 2)      
  || snd newHead > (height / 2)     
  || fst newHead < (-width / 2)     
  || snd newHead < (-height / 2)    

spawnApple : Float -> Float -> Apple
spawnApple randW randH =
  let x = randW * width - width / 2
      y = randH * height - height / 2
  in pos x y |> Just

isOverlap : Position -> Position -> Bool
isOverlap (snakeX, snakeY) (appleX, appleY) =
  let (xd, yd) = (appleX - snakeX, appleY - snakeY)
      distance = sqrt(xd * xd + yd * yd)
  in distance <= (appleRadius * 2)