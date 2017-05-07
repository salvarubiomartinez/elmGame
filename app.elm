import Html exposing (Html, div, p)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Time exposing (Time, second)
import Keyboard exposing (..)



main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


-- MODEL

type alias Model = {x: Int, y: Int, char: KeyCode, position: String, enemies: List (Int, Int)}


init : (Model, Cmd Msg)
init =
  ({x = 20, y = 100, char = 0, position = "20,100 20,120 70,110" , enemies = [(0,50),(0,100),(0,150)]}, Cmd.none)


-- UPDATE

type Msg
  = Move KeyCode | Tick Time


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Move code ->
        let movY = 
            -- up
            if code == 38 then model.y - 2 else
            -- down
            if code == 40 then model.y + 2 else model.y
            movX =
            -- left
            if code == 37 then model.x - 2 else
            -- right
            
            if code == 39 then model.x + 2 else model.x
            firstPoint = toString (movX) ++ "," ++ toString (movY)
            secondPoint = toString (movX) ++ "," ++ toString (movY + 20)
            thirdPoint = toString (movX + 50) ++ "," ++ toString (movY + 10)
            newPosition = firstPoint ++ " " ++ secondPoint ++ " " ++ thirdPoint
        in 
            ({model | char = code, y = movY, x = movX, position = newPosition}, Cmd.none)
    Tick time ->
        let
            enemy = List.head model.enemies
            enemiePos = 
                case enemy of 
                    Nothing -> 400
                    Just (x,y) -> x
        in
        ({model | enemies = [(rem (enemiePos - 1) 400,50),(enemiePos - 1,100),(enemiePos - 1,150)]}, Cmd.none)


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch [
        downs Move,
        Time.every (20 * Time.millisecond) Tick]


-- VIEW

view : Model -> Html Msg
view model =
    div [][
        svg [ viewBox "0 0 400 200", width "1000px" ]
       ([ circle [ cx (toString model.x), cy (toString (model.y + 10)), r "10", fill "#0B79CE" ] [],
        polyline [ fill "blue", points model.position ] []
      ] ++ (List.map (\(x,y)-> circle [cx (toString (400 + x)), cy (toString y), r "5"][]) model.enemies)),
        
      p[][text (toString model.char)]
    ]
    