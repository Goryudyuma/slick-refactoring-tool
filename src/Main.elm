module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Html exposing (Html, div, h1, img, text, input, br, textarea, pre)
import Html.Attributes exposing (src, value, placeholder, size, style)
import Html.Events exposing (onInput)
import Regex exposing (..)


---- MODEL ----


type alias Model =
    {
        textBox:String,
        textResult:String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( {textBox = "", textResult = ""}, Cmd.none )

preformat : String -> String
preformat s =
    let
        removeCRLF : String -> String
        removeCRLF =
            String.filter (\c -> not (String.contains (String.fromChar c) "\n"))

        replaceComma : String -> String
        replaceComma =
            String.replace ", " ",\n"

        replaceAny : String -> String -> String
        replaceAny any =
            String.replace any ("\n"++any++"\n")

        replaceAnyPrev : String -> String -> String
        replaceAnyPrev any =
            String.replace any ("\n"++any)

        replaceAnyNext : String -> String -> String
        replaceAnyNext any =
            String.replace any (any++"\n")

    in
        s
        |> removeCRLF
        |> replaceComma
        |> replaceAny "select"
        |> replaceAny "from"
        |> replaceAnyPrev "inner join"
        |> replaceAnyPrev "left outer join"
        |> replaceAny "where"

findNums : String -> String
findNums sx =
    --let
        --regexAll = Regex.find (Regex. "x(\\d+)")
    --in
        String.join sx ["1", "2"]

refactor : String -> String
refactor s =
    s
    |> preformat
    |> findNums

---- UPDATE ----


type Msg
    = ChangeTextBox String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeTextBox s ->( {model | textBox = s, textResult = refactor s}, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [
            textarea [ size 300, style "height" "200px", placeholder "New Task" , value model.textBox, onInput ChangeTextBox] [],
            br[][],
            pre [] [text model.textResult]
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
