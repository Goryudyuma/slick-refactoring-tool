module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Html exposing (Html, div, h1, img, text, input, br, textarea, pre, ul, li)
import Html.Attributes exposing (src, value, placeholder, size, style, type_, property)
import Html.Events exposing (onInput, onClick)
import Regex exposing (..)
import Json.Encode as Encode
import Dict.Extra exposing (groupBy)
import Dict


---- MODEL ----


type alias Model =
    {
        textBox:String,
        result:List ResultOneLine
    }

type alias ResultOneLine =
    {
        index:Int,
        deleteFlag:Bool,
        text:String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( {textBox = "", result = []}, Cmd.none )

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
        |> replaceAnyPrev "and"

findNums : String -> List String
findNums s =
    let
        location : Regex.Regex
        location =
            Maybe.withDefault Regex.never <|
            Regex.fromString "x\\d+"
        regexAll = Regex.find location
    in
        Dict.keys (Dict.filter (\k v -> List.length v == 1) (groupBy (\m -> m) (List.map .match (regexAll s))))

refactor : String -> List ResultOneLine
refactor s =
    let
        recommendNumList = findNums s

        location : String -> Regex.Regex
        location input =
            Maybe.withDefault Regex.never <|
            Regex.fromString (input ++ "[^\\d+]")

        recommend : List String -> String -> Bool
        recommend ls str =
            List.any (\l -> Regex.contains (location l) str) ls

        splitIndexed : String -> List (Int, String)
        splitIndexed ss =
            ss
                |> String.split "\n"
                |> List.filter (\one -> one /= "")
                |> List.indexedMap Tuple.pair

        changeType : String -> List ResultOneLine
        changeType ss =
            List.map (\m -> {deleteFlag=recommend recommendNumList (Tuple.second m), text=Tuple.second m, index=Tuple.first m}) (splitIndexed ss)
    in
    s
    |> preformat
    |> changeType

removeRecommend : List ResultOneLine -> String
removeRecommend lro =
    lro
        |> List.filter (\one -> not one.deleteFlag)
        |> List.map (\one -> one.text)
        |> List.filter (\one -> one /= "")
        |> String.join " \n"



---- UPDATE ----


type Msg
    = ChangeTextBox String
    | ToggleDelFlag Int
    | Execute


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        newTextBox = removeRecommend model.result
    in
    case msg of
        ChangeTextBox s ->( {model | textBox = s, result = refactor s}, Cmd.none )
        ToggleDelFlag i -> ({model | result=List.map (\r -> if r.index == i then {r | deleteFlag= not r.deleteFlag} else r) model.result}, Cmd.none)
        Execute -> ( {model | textBox = newTextBox, result = refactor newTextBox}, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [
            textarea [ size 300, style "height" "200px", placeholder "Query" , value model.textBox, onInput ChangeTextBox] [],
            input [ type_ "button", onClick Execute, value "Execute"][],
            br[][],
            ul [style "list-style-type" "none"] (List.map (\m -> viewOneLine m) model.result),
            text (String.join "\n" (findNums model.textBox))
        ]

viewOneLine : ResultOneLine -> Html Msg
viewOneLine rol =
    li[][
        div[][
            input [ type_ "checkbox", onClick (ToggleDelFlag rol.index) , property "checked" (Encode.bool rol.deleteFlag)] [],
            text rol.text
        ]
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
