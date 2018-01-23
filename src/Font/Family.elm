module Font.Family exposing (..)

import Html exposing (Html)
import Shared exposing (..)


type Family
    = Inherit
    | Sans
    | Serif
    | Mono


class : Family -> String
class family =
    case family of
        Inherit ->
            ""

        Sans ->
            "font-sans"

        Serif ->
            "font-serif"

        Mono ->
            "font-mono"


button : (Family -> a) -> Family -> Family -> Html a
button msg selected family =
    settingsButton
        { classNames = class family
        , buttonText = toString family
        , isSelected = family == selected
        , msg = msg family
        }


buttons : (Family -> a) -> Family -> List (Html a)
buttons msg selected =
    List.map (button msg selected)
        [ Inherit
        , Sans
        , Serif
        , Mono
        ]
