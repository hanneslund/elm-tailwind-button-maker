module Opacity exposing (Opacity(..), buttons, class)

import Html exposing (Html)
import Shared exposing (..)


type Opacity
    = Full
    | ThreeQuarters
    | Half
    | OneQuarter


class : Opacity -> String
class opacity =
    case opacity of
        Full ->
            ""

        ThreeQuarters ->
            "opacity-75"

        Half ->
            "opacity-50"

        OneQuarter ->
            "opacity-25"


button : (Opacity -> a) -> Opacity -> Opacity -> Html a
button msg selected opacity =
    let
        txt =
            case opacity of
                Full ->
                    "100%"

                ThreeQuarters ->
                    "75%"

                Half ->
                    "50%"

                OneQuarter ->
                    "25%"
    in
    settingsButton
        { classNames = class opacity
        , buttonText = txt
        , isSelected = opacity == selected
        , msg = msg opacity
        }


buttons : (Opacity -> a) -> Opacity -> List (Html a)
buttons msg opacity =
    List.map (button msg opacity)
        [ Full
        , ThreeQuarters
        , Half
        , OneQuarter
        ]
