module Shadow exposing (Shadow(..), buttons, class)

import Html exposing (Html)
import Shared exposing (..)


type Shadow
    = None
    | Normal
    | Medium
    | Large
    | Inner


class : Shadow -> String
class shadow =
    case shadow of
        None ->
            ""

        Normal ->
            "shadow"

        Medium ->
            "shadow-md"

        Large ->
            "shadow-lg"

        Inner ->
            "shadow-inner"


button : (Shadow -> a) -> Shadow -> Shadow -> Html a
button msg selected shadow =
    settingsButton
        { classNames = class shadow
        , buttonText = toString shadow
        , isSelected = shadow == selected
        , msg = msg shadow
        }


buttons : (Shadow -> a) -> Shadow -> List (Html a)
buttons msg selected =
    List.map (button msg selected)
        [ None
        , Inner
        , Normal
        , Medium
        , Large
        ]
