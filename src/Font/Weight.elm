module Font.Weight exposing (..)

import Html exposing (Html)
import Shared exposing (..)


type Weight
    = Inherit
    | Hairline
    | Thin
    | Light
    | Normal
    | Medium
    | Semibold
    | Bold
    | Extrabold
    | Black


class : Weight -> String
class weight =
    case weight of
        Inherit ->
            ""

        Hairline ->
            "font-hairline"

        Thin ->
            "font-thin"

        Light ->
            "font-light"

        Normal ->
            "font-normal"

        Medium ->
            "font-medium"

        Semibold ->
            "font-semibold"

        Bold ->
            "font-bold"

        Extrabold ->
            "font-extrabold"

        Black ->
            "font-black"


button : (Weight -> a) -> Weight -> Weight -> Html a
button msg selected weight =
    settingsButton
        { classNames = class weight
        , buttonText = toString weight
        , isSelected = weight == selected
        , msg = msg weight
        }


buttons : (Weight -> a) -> Weight -> List (Html a)
buttons msg selected =
    List.map (button msg selected)
        [ Inherit
        , Hairline
        , Thin
        , Light
        , Normal
        , Medium
        , Semibold
        , Bold
        , Extrabold
        , Black
        ]
