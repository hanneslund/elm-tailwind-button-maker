module Border.Style exposing (Style(..), buttons, class)

import Html exposing (Html)
import Shared exposing (..)


type Style
    = Solid
    | Dashed
    | Dotted


class : Style -> String
class style =
    case style of
        Solid ->
            ""

        Dashed ->
            "border-dashed"

        Dotted ->
            "border-dotted"


button : (Style -> a) -> Style -> Style -> Html a
button msg selected style =
    let
        isSelected =
            style == selected

        borderColor =
            if isSelected then
                "border-blue-darker"
            else
                "border-grey-dark"
    in
    settingsButton
        { classNames = class style ++ " border-4 " ++ borderColor
        , buttonText = toString style
        , isSelected = style == selected
        , msg = msg style
        }


buttons : (Style -> a) -> Style -> List (Html a)
buttons msg selected =
    List.map (button msg selected)
        [ Solid
        , Dashed
        , Dotted
        ]
