module Font.Size exposing (..)

import Html exposing (Html)
import Shared exposing (..)


type Size
    = Inherit
    | ExtraSmall
    | Small
    | Normal
    | Large
    | ExtraLarge
    | TwoExtraLarge
    | ThreeExtraLarge
    | FourExtraLarge
    | FiveExtraLarge


class : Size -> String
class size =
    case size of
        Inherit ->
            ""

        ExtraSmall ->
            "text-xs"

        Small ->
            "text-sm"

        Normal ->
            "text-base"

        Large ->
            "text-lg"

        ExtraLarge ->
            "text-xl"

        TwoExtraLarge ->
            "text-2xl"

        ThreeExtraLarge ->
            "text-3xl"

        FourExtraLarge ->
            "text-4xl"

        FiveExtraLarge ->
            "text-5xl"


button : (Size -> a) -> Size -> Size -> Html a
button msg selected size =
    let
        txt =
            case size of
                Inherit ->
                    "Inherit"

                ExtraSmall ->
                    "XS"

                Small ->
                    "S"

                Normal ->
                    "Base"

                Large ->
                    "L"

                ExtraLarge ->
                    "XL"

                TwoExtraLarge ->
                    "2XL"

                ThreeExtraLarge ->
                    "3XL"

                FourExtraLarge ->
                    "4XL"

                FiveExtraLarge ->
                    "5XL"
    in
    settingsButton
        { classNames = class size
        , buttonText = txt
        , isSelected = size == selected
        , msg = msg size
        }


buttons : (Size -> a) -> Size -> List (Html a)
buttons msg selected =
    List.map (button msg selected)
        [ Inherit
        , ExtraSmall
        , Small
        , Normal
        , Large
        , ExtraLarge
        , TwoExtraLarge
        , ThreeExtraLarge
        , FourExtraLarge
        , FiveExtraLarge
        ]
