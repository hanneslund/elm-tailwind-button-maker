module Colors exposing (Color(..), backgroundColor, borderColor, colorPicker, textColor)

import Html exposing (Html, button, div, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)


type Color
    = NoColor
    | Black
    | White
    | GreyLightest
    | GreyLighter
    | GreyLight
    | Grey
    | GreyDark
    | GreyDarker
    | GreyDarkest
    | RedLightest
    | RedLighter
    | RedLight
    | Red
    | RedDark
    | RedDarker
    | RedDarkest
    | OrangeLightest
    | OrangeLighter
    | OrangeLight
    | Orange
    | OrangeDark
    | OrangeDarker
    | OrangeDarkest
    | YellowLightest
    | YellowLighter
    | YellowLight
    | Yellow
    | YellowDark
    | YellowDarker
    | YellowDarkest
    | GreenLightest
    | GreenLighter
    | GreenLight
    | Green
    | GreenDark
    | GreenDarker
    | GreenDarkest
    | TealLightest
    | TealLighter
    | TealLight
    | Teal
    | TealDark
    | TealDarker
    | TealDarkest
    | BlueLightest
    | BlueLighter
    | BlueLight
    | Blue
    | BlueDark
    | BlueDarker
    | BlueDarkest
    | IndigoLightest
    | IndigoLighter
    | IndigoLight
    | Indigo
    | IndigoDark
    | IndigoDarker
    | IndigoDarkest
    | PurpleLightest
    | PurpleLighter
    | PurpleLight
    | Purple
    | PurpleDark
    | PurpleDarker
    | PurpleDarkest
    | PinkLightest
    | PinkLighter
    | PinkLight
    | Pink
    | PinkDark
    | PinkDarker
    | PinkDarkest


getColorClass : String -> Color -> String
getColorClass prefix color =
    let
        light =
            "-light"

        lighter =
            "-lighter"

        lightest =
            "-lightest"

        dark =
            "-dark"

        darker =
            "-darker"

        darkest =
            "-darkest"

        black =
            "black"

        white =
            "white"

        grey =
            "grey"

        red =
            "red"

        orange =
            "orange"

        yellow =
            "yellow"

        green =
            "green"

        teal =
            "teal"

        blue =
            "blue"

        indigo =
            "indigo"

        purple =
            "purple"

        pink =
            "pink"

        class =
            case color of
                NoColor ->
                    ""

                Black ->
                    black

                White ->
                    white

                GreyLightest ->
                    grey ++ lightest

                GreyLighter ->
                    grey ++ lighter

                GreyLight ->
                    grey ++ light

                Grey ->
                    grey

                GreyDark ->
                    grey ++ dark

                GreyDarker ->
                    grey ++ darker

                GreyDarkest ->
                    grey ++ darkest

                RedLightest ->
                    red ++ lightest

                RedLighter ->
                    red ++ lighter

                RedLight ->
                    red ++ light

                Red ->
                    red

                RedDark ->
                    red ++ dark

                RedDarker ->
                    red ++ darker

                RedDarkest ->
                    red ++ darkest

                OrangeLightest ->
                    orange ++ lightest

                OrangeLighter ->
                    orange ++ lighter

                OrangeLight ->
                    orange ++ light

                Orange ->
                    orange

                OrangeDark ->
                    orange ++ dark

                OrangeDarker ->
                    orange ++ darker

                OrangeDarkest ->
                    orange ++ darkest

                YellowLightest ->
                    yellow ++ lightest

                YellowLighter ->
                    yellow ++ lighter

                YellowLight ->
                    yellow ++ light

                Yellow ->
                    yellow

                YellowDark ->
                    yellow ++ dark

                YellowDarker ->
                    yellow ++ darker

                YellowDarkest ->
                    yellow ++ darkest

                GreenLightest ->
                    green ++ lightest

                GreenLighter ->
                    green ++ lighter

                GreenLight ->
                    green ++ light

                Green ->
                    green

                GreenDark ->
                    green ++ dark

                GreenDarker ->
                    green ++ darker

                GreenDarkest ->
                    green ++ darkest

                TealLightest ->
                    teal ++ lightest

                TealLighter ->
                    teal ++ lighter

                TealLight ->
                    teal ++ light

                Teal ->
                    teal

                TealDark ->
                    teal ++ dark

                TealDarker ->
                    teal ++ darker

                TealDarkest ->
                    teal ++ darkest

                BlueLightest ->
                    blue ++ lightest

                BlueLighter ->
                    blue ++ lighter

                BlueLight ->
                    blue ++ light

                Blue ->
                    blue

                BlueDark ->
                    blue ++ dark

                BlueDarker ->
                    blue ++ darker

                BlueDarkest ->
                    blue ++ darkest

                IndigoLightest ->
                    indigo ++ lightest

                IndigoLighter ->
                    indigo ++ lighter

                IndigoLight ->
                    indigo ++ light

                Indigo ->
                    indigo

                IndigoDark ->
                    indigo ++ dark

                IndigoDarker ->
                    indigo ++ darker

                IndigoDarkest ->
                    indigo ++ darkest

                PurpleLightest ->
                    purple ++ lightest

                PurpleLighter ->
                    purple ++ lighter

                PurpleLight ->
                    purple ++ light

                Purple ->
                    purple

                PurpleDark ->
                    purple ++ dark

                PurpleDarker ->
                    purple ++ darker

                PurpleDarkest ->
                    purple ++ darkest

                PinkLightest ->
                    pink ++ lightest

                PinkLighter ->
                    pink ++ lighter

                PinkLight ->
                    pink ++ light

                Pink ->
                    pink

                PinkDark ->
                    pink ++ dark

                PinkDarker ->
                    pink ++ darker

                PinkDarkest ->
                    pink ++ darkest
    in
    case color of
        NoColor ->
            ""

        _ ->
            prefix ++ class


textColor : Color -> String
textColor color =
    getColorClass "text-" color


backgroundColor : Color -> String
backgroundColor color =
    getColorClass "bg-" color


borderColor : Color -> String
borderColor color =
    getColorClass "border-" color


colorPicker : (Color -> msg) -> Color -> Html msg
colorPicker msg currentColor =
    let
        txt color =
            if color == currentColor then
                "❤️"
            else
                case color of
                    NoColor ->
                        "None"

                    _ ->
                        ""

        background color =
            case color of
                NoColor ->
                    "none-background"

                _ ->
                    backgroundColor color

        border color =
            if color == currentColor then
                ""
                -- " border-8 border-blue"
            else
                ""

        btn color =
            button
                [ class ("h-8 " ++ background color ++ border color)
                , onClick (msg color)
                ]
                [ text (txt color) ]

        colors =
            [ NoColor
            , White
            , Black
            , GreyLightest
            , GreyLighter
            , GreyLight
            , Grey
            , GreyDark
            , GreyDarker
            , GreyDarkest
            , RedLightest
            , RedLighter
            , RedLight
            , Red
            , RedDark
            , RedDarker
            , RedDarkest
            , OrangeLightest
            , OrangeLighter
            , OrangeLight
            , Orange
            , OrangeDark
            , OrangeDarker
            , OrangeDarkest
            , YellowLightest
            , YellowLighter
            , YellowLight
            , Yellow
            , YellowDark
            , YellowDarker
            , YellowDarkest
            , GreenLightest
            , GreenLighter
            , GreenLight
            , Green
            , GreenDark
            , GreenDarker
            , GreenDarkest
            , TealLightest
            , TealLighter
            , TealLight
            , Teal
            , TealDark
            , TealDarker
            , TealDarkest
            , BlueLightest
            , BlueLighter
            , BlueLight
            , Blue
            , BlueDark
            , BlueDarker
            , BlueDarkest
            , IndigoLightest
            , IndigoLighter
            , IndigoLight
            , Indigo
            , IndigoDark
            , IndigoDarker
            , IndigoDarkest
            , PurpleLightest
            , PurpleLighter
            , PurpleLight
            , Purple
            , PurpleDark
            , PurpleDarker
            , PurpleDarkest
            , PinkLightest
            , PinkLighter
            , PinkLight
            , Pink
            , PinkDark
            , PinkDarker
            , PinkDarkest
            ]

        rowClass =
            "flex h-8"
    in
    div [ class "border p-1 colorpickergrid" ] <|
        List.map btn colors
