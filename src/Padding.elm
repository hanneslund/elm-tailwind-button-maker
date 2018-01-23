module Padding exposing (Msg, Paddings, classes, init, picker, update)

import Html exposing (Html, button, div, p, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Maybe.Extra as Maybe


type Side
    = All
    | Top
    | Right
    | Bottom
    | Left


type Padding
    = Zero
    | One
    | Two
    | Three
    | Four
    | Six
    | Eight
    | Px


type alias Paddings =
    { top : Padding
    , right : Padding
    , bottom : Padding
    , left : Padding
    }


init : Paddings
init =
    { top = Four
    , right = Four
    , bottom = Four
    , left = Four
    }


type Msg
    = SetAll Padding
    | SetTop Padding
    | SetRight Padding
    | SetBottom Padding
    | SetLeft Padding


update : Msg -> Paddings -> Paddings
update msg paddings =
    case msg of
        SetAll padding ->
            { paddings | top = padding, right = padding, bottom = padding, left = padding }

        SetTop padding ->
            { paddings | top = padding }

        SetRight padding ->
            { paddings | right = padding }

        SetBottom padding ->
            { paddings | bottom = padding }

        SetLeft padding ->
            { paddings | left = padding }


type ExtraSide
    = ESide Side
    | Horizontal
    | Vertical


paddingClass : ExtraSide -> Padding -> String
paddingClass extraside width =
    let
        sideStr =
            case extraside of
                ESide side ->
                    case side of
                        All ->
                            ""

                        Top ->
                            "t"

                        Right ->
                            "r"

                        Bottom ->
                            "b"

                        Left ->
                            "l"

                Horizontal ->
                    "x"

                Vertical ->
                    "y"

        paddingStr =
            case width of
                Zero ->
                    "-0"

                One ->
                    "-1"

                Two ->
                    "-2"

                Three ->
                    "-3"

                Four ->
                    "-4"

                Six ->
                    "-6"

                Eight ->
                    "-8"

                Px ->
                    "-px"
    in
    case width of
        Zero ->
            ""

        _ ->
            "p" ++ sideStr ++ paddingStr


classes : Paddings -> String
classes { top, right, bottom, left } =
    if top == right && top == bottom && top == left then
        paddingClass (ESide All) top
    else
        let
            horizontal =
                if right == left then
                    [ paddingClass Horizontal left ]
                else
                    [ paddingClass (ESide Left) left, paddingClass (ESide Right) right ]

            vertical =
                if top == bottom then
                    [ paddingClass Vertical top ]
                else
                    [ paddingClass (ESide Top) top, paddingClass (ESide Bottom) bottom ]

            both =
                List.filter (not << String.isEmpty) (horizontal ++ vertical)
        in
        String.join " " both


button : Side -> Maybe Padding -> Padding -> Html Msg
button side selected padding =
    let
        txt =
            case padding of
                Zero ->
                    "0"

                One ->
                    "0.25"

                Two ->
                    "0.5"

                Three ->
                    "0.75"

                Four ->
                    "1"

                Six ->
                    "1.5"

                Eight ->
                    "2"

                Px ->
                    "1px"

        msg =
            case side of
                All ->
                    SetAll padding

                Top ->
                    SetTop padding

                Right ->
                    SetRight padding

                Bottom ->
                    SetBottom padding

                Left ->
                    SetLeft padding

        bgClass isActive =
            if isActive then
                "bg-blue"
            else
                "bg-grey-light"

        background =
            selected
                |> Maybe.unwrap False ((==) padding)
                |> bgClass
    in
    Html.button [ onClick msg, class (background ++ " p-2 text-xs") ] [ text txt ]


buttons : Side -> Maybe Padding -> List (Html Msg)
buttons side selected =
    List.map (button side selected) [ Zero, Px, One, Two, Three, Four, Six, Eight ]


picker : Paddings -> Html Msg
picker paddings =
    let
        row =
            div [ class "flex span3 justify-center buttons-row" ]

        col =
            div [ class "flex flex-col-reverse buttons-column" ]
    in
    div []
        [ p [ class "text-sm mb-8" ] [ text "Paddings are in rem" ]
        , div [ class "pickergrid" ]
            [ row <| buttons Top (Just paddings.top)
            , col <| buttons Left (Just paddings.left)
            , div
                [ class
                    ("rounded m-4 bg-grey-dark transition-all "
                        ++ classes paddings
                    )
                ]
                [ div [ class "rounded h-full grid bg-grey-lighter" ]
                    [ div [ class "m-auto" ] <|
                        buttons All Nothing
                    ]
                ]
            , col <| buttons Right (Just paddings.right)
            , row <| buttons Bottom (Just paddings.bottom)
            ]
        ]
