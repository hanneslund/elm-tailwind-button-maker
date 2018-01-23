module Border.Radius exposing (Msg, Radiuses, classes, init, picker, update)

import Html exposing (Html, button, div, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Maybe.Extra as Maybe


type Corner
    = All
    | TopLeft
    | TopRight
    | BottomRight
    | BottomLeft


type Radius
    = None
    | Small
    | Normal
    | Large
    | Full


type alias Radiuses =
    { topLeft : Radius
    , topRight : Radius
    , bottomRight : Radius
    , bottomLeft : Radius
    }


init : Radiuses
init =
    { topLeft = None
    , topRight = None
    , bottomRight = None
    , bottomLeft = None
    }


type Msg
    = SetAll Radius
    | SetTopLeft Radius
    | SetTopRight Radius
    | SetBottomRight Radius
    | SetBottomLeft Radius


update : Msg -> Radiuses -> Radiuses
update msg widths =
    case msg of
        SetAll radius ->
            { widths | topLeft = radius, topRight = radius, bottomRight = radius, bottomLeft = radius }

        SetTopLeft radius ->
            { widths | topLeft = radius }

        SetTopRight radius ->
            { widths | topRight = radius }

        SetBottomRight radius ->
            { widths | bottomRight = radius }

        SetBottomLeft radius ->
            { widths | bottomLeft = radius }


type CornerOrSide
    = IsCorner Corner
    | Top
    | Right
    | Bottom
    | Left


radiusClass : CornerOrSide -> Radius -> String
radiusClass cornerOrSide radius =
    let
        cornerStr =
            case cornerOrSide of
                IsCorner corner ->
                    case corner of
                        All ->
                            ""

                        TopLeft ->
                            "-tl"

                        TopRight ->
                            "-tr"

                        BottomRight ->
                            "-br"

                        BottomLeft ->
                            "-bl"

                Top ->
                    "-t"

                Right ->
                    "-r"

                Bottom ->
                    "-b"

                Left ->
                    "-l"

        radiusStr =
            case radius of
                None ->
                    ""

                Small ->
                    "-sm"

                Normal ->
                    ""

                Large ->
                    "-lg"

                Full ->
                    "-full"
    in
    case radius of
        None ->
            ""

        _ ->
            "rounded" ++ cornerStr ++ radiusStr


classes : Radiuses -> String
classes { topLeft, topRight, bottomRight, bottomLeft } =
    let
        generateClasses a b =
            List.filter (not << String.isEmpty) (a ++ b)
                |> String.join " "
    in
    if topLeft == topRight && topLeft == bottomRight && topLeft == bottomLeft then
        radiusClass (IsCorner All) topLeft
    else if topLeft == topRight || bottomLeft == bottomRight then
        let
            top =
                if topLeft == topRight then
                    [ radiusClass Top topLeft ]
                else
                    [ radiusClass (IsCorner TopLeft) topLeft
                    , radiusClass (IsCorner TopRight) topRight
                    ]

            bottom =
                if bottomLeft == bottomRight then
                    [ radiusClass Bottom bottomLeft ]
                else
                    [ radiusClass (IsCorner BottomLeft) bottomLeft
                    , radiusClass (IsCorner BottomRight) bottomRight
                    ]
        in
        generateClasses top bottom
    else
        let
            left =
                if topLeft == bottomLeft then
                    [ radiusClass Left topLeft ]
                else
                    [ radiusClass (IsCorner TopLeft) topLeft
                    , radiusClass (IsCorner BottomLeft) bottomLeft
                    ]

            right =
                if topRight == bottomRight then
                    [ radiusClass Right topRight ]
                else
                    [ radiusClass (IsCorner TopRight) topRight
                    , radiusClass (IsCorner BottomRight) bottomRight
                    ]
        in
        generateClasses left right


button : Corner -> Maybe Radius -> Radius -> Html Msg
button corner selected radius =
    let
        txt =
            case radius of
                None ->
                    "0"

                Small ->
                    "S"

                Normal ->
                    "M"

                Large ->
                    "L"

                Full ->
                    "Full"

        msg =
            case corner of
                All ->
                    SetAll radius

                TopLeft ->
                    SetTopLeft radius

                TopRight ->
                    SetTopRight radius

                BottomRight ->
                    SetBottomRight radius

                BottomLeft ->
                    SetBottomLeft radius

        bgClass isActive =
            if isActive then
                "bg-blue"
            else
                "bg-grey-light"

        background =
            selected
                |> Maybe.unwrap False ((==) radius)
                |> bgClass
    in
    Html.button [ onClick msg, class (background ++ " p-2") ] [ text txt ]


buttons : Corner -> Maybe Radius -> List (Html Msg)
buttons side selected =
    List.map (button side selected) [ None, Small, Normal, Large, Full ]


picker : Radiuses -> Html Msg
picker radiuses =
    let
        row =
            div [ class "flex justify-between" ]

        buttonsRow =
            div [ class "buttons-row" ]
    in
    div []
        [ row
            [ buttonsRow <| buttons TopLeft (Just radiuses.topLeft)
            , buttonsRow <| buttons TopRight (Just radiuses.topRight)
            ]
        , div
            [ class
                ("grid h-32 bg-grey-lighter transition-all my-4 "
                    ++ classes radiuses
                )
            ]
            [ div [ class "m-auto" ] <|
                buttons All Nothing
            ]
        , row
            [ buttonsRow <| buttons BottomLeft (Just radiuses.bottomLeft)
            , buttonsRow <| buttons BottomRight (Just radiuses.bottomRight)
            ]
        ]
