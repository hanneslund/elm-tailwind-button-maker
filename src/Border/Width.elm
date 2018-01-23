module Border.Width exposing (Msg, Widths, classes, init, picker, update)

import Html exposing (Html, button, div, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Maybe.Extra as Maybe


type Side
    = All
    | Top
    | Right
    | Bottom
    | Left


type Width
    = None
    | One
    | Two
    | Four
    | Eight


type alias Widths =
    { top : Width
    , right : Width
    , bottom : Width
    , left : Width
    }


init : Widths
init =
    { top = None
    , right = None
    , bottom = None
    , left = None
    }


type Msg
    = SetAll Width
    | SetTop Width
    | SetRight Width
    | SetBottom Width
    | SetLeft Width


update : Msg -> Widths -> Widths
update msg widths =
    case msg of
        SetAll width ->
            { widths | top = width, right = width, bottom = width, left = width }

        SetTop width ->
            { widths | top = width }

        SetRight width ->
            { widths | right = width }

        SetBottom width ->
            { widths | bottom = width }

        SetLeft width ->
            { widths | left = width }


widthClass : Side -> Width -> String
widthClass side width =
    let
        sideStr =
            case side of
                All ->
                    ""

                Top ->
                    "-t"

                Right ->
                    "-r"

                Bottom ->
                    "-b"

                Left ->
                    "-l"

        widthStr =
            case width of
                None ->
                    ""

                One ->
                    ""

                Two ->
                    "-2"

                Four ->
                    "-4"

                Eight ->
                    "-8"
    in
    case width of
        None ->
            ""

        _ ->
            "border" ++ sideStr ++ widthStr


classes : Widths -> String
classes { top, right, bottom, left } =
    if top == right && top == bottom && top == left then
        widthClass All top
    else
        String.join " " <|
            List.filter (not << String.isEmpty)
                [ widthClass Top top
                , widthClass Right right
                , widthClass Bottom bottom
                , widthClass Left left
                ]


button : Side -> Maybe Width -> Width -> Html Msg
button side selected width =
    let
        txt =
            case width of
                None ->
                    "0"

                One ->
                    "1px"

                Two ->
                    "2px"

                Four ->
                    "4px"

                Eight ->
                    "8px"

        msg =
            case side of
                All ->
                    SetAll width

                Top ->
                    SetTop width

                Right ->
                    SetRight width

                Bottom ->
                    SetBottom width

                Left ->
                    SetLeft width

        bgClass isActive =
            if isActive then
                "bg-blue"
            else
                "bg-grey-light"

        background =
            selected
                |> Maybe.unwrap False ((==) width)
                |> bgClass
    in
    Html.button [ onClick msg, class (background ++ " p-2") ] [ text txt ]


buttons : Side -> Maybe Width -> List (Html Msg)
buttons side selected =
    List.map (button side selected) [ None, One, Two, Four, Eight ]


picker : Widths -> Html Msg
picker widths =
    let
        row =
            div [ class "flex span3 justify-center buttons-row" ]

        col =
            div [ class "flex flex-col-reverse buttons-column" ]
    in
    div [ class "pickergrid" ]
        [ row <| buttons Top (Just widths.top)
        , col <| buttons Left (Just widths.left)
        , div
            [ class
                ("grid bg-grey-lighter border-grey-dark transition-all m-4 rounded "
                    ++ classes widths
                )
            ]
            [ div [ class "m-auto" ] <|
                buttons All Nothing
            ]
        , col <| buttons Right (Just widths.right)
        , row <| buttons Bottom (Just widths.bottom)
        ]
