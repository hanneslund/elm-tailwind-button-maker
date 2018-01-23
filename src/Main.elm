module Main exposing (..)

import Border.Radius
import Border.Style
import Border.Width
import Colors exposing (..)
import Font.Family
import Font.Size
import Font.Weight
import Html exposing (Html, a, beginnerProgram, button, div, li, span, text, ul)
import Html.Attributes exposing (class, href)
import Html.Events exposing (onClick)
import Opacity
import Padding
import Shadow


type Setting
    = BackgroundSetting
    | BorderSetting
    | BorderRadiusSetting
    | OpacitySetting
    | PaddingSetting
    | ShadowSetting
    | TypographySetting


type alias Border =
    { widths : Border.Width.Widths
    , style : Border.Style.Style
    , radiuses : Border.Radius.Radiuses
    , color : Color
    }


type alias Font =
    { family : Font.Family.Family
    , size : Font.Size.Size
    , weight : Font.Weight.Weight
    , color : Color
    }


type alias Model =
    { currentSetting : Setting
    , background : Color
    , border : Border
    , font : Font
    , opacity : Opacity.Opacity
    , paddings : Padding.Paddings
    , shadow : Shadow.Shadow
    }


type Msg
    = ChangeCurrentSetting Setting
    | SetBackground Color
    | SetBorderWidth Border.Width.Msg
    | SetBorderStyle Border.Style.Style
    | SetBorderRadius Border.Radius.Msg
    | SetBorderColor Color
    | SetOpacity Opacity.Opacity
    | SetPadding Padding.Msg
    | SetShadow Shadow.Shadow
    | SetFontColor Color
    | SetFontFamily Font.Family.Family
    | SetFontSize Font.Size.Size
    | SetFontWeight Font.Weight.Weight


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    let
        border =
            model.border

        font =
            model.font

        newModel =
            case msg of
                ChangeCurrentSetting setting ->
                    { model | currentSetting = setting }

                SetBackground color ->
                    { model | background = color }

                SetBorderWidth msg ->
                    { model | border = { border | widths = Border.Width.update msg model.border.widths } }

                SetBorderStyle style ->
                    { model | border = { border | style = style } }

                SetBorderRadius msg ->
                    { model | border = { border | radiuses = Border.Radius.update msg model.border.radiuses } }

                SetBorderColor color ->
                    { model | border = { border | color = color } }

                SetOpacity opacity ->
                    { model | opacity = opacity }

                SetPadding msg ->
                    { model | paddings = Padding.update msg model.paddings }

                SetShadow shadow ->
                    { model | shadow = shadow }

                SetFontColor color ->
                    { model | font = { font | color = color } }

                SetFontFamily family ->
                    { model | font = { font | family = family } }

                SetFontSize size ->
                    { model | font = { font | size = size } }

                SetFontWeight weight ->
                    { model | font = { font | weight = weight } }
    in
    ( newModel, Cmd.none )


view : Model -> Html Msg
view model =
    div [ class "flex p-8" ]
        [ div
            [ class "w-48 pt-4" ]
            [ ul [ class "list-reset" ] <|
                List.map
                    (link model)
                    [ BackgroundSetting
                    , BorderSetting
                    , BorderRadiusSetting
                    , OpacitySetting
                    , PaddingSetting
                    , ShadowSetting
                    , TypographySetting
                    ]
            ]
        , div [ class "w-full contentgrid" ]
            [ card <|
                [ heading1 (settingText model.currentSetting)
                , settingContent model
                ]
            , card <|
                [ heading1 "Preview"
                , div [ class "border" ] <|
                    List.map (previewButton (getButtonClass model))
                        [ "bg-white", "bg-grey-lighter", "bg-grey", "bg-grey-darker", "bg-black" ]
                ]
            , div [ class ("span2 bg-black p-4 font-mono text-sm rounded-lg " ++ textColor Grey) ]
                [ text "<"
                , span [ class (textColor Red) ] [ text "button " ]
                , span [ class (textColor Orange) ] [ text "class" ]
                , text "="
                , span [ class (textColor Green) ] [ text ("\"" ++ getButtonClass model ++ "\"") ]
                , text ">"
                , text "Button text"
                , text "</"
                , span [ class (textColor Red) ] [ text "button" ]
                , text ">"
                ]
            ]
        ]


getButtonClass : Model -> String
getButtonClass model =
    String.join " " <|
        List.filter (not << String.isEmpty)
            [ backgroundColor model.background
            , Border.Width.classes model.border.widths
            , Border.Style.class model.border.style
            , Border.Radius.classes model.border.radiuses
            , borderColor model.border.color
            , Opacity.class model.opacity
            , Padding.classes model.paddings
            , Shadow.class model.shadow
            , Font.Family.class model.font.family
            , Font.Size.class model.font.size
            , Font.Weight.class model.font.weight
            , textColor model.font.color
            ]


card : List (Html msg) -> Html msg
card =
    div [ class "bg-white p-8 shadow rounded-lg" ]


link : Model -> Setting -> Html Msg
link model setting =
    let
        activeClass =
            if model.currentSetting == setting then
                " font-bold"
            else
                ""
    in
    li [ class "mb-2" ]
        [ a
            [ href "#"
            , onClick (ChangeCurrentSetting setting)
            , class ("no-underline hover:underline font-light text-black" ++ activeClass)
            ]
            [ text (settingText setting) ]
        ]


settingText : Setting -> String
settingText setting =
    case setting of
        BackgroundSetting ->
            "Background"

        BorderSetting ->
            "Border"

        BorderRadiusSetting ->
            "Border Radius"

        ShadowSetting ->
            "Shadow"

        OpacitySetting ->
            "Opacity"

        PaddingSetting ->
            "Padding"

        TypographySetting ->
            "Typography"


settingContent : Model -> Html Msg
settingContent model =
    let
        section =
            div [ class "mb-8" ]
    in
    case model.currentSetting of
        BackgroundSetting ->
            colorPicker SetBackground model.background

        BorderSetting ->
            div []
                [ heading2 "Width"
                , section [ Html.map SetBorderWidth <| Border.Width.picker model.border.widths ]
                , heading2 "Style"
                , section (Border.Style.buttons SetBorderStyle model.border.style)
                , heading2 "Color"
                , colorPicker SetBorderColor model.border.color
                ]

        BorderRadiusSetting ->
            div []
                [ section [ Html.map SetBorderRadius <| Border.Radius.picker model.border.radiuses ]
                ]

        OpacitySetting ->
            div [] (Opacity.buttons SetOpacity model.opacity)

        PaddingSetting ->
            div [] [ Html.map SetPadding (Padding.picker model.paddings) ]

        ShadowSetting ->
            div [] (Shadow.buttons SetShadow model.shadow)

        TypographySetting ->
            div []
                [ heading2 "Family"
                , section (Font.Family.buttons SetFontFamily model.font.family)
                , heading2 "Size"
                , section (Font.Size.buttons SetFontSize model.font.size)
                , heading2 "Weight"
                , section (Font.Weight.buttons SetFontWeight model.font.weight)
                , heading2 "Color"
                , section [ colorPicker SetFontColor model.font.color ]
                ]


heading1 : String -> Html Msg
heading1 txt =
    div [ class "mb-4 text-xl font-bold" ] [ text txt ]


heading2 : String -> Html Msg
heading2 txt =
    div [ class "uppercase mb-4 text-sm" ] [ text txt ]


previewButton : String -> String -> Html Msg
previewButton buttonClass bgColor =
    div [ class ("py-8 flex justify-center " ++ bgColor) ]
        [ button
            [ class (buttonClass ++ " transition-all") ]
            [ text "Button text" ]
        ]


initalModel : Model
initalModel =
    { currentSetting = BackgroundSetting
    , background = GreyLight
    , border =
        { widths = Border.Width.init
        , style = Border.Style.Solid
        , radiuses = Border.Radius.init
        , color = NoColor
        }
    , opacity = Opacity.Full
    , paddings = Padding.init
    , shadow = Shadow.None
    , font =
        { family = Font.Family.Inherit
        , size = Font.Size.Inherit
        , weight = Font.Weight.Inherit
        , color = NoColor
        }
    }


main : Program Never Model Msg
main =
    Html.program
        { init = ( initalModel, Cmd.none )
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }
