module Shared exposing (..)

import Html exposing (Html, button, div, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)


type alias SettingsButton msg =
    { classNames : String
    , buttonText : String
    , isSelected : Bool
    , msg : msg
    }


settingsButton : SettingsButton msg -> Html.Html msg
settingsButton { classNames, buttonText, isSelected, msg } =
    let
        background =
            if isSelected then
                "bg-blue"
            else
                "bg-grey-light"
    in
    button
        [ onClick msg
        , class (background ++ " text-sm rounded mb-4 mr-4 p-4 align-top " ++ classNames)
        ]
        [ text buttonText ]
