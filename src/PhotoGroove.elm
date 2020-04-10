{-  PURPOSE:    Elm In Action: example code
    2020.04.09  01  - Ch 2 - Step 1

-}

module PhotoGroove exposing (main)

import Html exposing (div, h1, img, text)
import Html.Attributes exposing (..)

view model =
    div [ class "content"]
        [h1 [] [ text "Photo Groove" ]
        , div [ id "thumbnails"]
                [ img [ src "http://elm-in-action.com/1.jpeg " ] []
                , img [ src "http://elm-in-action.com/2.jpeg " ] []
                , img [ src "http://elm-in-action.com/3.jpeg " ] []
                ]
        ]

main =
    view "no model yet"
    