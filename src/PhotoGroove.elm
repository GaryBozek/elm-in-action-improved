{-  PURPOSE:    Elm In Action: example code
    2020.04.09  GB  01  - Ch 2 - Step 1 - Create PhotoGroove
    2020.04.10  GB  02  - Add thumbnails to the model


-}


module PhotoGroove exposing (main)

import Html exposing (div, h1, img, text)
import Html.Attributes exposing (..)


-------------------------
--  APPLICATION CODE   --
-------------------------
    

-- Base URL for images
urlPrefix =
    "http://elm-in-action.com/"
 

-------------
--  MODEL  --
-------------


initialModel =
    { photos =
        [ { url = "1.jpeg" }
        , { url = "2.jpeg" }
        , { url = "3.jpeg" }
        ]
    , selectedUrl = "1.jpeg"        -- select first by default
    }

---------------
--  HELPERS  --
---------------


-- Create the image url  
viewThumbnail selectedUrl thumb =
    img
        [ src (urlPrefix ++ thumb.url)
        , classList [ ( "selected", selectedUrl == thumb.url ) ]
        ]
        []


------------------
----  UPDATE  ----
------------------


------------
--  VIEW  --
------------


view model =
    div [ class "content" ]
        [ h1 [] [ text "Photo Groove" ]
        , div [ id "thumbnails" ]
            (List.map (viewThumbnail model.selectedUrl) model.photos
            )
        , img
            [ class "large"
            , src (urlPrefix ++ "large/" ++ model.selectedUrl)  -- uses image:  "http://elm-in-action.com/large/1.jpeg"
            ]
            []
        ]
 

------------------------
----  MAIN PROGRAM  ----
------------------------


main =
    view initialModel
