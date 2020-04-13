{-  PURPOSE:    Elm In Action: example code
    2020.04.09  GB  01  - Ch 2 - Step 1 - Create PhotoGroove
    2020.04.10  GB  02  - Add thumbnails to the model
                GB  03  - Add event processing
                GB  04  - Build out the module
    2020.04.11  GB  05  - Push to GitHub repository
            13  GB  06  - Update and move README.md

-}



module PhotoGroove exposing (main)


import Array exposing (Array)
import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Random



-------------------------
--  APPLICATION CODE   --
-------------------------
    

-- Base URL for images
urlPrefix : String
urlPrefix =
    "http://elm-in-action.com/"
 

type ThumbnailSize
    = Small
    | Medium
    | Large


type alias Photo =
    { url : String }


photoArray : Array Photo
photoArray =
    Array.fromList initialModel.photos



-------------
--  MODEL  --
-------------


type alias Model =
    { photos      : List Photo
    , selectedUrl : String
    , chosenSize  : ThumbnailSize
    }


initialModel : Model
initialModel =
    { photos = 
        [ { url = "1.jpeg" }
        , { url = "2.jpeg" }
        , { url = "3.jpeg" }
        ]
    , selectedUrl = "1.jpeg"
    , chosenSize  = Small
    }



---------------
--  HELPERS  --
---------------


-- Create the image url  
viewThumbnail selectedUrl thumb =
    img
        [ src (urlPrefix ++ thumb.url)
        , classList [ ( "selected"
                      , selectedUrl == thumb.url 
                      ) 
                    ]
        , onClick ( ClickedPhoto thumb.url )
        ]
        []


viewSizeChooser : ThumbnailSize -> Html Msg
viewSizeChooser size =
    label 
        []
        [ input [ type_ "radio"
                , name  "size"
                , onClick ( ClickedSize size ) 
                ] 
                [] 
        , text ( sizeToString size )
        ]


sizeToString : ThumbnailSize -> String
sizeToString size =
    case size of
        Small ->
            "small"

        Medium ->
            "med"

        Large ->
            "large"


getPhotoUrl : Int -> String
getPhotoUrl index =
    case Array.get index photoArray of
        Just photo ->
            photo.url

        Nothing ->
            ""


randomPhotoPicker : Random.Generator Int
randomPhotoPicker =
    Random.int 0 ( Array.length photoArray - 1 )



------------------
----  UPDATE  ----
------------------


type Msg
    = ClickedPhoto      String
    | ClickedSize       ThumbnailSize
    | ClickedSurpriseMe
    | GotSelectedIndex  Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedPhoto url ->
            ( { model | selectedUrl = url }, Cmd.none )

        ClickedSize size ->
            ( { model | chosenSize = size }, Cmd.none )

        ClickedSurpriseMe ->
            ( model, Random.generate GotSelectedIndex randomPhotoPicker )

        GotSelectedIndex index ->
            ( { model | selectedUrl = getPhotoUrl index }, Cmd.none )
        


------------
--  VIEW  --
------------


--view : Model -> Html Msg      -- book/online error:  Html not defined yet
view : Model -> Html Msg
view model =
    div 
        [ class "content" ]
        [ h1 
            [] 
            [ text "Photo Groove" ]
        , button
            [ onClick ClickedSurpriseMe ]
            [ text "Surprise Me!" ]
        , h3 
            [] 
            [ text "Thumbnail Size:" ]
        , div 
            [ id "choose-size" ]
            ( List.map viewSizeChooser [ Small, Medium, Large ] )
        , div 
            [ id "thumbnails"
            , class ( sizeToString model.chosenSize ) 
            ]
            ( List.map ( viewThumbnail model.selectedUrl ) model.photos )
        , img
            [ class "large"
            , src ( urlPrefix ++ "large/" ++ model.selectedUrl )  -- uses image:  "http://elm-in-action.com/large/1.jpeg"
            ]
            []
        ]
 

------------------------
----  MAIN PROGRAM  ----
------------------------


-- more traditional elm structure for an application
main : Program () Model Msg
main =
    Browser.element
        { init          = \flags -> ( initialModel, Cmd.none )
        , view          = view
        , update        = update
        , subscriptions = \model -> Sub.none
        }