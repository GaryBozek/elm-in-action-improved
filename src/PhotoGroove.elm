{-  PURPOSE:    Elm In Action: example code
    2020.04.09  GB  01  - Ch 2 - Step 1 - Create PhotoGroove
    2020.04.10  GB  02  - Add thumbnails to the model
                GB  03  - Add event processing
                GB  04  - Build out the module
    2020.04.11  GB  05  - Push to GitHub repository
    2020.04.13  GB  06  - Update and move README.md     TAG:  Chapter 3 - Compiler as Assistant
    2020.04.14  GB  07  - Ch 3 - Talking to Servers
                        - Handle the communication states


-}



module PhotoGroove exposing (main)


--import Array exposing (Array)
import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Random



--============================================================================
--=  APPLICATION CODE                                                        =
--============================================================================
    

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


type Status
    = Loading
    | Loaded (List Photo) String
    | Errored String


---------------------------
--  APPLICATION HELPERS  --
---------------------------


--============================================================================
--=  MODEL                                                                   =
--============================================================================


type alias Model =
    { status     : Status
    , chosenSize : ThumbnailSize
    }


initialModel : Model
initialModel =
    { status      = Loading
    , chosenSize  = Small
    }



--============================================================================
--=  UPDATE                                                                  =
--============================================================================


type Msg
    = ClickedPhoto      String
    | ClickedSize       ThumbnailSize
    | ClickedSurpriseMe
    | GotRandomPhoto    Photo


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedPhoto url ->
            ( { model | status = selectUrl url model.status }, Cmd.none )

        ClickedSize size ->
            ( { model | chosenSize = size }, Cmd.none )

        ClickedSurpriseMe ->
            case model.status of
                Loaded (firstPhoto :: otherPhotos) _ ->
                    Random.uniform firstPhoto otherPhotos
                        |> Random.generate GotRandomPhoto
                        |> Tuple.pair model
                
                Loaded [] _ ->
                    ( model, Cmd.none)
                
                Loading ->
                    ( model, Cmd.none )
                
                Errored errorMessage ->
                    ( model, Cmd.none )

        GotRandomPhoto photo ->
            ( { model | status = selectUrl photo.url model.status }, Cmd.none )
     

----------------------
--  UPDATE HELPERS  --
----------------------


selectUrl : String -> Status -> Status
selectUrl url status =
    case status of
        Loaded photos _ ->
            Loaded photos url
        
        Loading ->
            status
        
        Errored errorMessage ->
            status



--============================================================================
--=  VIEW                                                                    =
--============================================================================


view : Model -> Html Msg
view model =
    div 
        [ class "content" ] <|
        case model.status of
            Loaded photos selectedUrl ->
                viewLoaded photos selectedUrl model.chosenSize
            
            Loading ->
                []
            
            Errored errorMessage ->
                [ text ("Error: " ++ errorMessage) ]


viewLoaded : List Photo -> String -> ThumbnailSize -> List (Html Msg)
viewLoaded photos selectedUrl chosenSize = 
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
            , class ( sizeToString chosenSize ) 
            ]
            ( List.map ( viewThumbnail selectedUrl ) photos )
        , img
            [ class "large"
            , src ( urlPrefix ++ "large/" ++ selectedUrl )  -- uses image:  "http://elm-in-action.com/large/1.jpeg"
            ]
            []
        ]


--------------------
--  VIEW HELPERS  --
--------------------


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



--============================================================================
--=  MAIN PROGRAM                                                            =
--============================================================================


-- more traditional elm structure for an application
main : Program () Model Msg
main =
    Browser.element
        { init          = \flags -> ( initialModel, Cmd.none )
        , view          = view
        , update        = update
        , subscriptions = \model -> Sub.none
        }