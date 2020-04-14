{-  PURPOSE:    Elm In Action: example code
    2020.04.09  GB  01  - Ch 2 - Step 1 - Create PhotoGroove
    2020.04.10  GB  02  - Add thumbnails to the model
                GB  03  - Add event processing
                GB  04  - Build out the module
    2020.04.11  GB  05  - Push to GitHub repository
    2020.04.13  GB  06  - Update and move README.md     TAG:  Chapter 3 - Compiler as Assistant
    2020.04.14  GB  07  - Ch 4 - Talking to Servers
                        - Handle the communication states
                    08  - Implementing HTTP Requests
                    09  - Decoding JSON


-}



module PhotoGroove exposing (main)


--import Array exposing (Array)
import Browser
import Html                             exposing (..)
import Html.Attributes                  exposing (..)
import Html.Events                      exposing (onClick)
import Http
import Json.Decode          as Decode   exposing (Decoder, int, list, string, succeed)
import Json.Decode.Pipeline as Pipeline exposing (optional, required)
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
    { url : String
    , size : Int
    , title : String
    }


type Status
    = Loading
    | Loaded (List Photo) String
    | Errored String


---------------------------
--  APPLICATION HELPERS  --
---------------------------


{--  REPLACED BELOW
photoDecoder : Decoder Photo
photoDecoder =
    map3
        (\url size title -> { url = url, size = size, title = title })
        (field "url"   string)
        (field "size"  int)
        (field "title" string)
--}


photoDecoder : Decoder Photo
photoDecoder =
    succeed Photo           -- replace the function with the Photo type alias constructor
--    succeed buildPhoto
        |> Pipeline.required "url"   string
        |> Pipeline.required "size"  int
        |> Pipeline.optional "title" string "(untitled)"
 

{--  replaced this function with the type alias constructor
buildPhoto : String -> Int -> String -> Photo
buildPhoto url size title =
    { url = url, size = size, title = title }
--}



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
      -- HTTP Requests
    | GotPhotos         ( Result Http.Error ( List Photo ) )


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

        GotPhotos (Ok photos) ->
            case photos of
                (first :: rest) ->
                    ( { model | status = Loaded photos first.url }, Cmd.none )
                    
                [] ->  
                    ( { model | status = Errored "0 photos found" }, Cmd.none )

        GotPhotos  (Err _) ->
            ( { model | status = Errored "Server error!" }, Cmd.none )


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
        [ src ( urlPrefix ++ thumb.url )
        , title ( thumb.title ++ " [" ++ String.fromInt thumb.size ++ " KB]" )
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


initialCmd : Cmd Msg
initialCmd =
    Http.get
        { url = "http://elm-in-action.com/photos/list.json"
        , expect = Http.expectJson GotPhotos ( Decode.list photoDecoder )
        }


-- more traditional elm structure for an application
-- main : Program () Model Msg  -->  alternative:  replace unit type "()" with Never (see elm/Core.Basics)
main : Program () Model Msg
main =
    Browser.element
        { init          = \_ -> ( initialModel, initialCmd )
        , view          = view
        , update        = update
        , subscriptions = \_ -> Sub.none
        }