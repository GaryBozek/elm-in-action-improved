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
                    10  - Ch 5 - Talking to JavaScript
                        - Rendering Custom Elements
    2020.04.15  GB  11  - Sending Data to JavaScript
                    12  - Getting Data from JS (subscriptions/flags)
    2020.04.16  GB  13  - Ch 6 - Testing
                        - Writing Unit Tests, JSON Decoders, Writing Fuzz Tests
                    14  - Testing Update, multiple test in one function
    2020.04.17  GB  15  - Test View, DOM structure, User Interactions
    2020.04.20  GB  21  - Delegating Pages
                        - rename to PhotoGallery.elm
                        - expose (Model, Msg, init, update, view)
    2020.04.21  GB  22  - deduplicate urlPrefix

-}



port module PhotoGallery exposing (Model, Msg, init, subscriptions, update, view)
--port module PhotoGroove exposing (main, Model, Msg(..), Photo, Status(..), initialModel, photoDecoder, update, urlPrefix, view)


--import Array exposing (Array)
import Browser
import Common
import Html                             exposing (..)
import Html.Attributes      as Attr     exposing (..)
import Html.Events                      exposing (on, onClick)
import Http
import Json.Decode          as Decode   exposing (Decoder, at, int, list, string, succeed)
import Json.Decode.Pipeline as Pipeline exposing (optional, required)
import Json.Encode          as Encode
import Random



--============================================================================
--=  MAIN PROGRAM                                                            =
--============================================================================


-- more traditional elm structure for an application
-- main : Program () Model Msg  -->  alternative:  replace unit type "()" with Never (see elm/Core.Basics)
main : Program Float Model Msg
main =
    Browser.element
        { init          = init
        , view          = view
        , update        = update
        , subscriptions = subscriptions
        }


init : Float -> ( Model, Cmd Msg )
init flags =
    let
        activity =
            "Initializing Pasta v" ++ String.fromFloat flags
    in
    ( { initialModel | activity = activity }
    , initialCmd 
    )    


initialCmd : Cmd Msg
initialCmd =
    Http.get
        { url = "http://elm-in-action.com/photos/list.json"
        , expect = Http.expectJson GotPhotos ( Decode.list photoDecoder )
        }




--============================================================================
--=  MODEL                                                                   =
--============================================================================


type alias Model =
    { status     : Status
    , activity   : String
    , chosenSize : ThumbnailSize
    , hue        : Int
    , ripple     : Int
    , noise      : Int
    }


initialModel : Model
initialModel =
    { status      = Loading
    , activity    = ""
    , chosenSize  = Small
    , hue         = 5
    , ripple      = 5
    , noise       = 5
    }



--============================================================================
--=  PORTS                                                                   =
--============================================================================


type alias FilterOptions =
    { url     : String
    , filters : List { name   : String
                     , amount : Float   
                     }
    }


port setFilters : FilterOptions -> Cmd msg

port activityChanges : (String -> msg) -> Sub msg



--============================================================================
--=  SUBSCRIPTIONS                                                           =
--============================================================================
 

subscriptions : Model -> Sub Msg
subscriptions model =
    activityChanges GotActivity



--============================================================================
--=  APPLICATION CODE                                                        =
--============================================================================


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
--=  UPDATE                                                                  =
--============================================================================


type Msg
    = ClickedPhoto      String
    | ClickedSize       ThumbnailSize
    | ClickedSurpriseMe
    | GotRandomPhoto    Photo
      -- HTTP Requests
    | GotPhotos         ( Result Http.Error ( List Photo ) )
      -- Sliders
    | SlidHue           Int
    | SlidRipple        Int
    | SlidNoise         Int
      -- Data from JavaScript
    | GotActivity       String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedPhoto url ->
            applyFilters { model | status = selectUrl url model.status }

        ClickedSize size ->
            ( { model | chosenSize = size }
            , Cmd.none 
            )

        ClickedSurpriseMe ->
            case model.status of
                Loaded (firstPhoto :: otherPhotos) _ ->
                    Random.uniform firstPhoto otherPhotos
                        |> Random.generate GotRandomPhoto
                        |> Tuple.pair model
                
                Loaded [] _ ->
                    ( model
                    , Cmd.none
                    )
                
                Loading ->
                    ( model    
                    , Cmd.none 
                    )
                
                Errored errorMessage ->
                    ( model
                    , Cmd.none 
                    )

        GotRandomPhoto photo ->
            applyFilters { model | status = selectUrl photo.url model.status }

        -- HTTP Requests
        GotPhotos (Ok photos) ->
            case photos of
                first :: rest ->
                    applyFilters
                        { model
                            | status =
                                case List.head photos of
                                    Just photo ->
                                        Loaded photos photo.url
        
                                    Nothing ->
                                        Loaded [] ""
                        }
                    
                [] ->  
                    ( { model | status = Errored "0 photos found" }
                    , Cmd.none 
                    )

        GotPhotos  (Err _) ->
            ( { model | status = Errored "Server error!" }
            , Cmd.none 
            )

        -- Sliders
        SlidHue hue ->
            applyFilters { model | hue    = hue    }

        SlidRipple ripple ->
            applyFilters { model | ripple = ripple }
  
        SlidNoise noise ->
            applyFilters { model | noise  = noise  }

          -- Data from JavaScript
        GotActivity activity ->
            ( { model | activity = activity }
            , Cmd.none
            )


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


rangeSlider : List (Attribute msg) -> List (Html msg) -> Html msg
rangeSlider attributes children =
    node "range-slider" attributes children


onSlide : (Int -> msg) -> Attribute msg
onSlide toMsg =
    at [ "detail", "userSlidTo" ] int
        |> Decode.map toMsg
        |> on "slide"
{-- Original
    let
        detailUserSlidTo : Decoder Int
        detailUserSlidTo =
            at [ "detail", "userSlidTo" ] int
 
        msgDecoder : Decoder msg
        msgDecoder =
            Json.Decode.map toMsg detailUserSlidTo
    in
    on "slide" msgDecoder
--}


applyFilters : Model -> ( Model, Cmd Msg )
applyFilters model =
    case model.status of
        Loaded photos selectedUrl ->
            let
                filters =
                    [ { name = "Hue",    amount = toFloat model.hue    / 11 }
                    , { name = "Ripple", amount = toFloat model.ripple / 11 }
                    , { name = "Noise",  amount = toFloat model.noise  / 11 }
                    ]
 
                url =
                    Common.urlPrefix ++ "large/" ++ selectedUrl
            in
            ( model
            , setFilters { url     = url
                         , filters = filters 
                         } 
            )
 
        Loading ->
            ( model
            , Cmd.none 
            )
 
        Errored errorMessage ->
            ( model
            , Cmd.none 
            )



--============================================================================
--=  VIEW                                                                    =
--============================================================================


view : Model -> Html Msg
view model =
    div 
        [ class "content" ] <|
        case model.status of
            Loaded photos selectedUrl ->
                viewLoaded photos selectedUrl model
            
            Loading ->
                []
            
            Errored errorMessage ->
                [ text ( "Error: " ++ errorMessage ) ]


viewLoaded : List Photo -> String -> Model -> List (Html Msg)
viewLoaded photos selectedUrl model = 
{--        [ h1 
            [] 
            [ text "Photo Groove" ]
        , --}
        [ button
            [ onClick ClickedSurpriseMe ]
            [ text "Surprise Me!"       ]
        , div 
            [ class "activity"    ] 
            [ text model.activity ]
        , div 
            [ class "filters" ]
            [ viewFilter SlidHue    "Hue"    model.hue
            , viewFilter SlidRipple "Ripple" model.ripple
            , viewFilter SlidNoise  "Noise"  model.noise
            ]             
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
            ( List.map ( viewThumbnail selectedUrl ) photos )
        , canvas 
            [ id    "main-canvas"
            , class "large" 
            ] 
            []
{-- replace img with canvas       
        , img
            [ class "large"
            , src ( urlPrefix ++ "large/" ++ selectedUrl )      -- uses image:  "http://elm-in-action.com/large/1.jpeg"
            ]
            []
--}
        ]


--------------------
--  VIEW HELPERS  --
--------------------


-- Create the image url  
viewThumbnail selectedUrl thumb =
    img
        [ src ( Common.urlPrefix ++ thumb.url )
        , title ( thumb.title ++ " [" ++ String.fromInt thumb.size ++ " KB]" )
        , classList [   ( "selected"
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
        [ input 
            [ type_ "radio"
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


viewFilter : (Int -> Msg) -> String -> Int -> Html Msg
viewFilter toMsg name magnitude =
    div 
        [ class "filter-slider" ]
        [ label 
            [] 
            [ text name ]
        , rangeSlider 
            [ Attr.max "11"
            , Attr.property "val" (Encode.int magnitude)
            , onSlide toMsg 
            ] 
            []
        , label 
            [] 
            [ text ( String.fromInt magnitude ) ]
        ]
