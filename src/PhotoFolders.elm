{-  PURPOSE:    Elm In Action: example code

    2020.04.17  GB  16  - Ch 7 - Data Modeling
                        - Using dictionaries
                    17  - Model Trees by using recursive custom types
                    18  - Decoding Graphs and Trees
    2020.04.20  GB  21  - Delegating Pages
                        - expose (Model, Msg, init, update, view)
    2020.04.21  GB  22  - deduplicate urlPrefix

-}

module PhotoFolders exposing (Model, Msg, init, update, view)


import Browser
import Common
import Dict                             exposing (Dict)
import Html                             exposing (..)
import Html.Attributes                  exposing ( class, href, src )
import Html.Events                      exposing ( onClick )
import Http
import Json.Decode          as Decode   exposing ( Decoder, int, list, string )
import Json.Decode.Pipeline             exposing ( required )



--============================================================================
--=  MAIN PROGRAM                                                            =
--============================================================================


{--  No longer needed
main : Program () Model Msg
main =
    Browser.element
        { init          = init
        , view          = view
        , update        = update
        , subscriptions = \_ -> Sub.none
        }
--}

 
init : Maybe String -> ( Model, Cmd Msg )
init selectedFilename =
    ( { initialModel | selectedPhotoUrl = selectedFilename }
    , Http.get
        { url    = "http://elm-in-action.com/folders/list"
        , expect = Http.expectJson GotInitialModel modelDecoder
        }
    )



--============================================================================
--=  MODEL                                                                   =
--============================================================================

 
type alias Model =
    { selectedPhotoUrl  : Maybe String
    , photos            : Dict String Photo
    , root              : Folder
    }

 
initialModel : Model
initialModel =
    { selectedPhotoUrl  = Nothing 
    , photos            = Dict.empty
    , root              = Folder 
                            { name       = "Loading. . ."
                            , photoUrls  = []
                            , subfolders = []
                            , expanded   = True 
                            }
    }





--============================================================================
--=  PORTS                                                                   =
--============================================================================



    
--============================================================================
--=  APPLICATION CODE                                                        =
--============================================================================  


type alias Photo =
    { title       : String
    , size        : Int
    , relatedUrls : List String
    , url         : String
    }


type Folder =
    Folder
        { name       : String
        , photoUrls  : List String
        , subfolders : List Folder
        , expanded   : Bool
        }


type FolderPath
    = End
    | Subfolder Int FolderPath


type alias JsonPhoto =
    { title       : String
    , size        : Int
    , relatedUrls : List String
    }


---------------------------
--  JSON DECODERS        --
---------------------------

--
-- SERVER JSON DECODER
--
modelDecoder : Decoder Model
modelDecoder =
   Decode.map2
        ( \photos root ->
            { photos            = photos
            , root              = root
            , selectedPhotoUrl  = Nothing 
            }
        )
        modelPhotosDecoder
        folderDecoder

{-- Hardcoded model:
    Decode.succeed
        { selectedPhotoUrl  = Just "trevi"
        , photos            = Dict.fromList
             [ ( "trevi"
               , { title        = "Trevi"
                 , relatedUrls  = [ "coli", "fresco" ]
                 , size         = 34
                 , url          = "trevi" 
                 }
               )
             , ( "fresco"
               , { title        = "Fresco"
                 , relatedUrls  = [ "trevi" ]
                 , size         = 46
                 , url          = "fresco" 
                 } 
               )
             , ( "coli"
               , { title        = "Coliseum"
                 , relatedUrls  = [ "trevi", "fresco" ]
                 , size         = 36
                 , url          = "coli"
                 }
               )
             ]
        , root =
            Folder
                { name       = "Photos"
                , photoUrls  = []
                , subfolders =
                    [ Folder
                        { name       = "2016"
                        , photoUrls  = [ "trevi", "coli" ]
                        , subfolders =
                            [ Folder
                                { name       = "outdoors"
                                , photoUrls  = []
                                , subfolders = []
                                , expanded   = True
                                }
                            , Folder 
                                { name       = "indoors"
                                , photoUrls  = [ "fresco" ]
                                , subfolders = []
                                , expanded   = True
                                }
                            ]
                        , expanded   = True
                        }
                    , Folder
                        { name       = "2017"
                        , photoUrls  = []
                        , subfolders = 
                            [ Folder
                                { name       = "outdoors"
                                , photoUrls  = []
                                , subfolders = []
                                , expanded   = True
                                }
                            , Folder
                                { name       = "indoors"
                                , photoUrls  = []
                                , subfolders = []
                                , expanded   = True
                                }
                            ]
                        , expanded   = True
                        }
                    ]
                , expanded   = True
                } 

        }
--}


--
-- MODEL & FOLDER DECODER
--
modelPhotosDecoder : Decoder (Dict String Photo)
modelPhotosDecoder =
    Decode.succeed modelPhotosFromJson
        |> required "photos"      photosDecoder
        |> required "subfolders" ( Decode.lazy ( \_ -> list modelPhotosDecoder ) )
 
 
modelPhotosFromJson :
    Dict String Photo
    -> List (Dict String Photo)
    -> Dict String Photo
modelPhotosFromJson folderPhotos subfolderPhotos =
    List.foldl Dict.union folderPhotos subfolderPhotos


--
-- FOLDER DECODER
--
folderDecoder : Decoder Folder
folderDecoder =
    Decode.succeed folderFromJson
        |> required "name"       string
        |> required "photos"     photosDecoder
        |> required "subfolders" ( Decode.lazy ( \_ -> list folderDecoder ) )   -- .lazy delays evaluation until it is required
        -- |> required "subfolders" ( list folderDecoder )                      -- cyclic definition - fails
 

folderFromJson : String -> Dict String Photo -> List Folder -> Folder
folderFromJson name photos subfolders =
    Folder
        { name       = name
        , expanded   = True
        , subfolders = subfolders
        , photoUrls  = Dict.keys photos
        }


--
-- PHOTOS DECODER
--
photosDecoder : Decoder (Dict String Photo)
photosDecoder =
    Decode.keyValuePairs jsonPhotoDecoder
        |> Decode.map fromPairs

 
jsonPhotoDecoder : Decoder JsonPhoto
jsonPhotoDecoder =
    Decode.succeed JsonPhoto
        |> required "title"          string
        |> required "size"           int
        |> required "related_photos" ( list string )


fromPairs : List ( String, JsonPhoto ) -> Dict String Photo
fromPairs pairs =
    pairs
        |> List.map finishPhoto
        |> Dict.fromList


finishPhoto : ( String, JsonPhoto ) -> ( String, Photo )
finishPhoto ( url, json ) =
    ( url
    , { url         = url
      , size        = json.size
      , title       = json.title
      , relatedUrls = json.relatedUrls
      }
    )


---------------------------
--  APPLICATION HELPERS  --
---------------------------




--============================================================================
--=  UPDATE                                                                  =
--============================================================================


type Msg
    = ClickedPhoto      String
    | GotInitialModel   ( Result Http.Error Model )
      -- Folder expansion
    | ClickedFolder     FolderPath

 
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedPhoto url ->
            ( { model | selectedPhotoUrl = Just url }
            , Cmd.none 
            )

        GotInitialModel ( Ok newModel ) ->
            ( { newModel | selectedPhotoUrl = model.selectedPhotoUrl }
            , Cmd.none 
            )

        GotInitialModel ( Err _ ) ->
            ( model
            , Cmd.none 
            )

        -- Folder expansion
        ClickedFolder path ->
            ( { model | root = toggleExpanded path model.root }
            , Cmd.none 
            )


----------------------
--  UPDATE HELPERS  --
----------------------




--============================================================================
--=  VIEW                                                                    =
--============================================================================


view : Model -> Html Msg
view model =
    let
        photoByUrl : String -> Maybe Photo
        photoByUrl url =
            Dict.get url model.photos

        selectedPhoto : Html Msg
        selectedPhoto =
            case Maybe.andThen photoByUrl model.selectedPhotoUrl of
                Just photo ->
                    viewSelectedPhoto photo

                Nothing ->
                    text ""
    in
    div [ class "content" ]
        [ div 
            [ class "folders" ]
            [ {-- h1 
                [] 
                [ text "Folders" ]
            , --}
              viewFolder End model.root
            ]
        , div 
            [ class "selected-photo" ] 
            [ selectedPhoto ]
        ]


viewSelectedPhoto : Photo -> Html Msg
viewSelectedPhoto photo =
    div
        [ class "selected-photo" ]
        [ h2 
            [] 
            [ text photo.title ]
        , img 
            [ src ( Common.urlPrefix ++ "photos/" ++ photo.url ++ "/full" ) ] 
            []
        , span 
            [] 
            [ text ( String.fromInt photo.size ++ "KB" ) ]
        , h3 
            [] 
            [ text "Related" ]
        , div 
            [ class "related-photos" ]
            ( List.map viewRelatedPhoto photo.relatedUrls )
        ]


viewRelatedPhoto : String -> Html Msg
viewRelatedPhoto url =
    img
        [ class   "related-photo"
        , onClick ( ClickedPhoto url )
        , src     ( Common.urlPrefix ++ "photos/" ++ url ++ "/thumb" )
        ]
        []


viewFolder : FolderPath -> Folder -> Html Msg
viewFolder path (Folder folder) =
    let
        viewSubfolder : Int -> Folder -> Html Msg
        viewSubfolder index subfolder =
            viewFolder ( appendIndex index path ) subfolder

        folderLabel =
            label 
                [ onClick ( ClickedFolder path ) ] 
                [ text folder.name ]
    in
    if folder.expanded then
        let
            contents =
                List.append
                    ( List.indexedMap viewSubfolder folder.subfolders )
                    ( List.map        viewPhoto     folder.photoUrls  )
        in
        div 
            [ class "folder expanded" ]
            [ folderLabel
            , div 
                [ class "contents" ] 
                contents
            ]
    else 
        div 
            [ class "folder collapsed" ] 
            [ folderLabel ]


appendIndex : Int -> FolderPath -> FolderPath
appendIndex index path =
    case path of
        End ->
            Subfolder index End

        Subfolder subfolderIndex remainingPath ->
            Subfolder subfolderIndex (appendIndex index remainingPath)


viewPhoto : String -> Html Msg
viewPhoto url =
     a 
        [ href    ("/photos/" ++ url)
        , class   "photo" 
        , onClick ( ClickedPhoto url ) 
        ]
        [ text url ]


--------------------
--  VIEW HELPERS  --
--------------------


toggleExpanded : FolderPath -> Folder -> Folder
toggleExpanded path (Folder folder) =
    case path of
        End ->
            Folder { folder | expanded = not folder.expanded }

        Subfolder targetIndex remainingPath ->
            let
                subfolders : List Folder
                subfolders =
                    List.indexedMap transform folder.subfolders

                transform : Int -> Folder -> Folder
                transform currentIndex currentSubfolder =
                    if currentIndex == targetIndex then
                        toggleExpanded remainingPath currentSubfolder
                    else
                        currentSubfolder
            in
            Folder { folder | subfolders = subfolders }


