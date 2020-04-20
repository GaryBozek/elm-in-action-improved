{-  PURPOSE:    Elm In Action: example code

    2020.04.18  GB  19  - Ch 8 - Single Page Apps (SPAs)
                    20  - Routing

-}

module Main exposing ( main )


import Browser                          exposing ( Document )
import Browser.Navigation   as Nav
import Html                             exposing ( Html, a, footer, h1, li, nav, text, ul )
import Html.Attributes                  exposing ( classList, href )
--import Html.Events                      exposing ( onMouseOver )
import Html.Lazy                        exposing ( lazy )
import Url                              exposing (Url)
import Url.Parser           as Parser   exposing ((</>), Parser, s, string)



--============================================================================
--=  MAIN PROGRAM                                                            =
--============================================================================


main : Program () Model Msg
main =
    Browser.application
        { init          = init
        , onUrlRequest  = ClickedLink
        , onUrlChange   = ChangedUrl
        , update        = update
        , view          = view
        , subscriptions = subscriptions
        }

{--
init : () -> Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    ( { page = Folders }
    , Cmd.none 
    )
--}
{--
init : () -> Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    case url.path of
        "/gallery" ->
            ( { page = Gallery }
            , Cmd.none 
            )

        "/" ->
            ( { page = Folders }
            , Cmd.none 
            )

        _ ->
            ( { page = NotFound }
            , Cmd.none 
            )
--}
init : () -> Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    ( { page = urlToPage url
      , key  = key 
      }
    , Cmd.none 
    )
 
 
urlToPage : Url -> Page
urlToPage url =
    Parser.parse parser url
        |> Maybe.withDefault NotFound
 

--============================================================================
--=  MODEL                                                                   =
--============================================================================


type alias Model =
    { page : Page 
    , key  : Nav.Key        -- never changes; only used to prevent calls to Nav.pushUrl unless started by Browser.application
    }



--============================================================================
--=  PORTS / SUBSCRIPTIONS                                                   =
--============================================================================
 
 
subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none




    
--============================================================================
--=  APPLICATION CODE                                                        =
--============================================================================  


type Page
    = SelectedPhoto String
    | Gallery
    | Folders
    | NotFound


{--
parser : Parser (Page -> a) a
parser =
    Parser.map SelectedPhoto (s "photos" </> Parser.string)
--}
parser : Parser (Page -> a) a
parser =
    Parser.oneOf
        [ Parser.map Folders Parser.top
        , Parser.map Gallery (s "gallery")
        , Parser.map SelectedPhoto (s "photos" </> Parser.string)
        ]


---------------------------
--  JSON DECODERS        --
---------------------------



---------------------------
--  APPLICATION HELPERS  --
---------------------------




--============================================================================
--=  UPDATE                                                                  =
--============================================================================

 
type Msg
    = ClickedLink   Browser.UrlRequest
    | ChangedUrl    Url


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedLink urlRequest ->
            case urlRequest of
                Browser.External href ->
                    ( model
                    , Nav.load href 
                    )
 
                Browser.Internal url ->
                    ( model
                    , Nav.pushUrl model.key ( Url.toString url ) 
                    )
        ChangedUrl url ->
            ( { model | page = urlToPage url }
            , Cmd.none 
            )


----------------------
--  UPDATE HELPERS  --
----------------------




--============================================================================
--=  VIEW                                                                    =
--============================================================================
 



view : Model -> Document Msg
view model =
    let
        content =
            text "This isn't even my final form!"
    in
    { title = "Photo Groove, SPA Style"
    , body =
        [ lazy viewHeader model.page        -- lazy: caches the header and only creates a Html Msg when the model changes
        , content
        , viewFooter
        ]
    }


viewHeader : Page -> Html Msg
viewHeader page =
    let
        logo =
            h1 
                [] 
                [ text "Photo Groove" ]

        links =
            ul 
                []
                [ navLink Folders   { url       = "/"
                                    , caption   = "Folders" 
                                    }
                , navLink Gallery   { url       = "/gallery"
                                    , caption   = "Gallery" 
                                    }
                ]

        -- determines if the Navigation Link is active (shown as selected)
        navLink : Page -> { url : String, caption : String } -> Html msg
        navLink targetPage { url, caption } =
            li 
{-- replaced below
                [ classList 
                    [   ( "active"
                        , page == targetPage 
                        ) 
                    ] 
                ]
--}
                [ classList 
                    [   ( "active"
                        , isActive  { link = targetPage
                                    , page = page 
                                    } 
                        ) 
                    ] 
                ]
                [ a 
                    [ href url     ] 
                    [ text caption ] 
                ]
    in
    nav 
        [] 
        [ logo
        , links 
        ]


isActive : { link : Page, page : Page } -> Bool
isActive { link, page } =
    case ( link,           page            ) of
         --------------------------------------------
         ( Gallery,        Gallery         ) -> True
         ( Gallery,        _               ) -> False
         ( Folders,        Folders         ) -> True
         ( Folders,        SelectedPhoto _ ) -> True
         ( Folders,         _              ) -> False
         ( SelectedPhoto _, _              ) -> False
         ( NotFound,        _              ) -> False


viewFooter : Html msg
viewFooter =
    footer 
        [] 
        [ text "One is never alone with a rubber duck. - Douglas Adams" ]



--------------------
--  VIEW HELPERS  --
--------------------


