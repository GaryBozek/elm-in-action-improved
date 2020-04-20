{-  PURPOSE:    Elm In Action: example code

    2020.04.18  GB  19  - Ch 8 - Single Page Apps (SPAs)


-}


module ? exposing (main)

import Browser                          exposing ( Document )
import Html                             exposing ( Html, a, footer, h1, li, nav, text, ul)
import Html.Attributes                  exposing ( class, classList, href, src )
import Html.Events                      exposing ( onClick )
import Http
import Json.Decode          as Decode   exposing ( Decoder, int, list, string )
import Json.Decode.Pipeline             exposing ( required )



--============================================================================
--=  MAIN PROGRAM                                                            =
--============================================================================


main : Program () Model Msg
main =
    Browser.document
        { init          = \_ -> ( {}, Cmd.none )
        , update        = update
        , view          = view
        , subscriptions = subscriptions
        }



--============================================================================
--=  MODEL                                                                   =
--============================================================================
 

type alias Model =
    {}



--============================================================================
--=  PORTS / SUBSCRIPTIONS                                                   =
--============================================================================
 
 
subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none




    
--============================================================================
--=  APPLICATION CODE                                                        =
--============================================================================  




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
    = NothingYet


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model
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
    { title = "Photo Groove, SPA Style"
    , body  = [ text "This isn’t even my final form!" ]
    }


--------------------
--  VIEW HELPERS  --
--------------------


