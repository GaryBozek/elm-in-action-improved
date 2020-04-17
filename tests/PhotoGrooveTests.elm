module PhotoGrooveTests exposing (..)

-- Test modules:
import Expect                           exposing ( Expectation )
import Fuzz                             exposing ( Fuzzer, int, list, string )
import Test                             exposing (..)
import Test.Html.Event      as Event
import Test.Html.Query      as Query
import Test.Html.Selector               exposing ( attribute, tag, text )
-- Helper modules:
import Html.Attributes      as Attr     exposing ( src )
import Json.Decode          as Decode   exposing ( decodeString, decodeValue )
import Json.Encode          as Encode
-- Modules to test:
import PhotoGroove                      
    exposing 
        ( Model
        , Msg(..)
        , Photo
        , Status(..)
        , initialModel
        , update
        , urlPrefix
        , view
        )



{-- PURPOSE:    To test the Elm In Action code.

    Tests Created:  12

--}


-- basic test
suite : Test
suite =
    test "one plus one equals two" ( \_ -> Expect.equal 2 ( 1 + 1 ) )
--    todo "Implement our first test. See https://package.elm-lang.org/packages/elm-explorations/test/latest for how to do this!"


-- Pass JSON string to test against a Photo
decoderTest : Test
decoderTest =
    test "untitled Photo" <|
        \_ ->
            """ {"url": "fruits.com", "size": 5} """        
                |> decodeString PhotoGroove.photoDecoder
                |> Expect.equal
                    (Ok { url = "fruits.com", size = 5, title = "(untitled)" })



-- Just verify the title field of Photo
decoderTest2 : Test
decoderTest2 =
    test "title defaults to (untitled) - #1" <|
        \_ ->
            """ {"url": "fruits.com", "size": 5} """        
                |> decodeString PhotoGroove.photoDecoder
                |> Result.map   ( \photo -> photo.title )
                |> Expect.equal ( Ok "(untitled)" )


-- Just verify the title field of Photo (prep to convert to fuzz test)
decoderTest3 : Test
decoderTest3 =
    test "title defaults to (untitled) - #2" <|
        \_ ->
            [ ( "url",  Encode.string   "fruits.com" )
            , ( "size", Encode.int      5            )
            ]
                |> Encode.object
                |> decodeValue  PhotoGroove.photoDecoder
                |> Result.map   .title
                |> Expect.equal ( Ok "(untitled)" )


-- verify the title field with a fuzz test:
    -- fuzz2:       takes two arguments (string and int)
    -- \ function:  name the arguments in the function call
    -- url, size:   substitute the arguments where the hardcoded data currently is 
decoderTest4 : Test
decoderTest4 =
    fuzz2 string int "title defaults to (untitled) - #3" <|
        \url size ->
            [ ( "url",  Encode.string   url  )
            , ( "size", Encode.int      size )
            ]
                |> Encode.object
                |> decodeValue  PhotoGroove.photoDecoder
                |> Result.map   .title
                |> Expect.equal ( Ok "(untitled)" )


-- test update function
slidHueSetsHue : Test
slidHueSetsHue =
    fuzz int "SlidHue sets the hue" <|
        \amount ->
            initialModel
                |> update (SlidHue amount)
                |> Tuple.first
                |> .hue
                |> Expect.equal amount


-- test multiple similar components by passing arguments
sliders : Test
sliders =
    describe "Slider sets the desired field in the Model"
        [ testSlider "SlidHue"      SlidHue     .hue
        , testSlider "SlidRipple"   SlidRipple  .ripple
        , testSlider "SlidNoise"    SlidNoise   .noise
        ]
 

-- common tester
testSlider : String -> (Int -> Msg) -> (Model -> Int) -> Test
testSlider description toMsg amountFromModel =
    fuzz int description <|
        \amount ->
            initialModel
                |> update (toMsg amount)
                |> Tuple.first
                |> amountFromModel
                |> Expect.equal amount       


-- Initial startup - no photo rendered
noPhotosNoThumbnails : Test
noPhotosNoThumbnails =
    test "No thumbnails render when there are no photos to render." <|
        \_ ->
            initialModel
                |> PhotoGroove.view
                |> Query.fromHtml
                |> Query.findAll [ tag "img" ]
                |> Query.count (Expect.equal 0)      


-- fuzz thumbnails and verify rendering
thumbnailsWork : Test
thumbnailsWork =
    fuzz (Fuzz.intRange 1 5) "URLs render as thumbnails" <|
        \urlCount ->
            let
                urls : List String
                urls =
                    List.range 1 urlCount
                        |> List.map (\num -> String.fromInt num ++ ".png")
 
                thumbnailChecks : List (Query.Single msg -> Expectation)
                thumbnailChecks =
                    List.map thumbnailRendered urls
            in
            { initialModel | status = Loaded (List.map photoFromUrl urls) "" }              
                |> view
                |> Query.fromHtml
                |> Expect.all thumbnailChecks


-- helper:  verify the thumbnail was created
thumbnailRendered : String -> Query.Single msg -> Expectation
thumbnailRendered url query =
    query
        |> Query.findAll[ tag "img", attribute (Attr.src (urlPrefix ++ url)) ]
        |> Query.count (Expect.atLeast 1)


-- helper:  create a Photo from a string (url)
photoFromUrl : String -> Photo
photoFromUrl url =
    { url = url, size = 0, title = "" }


-- test clicking on a thumbnail
clickThumbnail : Test
clickThumbnail =
    fuzz3 urlFuzzer string urlFuzzer "clicking a thumbnail selects it" <|
        \urlsBefore urlToSelect urlsAfter ->
            let
                url =
                    urlToSelect ++ ".jpeg"
 
                photos =
                    (urlsBefore ++ url :: urlsAfter)
                        |> List.map photoFromUrl
 
                srcToClick =
                    urlPrefix ++ url
            in
            { initialModel | status = Loaded photos "" }
                |> view
                |> Query.fromHtml
                |> Query.find [ tag "img", attribute (Attr.src srcToClick) ]
                |> Event.simulate Event.click
                |> Event.expect (ClickedPhoto url)


-- helper:  creates fuzzer of urls
urlFuzzer : Fuzzer (List String)
urlFuzzer =
    Fuzz.intRange 1 5
        |> Fuzz.map urlsFromCount
 

-- helper:  create list of urls 
urlsFromCount : Int -> List String
urlsFromCount urlCount =
    List.range 1 urlCount
        |> List.map (\num -> String.fromInt num ++ ".png")
