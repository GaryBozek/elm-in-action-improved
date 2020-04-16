module PhotoGrooveTests exposing (..)

import Expect                   exposing (Expectation)
import Fuzz                     exposing (Fuzzer, int, list, string)
import Json.Decode  as Decode   exposing (decodeString, decodeValue)
import Json.Encode  as Encode
import Test                     exposing (..)
-- modules to test:
--import PhotoGroove
import PhotoGroove              exposing (Model, Msg(..), Photo, initialModel, update)



{-- PURPOSE:    To test the Elm In Action code.

    Tests Created:  9

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