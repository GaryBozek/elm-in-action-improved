module PhotoGrooveTests exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
-- modules to test:
import Json.Decode exposing (decodeString)
import PhotoGroove

suite : Test
suite =
    test "one plus one equals two" (\_ -> Expect.equal 2 (1 + 1) )
--    todo "Implement our first test. See https://package.elm-lang.org/packages/elm-explorations/test/latest for how to do this!"


decoderTest : Test
decoderTest =
    test "untitled Photo" <|
        \_ ->
            """ {"url": "fruits.com", "size": 5} """        
                |> decodeString PhotoGroove.photoDecoder
                |> Expect.equal
                    (Ok { url = "fruits.com", size = 5, title = "(untitled)" })


decoderTest2 : Test
decoderTest2 =
    test "title defaults to (untitled)" <|
        \_ ->
            """ {"url": "fruits.com", "size": 5} """        
                |> decodeString PhotoGroove.photoDecoder
                |> Result.map (\photo -> photo.title)
                |> Expect.equal (Ok "(untitled)")

