module Tests exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)
import SvgParser exposing (..)
import Cases exposing (all)


testParseToNode : ( String, String, Element ) -> Test
testParseToNode ( title, source, element ) =
    test title <|
        \_ ->
            Expect.equal (parseToNode source) (Ok (SvgElement element))


suite : Test
suite =
    List.map testParseToNode all |> concat
