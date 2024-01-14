module Tests exposing (suite, testParseToNode)

import Cases exposing (all)
import Expect
import SvgParser exposing (..)
import Test exposing (..)


testParseToNode : ( String, String, Element ) -> Test
testParseToNode ( title, source, element ) =
    test title <|
        \_ ->
            Expect.equal (parseToNode source) (Ok (SvgElement element))


suite : Test
suite =
    List.map testParseToNode all |> concat
