module SvgParser
    exposing
        ( SvgNode(..)
        , Element
        , SvgAttribute
        , parseToNode
        , parse
        , nodeToSvg
        , toAttribute
        )

{-| String to SVG parser


# Primitives

@docs SvgNode, Element, SvgAttribute


# Parsing

@docs parse, parseToNode, nodeToSvg, toAttribute

-}

import Combine exposing (..)
import Combine.Char exposing (anyChar)
import Html exposing (Html)
import Svg exposing (Svg, Attribute, svg, node, text)
import VirtualDom exposing (attribute)


{-| A SVG node can be one of the three: SvgElement, SvgText or SvgComment.
-}
type SvgNode
    = SvgElement Element
    | SvgText String
    | SvgComment String


{-| An Element consists of a tag name, a list of attributes, a list of children nodes.

    <svg xmlns="http://www.w3.org/2000/svg"></svg>

will be parsed as

    Element "svg" [("xmlns", "http://www.w3.org/2000/svg")] []

-}
type alias Element =
    { name : String
    , attributes : List SvgAttribute
    , children : List SvgNode
    }


{-| A name/value pair to denote attribute.
-}
type alias SvgAttribute =
    ( String, String )


attributeParser : Parser s SvgAttribute
attributeParser =
    (,)
        <$> regex "[^=>/]+"
        <*> optional "" (string "=\"" *> regex "[^\"]*" <* string "\"")


openingParser : Parser s Element
openingParser =
    (\_ tagName attributes ->
        Element tagName attributes []
    )
        <$> string "<"
        <*> regex "[^/> ]+"
        <*> (whitespace *> sepBy whitespace attributeParser <* whitespace)


closingOrChildrenParser : Element -> Parser s Element
closingOrChildrenParser element =
    let
        childrenParser =
            (\children -> { element | children = children })
                <$> (whitespace
                        *> string ">"
                        *> many nodeParser
                        <* whitespace
                        <* string ("</" ++ element.name ++ ">")
                    )
    in
        lazy <|
            \_ ->
                choice
                    [ whitespace *> string "/>" *> succeed element
                    , childrenParser
                    ]


elementParser : Parser s SvgNode
elementParser =
    lazy <|
        \_ ->
            SvgElement
                <$> (whitespace
                        *> openingParser
                        >>= closingOrChildrenParser
                    )


textParser : Parser s SvgNode
textParser =
    lazy <|
        \_ ->
            SvgText
                <$> (whitespace
                        *> regex "[^<]+"
                    )


commentParser : Parser s SvgNode
commentParser =
    lazy <|
        \_ ->
            SvgComment
                << String.fromList
                <$> (whitespace *> string "<!--" *> manyTill anyChar (string "-->"))


nodeParser : Parser s SvgNode
nodeParser =
    lazy <|
        \_ ->
            choice
                [ textParser
                , commentParser
                , elementParser
                ]


{-| Convert `SvgAttribute` to `Attribute msg`. This is useful when you want to manipulate `SvgAttribute` before converting to `Html msg`.
-}
toAttribute : SvgAttribute -> Attribute msg
toAttribute ( name, value ) =
    attribute name value


elementToSvg : Element -> Svg msg
elementToSvg element =
    node element.name
        (List.map toAttribute element.attributes)
        (List.map nodeToSvg element.children)


{-| Convert `SvgNode` to `Svg msg`. This is useful when you want to manipulate `SvgNode` before conveting to `Html msg`.
-}
nodeToSvg : SvgNode -> Svg msg
nodeToSvg svgNode =
    case svgNode of
        SvgElement element ->
            elementToSvg element

        SvgText content ->
            text content

        SvgComment content ->
            text ""


{-| Parse xml declaration
-}
xmlDeclarationParser : Parser s String
xmlDeclarationParser =
    String.fromList
        <$> (whitespace
                *> string "<?xml"
                *> manyTill anyChar (string "?>")
            )


{-| Parses `String` to `SvgNode`. Normally you will use `parse` instead of this.

    parse "<svg xmlns=\"http://www.w3.org/2000/svg\"></svg>"
        == Ok (SvgElement (Element "svg" [("xmlns", "http://www.w3.org/2000/svg")] []))

-}
parseToNode : String -> Result String SvgNode
parseToNode input =
    case Combine.runParser (optional "" xmlDeclarationParser *> nodeParser) [] input of
        Ok ( _, _, svgNode ) ->
            Ok svgNode

        Err ( _, stream, errors ) ->
            Err <| String.join " or " errors


{-| Parses `String` to `Html msg`. Usually this is the only function you need.
-}
parse : String -> Result String (Html msg)
parse input =
    let
        toHtml svgNode =
            case svgNode of
                SvgElement element ->
                    if element.name == "svg" then
                        Ok <|
                            svg (List.map toAttribute element.attributes)
                                (List.map nodeToSvg element.children)
                    else
                        Err "Top element is not svg"

                _ ->
                    Err "Top element is not svg"
    in
        parseToNode input |> Result.andThen toHtml
