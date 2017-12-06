# elm-svg-parser

A library to parse String to SVG.

[![Build Status](https://travis-ci.org/rnons/elm-svg-parser.svg?branch=master)](https://travis-ci.org/rnons/elm-svg-parser)

## Parse

Normally `parse` is the only function you need.

```
import SvgParser exposing (parse)

parse "<svg xmlns=\"http://www.w3.org/2000/svg\"></svg>" : Result String (Html msg)
```
