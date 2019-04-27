# elm-svg-parser

**Note: This repo is an update to work with elm 0.19.**

A library to parse String to SVG.

[![Build Status](https://travis-ci.org/Garados007/elm-svg-parser.svg?branch=master)](https://travis-ci.org/Garados007/elm-svg-parser)
[![Elm package](https://img.shields.io/elm-package/v/Garados007/elm-svg-parser.svg)](http://package.elm-lang.org/packages/Garados007/elm-svg-parser/latest)

## Parse

Normally `parse` is the only function you need.

```
import SvgParser exposing (parse)

parse "<svg xmlns=\"http://www.w3.org/2000/svg\"></svg>" : Result String (Html msg)
```
