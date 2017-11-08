A library to parse String to SVG.

<a target='_blank' rel='nofollow' href='https://app.codesponsor.io/link/Uwsz51h7LnBMxKdsoXrxgQps/rnons/elm-svg-parser'>
  <img alt='Sponsor' width='888' height='68' src='https://app.codesponsor.io/embed/Uwsz51h7LnBMxKdsoXrxgQps/rnons/elm-svg-parser.svg' />
</a>

## Parse

Normally `parse` is the only function you need.

```
import SvgParser exposing (parse)

parse "<svg xmlns=\"http://www.w3.org/2000/svg\"></svg>" : Result String (Html msg)
```
