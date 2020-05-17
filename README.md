# elm-review-nobooleancase

Prohibits using `case <boolean expression> of`, with a preference for using
`if <expr> then <expr> else <expr>` for such cases.

## Configuration

```elm
module ReviewConfig exposing (config)

import NoBooleanCase
import Review.Rule exposing (Rule)

config : List Rule
config =
    [ NoBooleanCase.rule
    ]
```
