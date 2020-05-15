module NoBooleanCaseTest exposing (..)

import NoBooleanCase
import Review.Test
import Test exposing (Test, describe, test)


tests : Test
tests =
    describe "noBooleanCase"
        [ test "Simple one" <|
            \_ ->
                """module A exposing (..)

a =
    case expr of
        True -> True
        False -> False
"""
                    |> Review.Test.run NoBooleanCase.rule
                    |> Review.Test.expectErrors
                        [ makeError
                            """case expr of
        True -> True
        False -> False"""
                            |> Review.Test.whenFixed
                                """module A exposing (..)

a =
    if expr then
      True
    else
      False
"""
                        ]
        , test "Flip it and reverse it" <|
            \_ ->
                """module A exposing (..)

a =
    case expr of
        False -> "nope"
        _ -> "okay"
"""
                    |> Review.Test.run NoBooleanCase.rule
                    |> Review.Test.expectErrors
                        [ makeError
                            """case expr of
        False -> "nope"
        _ -> "okay\""""
                            |> Review.Test.whenFixed
                                """module A exposing (..)

a =
    if expr then
      "okay"
    else
      "nope"
"""
                        ]
        ]


makeError : String -> Review.Test.ExpectedError
makeError under =
    Review.Test.error
        { message = "Matching boolean values in a case .. of expression"
        , details =
            [ "It's quite silly"
            ]
        , under = under
        }
