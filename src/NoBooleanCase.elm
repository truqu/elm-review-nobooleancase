module NoBooleanCase exposing (rule)

import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.Node as Node exposing (Node)
import Elm.Syntax.Pattern as Pattern
import Elm.Syntax.Range exposing (Range)
import Review.Fix as Fix
import Review.Rule as Rule exposing (Error, Rule)
import Util


rule : Rule
rule =
    Rule.newModuleRuleSchema "NoBooleanCase" ()
        |> Rule.withSimpleExpressionVisitor expressionVisitor
        |> Rule.fromModuleRuleSchema


expressionVisitor : Node Expression -> List (Error {})
expressionVisitor (Node.Node range expression) =
    case expression of
        Expression.CaseExpression caseBlock ->
            errorsForCaseBlock range caseBlock

        _ ->
            []


errorsForCaseBlock : Range -> Expression.CaseBlock -> List (Error {})
errorsForCaseBlock ({ end } as range) { expression, cases } =
    case cases of
        [ ( Node.Node _ (Pattern.NamedPattern { moduleName, name } []), expr1 ), ( _, expr2 ) ] ->
            if moduleName == [] && name == "True" then
                [ makeError range expression expr1 expr2 ]

            else if moduleName == [] && name == "False" then
                [ makeError range expression expr2 expr1 ]

            else
                []

        _ ->
            []


makeError : Range -> Node Expression -> Node Expression -> Node Expression -> Error {}
makeError ({ end } as range) cond pos neg =
    Rule.errorWithFix
        { message = "Matching boolean values in a case .. of expression"
        , details = [ "It's quite silly" ]
        }
        -- { range | end = { end | column = 0 } }
        range
        [ Fix.replaceRangeBy range
            (Util.expressionToString range (Expression.IfBlock cond pos neg))
        ]
