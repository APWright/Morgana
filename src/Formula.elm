module Formula exposing (EvaluatedFormula(..), Formula(..), TimeUnit(..), boundToMillis, errorMessage, evaluate, formulaToString, getFormulaTimeline, parse, timeUnitString)

import Basics exposing (identity, round)
import Bool.Extra
import Data exposing (Simulation, Timeline, estimate)
import Dict exposing (get)
import List exposing (map, map2)
import Parser exposing ((|.), (|=), DeadEnd, Parser, Problem(..), andThen, backtrackable, end, keyword, lazy, oneOf, run, spaces, succeed, symbol, variable)
import Set
import Time exposing (Posix, posixToMillis)



-- Formula


type Formula
    = Falsity
    | Truth
    | State String
    | Not Formula
    | And Formula Formula
    | Or Formula Formula
    | Implies Formula Formula
    | Eventually Formula
    | Globally Formula
    | Until Formula Formula
    | BoundedEventually Formula Float TimeUnit
    | BoundedGlobally Formula Float TimeUnit
    | BoundedUntil Formula Formula Float TimeUnit


type TimeUnit
    = MilliSecond
    | Second
    | Minute
    | Hour
    | Day
    | Year


timeUnitString : TimeUnit -> String
timeUnitString unit =
    case unit of
        MilliSecond ->
            "ms"

        Second ->
            "s"

        Minute ->
            "m"

        Hour ->
            "h"

        Day ->
            "d"

        Year ->
            "y"


boundToMillis : Float -> TimeUnit -> Int
boundToMillis bound unit =
    case unit of
        MilliSecond ->
            round bound

        Second ->
            round (bound * 1000)

        Minute ->
            round (bound * 60000)

        Hour ->
            round (bound * 3.6e6)

        Day ->
            round (bound * 8.64e7)

        Year ->
            round (bound * 3.1556952e10)



--SYNTAX


parse : String -> Result (List DeadEnd) Formula
parse string =
    run fullFormula string


fullFormula : Parser Formula
fullFormula =
    succeed identity
        |. spaces
        |= expression
        |. end


expression : Parser Formula
expression =
    oneOf
        [ backtrackable infixOp
        , term
        ]


term : Parser Formula
term =
    oneOf
        [ atom
        , grouping
        , prefixOp
        ]



-- current implementation assumes right associativity with no precidence


infixOp : Parser Formula
infixOp =
    succeed identity
        |= lazy (\_ -> term)
        |> andThen
            (\leftTerm ->
                oneOf
                    [ succeed (Implies leftTerm)
                        |. oneOf
                            [ keyword "->"
                            , keyword "IMPLIES"
                            ]
                        |. spaces
                        |= lazy (\_ -> expression)
                    , succeed (And leftTerm)
                        |. keyword "AND"
                        |. spaces
                        |= lazy (\_ -> expression)
                    , succeed (Or leftTerm)
                        |. keyword "OR"
                        |. spaces
                        |= lazy (\_ -> expression)
                    , succeed (Until leftTerm)
                        |. keyword "UNTIL"
                        |. spaces
                        |= lazy (\_ -> expression)
                    , succeed (\t u f -> BoundedUntil leftTerm f t u)
                        |. keyword "HOLDS UNTIL WITHIN"
                        |. spaces
                        |= Parser.float
                        |. spaces
                        |= timeUnit
                        |. spaces
                        |= lazy (\_ -> expression)
                    ]
            )


prefixOp : Parser Formula
prefixOp =
    oneOf
        [ succeed Not
            |. keyword "NOT"
            |. spaces
            |= lazy (\_ -> expression)
        , succeed Eventually
            |. keyword "EVENTUALLY"
            |. spaces
            |= lazy (\_ -> expression)
        , succeed Globally
            |. keyword "GLOBALLY"
            |. spaces
            |= lazy (\_ -> expression)
        , succeed (\t u f -> BoundedEventually f t u)
            |. keyword "WITHIN"
            |. spaces
            |= Parser.float
            |. spaces
            |= timeUnit
            |. spaces
            |= lazy (\_ -> expression)
        , succeed (\t u f -> BoundedGlobally f t u)
            |. keyword "FOR"
            |. spaces
            |= Parser.float
            |. spaces
            |= timeUnit
            |. spaces
            |= lazy (\_ -> expression)
        , succeed Implies
            |. oneOf [ keyword "IF", keyword "WHEN" ]
            |. spaces
            |= lazy (\_ -> expression)
            |. spaces
            |. keyword "THEN"
            |. spaces
            |= lazy (\_ -> expression)
        ]


grouping : Parser Formula
grouping =
    succeed identity
        |. symbol "("
        |. spaces
        |= lazy (\_ -> expression)
        |. spaces
        |. symbol ")"
        |. spaces


timeUnit : Parser TimeUnit
timeUnit =
    oneOf
        [ succeed MilliSecond |. keyword "MS"
        , succeed Second |. keyword "S"
        , succeed Hour |. keyword "H"
        , succeed Day |. keyword "D"
        , succeed Year |. keyword "Y"
        ]


atom : Parser Formula
atom =
    succeed identity
        |= oneOf
            [ backtrackable numericComparison
            , backtrackable categoricalComparison
            , stateName
            , booleanLiteral
            ]
        |. spaces


stateName : Parser Formula
stateName =
    succeed State
        |= variable
            { start = Char.isLower
            , inner = \c -> Char.isAlphaNum c || c == '_'
            , reserved = Set.fromList [ "and", "or", "f", "g", "not", "implies", "if", "then" ]
            }


booleanLiteral : Parser Formula
booleanLiteral =
    oneOf
        [ succeed Truth
            |. keyword "TRUE"
        , succeed Falsity
            |. keyword "FALSE"
        ]


categoricalComparison : Parser Formula
categoricalComparison =
    succeed (\name val -> State (name ++ "__" ++ String.replace " " "_" val))
        |= variable
            { start = Char.isLower
            , inner = \c -> Char.isAlphaNum c || c == '_'
            , reserved = Set.fromList [ "and", "or", "f", "g", "not", "implies", "if", "then" ]
            }
        |. spaces
        |. keyword "IS"
        |. spaces
        |. symbol "\""
        |. spaces
        |= variable
            { start = \c -> c /= '"'
            , inner = \c -> c /= '"'
            , reserved = Set.fromList []
            }
        |. symbol "\""


numericComparison : Parser Formula
numericComparison =
    succeed (\name op val unit -> State (name ++ "__" ++ op ++ "__" ++ String.fromFloat val ++ "__" ++ unit))
        |= variable
            { start = Char.isLower
            , inner = \c -> Char.isAlphaNum c || c == '_'
            , reserved = Set.fromList [ "and", "or", "f", "g", "not", "implies", "if", "then" ]
            }
        |. spaces
        |= oneOf
            [ succeed ">" |. keyword ">"
            , succeed ">=" |. keyword ">="
            , succeed "<" |. keyword "<"
            , succeed "<=" |. keyword "<="
            , succeed "==" |. keyword "=="
            , succeed "!=" |. keyword "!="
            ]
        |. spaces
        |= Parser.float
        |. spaces
        |= oneOf
            [ succeed "AU" |. keyword "AU"
            , succeed "DEG" |. keyword "DEG"
            , succeed "AMP" |. keyword "AMP"
            ]


formulaToString : Formula -> String
formulaToString formula =
    let
        formulaToStringAux formulaAux tabLevel =
            case formulaAux of
                Truth ->
                    " ⊤"

                Falsity ->
                    " ⊥"

                State name ->
                    " " ++ name

                Not f ->
                    " ¬(" ++ formulaToStringAux f tabLevel ++ " )"

                And f1 f2 ->
                    formulaToStringAux f1 tabLevel ++ " /\\" ++ formulaToStringAux f2 tabLevel

                Or f1 f2 ->
                    formulaToStringAux f1 tabLevel ++ " \\/" ++ formulaToStringAux f2 tabLevel

                Implies f1 f2 ->
                    " (" ++ formulaToStringAux f1 tabLevel ++ " ) => (\n" ++ String.repeat (tabLevel + 1) "  " ++ formulaToStringAux f2 (tabLevel + 1) ++ ")\n"

                Eventually f ->
                    " (F \n" ++ String.repeat (tabLevel + 1) "  " ++ formulaToStringAux f (tabLevel + 1) ++ " )\n" ++ String.repeat tabLevel "  "

                Globally f ->
                    " (G \n" ++ String.repeat (tabLevel + 1) "  " ++ formulaToStringAux f (tabLevel + 1) ++ " )\n" ++ String.repeat tabLevel "  "

                Until f1 f2 ->
                    " (" ++ formulaToStringAux f1 0 ++ " U " ++ formulaToStringAux f2 0 ++ " )\n" ++ String.repeat tabLevel "  "

                BoundedEventually f bound unit ->
                    " (F [0, " ++ String.fromFloat bound ++ " " ++ timeUnitString unit ++ "]\n" ++ String.repeat (tabLevel + 1) "  " ++ formulaToStringAux f (tabLevel + 1) ++ " )\n" ++ String.repeat tabLevel "  "

                BoundedGlobally f bound unit ->
                    " (G [0, " ++ String.fromFloat bound ++ " " ++ timeUnitString unit ++ "]\n" ++ String.repeat (tabLevel + 1) "  " ++ formulaToStringAux f (tabLevel + 1) ++ " )\n" ++ String.repeat tabLevel "  "

                BoundedUntil f1 f2 bound unit ->
                    " (" ++ formulaToStringAux f1 0 ++ " U[0, " ++ String.fromFloat bound ++ " " ++ timeUnitString unit ++ "] " ++ formulaToStringAux f2 0 ++ " )\n" ++ String.repeat tabLevel "  "
    in
    formulaToStringAux formula 0


errorMessage : List DeadEnd -> String
errorMessage errs =
    List.foldl (++) "" <|
        (errs
            |> map
                (\err ->
                    "Error at line: "
                        ++ String.fromInt err.row
                        ++ " column: "
                        ++ String.fromInt err.col
                        ++ " \n  "
                        ++ problemToString err.problem
                        ++ "\n\n"
                )
        )


problemToString : Problem -> String
problemToString problem =
    case problem of
        Expecting string ->
            "Expecting " ++ string ++ " "

        ExpectingVariable ->
            "Expecting Variable"

        ExpectingSymbol string ->
            "Expecting Symbol" ++ string ++ " "

        ExpectingKeyword string ->
            "Expecting Keyword" ++ string ++ " "

        ExpectingEnd ->
            "Expecting End of Rule"

        UnexpectedChar ->
            "Unecpected Character"

        Problem string ->
            "Problem Parsing " ++ string

        BadRepeat ->
            "Bad Repeat"

        _ ->
            ""



-- SEMANTICS


type EvaluatedFormula
    = EvaluatedFalsity Timeline
    | EvaluatedTruth Timeline
    | EvaluatedState String Timeline
    | EvaluatedNot EvaluatedFormula Timeline
    | EvaluatedAnd EvaluatedFormula EvaluatedFormula Timeline
    | EvaluatedOr EvaluatedFormula EvaluatedFormula Timeline
    | EvaluatedImplies EvaluatedFormula EvaluatedFormula Timeline
    | EvaluatedEventually EvaluatedFormula Timeline
    | EvaluatedGlobally EvaluatedFormula Timeline
    | EvaluatedUntil EvaluatedFormula EvaluatedFormula Timeline
    | EvaluatedBoundedEventually EvaluatedFormula Float TimeUnit Timeline
    | EvaluatedBoundedGlobally EvaluatedFormula Float TimeUnit Timeline
    | EvaluatedBoundedUntil EvaluatedFormula EvaluatedFormula Float TimeUnit Timeline


getFormulaTimeline : EvaluatedFormula -> Timeline
getFormulaTimeline ef =
    case ef of
        EvaluatedFalsity timeline ->
            timeline

        EvaluatedTruth timeline ->
            timeline

        EvaluatedState _ timeline ->
            timeline

        EvaluatedNot _ timeline ->
            timeline

        EvaluatedAnd _ _ timeline ->
            timeline

        EvaluatedOr _ _ timeline ->
            timeline

        EvaluatedImplies _ _ timeline ->
            timeline

        EvaluatedEventually _ timeline ->
            timeline

        EvaluatedGlobally _ timeline ->
            timeline

        EvaluatedUntil _ _ timeline ->
            timeline

        EvaluatedBoundedEventually _ _ _ timeline ->
            timeline

        EvaluatedBoundedGlobally _ _ _ timeline ->
            timeline

        EvaluatedBoundedUntil _ _ _ _ timeline ->
            timeline


evaluate : Simulation -> Formula -> List Posix -> EvaluatedFormula
evaluate sim formula times =
    case formula of
        Falsity ->
            times
                |> map (\t -> { time = t, value = False })
                |> EvaluatedFalsity

        Truth ->
            times
                |> map (\t -> { time = t, value = True })
                |> EvaluatedTruth

        State name ->
            let
                stateTimeline =
                    get name sim |> Maybe.withDefault []
            in
            times
                |> map (\t -> { time = t, value = estimate stateTimeline t |> .value })
                |> EvaluatedState name

        Not f ->
            let
                eval_f =
                    evaluate sim f times
            in
            getFormulaTimeline eval_f
                |> map .value
                |> map not
                |> map2 (\t v -> { time = t, value = v }) times
                |> EvaluatedNot eval_f

        And f1 f2 ->
            let
                eval_f1 =
                    evaluate sim f1 times

                eval_f2 =
                    evaluate sim f2 times
            in
            getFormulaTimeline eval_f1
                |> map .value
                |> (getFormulaTimeline eval_f2
                        |> map .value
                        |> map2 (&&)
                   )
                |> map2 (\t v -> { time = t, value = v }) times
                |> EvaluatedAnd eval_f1 eval_f2

        Or f1 f2 ->
            let
                eval_f1 =
                    evaluate sim f1 times

                eval_f2 =
                    evaluate sim f2 times
            in
            getFormulaTimeline eval_f1
                |> map .value
                |> (getFormulaTimeline eval_f2
                        |> map .value
                        |> map2 (||)
                   )
                |> map2 (\t v -> { time = t, value = v }) times
                |> EvaluatedOr eval_f1 eval_f2

        Implies f1 f2 ->
            let
                eval_f1 =
                    evaluate sim f1 times

                eval_f2 =
                    evaluate sim f2 times
            in
            getFormulaTimeline eval_f2
                |> map .value
                |> (getFormulaTimeline eval_f1
                        |> map .value
                        |> map2
                            (\condition result ->
                                if condition then
                                    result

                                else
                                    True
                            )
                   )
                |> map2 (\t v -> { time = t, value = v }) times
                |> EvaluatedImplies eval_f1 eval_f2

        Eventually f ->
            let
                eval_f =
                    evaluate sim f times
            in
            getFormulaTimeline eval_f
                |> map .value
                |> evalEventually
                |> map2 (\t v -> { time = t, value = v }) times
                |> EvaluatedEventually eval_f

        Globally f ->
            let
                eval_f =
                    evaluate sim f times
            in
            getFormulaTimeline eval_f
                |> map .value
                |> evalGlobally
                |> map2 (\t v -> { time = t, value = v }) times
                |> EvaluatedGlobally eval_f

        Until f1 f2 ->
            let
                eval_f1 =
                    evaluate sim f1 times

                eval_f2 =
                    evaluate sim f2 times
            in
            getFormulaTimeline eval_f2
                |> map .value
                |> (getFormulaTimeline eval_f1
                        |> map .value
                        |> evalUntil
                   )
                |> map2 (\t v -> { time = t, value = v }) times
                |> EvaluatedUntil eval_f1 eval_f2

        BoundedEventually f bound units ->
            let
                eval_f =
                    evaluate sim f times

                boundMs =
                    boundToMillis bound units

                boundingFilter evaluationPoint lookAheadPoint =
                    let
                        timeInterval =
                            posixToMillis lookAheadPoint.time - posixToMillis evaluationPoint.time
                    in
                    (timeInterval >= 0) && (timeInterval <= boundMs)
            in
            getFormulaTimeline eval_f
                |> map
                    (\evaluationPoint ->
                        { value =
                            getFormulaTimeline eval_f
                                |> List.filter (boundingFilter evaluationPoint)
                                |> List.any .value
                        , time = evaluationPoint.time
                        }
                    )
                |> EvaluatedBoundedEventually eval_f bound units

        BoundedGlobally f bound units ->
            let
                eval_f =
                    evaluate sim f times

                boundMs =
                    boundToMillis bound units

                boundingFilter evaluationPoint lookAheadPoint =
                    let
                        timeInterval =
                            posixToMillis lookAheadPoint.time - posixToMillis evaluationPoint.time
                    in
                    (timeInterval >= 0) && (timeInterval <= boundMs)
            in
            getFormulaTimeline eval_f
                |> map
                    (\evaluationPoint ->
                        { value =
                            getFormulaTimeline eval_f
                                |> List.filter (boundingFilter evaluationPoint)
                                |> List.all .value
                        , time = evaluationPoint.time
                        }
                    )
                |> EvaluatedBoundedGlobally eval_f bound units

        BoundedUntil f1 f2 bound units ->
            let
                eval_f1 =
                    evaluate sim f1 times

                eval_f2 =
                    evaluate sim f2 times
            in
            --TODO Check this is doing what the docs say it is doing
            evaluate sim (And (Until f1 f2) (BoundedEventually f2 bound units)) times
                |> getFormulaTimeline
                |> EvaluatedBoundedUntil eval_f1 eval_f2 bound units


evalUntil : List Bool -> List Bool -> List Bool
evalUntil input1 input2 =
    case ( input1, input2 ) of
        ( [], _ ) ->
            []

        ( _, [] ) ->
            []

        ( _ :: [], b2 :: [] ) ->
            [ b2 ]

        ( b1 :: rest1, b2 :: rest2 ) ->
            let
                evalRest =
                    evalUntil rest1 rest2
            in
            if b2 then
                True :: evalRest

            else if not b1 then
                False :: evalRest

            else
                case evalRest of
                    [] ->
                        False :: evalRest

                    b :: _ ->
                        b :: evalRest


evalEventually : List Bool -> List Bool
evalEventually input =
    case input of
        [] ->
            []

        b :: [] ->
            [ b ]

        b :: rest ->
            if b then
                True :: evalEventually rest

            else
                let
                    eventually =
                        evalEventually rest
                in
                Bool.Extra.any eventually :: eventually


evalGlobally : List Bool -> List Bool
evalGlobally input =
    case input of
        [] ->
            []

        b :: [] ->
            [ b ]

        b :: rest ->
            if not b then
                False :: evalGlobally rest

            else
                let
                    global =
                        evalGlobally rest
                in
                Bool.Extra.all global :: global
