module Diagram exposing (untilConnectorLine, viewCanvas)

import Data exposing (Simulation, estimate)
import Formula exposing (EvaluatedFormula(..), Formula(..), getFormulaTimeline, timeUnitString)
import Html exposing (Html, text)
import Html.Attributes exposing (class)
import Model exposing (Model, Msg(..), ViewMode(..))
import String exposing (String)
import Svg exposing (Svg)
import Svg.Attributes as SA
import Time exposing (Posix)
import Zoom exposing (Zoom)


type alias DiagramContext =
    { x : Float
    , y : Float
    , stateHeight : Float
    , glyphSize :
        { width : Float
        , height : Float
        }
    , blockSize : Float
    , time : Posix
    , simulation : Simulation
    }


type EvaluationContext
    = EvalTrue
    | EvalFalse
    | Undetermined


getEvalContext : EvaluationContext -> Bool -> EvaluationContext
getEvalContext ctx estimate =
    if ctx == Undetermined then
        Undetermined

    else if estimate then
        EvalTrue

    else
        EvalFalse


type TargetContext
    = TrueGood
    | FalseGood
    | Neutral


type alias ColoringContext =
    { tgt : TargetContext
    , eval : EvaluationContext
    , violation : Bool
    , colored : Bool
    }


toColorString : ColoringContext -> String -> String
toColorString colorCtx extraClass =
    extraClass
        ++ " "
        ++ (if colorCtx.colored then
                (if colorCtx.violation then
                    "Violation "

                 else
                    "Validation "
                )
                    ++ (case colorCtx.eval of
                            EvalTrue ->
                                "EvalTrue "

                            EvalFalse ->
                                "EvalFalse "

                            Undetermined ->
                                "Undetermined "
                       )
                    ++ (case colorCtx.tgt of
                            TrueGood ->
                                "TrueGood"

                            FalseGood ->
                                "FalseGood"

                            Neutral ->
                                "NeutralTarget"
                       )

            else
                ""
           )


viewCanvas : EvaluatedFormula -> Model -> Float -> Float -> Zoom -> Html Msg
viewCanvas formula model width height zoom =
    let
        ruleEvaluation =
            estimate (getFormulaTimeline formula) model.currentTime |> .value
    in
    Svg.svg
        ([ SA.x "0"
         , SA.y "0"
         , SA.width "100%"
         , SA.height "100%"
         , SA.viewBox ("0 0 " ++ String.fromFloat width ++ " " ++ String.fromFloat height)
         , SA.preserveAspectRatio "xMinYMin meet"
         , SA.class
            ("diagramCanvas "
                ++ (if not ruleEvaluation && (model.viewMode /= Authoring) then
                        "OverallViolation"

                    else
                        ""
                   )
            )
         ]
            ++ Zoom.onDoubleClick zoom ResetZoom
            :: Zoom.onWheel zoom ZoomMsg
            :: Zoom.onDrag zoom ZoomMsg
            ++ Zoom.onGesture zoom ZoomMsg
            ++ Zoom.onTouch zoom ZoomMsg
        )
        [ Svg.g [ Zoom.transform zoom ]
            [ viewDiagram formula
                { x = 10
                , y = 10
                , blockSize = 20
                , glyphSize = { width = 10, height = 20 }
                , stateHeight = 10
                , time = model.currentTime
                , simulation = model.simulation
                }
                { tgt = TrueGood
                , eval =
                    if ruleEvaluation then
                        EvalTrue

                    else
                        EvalFalse
                , violation = not ruleEvaluation
                , colored =
                    case model.viewMode of
                        Authoring ->
                            False

                        Validation ->
                            True

                        Evaluation ->
                            True
                }
            ]
        ]


viewDiagram : EvaluatedFormula -> DiagramContext -> ColoringContext -> Svg Msg
viewDiagram formula ctx clr =
    case formula of
        EvaluatedTruth _ ->
            Svg.g []
                [ Svg.circle
                    [ SA.cx (String.fromFloat (ctx.x + 0.5 * ctx.glyphSize.width))
                    , SA.cy (String.fromFloat (ctx.y + 0.5 * ctx.blockSize))
                    , SA.r (String.fromFloat (0.5 * ctx.glyphSize.width))
                    , SA.class (toColorString { clr | eval = EvalTrue } "shape")
                    ]
                    []
                ]

        EvaluatedFalsity _ ->
            Svg.g []
                [ Svg.circle
                    [ SA.cx (String.fromFloat (ctx.x + 0.5 * ctx.glyphSize.width))
                    , SA.cy (String.fromFloat (ctx.y + 0.5 * ctx.blockSize))
                    , SA.r (String.fromFloat (0.5 * ctx.glyphSize.width))
                    , SA.class (toColorString { clr | eval = EvalFalse } "shape")
                    ]
                    []
                ]

        EvaluatedState name timeline ->
            let
                val =
                    estimate timeline ctx.time |> .value
            in
            Svg.g [ SA.class "stateNode" ]
                [ Svg.rect
                    [ SA.x (String.fromFloat ctx.x)
                    , SA.y (String.fromFloat ctx.y)
                    , SA.width (String.fromFloat (getMonoWidth ctx name))
                    , SA.height (String.fromFloat ctx.stateHeight)
                    , SA.rx "1"
                    , SA.ry "1"
                    , SA.class
                        (toColorString
                            { clr | eval = getEvalContext clr.eval val }
                            "shape stateNodeRect"
                        )
                    ]
                    []
                , Svg.text_
                    [ SA.fill "white"
                    , SA.x (String.fromFloat (ctx.x + 0.25 * ctx.stateHeight))
                    , SA.y (String.fromFloat (ctx.y + 0.85 * ctx.stateHeight))
                    , SA.fontSize (String.fromFloat ctx.stateHeight)
                    , SA.textLength (String.fromFloat (getMonoWidth ctx name - ctx.stateHeight))
                    , SA.fontFamily "Monaco, monospace"
                    , SA.class
                        (toColorString
                            { clr
                                | eval =
                                    if val then
                                        EvalTrue

                                    else
                                        EvalFalse
                            }
                            "text stateNodeText"
                        )
                    ]
                    [ text name ]
                ]

        EvaluatedAnd f1 f2 timeline ->
            let
                f1_bb =
                    boundingBox ctx f1

                f2_bb =
                    boundingBox ctx f2

                estimated_f1 =
                    estimate (getFormulaTimeline f1) ctx.time |> .value

                estimated_f2 =
                    estimate (getFormulaTimeline f2) ctx.time |> .value

                estimated_this =
                    estimate timeline ctx.time |> .value

                view_1 =
                    viewDiagram f1 { ctx | x = ctx.x + ctx.blockSize } clr

                view_2 =
                    viewDiagram f2 { ctx | x = ctx.x + ctx.blockSize, y = ctx.y + f1_bb.y + ctx.blockSize } clr
            in
            Svg.g []
                [ mergeConnector ctx
                    { clr | eval = getEvalContext clr.eval estimated_f1 }
                    { clr | eval = getEvalContext clr.eval estimated_f2 }
                    (ctx.y + 0.5 * f1_bb.y)
                    (ctx.y + f1_bb.y + ctx.blockSize + 0.5 * f2_bb.y)
                    (ctx.y + 0.5 * (f1_bb.y + ctx.blockSize + f2_bb.y))
                    (ctx.y + 0.5 * (f1_bb.y + ctx.blockSize + f2_bb.y))
                , andGlyph ctx
                    { clr | eval = getEvalContext clr.eval estimated_this }
                    (ctx.y + 0.5 * (f1_bb.y + ctx.blockSize + f2_bb.y))
                , view_1
                , view_2
                ]

        EvaluatedOr f1 f2 timeline ->
            let
                f1_bb =
                    boundingBox ctx f1

                f2_bb =
                    boundingBox ctx f2

                view_1 =
                    viewDiagram f1 { ctx | x = ctx.x + ctx.blockSize } clr

                view_2 =
                    viewDiagram f2 { ctx | x = ctx.x + ctx.blockSize, y = ctx.y + f1_bb.y + ctx.blockSize } clr

                estimated_f1 =
                    estimate (getFormulaTimeline f1) ctx.time |> .value

                estimated_f2 =
                    estimate (getFormulaTimeline f2) ctx.time |> .value

                estimated_this =
                    estimate timeline ctx.time |> .value
            in
            Svg.g []
                [ mergeConnector ctx
                    { clr | eval = getEvalContext clr.eval estimated_f1 }
                    { clr | eval = getEvalContext clr.eval estimated_f2 }
                    (ctx.y + 0.5 * f1_bb.y)
                    (ctx.y + f1_bb.y + ctx.blockSize + 0.5 * f2_bb.y)
                    (ctx.y + 0.5 * (f1_bb.y + ctx.blockSize + f2_bb.y) - 0.2 * ctx.glyphSize.height)
                    (ctx.y + 0.5 * (f1_bb.y + ctx.blockSize + f2_bb.y) + 0.2 * ctx.glyphSize.height)
                , orGlyph ctx
                    { clr | eval = getEvalContext clr.eval estimated_this }
                    { clr | eval = getEvalContext clr.eval estimated_f1 }
                    { clr | eval = getEvalContext clr.eval estimated_f2 }
                    (ctx.y + 0.5 * (f1_bb.y + ctx.blockSize + f2_bb.y))
                , view_1
                , view_2
                ]

        EvaluatedImplies f1 f2 timeline ->
            let
                estimated_f1 =
                    estimate (getFormulaTimeline f1) ctx.time |> .value

                estimated_f2 =
                    estimate (getFormulaTimeline f2) ctx.time |> .value

                estimated_this =
                    estimate timeline ctx.time |> .value

                f1_bb =
                    boundingBox ctx f1

                f2_bb =
                    boundingBox ctx f2

                view_1 =
                    viewDiagram f1 { ctx | x = ctx.x + ctx.blockSize } { clr | tgt = Neutral }

                view_2 =
                    viewDiagram f2
                        { ctx
                            | x = ctx.x + ctx.blockSize
                            , y = ctx.y + f1_bb.y + ctx.blockSize
                        }
                        { clr
                            | eval =
                                if not estimated_f1 then
                                    Undetermined

                                else
                                    clr.eval
                        }
            in
            Svg.g []
                [ curvedConnector ctx
                    { clr | eval = getEvalContext clr.eval estimated_f1 }
                    { clr | eval = getEvalContext clr.eval estimated_f2 }
                    (ctx.y + 0.5 * f1_bb.y)
                    (ctx.y + 0.5 * (f1_bb.y + ctx.blockSize + f2_bb.y))
                    (ctx.y + f1_bb.y + ctx.blockSize + 0.5 * f2_bb.y)
                , impliesGlyph ctx
                    { clr | eval = getEvalContext clr.eval estimated_this }
                    { clr | eval = getEvalContext clr.eval estimated_f1 }
                    { clr | eval = getEvalContext clr.eval estimated_f2 }
                    (ctx.y + 0.5 * (f1_bb.y + ctx.blockSize + f2_bb.y))
                , view_1
                , view_2
                ]

        EvaluatedNot f timeline ->
            let
                estimated_this =
                    estimate timeline ctx.time |> .value

                f_bb =
                    boundingBox ctx f

                view_f =
                    viewDiagram f
                        { ctx | x = ctx.x + 0.25 * ctx.blockSize + ctx.glyphSize.width }
                        { clr
                            | tgt =
                                if clr.tgt == TrueGood then
                                    FalseGood

                                else
                                    TrueGood
                        }
            in
            Svg.g []
                [ notGlyph { ctx | y = ctx.y + 0.5 * f_bb.y - 0.5 * ctx.glyphSize.width }
                    { clr
                        | eval = getEvalContext clr.eval estimated_this
                        , tgt =
                            if clr.tgt == TrueGood then
                                FalseGood

                            else
                                TrueGood
                    }
                , view_f
                ]

        EvaluatedEventually f timeline ->
            let
                estimated_this =
                    estimate timeline ctx.time |> .value

                f_bb =
                    boundingBox ctx f

                bracketHeight =
                    ctx.blockSize + max f_bb.y (2 * ctx.blockSize)
            in
            Svg.g []
                [ openBracket ctx clr bracketHeight
                , closedBracket { ctx | x = ctx.x + f_bb.x + 2.5 * ctx.blockSize } { clr | eval = getEvalContext clr.eval estimated_this } bracketHeight
                , eventuallyBacketLine { ctx | x = ctx.x + 0.5 * ctx.blockSize } { clr | eval = getEvalContext clr.eval estimated_this } bracketHeight (f_bb.x + 3 * ctx.blockSize) ctx.x
                , viewDiagram f { ctx | x = ctx.x + 2 * ctx.blockSize, y = ctx.y - 0.5 * (f_bb.y - bracketHeight) } clr
                ]

        EvaluatedGlobally f timeline ->
            let
                estimated_this =
                    estimate timeline ctx.time |> .value

                f_bb =
                    boundingBox ctx f

                bracketHeight =
                    ctx.blockSize + max f_bb.y (2 * ctx.blockSize)
            in
            Svg.g []
                [ openBracket ctx { clr | eval = getEvalContext clr.eval estimated_this } bracketHeight
                , closedBracket { ctx | x = ctx.x + f_bb.x + 2.5 * ctx.blockSize } { clr | eval = getEvalContext clr.eval estimated_this } bracketHeight
                , globallyBracketLine { ctx | x = ctx.x + 0.5 * ctx.blockSize } { clr | eval = getEvalContext clr.eval estimated_this } bracketHeight (f_bb.x + 2.5 * ctx.blockSize)
                , viewDiagram f { ctx | x = ctx.x + 2 * ctx.blockSize, y = ctx.y - 0.5 * (f_bb.y - bracketHeight) } clr
                ]

        EvaluatedUntil f1 f2 timeline ->
            let
                estimated_f1 =
                    estimate (getFormulaTimeline f1) ctx.time |> .value

                estimated_f2 =
                    estimate (getFormulaTimeline f2) ctx.time |> .value

                estimated_this =
                    estimate timeline ctx.time |> .value

                f1_bb =
                    boundingBox ctx f1

                f2_bb =
                    boundingBox ctx f2

                bracketHeight =
                    ctx.blockSize + max (max f1_bb.y f2_bb.y) (2 * ctx.blockSize)

                view_1 =
                    viewDiagram f1 { ctx | x = ctx.x + 2 * ctx.blockSize, y = ctx.y - 0.5 * (f1_bb.y - bracketHeight) } clr

                view_2 =
                    viewDiagram f2 { ctx | x = ctx.x + f1_bb.x + 3 * ctx.blockSize, y = ctx.y - 0.5 * (f2_bb.y - bracketHeight) } clr
            in
            Svg.g []
                [ openBracket ctx { clr | eval = getEvalContext clr.eval estimated_this } bracketHeight
                , closedBracket { ctx | x = ctx.x + f1_bb.x + f2_bb.x + 3.5 * ctx.blockSize } { clr | eval = getEvalContext clr.eval estimated_this } bracketHeight
                , globallyBracketLine { ctx | x = ctx.x + 0.5 * ctx.blockSize } { clr | eval = getEvalContext clr.eval estimated_f1 } bracketHeight (f1_bb.x + 2.0 * ctx.blockSize)
                , eventuallyBacketLine { ctx | x = ctx.x + f1_bb.x + 1.5 * ctx.blockSize } { clr | eval = getEvalContext clr.eval estimated_f2 } bracketHeight (f1_bb.x + f2_bb.x + 4 * ctx.blockSize) ctx.x
                , untilConnectorLine { ctx | x = ctx.x + f1_bb.x + 2.5 * ctx.blockSize } { clr | eval = getEvalContext clr.eval estimated_f2 } bracketHeight
                , view_1
                , view_2
                ]

        EvaluatedBoundedEventually f bound unit timeline ->
            let
                estimated_this =
                    estimate timeline ctx.time |> .value

                f_bb =
                    boundingBox ctx f

                bracketHeight =
                    ctx.blockSize + max f_bb.y (2 * ctx.blockSize)
            in
            Svg.g []
                [ openBracket ctx { clr | eval = getEvalContext clr.eval estimated_this } bracketHeight
                , closedBracket { ctx | x = ctx.x + f_bb.x + 2.5 * ctx.blockSize } { clr | eval = getEvalContext clr.eval estimated_this } bracketHeight
                , eventuallyBacketLine { ctx | x = ctx.x + 0.5 * ctx.blockSize } { clr | eval = getEvalContext clr.eval estimated_this } bracketHeight (f_bb.x + 3 * ctx.blockSize) ctx.x
                , viewDiagram f { ctx | x = ctx.x + 2 * ctx.blockSize, y = ctx.y - 0.5 * (f_bb.y - bracketHeight) } { clr | eval = getEvalContext clr.eval estimated_this }
                , bracketBoundText { ctx | x = ctx.x + f_bb.x + 3 * ctx.blockSize, y = ctx.y + bracketHeight + ctx.stateHeight } { clr | eval = getEvalContext clr.eval estimated_this } (String.fromFloat bound ++ " " ++ timeUnitString unit)
                ]

        EvaluatedBoundedGlobally f bound unit timeline ->
            let
                estimated_this =
                    estimate timeline ctx.time |> .value

                f_bb =
                    boundingBox ctx f

                bracketHeight =
                    ctx.blockSize + max f_bb.y (2 * ctx.blockSize)
            in
            Svg.g []
                [ openBracket ctx { clr | eval = getEvalContext clr.eval estimated_this } bracketHeight
                , closedBracket { ctx | x = ctx.x + f_bb.x + 2.5 * ctx.blockSize } { clr | eval = getEvalContext clr.eval estimated_this } bracketHeight
                , globallyBracketLine { ctx | x = ctx.x + 0.5 * ctx.blockSize } { clr | eval = getEvalContext clr.eval estimated_this } bracketHeight (f_bb.x + 2.5 * ctx.blockSize)
                , viewDiagram f { ctx | x = ctx.x + 2 * ctx.blockSize, y = ctx.y - 0.5 * (f_bb.y - bracketHeight) } { clr | eval = getEvalContext clr.eval estimated_this }
                , bracketBoundText { ctx | x = ctx.x + f_bb.x + 3 * ctx.blockSize, y = ctx.y + bracketHeight + ctx.stateHeight } { clr | eval = getEvalContext clr.eval estimated_this } (String.fromFloat bound ++ " " ++ timeUnitString unit)
                ]

        EvaluatedBoundedUntil f1 f2 bound unit timeline ->
            let
                estimated_f1 =
                    estimate (getFormulaTimeline f1) ctx.time |> .value

                estimated_f2 =
                    estimate (getFormulaTimeline f2) ctx.time |> .value

                estimated_this =
                    estimate timeline ctx.time |> .value

                f1_bb =
                    boundingBox ctx f1

                f2_bb =
                    boundingBox ctx f2

                bracketHeight =
                    ctx.blockSize + max (max f1_bb.y f2_bb.y) (2 * ctx.blockSize)

                view_1 =
                    viewDiagram f1 { ctx | x = ctx.x + 2 * ctx.blockSize, y = ctx.y - 0.5 * (f1_bb.y - bracketHeight) } { clr | eval = getEvalContext clr.eval estimated_this }

                view_2 =
                    viewDiagram f2 { ctx | x = ctx.x + f1_bb.x + 3 * ctx.blockSize, y = ctx.y - 0.5 * (f2_bb.y - bracketHeight) } { clr | eval = getEvalContext clr.eval estimated_this }
            in
            Svg.g []
                [ openBracket ctx { clr | eval = getEvalContext clr.eval estimated_this } bracketHeight
                , closedBracket { ctx | x = ctx.x + f1_bb.x + f2_bb.x + 3.5 * ctx.blockSize } { clr | eval = getEvalContext clr.eval estimated_this } bracketHeight
                , globallyBracketLine { ctx | x = ctx.x + 0.5 * ctx.blockSize } { clr | eval = getEvalContext clr.eval estimated_f1 } bracketHeight (f1_bb.x + 2.0 * ctx.blockSize)
                , eventuallyBacketLine { ctx | x = ctx.x + f1_bb.x + 1.5 * ctx.blockSize } { clr | eval = getEvalContext clr.eval estimated_f2 } bracketHeight (f1_bb.x + f2_bb.x + 4 * ctx.blockSize) ctx.x
                , untilConnectorLine { ctx | x = ctx.x + f1_bb.x + 2.5 * ctx.blockSize } { clr | eval = getEvalContext clr.eval estimated_f2 } bracketHeight
                , bracketBoundText { ctx | x = ctx.x + f1_bb.x + f2_bb.x + 4 * ctx.blockSize, y = ctx.y + bracketHeight + ctx.stateHeight } { clr | eval = getEvalContext clr.eval estimated_this } (String.fromFloat bound ++ " " ++ timeUnitString unit)
                , view_1
                , view_2
                ]


bracketBoundText : DiagramContext -> ColoringContext -> String -> Svg Msg
bracketBoundText ctx clr boundString =
    Svg.text_
        [ SA.fill "black"
        , SA.x (String.fromFloat ctx.x)
        , SA.y (String.fromFloat ctx.y)
        , SA.fontSize (String.fromFloat ctx.stateHeight)
        , SA.textAnchor "end"
        , SA.fontFamily "Monaco, monospace"
        , SA.class (toColorString clr "text shape boundText")
        ]
        [ text boundString ]


untilConnectorLine : DiagramContext -> ColoringContext -> Float -> Svg Msg
untilConnectorLine ctx clr bracketHeight =
    Svg.line
        [ SA.x1 (String.fromFloat ctx.x)
        , SA.y1 (String.fromFloat ctx.y)
        , SA.x2 (String.fromFloat ctx.x)
        , SA.y2 (String.fromFloat (ctx.y + bracketHeight))
        , SA.stroke "black"
        , SA.strokeWidth "1"
        , SA.strokeLinecap "round"
        , SA.class (toColorString clr "line")
        ]
        []


globallyBracketLine : DiagramContext -> ColoringContext -> Float -> Float -> Svg Msg
globallyBracketLine ctx clr bracketHeight bracketWidth =
    Svg.g []
        [ Svg.line
            [ SA.x1 (String.fromFloat ctx.x)
            , SA.y1 (String.fromFloat ctx.y)
            , SA.x2 (String.fromFloat (ctx.x + bracketWidth))
            , SA.y2 (String.fromFloat ctx.y)
            , SA.stroke "black"
            , SA.strokeWidth "1"
            , SA.class (toColorString clr "line")
            ]
            []
        , Svg.path
            [ SA.d
                ("M "
                    ++ String.fromFloat (ctx.x + 1.5 * ctx.blockSize)
                    ++ " "
                    ++ String.fromFloat (ctx.y + 0.5 * bracketHeight)
                    ++ " "
                    ++ " C "
                    ++ String.fromFloat (ctx.x + 1.0 * ctx.blockSize)
                    ++ " "
                    ++ String.fromFloat (ctx.y + 0.5 * bracketHeight)
                    ++ ", "
                    ++ String.fromFloat (ctx.x + 1.0 * ctx.blockSize)
                    ++ " "
                    ++ String.fromFloat (ctx.y + 0.5 * bracketHeight)
                    ++ ", "
                    ++ String.fromFloat (ctx.x + 1.0 * ctx.blockSize)
                    ++ " "
                    ++ String.fromFloat ctx.y
                )
            , SA.strokeWidth "1.0"
            , SA.strokeLinecap "round"
            , SA.stroke "black"
            , SA.fill "transparent"
            , SA.class (toColorString clr "line")
            ]
            []
        ]


eventuallyBacketLine : DiagramContext -> ColoringContext -> Float -> Float -> Float -> Svg Msg
eventuallyBacketLine ctx clr bracketHeight bracketWidth bracketStart =
    Svg.g []
        [ Svg.circle
            [ SA.cx (String.fromFloat (ctx.x + ctx.blockSize))
            , SA.cy (String.fromFloat (ctx.y + bracketHeight))
            , SA.r "2"
            , SA.class (toColorString clr "shape")
            ]
            []
        , Svg.line
            [ SA.x1 (String.fromFloat bracketStart)
            , SA.y1 (String.fromFloat (ctx.y + bracketHeight))
            , SA.x2 (String.fromFloat (bracketStart + bracketWidth))
            , SA.y2 (String.fromFloat (ctx.y + bracketHeight))
            , SA.stroke "black"
            , SA.strokeWidth "1"
            , SA.strokeDasharray "1 3"
            , SA.class (toColorString clr "line")
            ]
            []
        , Svg.path
            [ SA.d
                ("M "
                    ++ String.fromFloat (ctx.x + 1.5 * ctx.blockSize)
                    ++ " "
                    ++ String.fromFloat (ctx.y + 0.5 * bracketHeight)
                    ++ " "
                    ++ " C "
                    ++ String.fromFloat (ctx.x + 1.0 * ctx.blockSize)
                    ++ " "
                    ++ String.fromFloat (ctx.y + 0.5 * bracketHeight)
                    ++ ", "
                    ++ String.fromFloat (ctx.x + 1.0 * ctx.blockSize)
                    ++ " "
                    ++ String.fromFloat (ctx.y + 0.5 * bracketHeight)
                    ++ ", "
                    ++ String.fromFloat (ctx.x + 1.0 * ctx.blockSize)
                    ++ " "
                    ++ String.fromFloat (ctx.y + 1.0 * bracketHeight)
                )
            , SA.strokeWidth "1.0"
            , SA.strokeLinecap "round"
            , SA.stroke "black"
            , SA.fill "transparent"
            , SA.class (toColorString clr "line")
            ]
            []
        ]


openBracket : DiagramContext -> ColoringContext -> Float -> Svg Msg
openBracket ctx clr height =
    Svg.g
        [ SA.stroke "black"
        , SA.strokeLinecap "round"
        , SA.class (toColorString clr "line")
        ]
        [ Svg.line
            [ SA.x1 (String.fromFloat ctx.x)
            , SA.y1 (String.fromFloat ctx.y)
            , SA.x2 (String.fromFloat (ctx.x + 0.5 * ctx.blockSize))
            , SA.y2 (String.fromFloat ctx.y)
            ]
            []
        , Svg.line
            [ SA.x1 (String.fromFloat ctx.x)
            , SA.y1 (String.fromFloat ctx.y)
            , SA.x2 (String.fromFloat ctx.x)
            , SA.y2 (String.fromFloat (ctx.y + height))
            ]
            []
        , Svg.line
            [ SA.x1 (String.fromFloat ctx.x)
            , SA.y1 (String.fromFloat (ctx.y + height))
            , SA.x2 (String.fromFloat (ctx.x + 0.5 * ctx.blockSize))
            , SA.y2 (String.fromFloat (ctx.y + height))
            ]
            []
        ]


closedBracket : DiagramContext -> ColoringContext -> Float -> Svg Msg
closedBracket ctx clr height =
    Svg.g
        [ SA.stroke "black"
        , SA.strokeLinecap "round"
        , SA.class (toColorString clr "line")
        ]
        [ Svg.line
            [ SA.x1 (String.fromFloat (ctx.x + 0.5 * ctx.blockSize))
            , SA.y1 (String.fromFloat ctx.y)
            , SA.x2 (String.fromFloat ctx.x)
            , SA.y2 (String.fromFloat ctx.y)
            ]
            []
        , Svg.line
            [ SA.x1 (String.fromFloat (ctx.x + 0.5 * ctx.blockSize))
            , SA.y1 (String.fromFloat ctx.y)
            , SA.x2 (String.fromFloat (ctx.x + 0.5 * ctx.blockSize))
            , SA.y2 (String.fromFloat (ctx.y + height))
            ]
            []
        , Svg.line
            [ SA.x1 (String.fromFloat (ctx.x + 0.5 * ctx.blockSize))
            , SA.y1 (String.fromFloat (ctx.y + height))
            , SA.x2 (String.fromFloat ctx.x)
            , SA.y2 (String.fromFloat (ctx.y + height))
            ]
            []
        ]


impliesGlyph : DiagramContext -> ColoringContext -> ColoringContext -> ColoringContext -> Float -> Svg Msg
impliesGlyph ctx clr clrPre clrPost nodeY =
    let
        ( inputX, inputY ) =
            ( ctx.x + 0.5 * ctx.glyphSize.width, nodeY - 0.5 * ctx.glyphSize.height )

        outputCapHeight =
            ctx.glyphSize.height * 0.25

        ( outputX, outputY ) =
            ( inputX, inputY + ctx.glyphSize.height - outputCapHeight )
    in
    Svg.g []
        [ Svg.rect
            [ SA.x (String.fromFloat ctx.x)
            , SA.y (String.fromFloat (nodeY - 0.5 * ctx.glyphSize.height))
            , SA.height (String.fromFloat ctx.glyphSize.height)
            , SA.width (String.fromFloat ctx.glyphSize.width)
            , SA.rx (String.fromFloat (0.5 * ctx.glyphSize.width))
            , SA.fill "#CECECE"
            , SA.class (toColorString clr "inner shape")
            ]
            []
        , Svg.line
            [ SA.x1 (String.fromFloat inputX)
            , SA.y1 (String.fromFloat inputY)
            , SA.x2 (String.fromFloat outputX)
            , SA.y2
                (String.fromFloat
                    (if clr.colored && (clrPre.eval == EvalFalse) then
                        outputY - 1.0 * ctx.glyphSize.width

                     else
                        outputY - 0.5
                    )
                )
            , SA.stroke "black"
            , SA.strokeLinecap "round"
            , SA.class (toColorString clrPre "line")
            ]
            []
        , Svg.path
            [ SA.class (toColorString clrPost "shape")
            , SA.d
                ("M "
                    ++ String.fromFloat ctx.x
                    ++ " "
                    ++ String.fromFloat outputY
                    ++ " h "
                    ++ String.fromFloat ctx.glyphSize.width
                    ++ " V "
                    ++ String.fromFloat (outputY + outputCapHeight - 0.5 * ctx.glyphSize.width)
                    ++ " A "
                    ++ String.fromFloat (0.5 * ctx.glyphSize.width)
                    ++ " "
                    ++ String.fromFloat (0.5 * ctx.glyphSize.width)
                    ++ " 0 0 1 "
                    ++ String.fromFloat outputX
                    ++ " "
                    ++ String.fromFloat (outputY + outputCapHeight)
                    ++ " A "
                    ++ String.fromFloat (0.5 * ctx.glyphSize.width)
                    ++ " "
                    ++ String.fromFloat (0.5 * ctx.glyphSize.width)
                    ++ " 0 0 1 "
                    ++ String.fromFloat ctx.x
                    ++ " "
                    ++ String.fromFloat outputY
                    ++ " Z"
                )
            ]
            []
        , Svg.line
            [ SA.x1 (String.fromFloat outputX)
            , SA.y1 (String.fromFloat (outputY - 0.5))
            , SA.x2 (String.fromFloat (outputX + 0.3 * ctx.glyphSize.width))
            , SA.y2 (String.fromFloat (outputY - 0.45 * ctx.glyphSize.width))
            , SA.stroke "black"
            , SA.strokeLinecap "round"
            , SA.class (toColorString clrPre "line")
            ]
            []
        , Svg.line
            [ SA.x1 (String.fromFloat outputX)
            , SA.y1 (String.fromFloat (outputY - 0.5))
            , SA.x2 (String.fromFloat (outputX - 0.3 * ctx.glyphSize.width))
            , SA.y2 (String.fromFloat (outputY - 0.45 * ctx.glyphSize.width))
            , SA.stroke "black"
            , SA.strokeLinecap "round"
            , SA.class (toColorString clrPre "line")
            ]
            []
        ]


andGlyph : DiagramContext -> ColoringContext -> Float -> Svg Msg
andGlyph ctx clr nodeY =
    Svg.g []
        [ Svg.rect
            [ SA.x (String.fromFloat ctx.x)
            , SA.y (String.fromFloat (nodeY - 0.5 * ctx.glyphSize.height))
            , SA.height (String.fromFloat ctx.glyphSize.height)
            , SA.width (String.fromFloat ctx.glyphSize.width)
            , SA.rx (String.fromFloat (0.5 * ctx.glyphSize.width))
            , SA.class (toColorString clr "shape")
            ]
            []
        , Svg.rect
            [ SA.x (String.fromFloat (ctx.x + 0.1 * ctx.glyphSize.width))
            , SA.y (String.fromFloat (nodeY - 0.45 * ctx.glyphSize.height))
            , SA.height (String.fromFloat (0.9 * ctx.glyphSize.height))
            , SA.width (String.fromFloat (0.8 * ctx.glyphSize.width))
            , SA.rx (String.fromFloat (0.4 * ctx.glyphSize.width))
            , SA.fill "#CECECE"
            , SA.class (toColorString clr "inner shape")
            ]
            []
        ]


orGlyph : DiagramContext -> ColoringContext -> ColoringContext -> ColoringContext -> Float -> Svg Msg
orGlyph ctx clr clr1 clr2 nodeY =
    Svg.g []
        [ Svg.rect
            [ SA.x (String.fromFloat ctx.x)
            , SA.y (String.fromFloat (nodeY - 0.5 * ctx.glyphSize.height))
            , SA.height (String.fromFloat ctx.glyphSize.height)
            , SA.width (String.fromFloat ctx.glyphSize.width)
            , SA.rx (String.fromFloat (0.5 * ctx.glyphSize.width))
            , SA.class (toColorString clr "shape")
            ]
            []
        , Svg.rect
            [ SA.x (String.fromFloat (ctx.x + 0.1 * ctx.glyphSize.width))
            , SA.y (String.fromFloat (nodeY - 0.45 * ctx.glyphSize.height))
            , SA.height (String.fromFloat (0.4 * ctx.glyphSize.height))
            , SA.width (String.fromFloat (0.8 * ctx.glyphSize.width))
            , SA.rx (String.fromFloat (0.4 * ctx.glyphSize.width))
            , SA.fill "#CECECE"
            , SA.class ("inner " ++ toColorString clr1 "shape")
            ]
            []
        , Svg.rect
            [ SA.x (String.fromFloat (ctx.x + 0.1 * ctx.glyphSize.width))
            , SA.y (String.fromFloat (nodeY + 0.05 * ctx.glyphSize.height))
            , SA.height (String.fromFloat (0.4 * ctx.glyphSize.height))
            , SA.width (String.fromFloat (0.8 * ctx.glyphSize.width))
            , SA.rx (String.fromFloat (0.4 * ctx.glyphSize.width))
            , SA.fill "#CECECE"
            , SA.class ("inner " ++ toColorString clr2 "shape")
            ]
            []
        ]


notGlyph : DiagramContext -> ColoringContext -> Svg Msg
notGlyph ctx clr =
    Svg.g []
        [ Svg.circle
            [ SA.cx (String.fromFloat (ctx.x + 0.5 * ctx.glyphSize.width))
            , SA.cy (String.fromFloat (ctx.y + 0.5 * ctx.glyphSize.width))
            , SA.r (String.fromFloat (0.5 * ctx.glyphSize.width))
            , SA.class (toColorString clr "shape")
            ]
            []
        , Svg.circle
            [ SA.cx (String.fromFloat (ctx.x + 0.5 * ctx.glyphSize.width))
            , SA.cy (String.fromFloat (ctx.y + 0.5 * ctx.glyphSize.width))
            , SA.r (String.fromFloat (0.4 * ctx.glyphSize.width))
            , SA.fill "#CECECE"
            , SA.class ("inner " ++ toColorString clr "shape")
            ]
            []
        , Svg.line
            [ SA.x1 (String.fromFloat (ctx.x + ctx.glyphSize.width))
            , SA.y1 (String.fromFloat ctx.y)
            , SA.x2 (String.fromFloat ctx.x)
            , SA.y2 (String.fromFloat (ctx.y + ctx.glyphSize.width))
            , SA.stroke "black"
            , SA.strokeDasharray (String.fromFloat (0.9 * ctx.glyphSize.width))
            , SA.strokeDashoffset (String.fromFloat (-0.25 * ctx.glyphSize.width))
            , SA.class (toColorString clr "line")
            ]
            []
        , Svg.line
            [ SA.x1 (String.fromFloat (ctx.x + ctx.glyphSize.width))
            , SA.y1 (String.fromFloat (ctx.y + 0.5 * ctx.glyphSize.width))
            , SA.x2 (String.fromFloat (ctx.x + ctx.glyphSize.width + 0.25 * ctx.blockSize))
            , SA.y2 (String.fromFloat (ctx.y + 0.5 * ctx.glyphSize.width))
            , SA.stroke "black"
            , SA.class (toColorString clr "line")
            ]
            []
        ]


mergeConnector : DiagramContext -> ColoringContext -> ColoringContext -> Float -> Float -> Float -> Float -> Svg Msg
mergeConnector ctx clr1 clr2 sourceY1 sourceY2 targetY1 targetY2 =
    let
        ( sourceX, targetX ) =
            ( ctx.x + ctx.blockSize, ctx.x + 0.75 * ctx.glyphSize.width )
    in
    Svg.g []
        [ Svg.path
            [ SA.d
                ("M "
                    ++ String.fromFloat sourceX
                    ++ " "
                    ++ String.fromFloat sourceY1
                    ++ " "
                    ++ " C "
                    ++ String.fromFloat (0.25 * sourceX + 0.75 * targetX)
                    ++ " "
                    ++ String.fromFloat sourceY1
                    ++ ", "
                    ++ String.fromFloat (0.75 * sourceX + 0.25 * targetX)
                    ++ " "
                    ++ String.fromFloat targetY1
                    ++ ", "
                    ++ String.fromFloat targetX
                    ++ " "
                    ++ String.fromFloat targetY1
                )
            , SA.strokeWidth "1.0"
            , SA.strokeLinecap "round"
            , SA.stroke "black"
            , SA.fill "transparent"
            , SA.class (toColorString clr1 "line")
            ]
            []
        , Svg.path
            [ SA.d
                ("M "
                    ++ String.fromFloat sourceX
                    ++ " "
                    ++ String.fromFloat sourceY2
                    ++ " "
                    ++ " C "
                    ++ String.fromFloat (0.5 * (sourceX + targetX))
                    ++ " "
                    ++ String.fromFloat sourceY2
                    ++ ", "
                    ++ String.fromFloat (0.5 * (sourceX + targetX))
                    ++ " "
                    ++ String.fromFloat targetY2
                    ++ ", "
                    ++ String.fromFloat targetX
                    ++ " "
                    ++ String.fromFloat targetY2
                )
            , SA.strokeWidth "1.0"
            , SA.strokeLinecap "round"
            , SA.stroke "black"
            , SA.fill "transparent"
            , SA.class (toColorString clr2 "line")
            ]
            []
        ]


curvedConnector : DiagramContext -> ColoringContext -> ColoringContext -> Float -> Float -> Float -> Svg Msg
curvedConnector ctx clr1 clr2 sourceY nodeY targetY =
    let
        ( sourceX, targetX, nodeX ) =
            ( ctx.x + ctx.blockSize, ctx.x + ctx.blockSize, ctx.x + 0.5 * ctx.glyphSize.width )
    in
    Svg.g []
        [ Svg.path
            [ SA.d
                ("M "
                    ++ String.fromFloat sourceX
                    ++ " "
                    ++ String.fromFloat sourceY
                    ++ " "
                    ++ " C "
                    ++ String.fromFloat (1.0 * nodeX + 0.0 * sourceX)
                    ++ " "
                    ++ String.fromFloat sourceY
                    ++ ", "
                    ++ String.fromFloat nodeX
                    ++ " "
                    ++ String.fromFloat (0.0 * nodeY + 1.0 * sourceY)
                    ++ ", "
                    ++ String.fromFloat nodeX
                    ++ " "
                    ++ String.fromFloat (nodeY - 0.5 * ctx.glyphSize.height)
                )
            , SA.strokeWidth "1.0"
            , SA.strokeLinecap "round"
            , SA.stroke "black"
            , SA.fill "transparent"
            , SA.class (toColorString clr1 "line")
            ]
            []
        , Svg.path
            [ SA.d
                ("M "
                    ++ String.fromFloat nodeX
                    ++ " "
                    ++ String.fromFloat (nodeY + 0.5 * ctx.glyphSize.height)
                    ++ " "
                    ++ " C "
                    ++ String.fromFloat nodeX
                    ++ " "
                    ++ String.fromFloat (0.0 * nodeY + 1.0 * targetY)
                    ++ ", "
                    ++ String.fromFloat (1.0 * nodeX + 0.0 * targetX)
                    ++ " "
                    ++ String.fromFloat targetY
                    ++ ", "
                    ++ String.fromFloat targetX
                    ++ " "
                    ++ String.fromFloat targetY
                )
            , SA.strokeWidth "1.0"
            , SA.strokeLinecap "round"
            , SA.stroke "black"
            , SA.fill "transparent"
            , SA.class (toColorString clr2 "line")
            ]
            []
        ]


getMonoWidth : DiagramContext -> String -> Float
getMonoWidth ctx text =
    --Uses Monaco normal aspect of 0.55 (other monospace fonts can be as low as 0.45) and adding margin of ctx.stateheight
    toFloat (String.length text) * 0.55 * ctx.stateHeight + ctx.stateHeight


boundingBox : DiagramContext -> EvaluatedFormula -> { x : Float, y : Float }
boundingBox ctx formula =
    case formula of
        EvaluatedTruth _ ->
            { x = ctx.glyphSize.width, y = ctx.glyphSize.height }

        EvaluatedFalsity _ ->
            { x = ctx.glyphSize.width, y = ctx.glyphSize.height }

        EvaluatedState name _ ->
            { x = getMonoWidth ctx name, y = ctx.stateHeight }

        EvaluatedImplies f1 f2 _ ->
            let
                f1_bb =
                    boundingBox ctx f1

                f2_bb =
                    boundingBox ctx f2
            in
            { x = max f1_bb.x f2_bb.x + ctx.blockSize, y = f1_bb.y + ctx.blockSize + f2_bb.y }

        EvaluatedAnd f1 f2 _ ->
            let
                f1_bb =
                    boundingBox ctx f1

                f2_bb =
                    boundingBox ctx f2
            in
            { x = max f1_bb.x f2_bb.x + ctx.blockSize, y = f1_bb.y + ctx.blockSize + f2_bb.y }

        EvaluatedOr f1 f2 _ ->
            let
                f1_bb =
                    boundingBox ctx f1

                f2_bb =
                    boundingBox ctx f2
            in
            { x = max f1_bb.x f2_bb.x + ctx.blockSize, y = f1_bb.y + ctx.blockSize + f2_bb.y }

        EvaluatedNot f _ ->
            let
                f_bb =
                    boundingBox ctx f
            in
            { x = f_bb.x + 0.25 * ctx.blockSize + ctx.glyphSize.width, y = max ctx.glyphSize.width f_bb.y }

        EvaluatedEventually f _ ->
            let
                f_bb =
                    boundingBox ctx f
            in
            { x = f_bb.x + 3 * ctx.blockSize, y = ctx.blockSize + max f_bb.y (2 * ctx.blockSize) }

        EvaluatedGlobally f _ ->
            let
                f_bb =
                    boundingBox ctx f
            in
            { x = f_bb.x + 3 * ctx.blockSize, y = ctx.blockSize + max f_bb.y (2 * ctx.blockSize) }

        EvaluatedUntil f1 f2 _ ->
            let
                f1_bb =
                    boundingBox ctx f1

                f2_bb =
                    boundingBox ctx f2
            in
            { x = f1_bb.x + f2_bb.x + 4 * ctx.blockSize, y = ctx.blockSize + max (2 * ctx.blockSize) (max f2_bb.y f1_bb.y) }

        EvaluatedBoundedEventually f _ _ _ ->
            let
                f_bb =
                    boundingBox ctx f
            in
            { x = f_bb.x + 3 * ctx.blockSize, y = ctx.stateHeight + ctx.blockSize + max f_bb.y (2 * ctx.blockSize) }

        EvaluatedBoundedGlobally f _ _ _ ->
            let
                f_bb =
                    boundingBox ctx f
            in
            { x = f_bb.x + 3 * ctx.blockSize, y = ctx.stateHeight + ctx.blockSize + max f_bb.y (2 * ctx.blockSize) }

        EvaluatedBoundedUntil f1 f2 _ _ _ ->
            let
                f1_bb =
                    boundingBox ctx f1

                f2_bb =
                    boundingBox ctx f2
            in
            { x = f1_bb.x + f2_bb.x + 4 * ctx.blockSize, y = ctx.stateHeight + ctx.blockSize + max (2 * ctx.blockSize) (max f2_bb.y f1_bb.y) }
