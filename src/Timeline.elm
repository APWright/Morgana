module Timeline exposing (evalTimes, scale, scaleInvert, timeScrubber, viewTimeline)

import Data exposing (Timeline, estimate)
import Formula exposing (Formula(..))
import Html exposing (Html, div, input, text)
import Html.Attributes as HA exposing (class, type_)
import Html.Events
import Model exposing (Model, Msg(..))
import Svg
import Svg.Attributes as SA
import Time exposing (Posix, millisToPosix, posixToMillis)


resolution : Int
resolution =
    1000


type alias TimeBounds a =
    { a
        | minTime : Posix
        , maxTime : Posix
    }


evalTimes : TimeBounds a -> List Posix
evalTimes model =
    List.range 0 resolution
        |> List.map (\i -> (toFloat i / toFloat resolution) * 100)
        |> List.map (scaleInvert model)


scale : TimeBounds a -> Posix -> Float
scale model time =
    toFloat (posixToMillis time - posixToMillis model.minTime) / toFloat (posixToMillis model.maxTime - posixToMillis model.minTime) * 100


scaleInvert : TimeBounds a -> Float -> Posix
scaleInvert model pct =
    millisToPosix (Basics.round ((pct / 100) * toFloat (posixToMillis model.maxTime - posixToMillis model.minTime) + toFloat (posixToMillis model.minTime)))


timeScrubber : Model -> Html Msg
timeScrubber model =
    div [ class "ScrubberContainer" ]
        [ input
            [ class "ScrubberInput"
            , type_ "range"
            , HA.step "0.01"
            , HA.value (model.currentTime |> scale model |> String.fromFloat)
            , HA.min <| String.fromFloat <| scale model <| model.minTime
            , HA.max <| String.fromFloat <| scale model <| model.maxTime
            , Html.Events.onInput (\s -> String.toFloat s |> Maybe.withDefault 0 |> MoveScrubber)
            ]
            []
        ]


viewTimeline : Model -> String -> Timeline -> Html Msg
viewTimeline model label timeline =
    div [ class "TimelineContainer" ]
        [ div [ class "TimelineLabel" ] [ text ("\t" ++ label) ]
        , div [ class "TimelineView" ]
            [ div []
                [ Svg.svg
                    [ SA.x "0"
                    , SA.y "0"
                    , SA.width "100%"
                    , SA.height "30"
                    , SA.viewBox "0 0 100 100"
                    , SA.preserveAspectRatio "none"
                    , SA.class "TimelineSVG"
                    ]
                    ((let
                        est =
                            estimate timeline model.currentTime

                        coloringString =
                            if est.value then
                                "EstimateTrue"

                            else
                                "EstimateFalse"
                      in
                      Svg.line
                        [ SA.class ("ScrubberResidual " ++ label ++ " " ++ coloringString)
                        , SA.x1 (model.currentTime |> scale model |> String.fromFloat)
                        , SA.x2 (est |> .residualMilliSec |> (\s -> posixToMillis model.currentTime - s) |> millisToPosix |> scale model |> String.fromFloat)
                        , SA.y1 "50"
                        , SA.y2 "50"
                        ]
                        []
                     )
                        :: (timeline
                                |> List.map
                                    (\point ->
                                        Svg.rect
                                            [ point.time
                                                |> scale model
                                                |> String.fromFloat
                                                |> SA.x
                                            , SA.width ((100 / toFloat resolution) |> String.fromFloat)
                                            , SA.y "25"
                                            , SA.height "50"
                                            , SA.class
                                                (label
                                                    ++ " "
                                                    ++ (if point.value then
                                                            "EstimateTrue"

                                                        else
                                                            "EstimateFalse"
                                                       )
                                                )
                                            ]
                                            []
                                    )
                           )
                        ++ [ Svg.line
                                [ SA.class "ScrubberLine"
                                , SA.x1 (model.currentTime |> scale model |> String.fromFloat)
                                , SA.x2 (model.currentTime |> scale model |> String.fromFloat)
                                , SA.y1 "0"
                                , SA.y2 "100"
                                ]
                                []
                           ]
                    )
                ]
            ]
        ]
