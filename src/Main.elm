module Main exposing (main)

import Arborist.Settings exposing (canvasWidth)
import Browser
import Data exposing (HelixData, data, decodeRule, decodeSimulation, encodeRule, encodeSimulation, getMaxTime, getMinTime)
import Diagram exposing (viewCanvas)
import Dict
import File
import File.Download as Download
import File.Select as Select
import Formula exposing (Formula(..), errorMessage, evaluate, formulaToString, getFormulaTimeline, parse)
import Html exposing (Html, a, button, div, h4, li, node, option, p, pre, select, text, textarea, ul)
import Html.Attributes as HA exposing (class, style)
import Html.Events exposing (onClick, onInput)
import Json.Decode
import Json.Encode
import Model exposing (Model, Msg(..), ViewMode(..))
import Platform.Sub as Sub
import Result
import String exposing (String)
import Task
import Timeline exposing (evalTimes, scaleInvert, timeScrubber, viewTimeline)
import Zoom



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Zoom.subscriptions model.zoom ZoomMsg
        ]



--Init


initial_input : String
initial_input =
    "TRUE"


initial_formula : Formula
initial_formula =
    Result.withDefault Falsity (parse initial_input)


initial_metadata : HelixData
initial_metadata =
    { operationalCategory = ""
    , level = ""
    , cognizantEngineer = ""
    , flightRuleID = ""
    , flightRuleTitle = ""
    , description = ""
    , rationale = ""
    , violationImpact = ""
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { input = initial_input
      , output = initial_formula
      , parseError = Nothing
      , simulation = Data.data
      , currentTime = Data.getMinTime Data.data
      , minTime = Data.getMinTime Data.data
      , maxTime = Data.getMaxTime Data.data
      , evalRule =
            evaluate Data.data
                initial_formula
                (evalTimes
                    { minTime = Data.getMinTime Data.data
                    , maxTime =
                        Data.getMaxTime Data.data
                    }
                )
      , zoom =
            Zoom.init { width = canvasWidth, height = canvasHeight }
                |> Zoom.scaleExtent 0.3 10
      , showLegend = False
      , viewMode = Authoring
      , helixData = initial_metadata
      }
    , Cmd.none
    )



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CodeChange newInput ->
            ( { model | input = newInput }, Cmd.none )

        Parse ->
            let
                parseOut =
                    parse model.input

                new_formula =
                    parseOut |> Result.withDefault Falsity
            in
            ( { model
                | output = new_formula
                , parseError =
                    case parseOut of
                        Ok _ ->
                            Nothing

                        Err err ->
                            Just err
                , evalRule = evaluate model.simulation new_formula (evalTimes model)
              }
            , Cmd.none
            )

        ZoomMsg zoomMsg ->
            ( { model
                | zoom = Zoom.update zoomMsg model.zoom
              }
            , Cmd.none
            )

        MoveScrubber t ->
            ( { model
                | currentTime = scaleInvert model t
              }
            , Cmd.none
            )

        ResetZoom _ ->
            let
                initZoom =
                    Zoom.init { width = canvasWidth, height = canvasHeight }
                        |> Zoom.scaleExtent 0.3 10
            in
            ( { model | zoom = initZoom }
            , Cmd.none
            )

        ShowLegend b ->
            ( { model | showLegend = b }, Cmd.none )

        ChangeMode mode ->
            ( { model | viewMode = mode }, Cmd.none )

        DownloadSim ->
            ( model
            , Download.string "simulation.json" "application/json" (Json.Encode.encode 4 (encodeSimulation model.simulation))
            )

        SimFileRequested ->
            ( model
            , Select.file [ "application/json" ] SimFileSelected
            )

        SimFileSelected file ->
            ( model
            , Task.perform SimFileLoaded (File.toString file)
            )

        SimFileLoaded content ->
            let
                decodedFile =
                    decodeSimulation content
            in
            case decodedFile of
                Ok sim ->
                    update Parse { model | simulation = sim, minTime = getMinTime sim, maxTime = getMaxTime sim }

                Err _ ->
                    ( model, Cmd.none )

        DownloadRule ->
            ( model
            , Download.string (model.helixData.flightRuleID ++ ".json")
                "application/json"
                (Json.Encode.encode 4 (encodeRule model.input model.helixData))
            )

        RuleFileRequested ->
            ( model
            , Select.file [ "application/json" ] RuleFileSelected
            )

        RuleFileSelected file ->
            ( model
            , Task.perform RuleFileLoaded (File.toString file)
            )

        RuleFileLoaded content ->
            let
                decodedFile =
                    decodeRule content
            in
            case decodedFile of
                Ok { ruleText, helixData } ->
                    update Parse { model | input = ruleText, helixData = helixData }

                Err _ ->
                    ( model, Cmd.none )

        UpdateOperationalCategory value ->
            let
                oldhelix =
                    model.helixData

                newhelix =
                    { oldhelix | operationalCategory = value }
            in
            ( { model | helixData = newhelix }, Cmd.none )

        UpdateLevel value ->
            let
                oldhelix =
                    model.helixData

                newhelix =
                    { oldhelix | level = value }
            in
            ( { model | helixData = newhelix }, Cmd.none )

        UpdateCognizantEngineer value ->
            let
                oldhelix =
                    model.helixData

                newhelix =
                    { oldhelix | cognizantEngineer = value }
            in
            ( { model | helixData = newhelix }, Cmd.none )

        UpdateFlightRuleID value ->
            let
                oldhelix =
                    model.helixData

                newhelix =
                    { oldhelix | flightRuleID = value }
            in
            ( { model | helixData = newhelix }, Cmd.none )

        UpdateFlightRuleTitle value ->
            let
                oldhelix =
                    model.helixData

                newhelix =
                    { oldhelix | flightRuleTitle = value }
            in
            ( { model | helixData = newhelix }, Cmd.none )

        UpdateDescription value ->
            let
                oldhelix =
                    model.helixData

                newhelix =
                    { oldhelix | description = value }
            in
            ( { model | helixData = newhelix }, Cmd.none )

        UpdateRationale value ->
            let
                oldhelix =
                    model.helixData

                newhelix =
                    { oldhelix | rationale = value }
            in
            ( { model | helixData = newhelix }, Cmd.none )

        UpdateViolationImpact value ->
            let
                oldhelix =
                    model.helixData

                newhelix =
                    { oldhelix | violationImpact = value }
            in
            ( { model | helixData = newhelix }, Cmd.none )

        NewRule ->
            update Parse
                { model
                    | viewMode = Authoring
                    , helixData =
                        { operationalCategory = ""
                        , level = ""
                        , cognizantEngineer = ""
                        , flightRuleID = ""
                        , flightRuleTitle = ""
                        , description = ""
                        , rationale = ""
                        , violationImpact = ""
                        }
                    , input = ""
                }

        NoMsg ->
            ( model, Cmd.none )



-- VIEW


canvasWidth : Float
canvasWidth =
    600


canvasHeight : Float
canvasHeight =
    200


decodeViewModeSelector : String -> Msg
decodeViewModeSelector dropdownString =
    case dropdownString of
        "Authoring" ->
            ChangeMode Authoring

        "Validation" ->
            ChangeMode Validation

        "Evaluation" ->
            ChangeMode Evaluation

        _ ->
            NoMsg


view : Model -> Html Msg
view model =
    div []
        [ ul []
            [ li [ style "font-size" "16px", style "float" "left", style "letter-spacing" "4px" ] [ text "MORGANA | FLIGHT RULES STUDIO" ]
            , select [ onInput decodeViewModeSelector, style "float" "left" ]
                [ option [ HA.selected (model.viewMode == Authoring) ] [ text "Authoring" ]
                , option [ HA.selected (model.viewMode == Validation) ] [ text "Validation" ]
                , option [ HA.selected (model.viewMode == Evaluation) ] [ text "Evaluation" ]
                ]

            -- , li [] [ a [] [ text "Settings" ] ]
            , li [ onClick (ShowLegend True) ] [ a [] [ text "Legend" ] ]
            , li [ onClick SimFileRequested ] [ a [] [ text "Load Simulation" ] ]
            , li [ onClick RuleFileRequested ] [ a [] [ text "Load Rule" ] ]
            , li [ onClick NewRule ] [ a [] [ text "New Rule" ] ]
            ]
        , if model.showLegend then
            div [ class "legend-overlay", onClick (ShowLegend False) ] [ Html.img [ HA.src "../flightrules/images/legend.png", class "legend-img" ] [] ]

          else
            text ""
        , div [ class "container" ]
            [ div [ class "left" ]
                (case model.viewMode of
                    Authoring ->
                        [ div [ class "canvas fullHeight", style "height" "100%", style "max-height" "100%" ] [ viewCanvas model.evalRule model canvasWidth (2 * canvasHeight) model.zoom ] ]

                    Validation ->
                        [ div [ class "canvas" ] [ viewCanvas model.evalRule model canvasWidth canvasHeight model.zoom ]
                        , div [ class "timeline" ]
                            ([ timeScrubber model
                             , viewTimeline model "Rule" (getFormulaTimeline model.evalRule)
                             ]
                                ++ (model.simulation
                                        |> Dict.map (\name timeline -> viewTimeline model name timeline)
                                        |> Dict.values
                                   )
                                ++ [ div [ class "blank-buffer" ] []
                                   ]
                            )
                        ]

                    Evaluation ->
                        [ div [ class "canvas" ] [ viewCanvas model.evalRule model canvasWidth canvasHeight model.zoom ]
                        , div [ class "timeline" ]
                            [ timeScrubber model
                            , viewTimeline model "Rule" (getFormulaTimeline model.evalRule)
                            , button [] [ text "Upload Simulation" ]
                            , div [ class "blank-buffer" ] []
                            ]
                        ]
                )
            , div
                [ class "right" ]
                [ div [] [ viewEditor model ]
                , div []
                    [ button [ onClick Parse ] [ text "Parse" ]
                    , button [ onClick DownloadRule ] [ text "Download" ]
                    ]
                , div [ class "formulaText" ]
                    [ pre []
                        [ case model.parseError of
                            Nothing ->
                                text (formulaToString model.output)

                            Just err ->
                                text (errorMessage err)
                        ]
                    ]
                , viewHelixData model
                , div [ class "blank-buffer" ] []
                ]
            ]
        ]


viewHelixData : Model -> Html Msg
viewHelixData model =
    if model.viewMode == Authoring then
        div [ class "Helix" ]
            [ div [ style "display" "table-row" ] [ h4 [ style "display" "table-cell", style "width" "50%", style "vertical-align" "top" ] [ text "Operational Category:  " ], textarea [ style "display" "table-cell", style "text-align" "left", onInput UpdateOperationalCategory ] [ text model.helixData.operationalCategory ] ]
            , div [ style "display" "table-row" ] [ h4 [ style "display" "table-cell", style "width" "50%", style "vertical-align" "top" ] [ text "Level:                 " ], textarea [ style "display" "table-cell", style "text-align" "left", onInput UpdateLevel ] [ text model.helixData.level ] ]
            , div [ style "display" "table-row" ] [ h4 [ style "display" "table-cell", style "width" "50%", style "vertical-align" "top" ] [ text "Cognizant Engineer:    " ], textarea [ style "display" "table-cell", style "text-align" "left", onInput UpdateCognizantEngineer ] [ text model.helixData.cognizantEngineer ] ]
            , div [ style "display" "table-row" ] [ h4 [ style "display" "table-cell", style "width" "50%", style "vertical-align" "top" ] [ text "Flight Rule ID:        " ], textarea [ style "display" "table-cell", style "text-align" "left", onInput UpdateFlightRuleID ] [ text model.helixData.flightRuleID ] ]
            , div [ style "display" "table-row" ] [ h4 [ style "display" "table-cell", style "width" "50%", style "vertical-align" "top" ] [ text "Flight Rule Title:     " ], textarea [ style "display" "table-cell", style "text-align" "left", onInput UpdateFlightRuleTitle ] [ text model.helixData.flightRuleTitle ] ]
            , div [ style "display" "table-row" ] [ h4 [ style "display" "table-cell", style "width" "50%", style "vertical-align" "top" ] [ text "Description:           " ], textarea [ style "display" "table-cell", style "text-align" "left", onInput UpdateDescription ] [ text model.helixData.description ] ]
            , div [ style "display" "table-row" ] [ h4 [ style "display" "table-cell", style "width" "50%", style "vertical-align" "top" ] [ text "Rationale:             " ], textarea [ style "display" "table-cell", style "text-align" "left", onInput UpdateRationale ] [ text model.helixData.rationale ] ]
            , div [ style "display" "table-row" ] [ h4 [ style "display" "table-cell", style "width" "50%", style "vertical-align" "top" ] [ text "Violation Impact:      " ], textarea [ style "display" "table-cell", style "text-align" "left", onInput UpdateViolationImpact ] [ text model.helixData.violationImpact ] ]
            ]

    else
        div [ class "Helix" ]
            [ div [ style "display" "table-row" ] [ h4 [ style "display" "table-cell", style "width" "50%", style "vertical-align" "top" ] [ text "Operational Category:  " ], p [ style "display" "table-cell", style "text-align" "left" ] [ text model.helixData.operationalCategory ] ]
            , div [ style "display" "table-row" ] [ h4 [ style "display" "table-cell", style "width" "50%", style "vertical-align" "top" ] [ text "Level:                 " ], p [ style "display" "table-cell", style "text-align" "left" ] [ text model.helixData.level ] ]
            , div [ style "display" "table-row" ] [ h4 [ style "display" "table-cell", style "width" "50%", style "vertical-align" "top" ] [ text "Cognizant Engineer:    " ], p [ style "display" "table-cell", style "text-align" "left" ] [ text model.helixData.cognizantEngineer ] ]
            , div [ style "display" "table-row" ] [ h4 [ style "display" "table-cell", style "width" "50%", style "vertical-align" "top" ] [ text "Flight Rule ID:        " ], p [ style "display" "table-cell", style "text-align" "left" ] [ text model.helixData.flightRuleID ] ]
            , div [ style "display" "table-row" ] [ h4 [ style "display" "table-cell", style "width" "50%", style "vertical-align" "top" ] [ text "Flight Rule Title:     " ], p [ style "display" "table-cell", style "text-align" "left" ] [ text model.helixData.flightRuleTitle ] ]
            , div [ style "display" "table-row" ] [ h4 [ style "display" "table-cell", style "width" "50%", style "vertical-align" "top" ] [ text "Description:           " ], p [ style "display" "table-cell", style "text-align" "left" ] [ text model.helixData.description ] ]
            , div [ style "display" "table-row" ] [ h4 [ style "display" "table-cell", style "width" "50%", style "vertical-align" "top" ] [ text "Rationale:             " ], p [ style "display" "table-cell", style "text-align" "left" ] [ text model.helixData.rationale ] ]
            , div [ style "display" "table-row" ] [ h4 [ style "display" "table-cell", style "width" "50%", style "vertical-align" "top" ] [ text "Violation Impact:      " ], p [ style "display" "table-cell", style "text-align" "left" ] [ text model.helixData.violationImpact ] ]
            ]


viewEditor : Model -> Html Msg
viewEditor model =
    Html.node "code-editor"
        [ HA.property "editorValue" <|
            Json.Encode.string model.input
        , Html.Events.on "editorChanged" <|
            Json.Decode.map CodeChange <|
                Json.Decode.at [ "target", "editorValue" ] <|
                    Json.Decode.string
        , Html.Events.on "parse" (Json.Decode.succeed Parse)
        ]
        []
