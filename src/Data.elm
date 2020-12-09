module Data exposing (HelixData, Simulation, Timeline, data, decodePosix, decodeRule, decodeSimulation, encodeRule, encodeSimulation, estimate, getAllTimes, getMaxTime, getMinTime, getTimeline)

import Dict exposing (Dict, get)
import Json.Decode
import Json.Encode
import List exposing (foldl, map)
import Maybe
import Time exposing (Posix, millisToPosix, posixToMillis)



-- HELIX DATA


type alias HelixData =
    { operationalCategory : String
    , level : String
    , cognizantEngineer : String
    , flightRuleID : String
    , flightRuleTitle : String
    , description : String
    , rationale : String
    , violationImpact : String
    }


decodeRule : String -> Result Json.Decode.Error { ruleText : String, helixData : HelixData }
decodeRule json =
    json
        |> Json.Decode.decodeString
            (Json.Decode.map2
                (\s h -> { ruleText = s, helixData = h })
                (Json.Decode.field "LogicSpec" Json.Decode.string)
                (Json.Decode.map8
                    (\s1 s2 s3 s4 s5 s6 s7 s8 ->
                        { operationalCategory = s1
                        , level = s2
                        , cognizantEngineer = s3
                        , flightRuleID = s4
                        , flightRuleTitle = s5
                        , description = s6
                        , rationale = s7
                        , violationImpact = s8
                        }
                    )
                    (Json.Decode.field "Operational category" Json.Decode.string)
                    (Json.Decode.field "Level" Json.Decode.string)
                    (Json.Decode.field "Cognizant engineer" Json.Decode.string)
                    (Json.Decode.field "Flight Rule ID" Json.Decode.string)
                    (Json.Decode.field "Flight rule title" Json.Decode.string)
                    (Json.Decode.field "Description" Json.Decode.string)
                    (Json.Decode.field "Rationale" Json.Decode.string)
                    (Json.Decode.field "Violation impact" Json.Decode.string)
                )
            )


encodeRule : String -> HelixData -> Json.Encode.Value
encodeRule ruleText helixData =
    Json.Encode.object
        [ ( "Operational category", Json.Encode.string helixData.operationalCategory )
        , ( "Level", Json.Encode.string helixData.level )
        , ( "Cognizant engineer", Json.Encode.string helixData.cognizantEngineer )
        , ( "Flight Rule ID", Json.Encode.string helixData.flightRuleID )
        , ( "Flight rule title", Json.Encode.string helixData.flightRuleTitle )
        , ( "Description", Json.Encode.string helixData.description )
        , ( "Rationale", Json.Encode.string helixData.rationale )
        , ( "Violation impact", Json.Encode.string helixData.violationImpact )
        , ( "LogicSpec", Json.Encode.string ruleText )
        ]



-- TIMELINE DATA


type alias Timeline =
    List
        { time : Posix
        , value : Bool
        }


type alias Simulation =
    Dict String Timeline


getMinTime : Simulation -> Posix
getMinTime sim =
    sim
        |> Dict.values
        |> map
            (\l ->
                l
                    |> map .time
                    |> map posixToMillis
                    |> List.minimum
                    |> Maybe.withDefault 0
            )
        |> List.minimum
        |> Maybe.withDefault 0
        |> millisToPosix


getMaxTime : Simulation -> Posix
getMaxTime sim =
    sim
        |> Dict.values
        |> map
            (\l ->
                l
                    |> map .time
                    |> map posixToMillis
                    |> List.maximum
                    |> Maybe.withDefault 0
            )
        |> List.maximum
        |> Maybe.withDefault 0
        |> millisToPosix



--empty default data


data : Simulation
data =
    Dict.empty


getTimeline : Simulation -> String -> Timeline
getTimeline sim name =
    Dict.get name sim |> Maybe.withDefault []


getAllTimes : Simulation -> List Posix
getAllTimes sim =
    Dict.values sim
        |> map (map .time)
        |> foldl List.append []
        |> map posixToMillis
        |> List.sort
        |> map millisToPosix


estimate : Timeline -> Posix -> { value : Bool, residualMilliSec : Int }
estimate timeline query_time =
    --find esitmate using most recent sample, otherwise assume false
    case timeline of
        [] ->
            { value = False, residualMilliSec = 0 }

        [ sample ] ->
            let
                residual =
                    posixToMillis query_time - posixToMillis sample.time
            in
            if residual >= 0 then
                { value = sample.value, residualMilliSec = residual }

            else
                { value = False, residualMilliSec = 0 }

        [ sample1, sample2 ] ->
            let
                residual1 =
                    posixToMillis query_time - posixToMillis sample1.time

                residual2 =
                    posixToMillis query_time - posixToMillis sample2.time
            in
            if residual1 < 0 then
                { value = False, residualMilliSec = 0 }

            else if residual2 < 0 then
                { value = sample1.value, residualMilliSec = residual1 }

            else
                { value = sample2.value, residualMilliSec = residual2 }

        sample1 :: sample2 :: rest ->
            let
                residual1 =
                    posixToMillis query_time - posixToMillis sample1.time

                residual2 =
                    posixToMillis query_time - posixToMillis sample2.time
            in
            if residual1 < 0 then
                { value = False, residualMilliSec = 0 }

            else if residual2 < 0 then
                { value = sample1.value, residualMilliSec = residual1 }

            else
                estimate (sample2 :: rest) query_time


decodePosix : Result (List a) Posix -> Posix
decodePosix result =
    case result of
        Ok x ->
            x

        Err _ ->
            millisToPosix 0


decodeSimulation : String -> Result Json.Decode.Error Simulation
decodeSimulation json =
    json
        |> Json.Decode.decodeString
            (Json.Decode.dict
                (Json.Decode.list
                    (Json.Decode.map2 (\t v -> { time = millisToPosix t, value = v })
                        (Json.Decode.field "Time" Json.Decode.int)
                        (Json.Decode.field "Value" Json.Decode.bool)
                    )
                )
            )


encodeSimulation : Simulation -> Json.Encode.Value
encodeSimulation simulation =
    simulation
        |> Json.Encode.dict Basics.identity
            (\timeline ->
                timeline
                    |> Json.Encode.list
                        (\p ->
                            Json.Encode.object
                                [ ( "Time", Json.Encode.int (posixToMillis p.time) )
                                , ( "Value", Json.Encode.bool p.value )
                                ]
                        )
            )
