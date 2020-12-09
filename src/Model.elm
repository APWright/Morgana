module Model exposing (Model, Msg(..), ViewMode(..))

import Data exposing (HelixData, Simulation)
import File exposing (File)
import Formula exposing (EvaluatedFormula, Formula)
import Parser exposing (DeadEnd)
import Time exposing (Posix)
import Zoom exposing (OnZoom, Zoom)



-- MODEL


type alias Model =
    { input : String
    , output : Formula
    , parseError : Maybe (List DeadEnd)
    , simulation : Simulation
    , minTime : Posix
    , maxTime : Posix
    , currentTime : Posix
    , evalRule : EvaluatedFormula
    , showLegend : Bool
    , viewMode : ViewMode
    , zoom : Zoom
    , helixData : HelixData
    }


type Msg
    = Parse
    | CodeChange String
    | ZoomMsg OnZoom
    | ResetZoom OnZoom
    | MoveScrubber Float
    | ShowLegend Bool
    | ChangeMode ViewMode
    | DownloadSim
    | SimFileRequested
    | SimFileSelected File
    | SimFileLoaded String
    | DownloadRule
    | RuleFileRequested
    | RuleFileSelected File
    | RuleFileLoaded String
    | UpdateOperationalCategory String
    | UpdateLevel String
    | UpdateCognizantEngineer String
    | UpdateFlightRuleID String
    | UpdateFlightRuleTitle String
    | UpdateDescription String
    | UpdateRationale String
    | UpdateViolationImpact String
    | NewRule
    | NoMsg


type ViewMode
    = Authoring
    | Validation
    | Evaluation
