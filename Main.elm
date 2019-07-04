module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http 
import SalariesChart 
import Job 
import Json.Decode as Decode exposing (..)
import Json.Encode as Encode exposing (..) 
import Json.Decode.Pipeline as Pipeline exposing (decode, required)

main = 
    Html.program
    { init = init
    , view = view 
    , update = update 
    , subscriptions = subscriptions 
    }

type PageModel = SalChart SalariesChart.Model 
               | Job Job.Model  

type alias Model = 
    {

        pageModel : PageModel  
    }

initModel: Model 
initModel = 
    {
        pageModel =  SalChart SalariesChart.initModel

    }

init : ( Model, Cmd Msg )
init = 
    ( initModel, Cmd.none )


-- UPDATE 

type Msg = JobMsg Job.Msg  
        | SalChartMsg SalariesChart.Msg 

update : Msg -> Model -> (Model, Cmd Msg) 
update msg model = 
   case (msg,model.pageModel) of 

        (SalChartMsg submsg, SalChart submodel) ->
            let 
                (newModel, newCmd) = 
                    SalariesChart.update submsg submodel
            in 
                
                ({ model | pageModel = SalChart newModel}, Cmd.map SalChartMsg newCmd )


        (JobMsg submsg, Job submodel) ->
            let 
                (newModel, newCmd) = 
                    Job.update submsg submodel
            in 
                
                ({ model | pageModel = Job newModel}, Cmd.map JobMsg newCmd )


        _ -> 

            (model, Cmd.none)


-- VIEW 

view : Model -> Html Msg 
view model = 
    case model.pageModel of 

        SalChart submodel ->
            SalariesChart.view submodel
            |> Html.map SalChartMsg 

        Job submodel ->
            Job.view submodel
            |> Html.map JobMsg


-- SUBSCRIPTIONS 

subscriptions : Model -> Sub Msg 
subscriptions model = 
    Sub.none 


