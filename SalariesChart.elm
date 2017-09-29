module SalariesChart exposing (..)

import Html exposing (..)
import Http 
import Json.Decode as Decode exposing (..)
import Json.Decode.Pipeline exposing (decode, required)
import Svg 
import Svg.Attributes exposing (..)


-- RUN

main = 
    Html.program
    { init = init
    , view = view 
    , update = update 
    , subscriptions = subscriptions 
    }



-- MODEL 

type alias Salary = 

    { pay : Int }

type alias Model =
    {
      salaries : List Salary   
    }

initModel : Model 
initModel  = 
  { salaries = 
    [ { pay = 70000}
    , { pay = 90000 }
    , { pay = 55000 } 
    , { pay =86000 }
    ] 
  }


init : ( Model, Cmd Msg)
init = 
    ( initModel,  getSalaries )





-- UPDATE 

type Msg 
    = Salaries 
    | NewSalaries  ( Result Http.Error (List Salary))
    

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model = 

    case msg of 
        Salaries -> 
            ( model, Cmd.none )

        NewSalaries ( Ok newSalaries ) -> 
            ( { model | salaries = newSalaries }, Cmd.none )

        NewSalaries  ( Err error) ->
            let 
                _ = Debug.log "Error is:" error 
            in 
                 ( model, Cmd.none )



-- VIEW 

view : Model -> Html Msg 

view model = 
    div []
    [ div[] (List.map viewSalary model.salaries) ]





viewSalary : Salary -> Html Msg 

viewSalary salary = 
    div [] 
    [ text (toString salary.pay) ]



-- SUBSCRIPTIONS 

subscriptions : Model -> Sub Msg 

subscriptions model = 
    Sub.none 


-- HTTP 
 
getSalaries = 
    let 
        url = "http://127.0.0.1:8080/salaries" -- replace localhost with api.dollarTranscript.xyz/salaries

        request = 
        
           Http.get url decodeSalaries 

    in 

       Http.send NewSalaries request 


-- JSON

decodeSalaries : Decode.Decoder ( List Salary )
decodeSalaries = 

       Decode.list decodeSalary
       -- Decode.list <| Decode.map (\x -> { pay = x } ) ( Decode.field "pay" Decode.int )

decodeSalary : Decode.Decoder Salary
decodeSalary = 

      decode Salary 
             |> required "pay" Decode.int 


