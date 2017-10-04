module SalariesChart exposing (..)

import Html exposing (..)
import Http 
import Json.Decode as Decode exposing (..)
import Json.Decode.Pipeline exposing (decode, required)
import Svg exposing ( svg, circle, line, text_, text , g )
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
    article []
    [
      chart model 
    ]




chart : Model -> Svg.Svg msg  
chart model = 
  svg 
     [ width "auto", height "auto", viewBox " 0 0 100 10 " ]
     [  g[] (List.map salaryDot model.salaries )
     ,  line [ x1 "10", x2 "90", y1 "8", y2 "8", stroke "#0074d9", strokeWidth "0.1" ][] 
     ,  text_ [ x "6", y "9", fontSize "2" ][ Svg.text "$30k"  ] 
     ,  text_ [ x "90", y "9" , fontSize "2" ][ Svg.text "$150k" ] 
     ]


viewSalary : Salary -> Html Msg 
viewSalary salary = 
    div [] 
    [ Html.text (toString salary.pay) ]


salaryDot : Salary -> Svg.Svg msg 
salaryDot salary = 
        circle [ cx (toString ( salary.pay * 80 // 150000 ) ), cy "5", r "0.5" ] []
        
      


-- SUBSCRIPTIONS 

subscriptions : Model -> Sub Msg 

subscriptions model = 
    Sub.none 


-- HTTP 
 
getSalaries = 
    let 
        url = "http://api.dollartranscript.xyz/salaries" -- replace localhost with api.dollarTranscript.xyz/salaries

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


