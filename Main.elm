module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http 
import Json.Decode as Decode exposing (..)
import Json.Decode.Pipeline as Pipeline exposing (decode, required)

main = 
    Html.program
    { init = init
    , view = view 
    , update = update 
    , subscriptions = subscriptions 
    }


{-
    first things, first to gt this shit working, I need to display salaries just a list 
    so that means, setting my model then doing an http call then displaying shit I have 
    salaries come as a Json Array of Salary models e.g [{"pay": 70000},]

    second stage, is composing a form to input salary offer data then send it via http 

    //Job 

    var pay: Int
    var company: String
    var college: String
    var sponsorship: Bool?
    var latitude: Double
    var longitude: Double
    var major: String

    So basically,  I have six Html.Article items spread across the page since the items can survive independently 
-}

-- MODEL 

type alias Salary = 

    { pay : Int }

        
type alias Job = 

    {
       title : String
    ,  location : String 
    ,  pay : Int 
    ,  company : String 
    ,  sponsorship : Bool 
    ,  latitude : Float 
    ,  longitude : Float
    ,  major : String 

    }

emptyJob : Job 
emptyJob = 
    {
       title = ""
    ,  location = ""
    ,  pay = 0
    ,  company = ""
    ,  major = ""
    ,  sponsorship = False 
    ,  latitude = 0.0
    ,  longitude = 0.0 

    }



type alias Model = 
    {
        salaries : List Salary
    ,   job : Maybe Job 
     
    }

initModel : Model 
initModel = 
    {
        salaries = []
    ,   job = Just emptyJob
    }

init : ( Model, Cmd Msg)
init = 
    ( initModel ,  getSalaries )


-- UPDATE 

type Msg =
     NewSalaries  ( Result Http.Error (List Salary))
    | Title  String
    | Location String 
    | Company String
    | Pay String  
    | Major String
    | Sponsorship Bool 

    

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model = 

    case msg of 

        NewSalaries ( Ok newSalaries ) -> 
            ( { model | salaries = newSalaries }, Cmd.none )

        NewSalaries  ( Err error) ->
            let 
                _ = Debug.log "Error is:" error 
            in 
                 ( model, Cmd.none )

        Title title ->
            ( { model | job = Maybe.map (setJobTitle title) model.job }, Cmd.none )

        Location location ->
            ( { model | job = Maybe.map (setJobLocation location) model.job }, Cmd.none )

        Company company ->
            ( { model | job = Maybe.map (\job -> { job | company = company }) model.job }, Cmd.none )

        Pay pay ->
            ( { model | job = Maybe.map (\job -> { job | pay = Result.withDefault 0 (String.toInt pay) }) model.job}, Cmd.none )

        Major major ->
            ( { model | job = Maybe.map (\job -> { job | major = major }) model.job }, Cmd.none )

        Sponsorship sponsorship ->
            ( { model | job = Maybe.map (\job -> { job | sponsorship = sponsorship }) model.job }, Cmd.none )



        {-
        SetJob value field ->
            ( { model | job = Maybe.mpa (setJob value ) model.job  })
        -}

        -- Result.withDefault 0 (String.toInt "42") == 42

 -- { model | job = Maybe.map (setJobTitle newTitle) model.job }

 -- { model | job = Maybe.map (\job -> { job | title = newTitle } ) model.job }





-- VIEW 

view : Model -> Html Msg 
view model = 
        article []
        [ 
            salaries model
        ,   job model 
     
        ]

salaries : Model -> Html Msg
salaries model = 
    div []
    [ div[] (List.map viewSalary model.salaries) ]

viewSalary : Salary -> Html Msg 
viewSalary salary = 
    div [] 
    [ text (toString salary.pay) ]

job : Model -> Html Msg 
job model = 

    article []
    [   
        div []
        [
            input [ type_ "text", placeholder "Title", onInput Title ] []
        ]
    ,   div []
        [
            input [ type_ "text", placeholder "Location", onInput Location ] []
        ]
    ,   div []
        [
            input [ type_ "text", placeholder "Company", onInput Company ] []
        ]
    ,   div []
        [
            input [ type_ "number", placeholder "Pay", onInput Pay ] []
        ]
    ,   div []
        [
            input [ type_ "text", placeholder "Major", onInput Major] []
        ]
    ,   div []
        [    
            label []
            [
                text "Sponsorship"
            ,   input [ type_ "radio", onClick ( Sponsorship True ) ] []
               
            ]
        ]

    ]


-- SUBSCRIPTIONS 

subscriptions : Model -> Sub Msg 

subscriptions model = 
    Sub.none 


-- HTTP 
 
getSalaries = 
    let 
        url = "http://localhost:8080/salaries" -- replace localhost with api.dollarTranscript.xyz/salaries

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
             |> Pipeline.required "pay" Decode.int 


-- HELPERS 

setJobTitle value job  = 
    { job | title = value }

setJobLocation value job = 
    { job | location = value }

{-
setJob value job msg = 
    case msg of 
        title ->
            { job | title = value }

        location ->
            { job | location = value }

-}