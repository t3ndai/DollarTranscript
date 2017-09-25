module Areas exposing (..)

import Html exposing (..)
import Json.Decode as Decode exposing (..)
import Json.Decode.Pipeline exposing (decode, required)
import Http 

-- RUN 

main = 
    Html.program 
    {
       init = init 
    ,  update = update 
    ,  view = view 
    ,  subscriptions = subscriptions
    }


-- MODEL 

type alias Area = 
    {
       location : String
    ,  average : String
    }


type alias Major =
    {
        major : String
    ,   average : String
    } 
    

type alias Model = 
    {
        areas : List Area 
    ,   majors : List Major
    }

initModel : Model 
initModel = 
    {
        areas = []
    ,   majors = []
    }

init : ( Model, Cmd Msg )
init = 
    ( initModel, Cmd.batch [getAreas , getMajors ] )

-- UPDATE

type Msg = 
    Areas ( Result Http.Error (List Area))
    | Majors ( Result Http.Error (List Major))

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model = 

    case msg of 

        Areas (Ok new_areas) ->
            ( { model | areas = new_areas }, Cmd.none )

        Areas (Err error) ->
            let 
                _ = Debug.log "Error is:" error 
            in 
                ( model, Cmd.none )

        Majors (Ok new_majors) ->
            ( { model | majors = new_majors }, Cmd.none )

        Majors (Err error) ->
            let 
                _ = Debug.log "Error is:" error 
            in 
                ( model, Cmd.none )




-- VIEW

view : Model -> Html Msg 
view model = 
    article []
    [  areas model
    ,  majors model
    ]


areas : Model -> Html Msg 
areas model  =
    article []
    [ 
        h1 [] [ text "Area Rankings" ]
    ,   table[]
        [   
            thead []
            [
                tr[]
                [ th [] [ text "Location" ]
                , th [] [ text "Average" ]
                ]
            ]
        ,   tbody[] ( List.map areaRow model.areas ) 
        ]
    ]

areaRow : Area -> Html Msg  
areaRow model  = 

    tr [] 
    [
        td [] [ text model.location ]
    ,   td [] [ text model.average  ]
    ]


majors : Model -> Html Msg 
majors model  =
    article []
    [ h1 [] [text "Major Rankings"]
    ,   table[]
        [   
            thead []
            [
                tr[]
                [ th [] [ text "Major" ]
                , th [] [ text "Average" ]
                ]
            ]
        ,   tbody[] ( List.map majorRow model.majors ) 
        ]
    ]

majorRow : Major -> Html Msg  
majorRow model  = 

    tr [] 
    [
        td [] [ text model.major ]
    ,   td [] [ text model.average  ]
    ]

      
      
-- JSON 

decodeAreas : Decode.Decoder ( List Area )
decodeAreas = 

    Decode.list decodeArea

decodeArea : Decode.Decoder Area
decodeArea = 

    decode Area 
        |> required "location" Decode.string
        |> required "average"  Decode.string 


decodeMajors : Decode.Decoder ( List Major )
decodeMajors = 

    Decode.list decodeMajor

decodeMajor : Decode.Decoder Major
decodeMajor = 

    decode Major 
        |> required "major" Decode.string
        |> required "average"  Decode.string 




-- HTTP 

getAreas =

    let 
        url = "http://127.0.0.1:8080/areas" -- replace localhost with api.dollarTranscript.xyz/salaries

        request = 
        
           Http.get url decodeAreas 

    in 

       Http.send Areas request 


getMajors =

    let 
        url = "http://127.0.0.1:8080/majors" -- replace localhost with api.dollarTranscript.xyz/salaries

        request = 
        
           Http.get url decodeMajors 

    in 

       Http.send Majors request 



-- SUBSCRIPTIONS 

subscriptions : Model -> Sub Msg 
subscriptions model = 
    Sub.none 