module Areas exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Json.Decode as Decode exposing (..)
import Json.Decode.Pipeline  as Pipeline exposing (decode, required)
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

baseUrl = 
    { name = "http://api.dollartranscript.xyz" }

type alias Area = 
    {
       location : String
    ,  average : Int
    }

    

type alias Major =
    {
        major : String
    ,   average : Int
    } 
    
type alias College =
    {
        college : String 
    ,    average : Int 
    }
    

type alias Model = 
    {
        areas : List Area 
    ,   majors : List Major
    ,   colleges : List College
    }


initModel : Model 
initModel = 
    {
        areas = []
    ,   majors = []
    ,   colleges = []
    }

init : ( Model, Cmd Msg )
init = 
    ( initModel, Cmd.batch [getAreas , getMajors, getColleges ] )

-- UPDATE

type Msg = 
    Areas ( Result Http.Error (List Area))
    | Majors ( Result Http.Error (List Major))
    | Colleges ( Result Http.Error (List College))

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

        Colleges (Ok new_colleges) ->
            ( { model | colleges = new_colleges }, Cmd.none )

        Colleges (Err error) ->
            let 
                _ = Debug.log "Error is:" error
            in 
                ( model, Cmd.none )




-- VIEW

view : Model -> Html Msg 
view model = 

    let 
        articleStyle : Attribute msg 
        articleStyle = 
                style 
                    [
                        ( "width", "auto" )
                    ,   ( "margin", "2%" )
                    ,   ( "display", "flex" )
                    ,   ( "flexFlow", "row wrap")
                    ,   ( "justifyContent", "space-between")
                    ]
    in 

        article [ articleStyle ]
        [  areas model
        ,  majors model
        ,  colleges model 
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
    ,   td [] [ text ( toString model.average ) ]
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
    ,   td [] [ text ( toString model.average ) ]
    ]


colleges : Model -> Html Msg 
colleges model  =
    article []
    [ h1 [] [text "College Rankings"]
    ,   table[]
        [   
            thead []
            [
                tr[]
                [ th [] [ text "College" ]
                , th [] [ text "Average" ]
                ]
            ]
        ,   tbody[] ( List.map collegeRow model.colleges ) 
        ]
    ]

collegeRow : College -> Html Msg  
collegeRow model  = 

    tr [] 
    [
        td [] [ text model.college ]
    ,   td [] [ text (toString model.average )  ]
    ]


      
      
-- JSON 

decodeAreas : Decode.Decoder ( List Area )
decodeAreas = 

    Decode.list decodeArea

decodeArea : Decode.Decoder Area
decodeArea = 

    decode Area 
        |> Pipeline.required "location" Decode.string
        |> Pipeline.required "average"  Decode.int 


decodeMajors : Decode.Decoder ( List Major )
decodeMajors = 

    Decode.list decodeMajor

decodeMajor : Decode.Decoder Major
decodeMajor = 

    decode Major 
        |> Pipeline.required "major" Decode.string
        |> Pipeline.required "average"  Decode.int 


decodeColleges : Decode.Decoder ( List College )
decodeColleges = 

    Decode.list decodeCollege

decodeCollege : Decode.Decoder College 
decodeCollege = 

    decode College 
        |> Pipeline.required "college" Decode.string
        |> Pipeline.required "average" Decode.int


-- HTTP 



getAreas =

    let 
        url = baseUrl.name ++ "areas" -- replace localhost with api.dollarTranscript.xyz/salaries

        request = 
        
           Http.get url decodeAreas 

    in 

       Http.send Areas request 


getMajors =

    let 
        url = baseUrl.name ++ "majors" -- replace localhost with api.dollarTranscript.xyz/salaries

        request = 
        
           Http.get url decodeMajors 

    in 

       Http.send Majors request 


getColleges = 

    let 
        url = baseUrl.name ++ "colleges"

        request = 

            Http.get url decodeColleges

    in 

        Http.send Colleges request

    

-- SUBSCRIPTIONS 

subscriptions : Model -> Sub Msg 
subscriptions model = 
    Sub.none 