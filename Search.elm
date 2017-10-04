module Search exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events  exposing (..)
import Json.Decode as Decode exposing (..)
import Json.Encode as Encode exposing (..)
import Json.Decode.Pipeline as Pipeline exposing (decode, required)
import Http 


-- RUN 

main = 
    Html.program 
    { 
      init = init
    , update = update 
    , view = view
    , subscriptions = subscriptions

    }

-- MODEL 

type alias Model = 
    {
        location : String
    ,   college : String
    ,   pay  : Int 
    ,   searchType : String
    ,   jobs : List Job
    }

type alias Job = 
    {
        title : String
    ,   location : String
    ,   pay : Int 
    ,   college : String       
    }

type Match =
    Any
    | Exact


initModel: Model 
initModel = 
    {
        location = ""
    ,   college  = ""
    ,   pay   = 0
    ,   searchType = ""
    ,   jobs = []
    }

init : ( Model, Cmd Msg)
init = 
    ( initModel, Cmd.none )

-- UPDATE 


type Msg = 
    Location String
    | College String
    | Pay String
    | SwitchTo Match 
    | PerformSearch 
    | SearchResults ( Result Http.Error (List Job))


update : Msg -> Model -> ( Model, Cmd Msg)
update msg model =
    
    case msg of 

        Location location ->
            ( { model | location = location },  Cmd.none )

        College college  ->
            ( { model | college = college }, Cmd.none )

        Pay pay ->
            ( { model | pay = Result.withDefault 0 ( String.toInt pay ) }, Cmd.none )

        SwitchTo match ->
            case match of 
                Any ->
                    ( { model | searchType = "or" }, Cmd.none )
                Exact ->
                    ( { model | searchType = "and" }, Cmd.none )

        PerformSearch ->
            ( model, performSearch model)

        SearchResults (Ok jobs) -> 
            ( { model | jobs = jobs }, Cmd.none )

        SearchResults (Err error) ->
            let 
                _ = Debug.log "Error is:" error 

            in 
                ( model, Cmd.none )

-- VIEW

view: Model -> Html Msg 
view model =

    let  
        articleStyle : Attribute msg 
        articleStyle = 
                style 
                    [
                        ( "display", "flex")
                    ,   ( "width", "auto" )    
                    ,   ( "flexFlow", "row wrap")
                    ,   ( "margin", "2%")
                    ]

    in 
        article [ articleStyle ]
        [
            searchView model  ,viewSearchResults model
        ]

searchView : Model -> Html Msg
searchView model = 

    let 

        sliderStyle : Attribute msg
        sliderStyle = 
                style 
                    [
                        
                    ]

        buttonStyle : Attribute msg 
        buttonStyle =
                style
                    [
                        ( "backgroundColor", "#F0F8FF" )
                    ,   ( "borderRadius", "3px" )
                    ,   ( "padding", "5px")
                    ,   ( "margin",  "20px")
                    ,   ( "border", "none")
                    ,   ( "fontSize", "20px")
                    ,   ( "fontWeight", "bold")
                    ]

        textDivStyle : Attribute msg 
        textDivStyle = 
                style
                    [
                        ( "display", "inline-block" )
                    ,   ( "flexFlow", "row" )
                    ,   ( "justifyContent", "space-between" )   
                    ]

    in 

    article[]
    [
        div[]
        [
            input [ type_ "text", placeholder "Location/City", onInput Location ] []
        ]
    ,   div []
        [
            input [ type_ "text", placeholder "College", onInput College ] []
        ]
    ,   div [ textDivStyle ]
        [
            text "$30k"
        ,   input [ type_ "range", Html.Attributes.min  "30000", Html.Attributes.max  "150000",  onInput Pay ] []
        ,   text "$150k"
        ]
    ,   div []
        [
            label[]
            [
                text "Match"
            ,   label []
                [
                    div [][]
                ,   text "Any" 
                ,   input [ type_ "radio", name  "match", onClick ( SwitchTo Any) ] []
                ,   text "Exact"
                ,   input [ type_ "radio", name  "match", onClick ( SwitchTo Exact) ] []
                ]
 
            ]
            --⌕
        ]
    ,   div []
        [ button [  buttonStyle, onClick PerformSearch ] [ text "SEARCH" ] ]
    ]


viewSearchResults : Model -> Html Msg
viewSearchResults model =
    let 
            resultsStyle : Attribute msg 
            resultsStyle = 
                style 
                    [
                        ( "margin", "2%" )
                    ,    ( "display", "flex")
                    ,   ( "flexFlow", "row wrap" ) 
                    ]

    in 

            article []
            [
                div [ resultsStyle ] ( List.map viewJob model.jobs )
            ] 

viewJob : Job -> Html Msg 
viewJob job =

    let 
            jobStyle : Attribute msg 
            jobStyle = 
                style
                    [
                    
                       ( "padding", "5px" )
                    ]

    in 

    div [ jobStyle ]
    [
        text job.title 
    ,   br[][]
    ,   text job.location 
    ,   br[][]
    ,   text (toString job.pay) 
    ,   br[][]
    ,   text job.college 
    ,   br[][]
    ]

-- JSON 

encodeSearch : Model -> Encode.Value
encodeSearch model = 

    Encode.object 
        [ ( "location", Encode.string model.location )
        , ( "college", Encode.string model.college )
        , ( "pay", Encode.int model.pay )
        , ( "searchType", Encode.string model.searchType )
        ]

decodeSearchResults : Decode.Decoder ( List Job )
decodeSearchResults = 

    Decode.list decodeJob


decodeJob : Decode.Decoder Job 
decodeJob =

    decode Job 
            |> Pipeline.required "title" Decode.string
            |> Pipeline.required "location" Decode.string
            |> Pipeline.required "pay" Decode.int 
            |> Pipeline.required "college" Decode.string



-- HTTP 

performSearch model =

    let 
        url = "http://api.dollartranscript.xyz/search"

        body = 
            Http.jsonBody <| encodeSearch model

        request =
            Http.post url body decodeSearchResults

    in 

        Http.send SearchResults request


-- SUBSCRIPTIONS 
subscriptions: Model -> Sub Msg
subscriptions model = 
    Sub.none 
