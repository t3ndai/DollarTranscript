module Job exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events  exposing (..)
import Json.Encode as Encode exposing (..)
import Json.Decode as Decode exposing ( list, string )
import Http 

-- RUN 

main = 
    Html.program 
    {
      init = init
    , view = view 
    , update = update 
    , subscriptions = subscriptions
    }


-- MODEL 

type alias Model = 

    {
       title : String 
    ,  location : String 
    ,  pay : Int 
    ,  company : String 
    ,  sponsorship : Bool 
    ,  latitude : Float 
    ,  longitude : Float
    ,  major : String 
    ,  college : String
    }

emptyJob : Model 
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
    ,  college = ""
    }

{-
type alias Model = 
    Job

-}

initModel : Model 
initModel = 
    emptyJob 

init : ( Model, Cmd Msg )
init = 
    ( initModel, Cmd.none )


-- UPDATE 

type Msg =
    InitialScreen
    | Title String
    | Location String
    | Company String
    | Pay String
    | College String
    | Major String
    | Sponsorship Bool 
    | Add 
    | Job ( Result Http.Error () )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =

    case msg of 

        InitialScreen ->
            (  model, Cmd.none )

        Title title ->
            ( { model | title = title  }, Cmd.none )

        Location location ->
            ( { model | location = location  }, Cmd.none )

        Company company -> 
            ( { model | company = company  }, Cmd.none )

        Pay pay ->
            ( { model | pay = Result.withDefault 0 (String.toInt pay)  }, Cmd.none )

        College college ->
            ( { model | college = college }, Cmd.none )

        Major major ->
            ( { model | major = major  }, Cmd.none )

        Sponsorship sponsorship ->
            ( { model | sponsorship = sponsorship  }, Cmd.none )

        Add ->
            ( model, postJob model )

        Job (Err error ) ->
            let 
                _ = Debug.log "Error is:" error 
            in 
                 ( model, Cmd.none )

        Job _ ->

            ( model, Cmd.none )




-- VIEW 

view : Model -> Html Msg 
view model = 

    let 
        articleStyle : Attribute msg 
        articleStyle =
                style
                    [
                        ( "display", "flex" )
                    ,   ( "flexFlow", "column wrap" )
                    ,   ( "margin", "2%" )
                    ,   ( "alignContent", "space-between" )
                    ]

    in 

        article [ articleStyle ]
        [
            div[][ text "Add Your Job 😉"]
        ,   job model 
        ]



job : Model -> Html Msg 
job model = 

    let 

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
                    ,   ( "fontWeight", "bolder")
                    ]

        articleStyle : Attribute msg 
        articleStyle = 
                style 
                    [
                        ( "display", "flex")
                    ,   ( "flexFlow", "row wrap")
                    ,   ( "width", "auto" )
                    ,   ( "margin", "2%" )
                    ,   ( "padding", "2px" )
                    ,   ( "alignContent", "space-between" )
                    ]

        divStyle : Attribute msg 
        divStyle = 
                style 
                    [
                        ( "width", "auto" )
                    ,   ( "padding", "1px" )
                    ]

    in 

        article [ articleStyle ]
        [   
            div [ divStyle ]
            [
                input [ type_ "text", placeholder "Title", onInput Title ] []
            ]
        ,   div [ divStyle ]
            [
                input [ type_ "text", placeholder "Location", onInput Location ] []
            ]
        
        ,   div [ divStyle ]
            [
                input [ type_ "text", placeholder "Company", onInput Company ] []
            ]
        ,   div [ divStyle ]
            [
                input [ type_ "number", placeholder "Pay", onInput Pay ] []
            ]
        ,   div [ divStyle ]
            [
                input [ type_ "text", placeholder "College", onInput College ] []
            ]
        ,   div [ divStyle ]
            [
                input [ type_ "text", placeholder "Major", onInput Major] []
            ]
        ,   div [ divStyle ]
            [    
                label []
                [
                    text "Sponsorship"
                ,   input [ type_ "checkbox", onCheck Sponsorship ] []
                   
                ]
            ]
        ,   div []
                [ button [ buttonStyle, onClick Add  ] [ text "+"] ] 
            
        ]


-- JSON 

encodeJob : Model -> Encode.Value
encodeJob model =

        Encode.object 
            [ ( "title", Encode.string model.title )
            , ( "location", Encode.string model.location )
            , ( "company", Encode.string model.company )
            , ( "pay", Encode.int model.pay )
            , ( "college", Encode.string model.college )
            , ( "major", Encode.string model.major ) 
            , ( "sponsorship", Encode.bool model.sponsorship )
            , ( "latitude", Encode.float model.latitude )
            , ( "longitude", Encode.float model.longitude )
            ]


{-

 [major: Computer Science, location: New York, pay: 120000.0, sponsorship: false, title: iOS Developer , company: Dow Jones Inc., college: University of South Florida]
-}

voidDecoder : Decode.Decoder ()
voidDecoder = 
    Decode.succeed ()

-- HTTP 


postJob model = 
    let 
        url = "http://127.0.0.1:8080/jobs"

        body = 
            Http.jsonBody <| encodeJob model

        request =

            Http.post url body voidDecoder

    in 

        Http.send Job request

    

-- SUBSCRIPTIONS 

subscriptions : Model -> Sub Msg 
subscriptions model = 
    Sub.none 