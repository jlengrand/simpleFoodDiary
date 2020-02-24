port module Main exposing (..)

import Browser
import Html exposing (Html, button, div, h1, img, text)
import Html.Attributes exposing (src)
import Html.Events exposing (onClick)
import Json.Decode
import Json.Decode.Pipeline
import Json.Encode
import Time



---- MODEL ----


port signIn : () -> Cmd msg


port signOut : () -> Cmd msg


port saveFoodLog : Json.Encode.Value -> Cmd msg


port signInInfo : (Json.Encode.Value -> msg) -> Sub msg


type alias UserData =
    { token : String
    , email : String
    , uid : String
    }


portionToString : Portion -> String
portionToString portion =
    case portion of
        Small ->
            "Small"

        Medium ->
            "Medium"

        Large ->
            "Large"

        Huge ->
            "Huge"


type Portion
    = Small
    | Medium
    | Large
    | Huge


type alias FoodLog =
    { ts : Time.Posix
    , portion : Portion
    , keto : Bool
    , vegan : Bool
    , meat : Bool
    , alcohol : Bool
    , caffeine : Bool
    }


type alias Model =
    { userData : Maybe UserData
    , currentFoodLog : Maybe FoodLog
    }


init : ( Model, Cmd Msg )
init =
    ( { userData = Maybe.Nothing
      , currentFoodLog = Just fakeFoodLog
      }
    , Cmd.none
    )


fakeFoodLog : FoodLog
fakeFoodLog =
    { ts = Time.millisToPosix 0
    , portion = Medium
    , keto = False
    , vegan = False
    , meat = True
    , alcohol = False
    , caffeine = False
    }



---- UPDATE ----


type Msg
    = SignIn
    | LoggedInData (Result Json.Decode.Error UserData)
    | LogOut
    | SendCurrentFoodLog String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SignIn ->
            ( model, signIn () )

        LogOut ->
            ( { model | userData = Maybe.Nothing }, signOut () )

        LoggedInData result ->
            case result of
                Ok value ->
                    ( { model | userData = Just value }, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )

        SendCurrentFoodLog uid ->
            case model.currentFoodLog of
                Maybe.Nothing ->
                    ( model, Cmd.none )

                Just foodLog ->
                    ( { model | currentFoodLog = Maybe.Nothing }, saveFoodLog <| foodLogEncoder foodLog uid )


view : Model -> Html Msg
view model =
    div []
        [ img [ src "/logo.svg" ] []
        , h1 [] [ text "Your Elm App is working!" ]
        , case model.userData of
            Maybe.Nothing ->
                button [ onClick SignIn ] [ text "Sign in with Google" ]

            Just userData ->
                div []
                    [ text userData.email
                    , button [ onClick <| SendCurrentFoodLog userData.uid ] [ text "Send" ]
                    , button [ onClick LogOut ] [ text "Log out" ]
                    ]
        ]



---- PROGRAM ----


userDataDecoder : Json.Decode.Decoder UserData
userDataDecoder =
    Json.Decode.succeed UserData
        |> Json.Decode.Pipeline.required "token" Json.Decode.string
        |> Json.Decode.Pipeline.required "email" Json.Decode.string
        |> Json.Decode.Pipeline.required "uid" Json.Decode.string


foodLogEncoder : FoodLog -> String -> Json.Encode.Value
foodLogEncoder foodLog uid =
    Json.Encode.object
        [ ( "uid", Json.Encode.string <| uid )
        , ( "ts", Json.Encode.int <| Time.posixToMillis foodLog.ts )
        , ( "keto", Json.Encode.bool <| foodLog.keto )
        , ( "vegan", Json.Encode.bool <| foodLog.vegan )
        , ( "meat", Json.Encode.bool <| foodLog.meat )
        , ( "alcohol", Json.Encode.bool <| foodLog.alcohol )
        , ( "caffeine", Json.Encode.bool <| foodLog.caffeine )
        , ( "portion", Json.Encode.string <| portionToString foodLog.portion )
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ signInInfo (Json.Decode.decodeValue userDataDecoder >> LoggedInData)
        ]


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = subscriptions
        }
