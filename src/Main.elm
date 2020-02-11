module Main exposing (..)

import Browser
import Html exposing (Html, button, div, h1, h2, img, text)
import Html.Attributes exposing (src)
import Html.Events exposing (onClick)
import Json.Decode
import Json.Encode
import Ports exposing (..)
import Time



---- MODEL ----


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
    }


type alias Model =
    { userData : Maybe UserData
    , error : ErrorData
    , currentFoodLog : Maybe FoodLog
    }


randomFoodLog : FoodLog
randomFoodLog =
    FoodLog (Time.millisToPosix 0) Medium False False False True


init : ( Model, Cmd Msg )
init =
    ( { userData = Maybe.Nothing
      , error = emptyError

      --   , currentFoodLog = Maybe.Nothing
      , currentFoodLog = Just randomFoodLog
      }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = LogIn
    | LogOut
    | LoggedInData (Result Json.Decode.Error UserData)
    | LoggedInError (Result Json.Decode.Error ErrorData)
    | SaveFoodLog


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LogIn ->
            ( model, signIn () )

        LogOut ->
            ( { model | userData = Maybe.Nothing, error = emptyError }, signOut () )

        LoggedInData result ->
            case result of
                Ok value ->
                    ( { model | userData = Just value }, Cmd.none )

                Err error ->
                    ( { model | error = messageToError <| Json.Decode.errorToString error }, Cmd.none )

        LoggedInError result ->
            case result of
                Ok value ->
                    ( { model | error = value }, Cmd.none )

                Err error ->
                    ( { model | error = messageToError <| Json.Decode.errorToString error }, Cmd.none )

        SaveFoodLog ->
            case model.currentFoodLog of
                Just foodLog ->
                    ( model, saveFoodLog <| foodLogEncoder foodLog )

                Maybe.Nothing ->
                    ( model, Cmd.none )



---- VIEW ----


foodLogEncoder : FoodLog -> Json.Encode.Value
foodLogEncoder foodLog =
    Json.Encode.object
        [ ( "portion", Json.Encode.string <| portionToString foodLog.portion )
        , ( "keto", Json.Encode.bool foodLog.keto )
        , ( "vegan", Json.Encode.bool foodLog.vegan )
        , ( "meat", Json.Encode.bool foodLog.meat )
        , ( "alcohol", Json.Encode.bool foodLog.alcohol )
        , ( "ts", Json.Encode.int <| Time.posixToMillis foodLog.ts )
        ]


view : Model -> Html Msg
view model =
    div []
        [ img [ src "/logo.svg" ] []
        , h1 [] [ text "Your Elm App is working!" ]
        , case model.userData of
            Just data ->
                button [ onClick LogOut ] [ text "Logout from Google" ]

            Maybe.Nothing ->
                button [ onClick LogIn ] [ text "Login with Google" ]
        , h2 []
            [ text <|
                case model.userData of
                    Just data ->
                        data.email ++ " " ++ data.uid ++ " " ++ data.token

                    Maybe.Nothing ->
                        ""
            ]
        , h2 [] [ text <| errorPrinter model.error ]
        , case model.userData of
            Just _ ->
                button [ onClick SaveFoodLog ] [ text "Log food" ]

            Maybe.Nothing ->
                div [] []
        ]



---- PROGRAM ----


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ signInInfo (Json.Decode.decodeValue userDataDecoder >> LoggedInData)
        , signInError (Json.Decode.decodeValue logInErrorDecoder >> LoggedInError)
        ]


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = subscriptions
        }
