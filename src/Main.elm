port module Main exposing (..)

import Browser
import Html exposing (Html, button, div, h1, img, text)
import Html.Attributes exposing (src)
import Html.Events exposing (onClick)
import Json.Decode
import Json.Decode.Pipeline
import Json.Encode



---- MODEL ----


port signIn : () -> Cmd msg


port signOut : () -> Cmd msg


port signInInfo : (Json.Encode.Value -> msg) -> Sub msg


type alias UserData =
    { token : String
    , email : String
    , uid : String
    }


type alias Model =
    { userData : Maybe UserData }


init : ( Model, Cmd Msg )
init =
    ( { userData = Maybe.Nothing }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = SignIn
    | LoggedInData (Result Json.Decode.Error UserData)
    | LogOut


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
