module Main exposing (..)

import Browser
import Flip
import Html exposing (Attribute, Html, button, div, footer, h1, header, img, main_, p, span, text)
import Html.Attributes exposing (class, height, id, src, style, width)
import Html.Events exposing (onClick)
import Json.Decode
import Json.Decode.Pipeline
import Json.Encode
import Ports
import Styles
import Time



---- MODEL ----


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
    , currentFoodLog : FoodLog
    }


init : ( Model, Cmd Msg )
init =
    ( { userData = Just fakeUserData
      , currentFoodLog = defaultFoodLog
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


defaultFoodLog : FoodLog
defaultFoodLog =
    -- TODO : Use latest values + current timing
    { ts = Time.millisToPosix 0
    , portion = Medium
    , keto = False
    , vegan = False
    , meat = False
    , alcohol = False
    , caffeine = False
    }


fakeUserData : UserData
fakeUserData =
    UserData "eyJhbGciOiJSUzI1NiIsImtpZCI6IjBiYWJiMjI0NDBkYTAzMmM1ZDAwNDJjZGFhOWQyODVjZjhkMjAyYzQiLCJ0eXAiOiJKV1QifQ.eyJuYW1lIjoianVsaWVuIGxlbmdyYW5kLWxhbWJlcnQiLCJwaWN0dXJlIjoiaHR0cHM6Ly9saDMuZ29vZ2xldXNlcmNvbnRlbnQuY29tL2EtL0FBdUU3bUM1WE1yRm1CZ2JKaHBSai1ObjN4X3hrclM2SlNkdmVoUHhwRHpZQ2ciLCJpc3MiOiJodHRwczovL3NlY3VyZXRva2VuLmdvb2dsZS5jb20vc2ltcGxlZm9vZGRpYXJ5LWRhM2RiIiwiYXVkIjoic2ltcGxlZm9vZGRpYXJ5LWRhM2RiIiwiYXV0aF90aW1lIjoxNTg3MjAyMTI1LCJ1c2VyX2lkIjoibG1EclBVc0dDeWdRcTl4NURsNWtBcFBTYzlSMiIsInN1YiI6ImxtRHJQVXNHQ3lnUXE5eDVEbDVrQXBQU2M5UjIiLCJpYXQiOjE1ODcyMDIxMjUsImV4cCI6MTU4NzIwNTcyNSwiZW1haWwiOiJqbGVuZ3JhbmRAZ21haWwuY29tIiwiZW1haWxfdmVyaWZpZWQiOnRydWUsImZpcmViYXNlIjp7ImlkZW50aXRpZXMiOnsiZ29vZ2xlLmNvbSI6WyIxMDczNDMzMDQ3MzA0NTQzNjg4MTciXSwiZW1haWwiOlsiamxlbmdyYW5kQGdtYWlsLmNvbSJdfSwic2lnbl9pbl9wcm92aWRlciI6Imdvb2dsZS5jb20ifX0.IylpIBkxwPUU22fQDV6FWknBJNfQWZiNUckVum_a1SFXmwWUC5J4MNFmhn0J0JdkHwgPQ8lN1q3pxvJcuxmhHh2unj-uWPCIpDJ7WoMD1P3OMIwGIpiwyAYM8W_GBqv4Y2U4bbM7IFk4QwIIfeh5P4BBg3GSxSjTzbYZne5Q9SYwFxi-SdzZV9w5QldAlnSuhDtUDAFdgVkaSM1YX5PKOr6oln5XFAl8flbpu857LdXD77qv-VdxMx7pErK0KrnrHmfYP06XyUpT-tx8VW5dB8XUbDlU23F_Wx4RrAin7kLf6TmG18LfOSPBsGyXscsNt2deDcaJKaHN5WEuH-QkhA" "jlengrand@gmail.com" "lmDrPUsGCygQq9x5Dl5kApPSc9R2"



---- UPDATE ----


type Msg
    = SignIn
    | LoggedInData (Result Json.Decode.Error UserData)
    | LogOut
    | SendCurrentFoodLog FoodLog String
    | ClickedVegan
    | ClickedAlcohol
    | ClickedCaffeine
    | ClickedMeat
    | ClickedPortion Portion


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SignIn ->
            ( model, Ports.signIn () )

        LogOut ->
            ( { model | userData = Maybe.Nothing }, Ports.signOut () )

        LoggedInData result ->
            case result of
                Ok value ->
                    ( { model | userData = Just value }, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )

        SendCurrentFoodLog foodLog uid ->
            ( { model | currentFoodLog = defaultFoodLog }, Ports.saveFoodLog <| foodLogEncoder foodLog uid )

        ClickedVegan ->
            let
                newModel =
                    model.currentFoodLog
                        |> setVegan (not model.currentFoodLog.vegan)
                        |> asCurrentFoodLogIn model
            in
            ( newModel, Cmd.none )

        ClickedAlcohol ->
            let
                newModel =
                    model.currentFoodLog
                        |> setAlcohol (not model.currentFoodLog.alcohol)
                        |> asCurrentFoodLogIn model
            in
            ( newModel, Cmd.none )

        ClickedCaffeine ->
            let
                newModel =
                    model.currentFoodLog
                        |> setCaffeine (not model.currentFoodLog.caffeine)
                        |> asCurrentFoodLogIn model
            in
            ( newModel, Cmd.none )

        ClickedMeat ->
            let
                newModel =
                    model.currentFoodLog
                        |> setMeat (not model.currentFoodLog.meat)
                        |> asCurrentFoodLogIn model
            in
            ( newModel, Cmd.none )

        ClickedPortion portion ->
            let
                newModel =
                    model.currentFoodLog
                        |> setPortion portion
                        |> asCurrentFoodLogIn model
            in
            ( newModel, Cmd.none )


asCurrentFoodLogIn : Model -> FoodLog -> Model
asCurrentFoodLogIn =
    Flip.flip setCurrentFoodLog


setCurrentFoodLog : FoodLog -> Model -> Model
setCurrentFoodLog foodLog model =
    { model | currentFoodLog = foodLog }


setPortion : Portion -> FoodLog -> FoodLog
setPortion portion foodLog =
    { foodLog | portion = portion }


setMeat : Bool -> FoodLog -> FoodLog
setMeat value foodLog =
    { foodLog | meat = value }


setCaffeine : Bool -> FoodLog -> FoodLog
setCaffeine value foodLog =
    { foodLog | caffeine = value }


setAlcohol : Bool -> FoodLog -> FoodLog
setAlcohol value foodLog =
    { foodLog | alcohol = value }


setVegan : Bool -> FoodLog -> FoodLog
setVegan value foodLog =
    { foodLog | vegan = value }


view : Model -> Html Msg
view model =
    div
        [ style "height" "100%"
        , style "width" "100%"
        ]
        [ div
            [ class "flex-col max-w-screen-lg mx-auto h-full"
            ]
            [ header
                [ style "height" "10%"
                , style "background-color" Styles.mainColor
                , class "p-2"
                ]
                [ case model.userData of
                    Maybe.Nothing ->
                        div [] []

                    Just userData ->
                        div [ class "float-right h-full" ]
                            [ span [ class "h-full" ] [ text userData.email ]
                            , img [ class "inline h-full pl-2", src "/user-circle-solid.svg" ] []
                            ]
                ]
            , main_ [ style "height" "80%", style "background-color" Styles.mainColor, class "flex-grow" ] [ text "main" ]
            , footer
                [ style "height" "10%"
                , style "background-color" Styles.mainColor
                , class "py-4"
                ]
                [ text "footer" ]
            ]
        ]


view2 : Model -> Html Msg
view2 model =
    div []
        [ h1 [] [ text "Simple Food Diary" ]
        , case model.userData of
            Maybe.Nothing ->
                button [ onClick SignIn ] [ text "Login with Google" ]

            Just userData ->
                div []
                    [ text userData.email
                    , button [ onClick LogOut ] [ text "Logout" ]
                    ]
        , case model.userData of
            Maybe.Nothing ->
                div [] []

            Just userData ->
                div []
                    [ div [ id "portion" ]
                        [ button (Styles.portionButton ++ [ onClick <| ClickedPortion Small ])
                            [ img (Styles.imageButton ++ [ src "/pizza-slice-solid.svg" ]) []
                            ]
                        , button (Styles.portionButton ++ [ onClick <| ClickedPortion Medium ])
                            [ img (Styles.imageButton ++ [ src "/pizza-slice-solid.svg" ]) []
                            ]
                        , button (Styles.portionButton ++ [ onClick <| ClickedPortion Large ])
                            [ img (Styles.imageButton ++ [ src "/pizza-slice-solid.svg" ]) []
                            ]
                        , button (Styles.portionButton ++ [ onClick <| ClickedPortion Huge ])
                            [ img (Styles.imageButton ++ [ src "/pizza-slice-solid.svg" ]) []
                            ]
                        ]
                    , div [ id "details" ]
                        [ button (Styles.detailButton ++ [ onClick <| ClickedVegan ]) [ img [ src "/leaf-solid.svg" ] [] ]
                        , button (Styles.detailButton ++ [ onClick <| ClickedAlcohol ]) [ img [ src "/beer-solid.svg" ] [] ]
                        , button (Styles.detailButton ++ [ onClick <| ClickedCaffeine ]) [ img [ src "/coffee-solid.svg" ] [] ]
                        , button (Styles.detailButton ++ [ onClick <| ClickedMeat ]) [ img [ src "/drumstick-bite-solid.svg" ] [] ]
                        ]
                    , button [ onClick <| SendCurrentFoodLog model.currentFoodLog userData.uid ] [ text "Send" ]
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
        [ Ports.signInInfo (Json.Decode.decodeValue userDataDecoder >> LoggedInData)
        ]


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = subscriptions
        }
