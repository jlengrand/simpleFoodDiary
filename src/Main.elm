module Main exposing (..)

import Browser
import Browser.Events
import Element
import Element.Background
import Element.Border
import Element.Font
import Element.Input
import Flip
import Html exposing (Html)
import Json.Decode
import Json.Decode.Pipeline
import Json.Encode
import Ports
import Styles
import Task
import Time



---- MODEL ----


type alias Flags =
    { startingWidth : Int
    , startingHeight : Int
    }


type alias ScreenSize =
    { width : Int
    , height : Int
    }


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
    , screenSize : ScreenSize
    , currentFoodLog : FoodLog
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { userData = Maybe.Nothing -- Just fakeUserData
      , currentFoodLog = defaultFoodLog
      , screenSize = { width = flags.startingWidth, height = flags.startingHeight }
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
    { ts = Time.millisToPosix 0
    , portion = Small
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
    | SendCurrentFoodLog Time.Posix
    | ClickedSend
    | ClickedVegan
    | ClickedAlcohol
    | ClickedCaffeine
    | ClickedMeat
    | ClickedKeto
    | ClickedPortion Portion
    | GotNewScreenSize ScreenSize


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

        SendCurrentFoodLog posix ->
            case model.userData of
                Maybe.Nothing ->
                    ( model, Cmd.none )

                Just userData ->
                    let
                        uid =
                            userData.uid

                        foodLog =
                            model.currentFoodLog
                    in
                    ( { model | currentFoodLog = defaultFoodLog }, Ports.saveFoodLog <| foodLogEncoder { foodLog | ts = posix } uid )

        ClickedSend ->
            ( model, getNewTime )

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

        ClickedKeto ->
            let
                newModel =
                    model.currentFoodLog
                        |> setKeto (not model.currentFoodLog.keto)
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

        GotNewScreenSize screenSize ->
            ( { model | screenSize = screenSize }, Cmd.none )


asCurrentFoodLogIn : Model -> FoodLog -> Model
asCurrentFoodLogIn =
    Flip.flip setCurrentFoodLog


setCurrentFoodLog : FoodLog -> Model -> Model
setCurrentFoodLog foodLog model =
    { model | currentFoodLog = foodLog }


setPortion : Portion -> FoodLog -> FoodLog
setPortion portion foodLog =
    { foodLog | portion = portion }


setKeto : Bool -> FoodLog -> FoodLog
setKeto value foodLog =
    { foodLog | keto = value }


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
    Element.layoutWith
        { options =
            [ Element.noHover
            , Element.focusStyle
                { borderColor = Maybe.Nothing
                , backgroundColor = Maybe.Nothing
                , shadow = Maybe.Nothing
                }
            ]
        }
        [ Element.height Element.fill
        , Element.width Element.fill
        , Element.Background.color Styles.graybeige
        ]
        (Element.column
            [ Element.height Element.fill
            , Element.width
                (Element.fill
                    |> Element.maximum Styles.maxWidth
                )
            , Element.Background.color Styles.mainColor
            , Element.centerX
            , Element.padding 2
            ]
            [ -- header
              Element.row
                [ Element.height <| Element.px (get10PercentHeight model.screenSize)
                , Element.padding <| get1PercentHeight model.screenSize
                , Element.width Element.fill
                , Element.spacing <| get1PercentHeight model.screenSize
                ]
                (case model.userData of
                    Just data ->
                        [ Element.el
                            [ Element.alignRight
                            , Element.Font.size 14
                            ]
                            (Element.text data.email)
                        , Element.Input.button
                            [ Element.alignRight
                            , Element.width <| Element.px (getPercentHeight model.screenSize 6)
                            , Element.height Element.fill
                            , Element.centerX
                            ]
                            { onPress = Just LogOut
                            , label =
                                Element.el
                                    [ Element.width Element.fill
                                    , Element.height Element.fill
                                    , Element.Background.uncropped "/user-circle-solid.svg"
                                    ]
                                <|
                                    Element.none
                            }
                        ]

                    Maybe.Nothing ->
                        [ Element.none ]
                )

            -- app title
            , Element.el
                [ Element.height <| Element.px (getPercentHeight model.screenSize 20)
                , Element.width Element.fill
                ]
              <|
                showMainTitle

            -- main
            , Element.column
                [ Element.height Element.fill
                , Element.width Element.fill
                , Element.centerX
                , Element.padding 30
                ]
                (case model.userData of
                    Just userData ->
                        viewMain model userData

                    Maybe.Nothing ->
                        viewLoginMain model
                )

            -- footer
            , Element.row
                [ Element.height <| Element.px (get10PercentHeight model.screenSize)
                , Element.width Element.fill
                ]
                []
            ]
        )


showMainTitle : Element.Element Msg
showMainTitle =
    Element.el [ Element.centerX, Element.centerY ] (Element.text "Simple Food Log")


viewLoginMain : Model -> List (Element.Element Msg)
viewLoginMain model =
    [ Element.el
        [ Element.width Element.fill
        , Element.height Element.fill
        ]
      <|
        Element.Input.button
            [ Element.centerX
            , Element.centerY
            ]
            { onPress = Just SignIn
            , label = Element.text "Login with Google"
            }
    ]


portionSizer : Bool -> Portion -> Int -> Element.Element Msg
portionSizer selected portion sizePx =
    Element.el
        [ Element.Background.color Styles.darkerbeige
        , Element.Border.rounded 50
        , Element.Border.color Styles.black
        , Element.Border.width
            (if selected then
                3

             else
                0
            )
        , Element.width <| Element.px sizePx
        , Element.height <| Element.px sizePx
        , Element.centerY
        ]
    <|
        Element.Input.button
            [ Element.width Element.fill
            , Element.height Element.fill
            , Element.padding (getPercentage sizePx 25)
            ]
            { onPress = Just (ClickedPortion portion)
            , label =
                Element.el
                    [ Element.width Element.fill
                    , Element.height Element.fill
                    , Element.Background.uncropped "/pizza-slice-solid.svg"
                    ]
                <|
                    Element.none
            }


isSelected : Model -> Portion -> Bool
isSelected model portion =
    model.currentFoodLog.portion == portion


itemSelection : Bool -> String -> Msg -> ScreenSize -> Element.Element Msg
itemSelection selected src msg screenSize =
    let
        sizePx =
            get8PercentHeight screenSize
    in
    Element.el
        [ Element.Background.color Styles.darkerbeige
        , Element.Border.rounded 50
        , Element.Border.color Styles.black
        , Element.Border.width
            (if selected then
                3

             else
                0
            )
        , Element.width <| Element.px sizePx
        , Element.height <| Element.px sizePx
        , Element.centerY
        ]
    <|
        Element.Input.button
            [ Element.width Element.fill
            , Element.height Element.fill
            , Element.padding (getPercentage sizePx 25)
            ]
            { onPress = Just msg
            , label =
                Element.el
                    [ Element.width Element.fill
                    , Element.height Element.fill
                    , Element.Background.uncropped src
                    ]
                <|
                    Element.none
            }


viewMain : Model -> UserData -> List (Element.Element Msg)
viewMain model userData =
    [ -- portion size
      Element.column
        [ Element.width Element.fill
        , Element.centerX
        , Element.height <| Element.fillPortion 1
        ]
        [ Element.el
            [ Element.height <| Element.fillPortion 1
            , Element.centerX
            ]
            (Element.text "Your portion size")
        , Element.row
            [ Element.spaceEvenly
            , Element.width Element.fill
            , Element.height <| Element.fillPortion 3
            ]
            [ portionSizer (isSelected model Small) Small (getPercentHeight model.screenSize 6)
            , portionSizer (isSelected model Medium) Medium (getPercentHeight model.screenSize 7)
            , portionSizer (isSelected model Large) Large (getPercentHeight model.screenSize 8)
            , portionSizer (isSelected model Huge) Huge (getPercentHeight model.screenSize 9)
            ]
        ]
    , -- options
      Element.column
        [ Element.height <| Element.fillPortion 1
        , Element.width Element.fill
        ]
        [ Element.el
            [ Element.width Element.fill
            , Element.height <| Element.fillPortion 1
            , Element.centerX
            ]
            (Element.text "Any specifics?")
        , Element.wrappedRow
            [ Element.spaceEvenly
            , Element.width Element.fill
            , Element.height <| Element.fillPortion 3
            ]
            [ itemSelection (model.currentFoodLog.alcohol == True) "/beer-solid.svg" ClickedAlcohol model.screenSize
            , itemSelection (model.currentFoodLog.caffeine == True) "/coffee-solid.svg" ClickedCaffeine model.screenSize
            , itemSelection (model.currentFoodLog.meat == True) "/drumstick-bite-solid.svg" ClickedMeat model.screenSize
            , itemSelection (model.currentFoodLog.vegan == True) "/leaf-solid.svg" ClickedVegan model.screenSize
            , itemSelection (model.currentFoodLog.keto == True) "/bacon-solid.svg" ClickedKeto model.screenSize
            ]
        ]
    , -- validate
      Element.column
        [ Element.height <| Element.fillPortion 1
        , Element.width Element.fill
        ]
        [ Element.Input.button
            [ Element.centerX
            , Element.centerY
            ]
            { onPress = Just <| ClickedSend
            , label = Element.el [ Element.centerX, Element.centerY ] <| Element.text "Send your log!"
            }
        ]
    ]


getPercentage : Int -> Int -> Int
getPercentage original percentage =
    original * percentage // 100


getPercentHeight : ScreenSize -> Int -> Int
getPercentHeight size percentage =
    getPercentage size.height percentage


get8PercentHeight : ScreenSize -> Int
get8PercentHeight size =
    getPercentHeight size 8


get1PercentHeight : ScreenSize -> Int
get1PercentHeight size =
    getPercentHeight size 1


get10PercentHeight : ScreenSize -> Int
get10PercentHeight size =
    getPercentage size.height 10



---- PROGRAM ----


getNewTime : Cmd Msg
getNewTime =
    Task.perform SendCurrentFoodLog Time.now


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
        , Browser.Events.onResize (\width height -> GotNewScreenSize { width = width, height = height })
        ]


main : Program Flags Model Msg
main =
    Browser.element
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }
