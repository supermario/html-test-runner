module ViewPlain exposing (view)

{-| Plain view

Testing! Do not use :)

@docs view

-}

import Html
import String
import Test.Runner.Exploration as Runner
import Test.Runner.Html.View as View
import Time exposing (Time)


-- A plain text version of View.elm


{-| The view
-}
view : View.Model -> Html.Html msg
view model =
    app model


type Styles
    = None
    | App
    | Header Palette
    | Description Palette


type Palette
    = Primary
    | Secondary
    | Accent
    | Background
    | Good
    | Bad
    | Warning


app : View.Model -> Html.Html msg
app model =
    let
        wrapper nested =
            Html.text nested
    in
    wrapper <|
        case model of
            Nothing ->
                "Loading Tests..."
                    |> header
                    |> summary []

            Just ( duration, Runner.Pass passed ) ->
                ( Good, "Test Run Passed" )
                    |> finished duration passed []
                    |> summary []

            Just ( duration, Runner.Todo passed failures ) ->
                ( Warning, "Test Run Incomplete: TODO's remaining" )
                    |> finished duration passed failures
                    |> summary failures

            Just ( duration, Runner.Incomplete passed Runner.Only ) ->
                ( Warning, "Test Run Incomplete: Test.only was used" )
                    |> finished duration passed []
                    |> summary []

            Just ( duration, Runner.Incomplete passed Runner.Skip ) ->
                ( Warning, "Test Run Incomplete: Test.skip was used" )
                    |> finished duration passed []
                    |> summary []

            Just ( duration, Runner.Incomplete passed (Runner.Custom reason) ) ->
                ( Warning, "Test Run Incomplete: " ++ reason )
                    |> finished duration passed []
                    |> summary []

            Just ( duration, Runner.Fail passed failures ) ->
                ( Bad, "Test Run Failed" )
                    |> finished duration passed failures
                    |> summary failures

            Just ( duration, Runner.Running { passed, failures, remaining } ) ->
                running (passed + List.length failures) remaining
                    |> summary failures


running : Int -> Int -> String
running completed remaining =
    String.join "\n"
        [ header "Running Tests..."
        , toString completed ++ " completed"
        , toString remaining ++ " remaining"
        ]


finished : Time -> Int -> List a -> ( Palette, String ) -> String
finished duration passed failures ( headlineColor, headlineText ) =
    String.join "\n"
        [ headlineText
        , "Duration: " ++ formattedDuration duration
        , "Passed: " ++ toString passed
        , "Failed: " ++ toString (List.length failures)
        ]


summary : List Runner.Failure -> String -> String
summary failures message =
    String.join ""
        [ message
        , allFailures failures
        , "\n"
        ]


allFailures : List Runner.Failure -> String
allFailures failures =
    String.join "" <| List.map oneFailure failures


oneFailure : Runner.Failure -> String
oneFailure failure =
    let
        ( labels, expectations ) =
            Runner.formatFailure
                (coloredLabel '↓' Secondary)
                (coloredLabel '✗' Bad)
                failure

        inContext { given, message } =
            String.join "\n"
                [ case given of
                    Nothing ->
                        ""

                    Just x ->
                        givenCode x
                , code None message
                ]
    in
    String.join "\n" <| labels ++ [ spacer 3 ] ++ List.map inContext expectations


spacer : Int -> String
spacer x =
    String.join "" <| List.repeat x " "


givenCode : String -> String
givenCode value =
    code None ("Given " ++ value)


coloredLabel : Char -> Palette -> String -> String
coloredLabel char textColor str =
    String.cons char (String.cons ' ' str) ++ "\n"


formattedDuration : Time -> String
formattedDuration time =
    toString time ++ " ms"


code : style -> String -> String
code style str =
    str


header : String -> String
header text =
    text ++ "\n"
