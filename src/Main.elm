module Main exposing (main)

import Browser
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (id, class)
import Html.Events exposing (onClick)

{-
Calculatrice

Voici donc un premier exercice pour vous familiariser avec la programmation
fonctionnelle elm. Le but sera donc d'écrire une application calculatrice de base.
Les operation supportées seront l'addition, la soustraction, la multiplication et
la division. La calculatrice effectuera ses opération sur des nombres de type point
flottant (Float). Voici donc un résumé des spécifications:

1- Nombre de caractères numériques (0-9 et . ) affichés: 10
    ex: 87.882 (6 caractères), 90 (2 caractères), 82.9288273 (10 caractères)
2- Aucune priorité d'opération (4 + 5 * 2 + 3 = 21)
3- Il doit y avoir un bouton C qui reset la calculatrice à son état initial
4- 
-}


type alias Model =
    { result : Float }


initialModel : Model
initialModel =
    { result = 0.0 }


type Msg
    = Reset
 {- | Digit Int  -}


update : Msg -> Model -> Model
update msg model =
    case msg of
        Reset -> { model | result = 0.0 }

view : Model -> Html Msg
view model =
    div [ id "background" ]
        [ div [ id "result" ] [ text <| String.fromFloat model.result ]
        , div [ id "main" ]
            [ div [ id "first-rows" ]
                [ button [class "del-bg", id "delete", onClick Reset ] [ text "Del" ]
                , button [class "btn-style operator opera-bg fall-back"] [ text "%" ]
                , button [class "btn-style opera-bg value align operator"] [ text "+" ]
                ]
            , div [class "rows" ]
                [ button [class "btn-style num-bg num first-child"] [ text "7" ]
                , button [class "btn-style num-bg num"] [ text "8" ]
                , button [class "btn-style num-bg num"] [ text "9" ]
                , button [class "btn-style opera-bg operator"] [text "-"]
                ]
            , div [class "rows"]
                [ button [class "btn-style num-bg num first-child"] [text "4"]
                , button [class "btn-style num-bg num"] [text "5"]
                , button [class "btn-style num-bg num"] [text "6"]
                , button [class "btn-style opera-bg operator"] [text "x"]
                ]
            , div [class "rows"]
                [ button [class "btn-style num-bg num first-child"] [text "1"]
                , button [class "btn-style num-bg num"] [text "2"]
                , button [class "btn-style num-bg num"] [text "3"]
                , button [class "btn-style opera-bg operator"] [text "/"]
                ]
            , div [class "rows"]
                [ button [class "num-bg zero", id "delete"] [text "0"]
                , button [class "btn-style num-bg period fall-back"] [text "."]
                , button [id "eqn-bg", class "eqn align"] [text "="]
                ]
            ]
        ]

main : Program () Model Msg
main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }
