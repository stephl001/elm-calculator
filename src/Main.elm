module Main exposing (main)

import Browser
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (class, id)
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
   2- Priorité d'opération (4 + 5 * 2 + 3 = 17)
   3- Il doit y avoir un bouton DEL qui reset la calculatrice à son état initial
   4- Bonus: Calcul des pourcentage!
-}
{- type BinaryOperator -}
{- type AssociativeOperator -}
{- plusOperator : AssociativeOperator, etc... -}
{- extractOperator : AssociativeOperator -> BinaryOperator -}
{- performBinaryOperation : BinaryOperator -> Float -> Float -> Float -}
{- type Expression  (see solution) -}
{- reduce : Expression -> Expression -}
{- evaluate : Expression -> Float -}
{- evaluateReduced : Expression -> Float -}
{- pushExpression : Expression -> Expression -> Expression -}
{- pushNumber : Float -> Expression -> Expression -}
{- pushOperator : Float -> AssociativeOperator -> Expression -> Expression -}
{- swapOperator : AssociativeOperator -> Expression -> Expression -}


type alias Model =
    Int


initialModel : Model
initialModel =
    0


update : msg -> Model -> Model
update msg =
    identity


btnOperator : Char -> Html msg
btnOperator opCh =
    button
        [ class "btn-style opera-bg operator" ]
        [ text <| String.fromChar opCh ]


digitBtnClass : String -> Char -> Html msg
digitBtnClass classStr ch =
    button
        [ class classStr ]
        [ text <| String.fromChar ch ]


digitBtn : Char -> Html msg
digitBtn =
    digitBtnClass "btn-style num-bg num"


firstDigitBtn : Char -> Html msg
firstDigitBtn =
    digitBtnClass "btn-style num-bg num first-child"


view : Model -> Html msg
view model =
    div [ id "background" ]
        [ div [ id "result" ] [ text "0" ]
        , div [ id "main" ]
            [ div [ id "first-rows" ]
                [ button [ class "del-bg", id "delete" ] [ text "Del" ]
                , button [ class "btn-style operator opera-bg fall-back" ] [ text "%" ]
                , btnOperator '+'
                ]
            , div [ class "rows" ]
                [ firstDigitBtn '7'
                , digitBtn '8'
                , digitBtn '9'
                , btnOperator '-'
                ]
            , div [ class "rows" ]
                [ firstDigitBtn '4'
                , digitBtn '5'
                , digitBtn '6'
                , btnOperator 'X'
                ]
            , div [ class "rows" ]
                [ firstDigitBtn '1'
                , digitBtn '2'
                , digitBtn '3'
                , btnOperator '/'
                ]
            , div [ class "rows" ]
                [ button [ class "num-bg zero", id "delete" ] [ text "0" ]
                , button [ class "btn-style num-bg period fall-back" ] [ text "." ]
                , button [ id "eqn-bg", class "eqn align" ] [ text "=" ]
                ]
            ]
        ]


main : Program () Model msg
main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }
