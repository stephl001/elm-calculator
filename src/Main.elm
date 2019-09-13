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
   3- Il doit y avoir un bouton C qui reset la calculatrice à son état initial
   4-
-}

type BinaryOperator
    = Plus
    | Minus
    | Multiply
    | Divide


type AssociativeOperator
    = LeftAssociative BinaryOperator
    | RightAssociative BinaryOperator


plusOperator : AssociativeOperator
plusOperator =
    LeftAssociative Plus


minusOperator : AssociativeOperator
minusOperator =
    LeftAssociative Minus


multiplyOperator : AssociativeOperator
multiplyOperator =
    RightAssociative Multiply


divideOperator : AssociativeOperator
divideOperator =
    RightAssociative Plus


extractOperator : AssociativeOperator -> BinaryOperator
extractOperator operator =
    case operator of
        LeftAssociative o ->
            o

        RightAssociative o ->
            o


performBinaryOperation : BinaryOperator -> Float -> Float -> Float
performBinaryOperation op n1 n2 =
    case op of
        Plus ->
            n1 + n2

        Minus ->
            n1 - n2

        Multiply ->
            n1 * n2

        Divide ->
            n1 / n2


type Expression
    = Number Float
    | PartialOperation AssociativeOperator Float
    | BinaryOperation BinaryOperator Float Expression


pushExpression : Expression -> Expression -> Expression
pushExpression srcExpr targetExpr =
    case targetExpr of
        Number _ ->
            srcExpr

        BinaryOperation op n rightExpr ->
            pushExpression srcExpr rightExpr |> BinaryOperation op n

        PartialOperation op n ->
            let
                binaryOp =
                    extractOperator op
            in
            BinaryOperation binaryOp n srcExpr


pushNumber : Float -> Expression -> Expression
pushNumber =
    Number >> pushExpression


swapOperator : AssociativeOperator -> Expression -> Expression
swapOperator op expr =
    case expr of
        Number _ ->
            expr

        BinaryOperation binaryOp leftNb rightExpr ->
            swapOperator op rightExpr |> BinaryOperation binaryOp leftNb

        PartialOperation _ leftNb ->
            PartialOperation op leftNb


pushOperator : Float -> AssociativeOperator -> Expression -> Expression
pushOperator inputNumber operator expr =
    let
        toPartialOperation =
            PartialOperation operator
    in
    case operator of
        LeftAssociative op ->
            expr |> pushNumber inputNumber |> evaluateReduced |> toPartialOperation

        RightAssociative op ->
            case expr of
                Number nb ->
                    toPartialOperation nb

                BinaryOperation oper n rightExpr ->
                    pushOperator inputNumber operator rightExpr |> BinaryOperation oper n

                PartialOperation (LeftAssociative oper) n ->
                    inputNumber |> toPartialOperation |> BinaryOperation oper n

                PartialOperation (RightAssociative oper) n ->
                    inputNumber |> performBinaryOperation oper n |> toPartialOperation


reduce : Expression -> Expression
reduce expr =
    case expr of
        BinaryOperation op n exp ->
            exp |> evaluateReduced |> performBinaryOperation op n |> Number

        _ ->
            expr


evaluateReduced : Expression -> Float
evaluateReduced =
    reduce >> evaluate


evaluate : Expression -> Float
evaluate expr =
    case expr of
        Number n ->
            n

        PartialOperation _ n ->
            n

        BinaryOperation _ _ ex ->
            evaluate ex


type InputNumber
    = Uninitialized
    | Fixed Float
    | Editing String


type alias Model =
    { input : InputNumber
    , expression : Expression
    }


safeStringToFloat : String -> Float
safeStringToFloat =
    String.toFloat >> Maybe.withDefault 0.0


initialModel : Model
initialModel =
    Model Uninitialized (Number 0.0)


withNewExpression : Expression -> Model
withNewExpression =
    Model Uninitialized


withFixedExpression : Float -> Model
withFixedExpression nb =
    Model (Fixed nb) (Number nb)


updateInput : String -> Model -> Model
updateInput str model =
    { model | input = Editing str }


type CalculatorKey
    = Digit Char
    | Period


charFromCalculatorKey : CalculatorKey -> Char
charFromCalculatorKey key =
    case key of
        Digit c ->
            c

        Period ->
            '.'


type Msg
    = Reset
    | KeyPress CalculatorKey
    | OperatorPress AssociativeOperator
    | Equal


update : Msg -> Model -> Model
update msg model =
    case msg of
        Reset ->
            initialModel

        Equal ->
            case model.input of
                Editing nbStr ->
                    model.expression
                        |> pushNumber (safeStringToFloat nbStr)
                        |> evaluateReduced
                        |> withFixedExpression

                Uninitialized ->
                    { model | input = Fixed 0.0 }

                Fixed _ ->
                    model

        OperatorPress op ->
            case model.input of
                Uninitialized ->
                    swapOperator op model.expression
                        |> withNewExpression

                Fixed nb ->
                    model.expression
                        |> pushOperator nb op
                        |> withNewExpression

                Editing nbStr ->
                    let
                        nb =
                            safeStringToFloat nbStr
                    in
                    pushOperator nb op model.expression |> withNewExpression

        KeyPress key ->
            let
                chStr =
                    key |> charFromCalculatorKey |> String.fromChar
            in
            case model.input of
                Editing str ->
                    if String.length str == 10 then
                        model

                    else
                        let
                            newString =
                                String.concat [ str, chStr ]
                        in
                        case String.toFloat newString of
                            Just _ ->
                                model |> updateInput newString

                            Nothing ->
                                model

                Fixed _ ->
                    { model | input = Uninitialized }

                Uninitialized ->
                    if key == Digit '0' then
                        model

                    else
                        model |> updateInput chStr


textFromModel : Model -> String
textFromModel model =
    case model.input of
        Editing str ->
            str

        Fixed nb ->
            String.fromFloat nb

        Uninitialized ->
            evaluate model.expression |> String.fromFloat


btnOperator : AssociativeOperator -> Char -> Html Msg
btnOperator op opCh =
    button
        [ class "btn-style opera-bg operator"
        , onClick <| OperatorPress op
        ]
        [ text <| String.fromChar opCh ]


digitBtnClass : String -> Char -> Html Msg
digitBtnClass classStr ch =
    button
        [ class classStr, onClick <| KeyPress (Digit ch) ]
        [ text <| String.fromChar ch ]


digitBtn : Char -> Html Msg
digitBtn =
    digitBtnClass "btn-style num-bg num"


firstDigitBtn : Char -> Html Msg
firstDigitBtn =
    digitBtnClass "btn-style num-bg num first-child"


view : Model -> Html Msg
view model =
    div [ id "background" ]
        [ div [ id "result" ] [ text <| textFromModel model ]
        , div [ id "main" ]
            [ div [ id "first-rows" ]
                [ button [ class "del-bg", id "delete", onClick Reset ] [ text "Del" ]
                , button [ class "btn-style operator opera-bg fall-back" ] [ text "%" ]
                , btnOperator plusOperator '+'
                ]
            , div [ class "rows" ]
                [ firstDigitBtn '7'
                , digitBtn '8'
                , digitBtn '9'
                , btnOperator minusOperator '-'
                ]
            , div [ class "rows" ]
                [ firstDigitBtn '4'
                , digitBtn '5'
                , digitBtn '6'
                , btnOperator multiplyOperator 'X'
                ]
            , div [ class "rows" ]
                [ firstDigitBtn '1'
                , digitBtn '2'
                , digitBtn '3'
                , btnOperator divideOperator '/'
                ]
            , div [ class "rows" ]
                [ button [ class "num-bg zero", id "delete", onClick <| KeyPress (Digit '0') ] [ text "0" ]
                , button [ class "btn-style num-bg period fall-back", onClick <| KeyPress Period ] [ text "." ]
                , button [ id "eqn-bg", class "eqn align", onClick Equal ] [ text "=" ]
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
