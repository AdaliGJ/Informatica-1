--Estuardo Valenzuela
--Adalí Garrán
--Samantha Rodas

module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

--MAIN
main = Browser.sandbox { init = init, view = view, update = update}



--Funciones a Utilizar


mas ls = case ls of
        [] -> ["0"]
        [x] -> [x]
        x :: y :: bs ->if y == "+" then mas (String.fromInt (aInt2 (aInt x) + aInt2 (aInt (primero bs))) :: otros bs)
                       else x :: y :: mas bs
por ls = case ls of
        [] -> [ "0" ]
        [x] -> [x]
        x :: y :: bs -> if y == "x" then por (Debug.toString (aInt2 (aInt x) * aInt2 (aInt (primero bs))) :: otros bs)
                         else x :: y :: por bs
primero ls = case ls of
        [] -> ""
        x :: xs -> x

otros ls = case ls of
        [] ->[""]
        x :: xs -> xs

aInt n = String.toInt n
aInt2 n = case n of
  Just g -> g
  Nothing -> 0
    

listaString ls = case ls of
        [] -> ""
        b :: bs -> b ++ listaString bs

signo s = if s == "+" then False else if s == "x" then False else True

unir ls = case ls of
        [] -> []
        [n] ->[n]
        x :: y :: lista ->
            if signo x && signo y then (x ++ y) :: unir lista
            else if signo x == False then x :: unir (y :: lista)
            else ([x] ++ [y]) ++ unir lista



evaluar ls = primero (mas (por (unir ls)))



type Model = Model String (List String) String


type Msg
    = Numero String | Mas | Por | Reset | Igual


--Forma Modelo

init : Model
init =
    Model "0" ["0"] "0"


update : Msg -> Model -> Model
update msg (Model numero modelo resultado) =
    case msg of
        Numero x ->
            if numero == "0" then Model x [x] "0" 
            else if numero == resultado then Model x [x] (resultado)
            else Model (numero ++ x) (modelo++[x]) (resultado)
            
        Mas ->
            Model (numero ++ "+") (modelo++["+"]) (resultado)

        Por ->
            Model (numero ++ "x") (modelo++["x"]) (resultado)

        Reset ->
            init

        Igual ->
            Model (evaluar modelo) (modelo) (evaluar modelo)


view : Model -> Html Msg
view (Model numero modelo resultado) =
    div [][
     div [] [text (listaString modelo) ]
    , div [] [text resultado]
    , div [] [button [ onClick (Numero "1") ] [ text "1" ]
    , button [ onClick (Numero "2") ] [ text "2" ]
    , button [ onClick (Numero "3") ] [ text "3" ]]
    , div [] [button [ onClick (Numero "4") ] [ text "4" ]
    , button [ onClick (Numero "5") ] [ text "5" ]
    , button [ onClick (Numero "6") ] [ text "6" ]]
    , div [] [button [ onClick (Numero "7") ] [ text "7" ]
    , button [ onClick (Numero "8") ] [ text "8" ]
    , button [ onClick (Numero "9") ] [ text "9" ]]
    , button [ onClick Mas ] [ text "+" ]
    , button [ onClick Por ] [ text "x" ]
    , button [ onClick Igual] [ text "=" ]
    , div [] [button [ onClick (Numero "0") ] [ text "0" ]
    , button [ onClick Reset ] [ text ".Res." ]]
    ]