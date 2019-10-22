--Adalí Garrán
--Samantha Rodas
--Estuardo Valenzuela

module Main exposing (..)

import Browser
import Html.Events exposing (onClick)
import Html exposing (Html, div, button, text)


-- MAIN


main =
  Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model = (String, String)


init : Model
init =
  ("0", "0")



-- UPDATE


type Msg
  = Mas | Igual | Por | Numero Int | Resetear


update : Msg -> Model -> Model
update msg (m1,m2)=
  case msg of
    Mas ->
     (m1++"+", m2)

    Por ->
     (m1++"*",m2)
    
    Igual->
     (m1, Debug.toString (respuesta m1))
    
    Numero x -> if m1 == "0" then ((Debug.toString x), m2) else ((m1++(Debug.toString x)),m2)
    
    Resetear->
      ((Debug.toString 0), (Debug.toString 0)) 
      

aInt x = case x of
  Just g -> g
  Nothing -> 0

aList x = String.toList x 

caracter xs = case xs of 
  []->[]
  b::bs->(String.fromChar b)::(caracter bs)
  


suma b bs xs = 
 aInt (String.toInt (String.fromChar b ++ String.reverse (String.fromList bs))) + aInt (String.toInt (String.fromList xs))


por b bs xs = 
    aInt (String.toInt (String.fromChar b ++ String.reverse (String.fromList bs))) * aInt (String.toInt (String.fromList xs))

ans bs xs = case (bs, xs) of
    (ts, []) -> 0
    ([], n::ns) -> if n == '+' then suma '0' [] ns else if n == '*' then por '0' [] ns else ans (n::[]) (ns)
    (n::ns, c::cs) -> if c == '+' then suma n ns cs else if c == '*' then por n ns cs else ans (n::c::ns) cs

respuesta modelo = ans [] (aList modelo)

-- VIEW


view : Model -> Html Msg
view (m1, m2)=
  div []
    [div [] [ text (m1) ]
    , div [] [text (m2)]
    , div [] [button [ onClick (Numero 1) ] [ text "1" ]
    , button [ onClick (Numero 2) ] [ text "2" ]
    , button [ onClick (Numero 3) ] [ text "3" ]]
    , div [] [button [ onClick (Numero 4) ] [ text "4" ]
    , button [ onClick (Numero 5) ] [ text "5" ]
    , button [ onClick (Numero 6) ] [ text "6" ]]
    , div [] [button [ onClick (Numero 7) ] [ text "7" ]
    , button [ onClick (Numero 8) ] [ text "8" ]
    , button [ onClick (Numero 9) ] [ text "9" ]]
    , button [ onClick Mas ] [ text "+" ]
    , button [ onClick Por ] [ text "x" ]
    , button [ onClick Igual] [ text "=" ]
    , div [] [button [ onClick (Numero 0) ] [ text "0" ]
    , button [ onClick Resetear ] [ text ".Res." ]]
    ]