module Bem exposing (Node, class, block, element, modify, modifyIf)

import Html exposing (Attribute)
import Html.Attributes as Attributes


type Node
    = Block String (List String)
    | Element String String (List String)


className : Node -> String
className node =
    case node of
        Block blockName modifierNames ->
            blockName :: modifierClasses blockName modifierNames
            |> String.join " "

        Element blockName elementName modifierNames ->
            let
                elementClass =
                    blockName ++ "__" ++ elementName
            in
            elementClass :: modifierClasses elementClass modifierNames
            |> String.join " "


modifierClasses : String -> List String -> List String
modifierClasses prefix modifierNames =
    let
        modifierClass modifierName =
            prefix ++ "--" ++ modifierName
    in
    List.map modifierClass modifierNames


class : Node -> Attribute msg
class node =
    Attributes.class (className node)


block : String -> Node
block blockName =
    Block blockName []


element : String -> String -> Node
element blockName elementName =
    Element blockName elementName []


modify : String -> Node -> Node
modify modifierName node =
    case node of
        Block blockName modifierNames ->
            Block blockName (modifierName :: modifierNames)

        Element blockName elementName modifierNames ->
            Element blockName elementName (modifierName :: modifierNames)


modifyIf : Bool -> String -> Node -> Node
modifyIf condition modifierName node =
    if condition
    then modify modifierName node
    else node
