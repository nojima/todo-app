module Main exposing (main)

import Browser
import Dict as Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes as Attr
import Html.Events as Events


main : Program () Model Message
main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }



-- MODEL


type alias Model =
    { todoList : Dict TodoId Todo
    }


type alias TodoId =
    Int


type alias Todo =
    { id : TodoId
    , summary : String
    , done : Bool
    }


init : Model
init =
    { todoList =
        Dict.fromList
            [ ( 1
              , { id = 1
                , summary = "First TODO"
                , done = True
                }
              )
            , ( 2
              , { id = 2
                , summary = "Second TODO"
                , done = False
                }
              )
            ]
    }



-- UPDATE


type Message
    = TodoChecked TodoId Bool


update : Message -> Model -> Model
update message model =
    case message of
        TodoChecked id checked ->
            let
                updateTodo todo =
                    { todo | done = checked }

                newTodoList =
                    Dict.update id (Maybe.map updateTodo) model.todoList
            in
            { model | todoList = newTodoList }



-- VIEW


view : Model -> Html Message
view model =
    ul [ Attr.class "todo-list" ]
        (Dict.values model.todoList |> List.map renderTodo)


renderTodo : Todo -> Html Message
renderTodo todo =
    let
        id =
            "todo-" ++ String.fromInt todo.id
    in
    li [ Attr.class "todo-item", Attr.id id ]
        [ renderCheckBox todo
        , renderSummary todo
        ]


renderCheckBox : Todo -> Html Message
renderCheckBox todo =
    let
        id =
            "todo-" ++ String.fromInt todo.id ++ "-checkbox"
    in
    input
        [ Attr.class "todo-item-checkbox"
        , Attr.id id
        , Attr.type_ "checkbox"
        , Attr.name id
        , Attr.checked todo.done
        , Events.onCheck (\checked -> TodoChecked todo.id checked)
        ]
        []


renderSummary : Todo -> Html msg
renderSummary todo =
    span [ Attr.class "todo-item-summary" ]
        [ text todo.summary ]
