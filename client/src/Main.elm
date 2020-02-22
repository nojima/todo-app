module Main exposing (main)

import Browser
import DateFormat
import Dict as Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes as Attr
import Html.Events as Events
import Task
import Time


main : Program () Model Message
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type alias Model =
    { todoList : Dict TodoId Todo
    , timeZone : Time.Zone
    }


type alias TodoId =
    Int


type alias Todo =
    { id : TodoId
    , summary : String
    , done : Bool
    , createdAt : Time.Posix
    }


init : () -> ( Model, Cmd Message )
init _ =
    ( { todoList = Dict.empty, timeZone = Time.utc }
    , Task.map2 Initialize Time.now Time.here
        |> Task.perform identity
    )



-- UPDATE


type Message
    = Initialize Time.Posix Time.Zone
    | TodoChecked TodoId Bool


update : Message -> Model -> ( Model, Cmd Message )
update message model =
    case message of
        Initialize now timeZone ->
            ( initializeTodoList now timeZone, Cmd.none )

        TodoChecked id checked ->
            ( updateTodoCheck model id checked, Cmd.none )


initializeTodoList : Time.Posix -> Time.Zone -> Model
initializeTodoList now timeZone =
    { todoList =
        Dict.fromList
            [ ( 1
              , { id = 1
                , summary = "First TODO"
                , done = True
                , createdAt = now
                }
              )
            , ( 2
              , { id = 2
                , summary = "Second TODO"
                , done = False
                , createdAt = now
                }
              )
            ]
    , timeZone = timeZone
    }


updateTodoCheck : Model -> TodoId -> Bool -> Model
updateTodoCheck model id checked =
    let
        updateTodo todo =
            { todo | done = checked }

        newTodoList =
            Dict.update id (Maybe.map updateTodo) model.todoList
    in
    { model | todoList = newTodoList }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Message
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> Html Message
view model =
    let
        listItems =
            Dict.values model.todoList
                |> List.map (renderTodo model.timeZone)
    in
    ul [ Attr.class "todo-list" ] listItems


renderTodo : Time.Zone -> Todo -> Html Message
renderTodo timeZone todo =
    let
        id =
            "todo-" ++ String.fromInt todo.id
    in
    li [ Attr.class "todo-item", Attr.id id ]
        [ renderCheckBox todo
        , renderSummary todo
        , text " "
        , renderCreatedAt timeZone todo
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


renderCreatedAt : Time.Zone -> Todo -> Html msg
renderCreatedAt timeZone todo =
    let
        createdAt =
            formatDateTime timeZone todo.createdAt
    in
    span [ Attr.class "todo-item-created-at" ]
        [ text ("(" ++ createdAt ++ ")") ]


formatDateTime : Time.Zone -> Time.Posix -> String
formatDateTime =
    DateFormat.format
        [ DateFormat.yearNumber
        , DateFormat.text "-"
        , DateFormat.monthFixed
        , DateFormat.text "-"
        , DateFormat.dayOfMonthFixed
        , DateFormat.text " "
        , DateFormat.hourFixed
        , DateFormat.text ":"
        , DateFormat.minuteFixed
        , DateFormat.text ":"
        , DateFormat.secondFixed
        ]
