module Main exposing (main)

import Browser
import DateFormat
import Dict as Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes as Attr
import Html.Events as Events
import Task
import Time


main : Program () Model Msg
main =
    Browser.document
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
    , closedAt : Maybe Time.Posix
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { todoList = Dict.empty, timeZone = Time.utc }
    , Task.map2 OpenedApplication Time.now Time.here
        |> Task.perform identity
    )



-- UPDATE


type Msg
    = OpenedApplication Time.Posix Time.Zone
    | CloseTodo TodoId
    | CloseTodoNow TodoId Time.Posix
    | ReopenTodo TodoId


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        OpenedApplication now timeZone ->
            ( initializeTodoList now timeZone, Cmd.none )

        CloseTodo id ->
            ( model, Task.perform (CloseTodoNow id) Time.now )

        CloseTodoNow id now ->
            ( closeTodo model id now, Cmd.none )

        ReopenTodo id ->
            ( reopenTodo model id, Cmd.none )


initializeTodoList : Time.Posix -> Time.Zone -> Model
initializeTodoList now timeZone =
    { todoList =
        Dict.fromList
            [ ( 1
              , { id = 1
                , summary = "First TODO"
                , done = True
                , createdAt = now
                , closedAt = Just now
                }
              )
            , ( 2
              , { id = 2
                , summary = "Second TODO"
                , done = False
                , createdAt = now
                , closedAt = Nothing
                }
              )
            ]
    , timeZone = timeZone
    }


closeTodo : Model -> TodoId -> Time.Posix -> Model
closeTodo model id now =
    let
        updateTodo todo =
            { todo | done = True, closedAt = Just now }

        newTodoList =
            Dict.update id (Maybe.map updateTodo) model.todoList
    in
    { model | todoList = newTodoList }


reopenTodo : Model -> TodoId -> Model
reopenTodo model id =
    let
        updateTodo todo =
            { todo | done = False, closedAt = Nothing }

        newTodoList =
            Dict.update id (Maybe.map updateTodo) model.todoList
    in
    { model | todoList = newTodoList }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> Browser.Document Msg
view model =
    let
        undoneTasks =
            model.todoList
                |> Dict.filter (\_ todo -> not todo.done)
                |> Dict.size
    in
    { title = "(" ++ String.fromInt undoneTasks ++ ") TODO App"
    , body = renderContainer (renderTodoList model)
    }


renderContainer : List (Html msg) -> List (Html msg)
renderContainer inner =
    [ div [ Attr.class "container" ] inner ]


renderTodoList : Model -> List (Html Msg)
renderTodoList model =
    let
        listItems =
            Dict.values model.todoList
                |> List.map (renderTodo model.timeZone)
    in
    [ ul [ Attr.class "list-group" ] listItems ]


renderTodo : Time.Zone -> Todo -> Html Msg
renderTodo timeZone todo =
    let
        id = "todo-" ++ String.fromInt todo.id

        twoColumns left right =
            div [ Attr.class "row" ]
                [ div [ Attr.class "col-md-auto" ] left
                , div [ Attr.class "col" ] right
                ]
    in
    li [ Attr.class "list-group-item", Attr.id id ]
        [ twoColumns
            [ renderCheckBox todo ]
            [ renderSummary todo
            , text " "
            , renderCreatedAt timeZone todo
            , renderClosedAt timeZone todo
            ]
        ]


renderCheckBox : Todo -> Html Msg
renderCheckBox todo =
    let
        id = "todo-" ++ String.fromInt todo.id ++ "-checkbox"

        onCheck checked =
            if checked
            then CloseTodo todo.id
            else ReopenTodo todo.id
    in
    input
        [ Attr.class "todo-item-checkbox"
        , Attr.id id
        , Attr.type_ "checkbox"
        , Attr.name id
        , Attr.checked todo.done
        , Events.onCheck onCheck
        ]
        []


renderSummary : Todo -> Html msg
renderSummary todo =
    let
        strikeIfDone inner =
            if todo.done
            then [ del [] inner ]
            else inner
    in
    h5 [ Attr.class "todo-item-summary" ]
        (strikeIfDone [ text todo.summary ])


renderCreatedAt : Time.Zone -> Todo -> Html msg
renderCreatedAt timeZone todo =
    let
        createdAt =
            formatDateTime timeZone todo.createdAt
    in
    small [ Attr.class "text-muted" ]
        [ text ("Created at " ++ createdAt) ]


renderClosedAt : Time.Zone -> Todo -> Html msg
renderClosedAt timeZone todo =
    case todo.closedAt of
        Nothing ->
            span [] []

        Just closedAt ->
            let
                t = formatDateTime timeZone closedAt
            in
            small [ Attr.class "text-muted" ]
                [ text (", Closed at " ++ t) ]


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
