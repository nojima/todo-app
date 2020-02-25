module Main exposing (main)

import Bem
import Browser
import DateFormat
import Dict as Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes as Attr exposing (class)
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
    , todoSummary : String
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
    ( { todoList = Dict.empty, timeZone = Time.utc, todoSummary = "" }
    , Task.map2 OpenedApplication Time.now Time.here
        |> Task.perform identity
    )



-- UPDATE


type Msg
    = OpenedApplication Time.Posix Time.Zone
    | CloseTodo TodoId
    | CloseTodoNow TodoId Time.Posix
    | ReopenTodo TodoId
    | EnteredTodoSummary String
    | AddTodo
    | AddTodoNow Time.Posix


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        OpenedApplication now timeZone ->
            ( initializeTodoList model now timeZone, Cmd.none )

        CloseTodo id ->
            ( model, Task.perform (CloseTodoNow id) Time.now )

        CloseTodoNow id now ->
            ( closeTodo model id now, Cmd.none )

        ReopenTodo id ->
            ( reopenTodo model id, Cmd.none )

        EnteredTodoSummary summary ->
            ( { model | todoSummary = summary }, Cmd.none )

        AddTodo ->
            ( model, Task.perform AddTodoNow Time.now )

        AddTodoNow now ->
            ( addTodoNow model now, Cmd.none )


initializeTodoList : Model -> Time.Posix -> Time.Zone -> Model
initializeTodoList model now timeZone =
    { model
        | todoList = Dict.fromList
            [ ( 1
              , { id = 1
                , summary = "発表用スライドを作る"
                , done = True
                , createdAt = now
                , closedAt = Just now
                }
              )
            , ( 2
              , { id = 2
                , summary = "台所用洗剤を買ってくる"
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


addTodoNow : Model -> Time.Posix -> Model
addTodoNow model now =
    let
        maxId = Dict.foldl (\k _ acc -> max k acc) 0 model.todoList

        newTodo =
            { id = maxId + 1
            , summary = model.todoSummary
            , done = False
            , createdAt = now
            , closedAt = Nothing
            }

        newTodoList =
            Dict.insert newTodo.id newTodo model.todoList
    in
    { model | todoList = newTodoList, todoSummary = "" }



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

        gap =
            div [ Bem.class (Bem.block "Gap") ] []
    in
    { title = "(" ++ String.fromInt undoneTasks ++ ") TODO App"
    , body = renderContainer
        [ renderInputForm model
        , gap
        , renderTodoList model
        ]
    }


renderContainer : List (Html msg) -> List (Html msg)
renderContainer inner =
    [ div [ Bem.class (Bem.block "Main") ] inner ]


renderInputForm : Model -> Html Msg
renderInputForm model =
    let
        block = Bem.block "AddTodoForm"
    in
    form [ Bem.class block, Events.onSubmit AddTodo ]
        [ div
            [ Bem.class (Bem.element block "Label"), Attr.for "AddTodoForm_Input" ]
            [ span [ Bem.class (Bem.element block "LabelText") ]
                [ text "Add TODO" ]
            ]
        , input
            [ Bem.class (Bem.element block "Input")
            , Attr.id "AddTodoForm_Input"
            , Attr.type_ "text"
            , Attr.value model.todoSummary
            , Attr.placeholder "例: 部屋を掃除する"
            , Events.onInput EnteredTodoSummary
            ]
            []
        , button
            [ Bem.class (Bem.element block "SubmitButton")
            , Attr.type_ "submit"
            ]
            [ text "Submit" ]
        ]


renderTodoList : Model -> Html Msg
renderTodoList model =
    let
        block = Bem.block "TodoList"

        wrapItem content =
            li [ Bem.class (Bem.element block "Item") ] [ content ]

        listItems =
            Dict.values model.todoList
                |> List.map (renderTodo model.timeZone)
                |> List.map wrapItem
    in
    ul [ Bem.class block ] listItems


renderTodo : Time.Zone -> Todo -> Html Msg
renderTodo timeZone todo =
    let
        block = Bem.block "TodoItem"

        id = "TodoItem_" ++ String.fromInt todo.id

        layout left right =
            [ div [ Bem.class (Bem.element block "Left") ] left
            , div [ Bem.class (Bem.element block "Right") ] right
            ]
    in
    div [ Bem.class block, Attr.id id ] <|
        layout
            [ renderCheckBox block todo ]
            [ renderSummary block todo
            , text " "
            , renderCreatedAt block timeZone todo
            , renderClosedAt block timeZone todo
            ]


renderCheckBox : Bem.Node -> Todo -> Html Msg
renderCheckBox block todo =
    let
        id = "TodoItem_CheckBox_" ++ String.fromInt todo.id

        onCheck checked =
            if checked
            then CloseTodo todo.id
            else ReopenTodo todo.id
    in
    input
        [ Bem.class (Bem.element block "CheckBox")
        , Attr.id id
        , Attr.type_ "checkbox"
        , Attr.name id
        , Attr.checked todo.done
        , Events.onCheck onCheck
        ]
        []


renderSummary : Bem.Node -> Todo -> Html msg
renderSummary block todo =
    let
        element = Bem.element block "Summary"
            |> Bem.modifyIf todo.done "isClosed"
    in
    h5 [ Bem.class element ]
        [ text todo.summary ]


renderCreatedAt : Bem.Node -> Time.Zone -> Todo -> Html msg
renderCreatedAt block timeZone todo =
    let
        createdAt =
            formatDateTime timeZone todo.createdAt
    in
    small [ Bem.class (Bem.element block "CreatedAt") ]
        [ text ("Created at " ++ createdAt) ]


renderClosedAt : Bem.Node -> Time.Zone -> Todo -> Html msg
renderClosedAt block timeZone todo =
    case todo.closedAt of
        Nothing ->
            span [] []

        Just closedAt ->
            let
                t = formatDateTime timeZone closedAt
            in
            small [ Bem.class (Bem.element block "ClosedAt") ]
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
