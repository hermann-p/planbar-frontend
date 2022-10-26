module Editors exposing (..)

import Date exposing (Date)
import Html
    exposing
        ( Html
        , a
        , button
        , div
        , form
        , h5
        , i
        , input
        , label
        , li
        , option
        , section
        , select
        , span
        , text
        , textarea
        , ul
        )
import Html.Attributes exposing (checked, class, classList, for, href, id, selected, style, type_, value)
import Html.Events exposing (onCheck, onClick, onInput)
import Project
    exposing
        ( EditorMsg(..)
        , Model
        , Msg(..)
        , Project
        , ProjectID
        , Timeline
        , TimelineID
        , Todo
        , TodoID
        , getProject
        , getSelectedProject
        , getSelectedTimeline
        , getSelectedTodo
        , getTimeline
        , getTodo
        , noHtml
        )


onDate : (Date -> EditorMsg) -> String -> Msg
onDate msg isoDateString =
    case Date.fromIsoString isoDateString of
        Result.Ok date ->
            ProjectMsg (msg date)

        _ ->
            ProjectMsg Noop


projectEditor : Maybe Project -> Html Msg
projectEditor prj =
    case prj of
        Nothing ->
            noHtml

        Just project ->
            let
                setComment : String -> Msg
                setComment text =
                    if String.length text == 0 then
                        ProjectMsg (SetProjectComment Nothing)

                    else
                        ProjectMsg << SetProjectComment <| Just text

                comment =
                    Maybe.withDefault "" project.comment
            in
            form []
                [ label [ for "project-title" ] [ text "Name" ]
                , input [ type_ "text", id "project-title", onInput <| ProjectMsg << SetProjectTitle, value project.title ] []
                , div
                    [ class "grid grid-cols-3 u-gap-2" ]
                    [ div []
                        [ label [ for "project-color" ] [ text "Farbe" ]
                        , input [ type_ "color", id "project-color", onInput <| ProjectMsg << SetProjectColor, value project.color ] []
                        ]
                    , div []
                        [ label [ for "project-start" ] [ text "Projektanfang" ]
                        , input [ type_ "date", id "project-start", onInput (onDate SetProjectStart), value <| Date.toIsoString project.start ] []
                        ]
                    , div []
                        [ label [ for "project-end" ] [ text "Projektende" ]
                        , input [ type_ "date", id "project-end", onInput (onDate SetProjectEnd), value <| Date.toIsoString project.end ] []
                        ]
                    ]
                , label [ for "project-comment" ] [ text "Anmerkungen" ]
                , textarea [ id "project-comment", onInput setComment, value comment ] []
                ]


timelineEditor : Maybe Timeline -> Html Msg
timelineEditor tl =
    case tl of
        Nothing ->
            noHtml

        Just timeline ->
            let
                comment =
                    Maybe.withDefault "" timeline.comment
            in
            form []
                [ div []
                    [ label [ for "timeline-title" ] [ text "Name" ]
                    , input
                        [ type_ "text"
                        , id "timeline-title"
                        , value timeline.title
                        , onInput <| ProjectMsg << SetTimelineTitle
                        ]
                        []
                    ]
                , div []
                    [ label [ for "timeline-comment" ] [ text "Anmerkungen" ]
                    , textarea [ id "timeline-comment", value comment, onInput <| ProjectMsg << SetTimelineComment ] []
                    ]
                ]


todoEditor : Maybe Todo -> Html Msg
todoEditor td =
    case td of
        Nothing ->
            noHtml

        Just todo ->
            form []
                [ div []
                    [ label [ for "todo-title" ] [ text "Name" ]
                    , input [ type_ "text", id "todo-title", value todo.title, onInput <| ProjectMsg << SetTodoTitle ] []
                    ]
                , div [ class "grid grid-cols-2 u-gap-2" ]
                    [ div []
                        [ label [ for "todo-date" ] [ text "Datum" ]
                        , input
                            [ type_ "date"
                            , id "todo-date"
                            , value (Date.toIsoString todo.date)
                            , onInput (onDate SetTodoDate)
                            ]
                            []
                        ]
                    , div []
                        [ label [ for "todo-done" ] [ text "Erledigt" ]
                        , input [ type_ "checkbox", id "todo-done", checked todo.done, onCheck <| ProjectMsg << SetTodoDone ] []
                        ]
                    ]
                ]


projectSelector : Model -> Html Msg
projectSelector ({ projects } as model) =
    let
        selectProject project =
            ProjectMsg (EditProject (Just project))
    in
    buttonSelector "Projekt" selectProject (getSelectedProject model) projects


timelineSelector : Model -> Html Msg
timelineSelector model =
    let
        timelines =
            getSelectedProject model
                |> Maybe.andThen (\p -> Just p.timelines)
                |> Maybe.withDefault []

        selectTimeline timeline =
            ProjectMsg (EditTimeline (Just timeline))
    in
    buttonSelector "Phase" selectTimeline (getSelectedTimeline model) timelines


todoSelector : Model -> Html Msg
todoSelector model =
    let
        todos =
            getSelectedTimeline model
                |> Maybe.andThen (\tl -> Just tl.todos)
                |> Maybe.withDefault []

        selectTodo todo =
            ProjectMsg (EditTodo (Just todo))
    in
    buttonSelector "Aufgabe" selectTodo (getSelectedTodo model) todos


buttonSelector :
    String
    -> ({ x | id : number, title : String } -> Msg)
    -> Maybe { x | id : number, title : String }
    -> List { x | id : number, title : String }
    -> Html Msg
buttonSelector prefix handleClick selectedItem items =
    let
        fullWidth =
            style "width" "100%"

        buttonTitle =
            case selectedItem of
                Just item ->
                    prefix ++ ": " ++ item.title

                _ ->
                    prefix

        toOption : { x | id : number, title : String } -> Html Msg
        toOption item =
            li [ class "menu-item" ]
                [ a [ href "#", onClick (handleClick item) ]
                    [ text item.title ]
                ]
    in
    div
        [ class "list-dropdown", fullWidth ]
        [ button [ class "btn-success btn-dropdown m-0", fullWidth ]
            [ span [] [ text buttonTitle ]
            , i [ class "icofont-caret-down" ] []
            ]
        , ul [ class "menu", fullWidth ] (List.map toOption items)
        ]


type EditorView
    = EditorClosed
    | EditorOpen ProjectID TimelineID TodoID


editorCard : String -> Html Msg -> Html Msg -> Html Msg
editorCard title selector editor =
    section [ class "frame" ]
        [ div [ class "frame__header" ]
            [ selector
            ]
        , div [ class "frame__body p-2" ] [ editor ]
        ]


editorView : Model -> Html Msg
editorView ({ editorState } as model) =
    let
        { projectID, timelineID, todoID } =
            editorState

        state =
            case ( projectID, timelineID, todoID ) of
                ( Just pid, Just tid, Just todoid ) ->
                    EditorOpen pid tid todoid

                _ ->
                    EditorClosed

        baseClass =
            "editor-wrapper"
    in
    div
        [ classList
            [ ( baseClass ++ " bg-green-100 text-dark", True )
            , ( baseClass ++ "--open", state /= EditorClosed )
            ]
        ]
        [ div [ class "editor-content frame" ]
            [ div [ class "frame__header level" ]
                [ button
                    [ class "btn-transparent icofont-close-line icofont-2x  level-item"
                    , onClick (ProjectMsg <| EditProject Nothing)
                    ]
                    []
                , h5 [ class "frame__title level-item" ]
                    [ text "Projekte bearbeiten"
                    ]
                ]
            , div [ class "frame__body" ]
                [ div
                    []
                    [ editorCard "Project" (projectSelector model) (projectEditor <| getSelectedProject model)
                    , editorCard "Phase" (timelineSelector model) (timelineEditor <| getSelectedTimeline model)
                    , editorCard "Aufgabe" (todoSelector model) (todoEditor <| getSelectedTodo model)
                    ]
                ]
            ]
        ]
