module Editors exposing (..)

import Date exposing (Date)
import Html exposing (Html, a, button, div, footer, form, h3, h5, h6, header, input, label, li, option, section, select, text, textarea, ul)
import Html.Attributes exposing (checked, class, for, id, selected, type_, value)
import Html.Events exposing (onClick, onInput)
import Project exposing (EditorMsg(..), Model, Msg(..), Person, Project, Timeline, Todo, getProject, getTimeline, getTodo, noHtml)


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
                    , input [ type_ "text", id "timeline-title", value timeline.title ] []
                    ]
                , div []
                    [ label [ for "timeline-comment" ] [ text "Anmerkungen" ]
                    , textarea [ id "timeline-comment", value comment ] []
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
                    , input [ type_ "text", id "todo-title", value todo.title ] []
                    ]
                , div [ class "grid grid-cols-2 u-gap-2" ]
                    [ div []
                        [ label [ for "todo-date" ] [ text "Datum" ]
                        , input [ type_ "date", id "todo-date", value (Date.toIsoString todo.date) ] []
                        ]
                    , div []
                        [ label [ for "todo-done" ] [ text "Erledigt" ]
                        , input [ type_ "checkbox", id "todo-done", checked todo.done ] []
                        ]
                    ]
                ]


projectSelector : Model -> Html Msg
projectSelector ({ projects } as model) =
    let
        projectToOption : Project -> Html Msg
        projectToOption project =
            option
                [ value (String.fromInt project.id)
                , selected (project.id == (Maybe.withDefault -1 <| Maybe.andThen (\p -> Just p.id) model.editorState.project))
                ]
                [ text project.title ]

        selectProject : String -> Msg
        selectProject id =
            let
                project =
                    getProject projects (Maybe.withDefault -1 <| String.toInt id)
            in
            ProjectMsg <| EditProject project
    in
    select [ class "level-item", onInput selectProject ] (List.map projectToOption projects)


timelineSelector : Model -> Html Msg
timelineSelector model =
    let
        timelines =
            model.editorState.project
                |> Maybe.andThen (\p -> Just p.timelines)
                |> Maybe.withDefault []

        tlToOption : Timeline -> Html Msg
        tlToOption timeline =
            option
                [ value (String.fromInt timeline.id)
                , selected (timeline.id == (Maybe.withDefault -1 <| Maybe.andThen (\tl -> Just tl.id) model.editorState.timeline))
                ]
                [ text timeline.title ]

        selectTimeline : String -> Msg
        selectTimeline id =
            let
                timeline =
                    getTimeline model.projects (Maybe.withDefault -1 <| String.toInt id)
            in
            ProjectMsg <| EditTimeline timeline
    in
    select [ class "level-item", onInput selectTimeline ] (List.map tlToOption timelines)


todoSelector : Model -> Html Msg
todoSelector model =
    let
        todos =
            model.editorState.timeline
                |> Maybe.andThen (\tl -> Just tl.todos)
                |> Maybe.withDefault []

        todoToOption : Todo -> Html Msg
        todoToOption todo =
            option
                [ value (String.fromInt todo.id)
                , selected (todo.id == (Maybe.withDefault -1 <| Maybe.andThen (\td -> Just td.id) model.editorState.todo))
                ]
                [ text todo.title ]

        selectTodo : String -> Msg
        selectTodo id =
            let
                todo =
                    getTodo model.projects (Maybe.withDefault -1 <| String.toInt id)
            in
            ProjectMsg <| EditTodo todo
    in
    select [ class "level-item", onInput selectTodo ] (List.map todoToOption todos)


editorView : Model -> Html Msg
editorView ({ editorState } as model) =
    case ( editorState.project, editorState.timeline, editorState.todo ) of
        ( Nothing, Nothing, Nothing ) ->
            text ""

        _ ->
            div [ class "modal shown" ]
                [ div [ class "modal-content" ]
                    [ div [ class "modal-header bg-gray-100" ]
                        [ h5 []
                            [ -- text "Projekte bearbeiten"
                              button [ class "btn-close u-pull-right", onClick (ProjectMsg <| EditProject Nothing) ] []
                            ]
                        ]
                    , div [ class "modal-body" ]
                        [ section [ class "editor-view" ]
                            [ div [ class "level" ]
                                [ input [ type_ "checkbox", class "accordion-toggle" ] []
                                , projectSelector model
                                ]
                            , projectEditor editorState.project
                            ]
                        , section [ class "editor-view" ]
                            [ div [ class "level" ]
                                [ input [ type_ "checkbox", class "accordion-toggle" ] []
                                , timelineSelector model
                                ]
                            ]
                        , timelineEditor editorState.timeline
                        , section [ class "editor-view" ]
                            [ div
                                [ class "level" ]
                                [ input [ type_ "checkbox", class "accordion-toggle" ] [], todoSelector model ]
                            , todoEditor editorState.todo
                            ]
                        ]
                    ]
                ]
