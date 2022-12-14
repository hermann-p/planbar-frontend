module Editors exposing (..)

import Date exposing (Date)
import Fixtures exposing (projects)
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
        , section
        , span
        , text
        , textarea
        , ul
        )
import Html.Attributes as Attr exposing (checked, class, classList, for, href, id, style, title, type_, value)
import Html.Events exposing (onCheck, onClick, onInput, onSubmit)
import Html.Events.Extra exposing (onClickStopPropagation)
import Project
    exposing
        ( EditorMsg(..)
        , EditorState(..)
        , MainMsg(..)
        , Model
        , Msg(..)
        , Project
        , Timeline
        , Todo
        , getName
        , getParentProject
        , getParentTimeline
        , getProject
        , getSelectedProject
        , getSelectedTimeline
        , getSelectedTodo
        , getTimeline
        , getTodo
        , listIntersperse
        , noHtml
        )


onDate : (Date -> EditorMsg) -> String -> Msg
onDate msg isoDateString =
    case Date.fromIsoString isoDateString of
        Result.Ok date ->
            ProjectMsg (msg date)

        _ ->
            ProjectMsg Noop


type alias Selectable a =
    { a
        | id : Int
        , title : String
    }


elementPicker :
    String
    -> { options : List (Selectable a), onSelect : Int -> Msg, active : Bool, selectedOptionID : Int, onCreate : Msg }
    -> Html Msg
elementPicker _ { options, onSelect, active, selectedOptionID, onCreate } =
    let
        baseClass =
            "picker"

        toPickable : Selectable a -> Html Msg
        toPickable option =
            li
                [ classList [ ( baseClass ++ "__menu-item menu-item", True ), ( baseClass ++ "__menu-item--selected selected", option.id == selectedOptionID ) ]
                , onClick (onSelect option.id)
                ]
                [ a [ href "#", class (baseClass ++ "__item-text") ] [ text <| getName option ] ]
    in
    div [ classList [ ( baseClass, True ), ( "frame", True ), ( baseClass ++ "--active", active ) ] ]
        [ div [ class "frame__body" ]
            [ ul [ class (baseClass ++ "__item-list menu") ]
                (List.map toPickable options)
            , actionButton iconCreate (\_ -> onCreate)
            ]
        ]


type ActiveElement
    = AEProject
    | AETimeline
    | AETodo


editorSelection : Model -> Html Msg
editorSelection ({ editorState, projects, timelines, todos } as model) =
    let
        todo =
            getSelectedTodo model

        timeline =
            case editorState of
                EditorTimeline id ->
                    getTimeline id model

                EditorTodo id ->
                    getTodo id model |> Maybe.andThen (\td -> getParentTimeline td model)

                _ ->
                    Nothing

        project =
            case editorState of
                EditorProject id ->
                    getProject id model

                EditorTimeline id ->
                    getTimeline id model
                        |> Maybe.andThen (\tl -> getParentProject tl model)

                EditorTodo id ->
                    getTodo id model
                        |> Maybe.andThen (\td -> getParentTimeline td model)
                        |> Maybe.andThen (\tl -> getParentProject tl model)

                _ ->
                    Nothing

        projectOptions =
            projects

        activeElement =
            case editorState of
                EditorTodo _ ->
                    AETodo

                EditorTimeline _ ->
                    AETimeline

                _ ->
                    AEProject

        timelineOptions =
            project |> Maybe.map (\p -> List.filter (\tl -> tl.parentProject == p.id) timelines) |> Maybe.withDefault []

        todoOptions =
            timeline |> Maybe.map (\t -> List.filter (\td -> td.parentTimeline == t.id) todos) |> Maybe.withDefault []

        projectPicker =
            elementPicker "Projekt"
                { options = projectOptions
                , onSelect = \id -> ProjectMsg (getProject id model |> Maybe.map EditProject |> Maybe.withDefault Noop)
                , active = activeElement == AEProject
                , selectedOptionID = project |> Maybe.map .id |> Maybe.withDefault -1
                , onCreate = MainMsg CreateProject
                }

        timelinePicker =
            elementPicker "Phase"
                { options = timelineOptions
                , onSelect = \id -> ProjectMsg (getTimeline id model |> Maybe.map EditTimeline |> Maybe.withDefault Noop)
                , active = activeElement == AETimeline
                , selectedOptionID = timeline |> Maybe.map .id |> Maybe.withDefault -1
                , onCreate =
                    case project of
                        Just p ->
                            MainMsg (CreateTimeline p.id)

                        _ ->
                            ProjectMsg Noop
                }

        todoPicker =
            elementPicker "Aufgabe"
                { options = todoOptions
                , onSelect = \id -> ProjectMsg (getTodo id model |> Maybe.map EditTodo |> Maybe.withDefault Noop)
                , active = activeElement == AETodo
                , selectedOptionID = todo |> Maybe.map .id |> Maybe.withDefault -1
                , onCreate =
                    case timeline of
                        Just tl ->
                            MainMsg (CreateTodo tl.id)

                        _ ->
                            ProjectMsg Noop
                }
    in
    section [ class "editor-selection" ]
        [ projectPicker
        , timelinePicker
        , todoPicker
        ]


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
                        ProjectMsg (SetProject { project | comment = Nothing })

                    else
                        ProjectMsg (SetProject { project | comment = Just text })

                comment =
                    Maybe.withDefault "" project.comment
            in
            form [ onSubmit <| ProjectMsg Noop ]
                [ label [ for "project-title" ] [ text "Name" ]
                , input [ type_ "text", id "project-title", onInput <| \value -> ProjectMsg (SetProject { project | title = value }), value project.title ] []
                , div
                    [ class "grid grid-cols-3 u-gap-2" ]
                    [ div []
                        [ label [ for "project-color" ] [ text "Farbe" ]
                        , input [ type_ "color", id "project-color", onInput <| \color -> ProjectMsg (SetProject { project | color = color }), value project.color ] []
                        ]
                    , div []
                        [ label [ for "project-start" ] [ text "Projektanfang" ]
                        , input [ type_ "date", id "project-start", onInput (onDate (\date -> SetProject { project | start = date })), value <| Date.toIsoString project.start ] []
                        ]
                    , div []
                        [ label [ for "project-end" ] [ text "Projektende" ]
                        , input [ type_ "date", id "project-end", onInput (onDate (\date -> SetProject { project | end = date })), value <| Date.toIsoString project.end ] []
                        ]
                    ]
                , div []
                    [ label [ for "project-comment" ] [ text "Anmerkungen" ]
                    , textarea [ id "project-comment", onInput setComment, value comment ] []
                    ]
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
            form [ onSubmit (ProjectMsg Noop) ]
                [ div []
                    [ label [ for "timeline-title" ] [ text "Name" ]
                    , input
                        [ type_ "text"
                        , id "timeline-title"
                        , value timeline.title
                        , onInput <| \title -> ProjectMsg (SetTimeline { timeline | title = title })
                        ]
                        []
                    ]
                , div []
                    [ label [ for "timeline-comment" ] [ text "Anmerkungen" ]
                    , textarea [ id "timeline-comment", value comment, onInput <| \value -> ProjectMsg (SetTimeline { timeline | comment = Just value }) ] []
                    ]
                ]


todoEditor : Maybe Todo -> Html Msg
todoEditor td =
    case td of
        Nothing ->
            noHtml

        Just todo ->
            form [ onSubmit (ProjectMsg Noop) ]
                [ div []
                    [ label [ for "todo-title" ] [ text "Name" ]
                    , input [ type_ "text", id "todo-title", value todo.title, onInput <| \title -> ProjectMsg (SetTodo { todo | title = title }) ] []
                    ]
                , div [ class "grid grid-cols-2 u-gap-2" ]
                    [ div []
                        [ label [ for "todo-date" ] [ text "Datum" ]
                        , input
                            [ type_ "date"
                            , id "todo-date"
                            , value (Date.toIsoString todo.date)
                            , onInput (onDate (\date -> SetTodo { todo | date = date }))
                            ]
                            []
                        ]
                    , div []
                        [ label [ for "todo-done" ] [ text "Erledigt" ]
                        , input [ type_ "checkbox", class "form-ext-checkbox", id "todo-done", checked todo.done, onCheck <| \done -> ProjectMsg (SetTodo { todo | done = not done }) ] []
                        ]
                    ]
                ]


projectSelector : Model -> List (Html Msg)
projectSelector ({ projects } as model) =
    let
        selectProject project =
            ProjectMsg (EditProject project)
    in
    [ buttonSelector "Projekt" selectProject (getSelectedProject model) projects
    , actionButton iconCreate (\_ -> MainMsg CreateProject)
    ]


timelineSelector : Model -> List (Html Msg)
timelineSelector model =
    let
        isChildOf project timeline =
            timeline.parentProject == project.id

        timelines =
            getSelectedProject model
                |> Maybe.map (\p -> List.filter (isChildOf p) model.timelines)
                |> Maybe.withDefault []

        selectTimeline timeline =
            ProjectMsg (EditTimeline timeline)

        onCreate _ =
            getSelectedProject model
                |> Maybe.map (\p -> MainMsg (CreateTimeline p.id))
                |> Maybe.withDefault (ProjectMsg Noop)
    in
    [ buttonSelector "Phase" selectTimeline (getSelectedTimeline model) timelines
    , actionButton iconCreate onCreate
    ]


type alias IconName =
    String


type alias ButtonStyle =
    String


type Icon
    = Icon IconName
    | StyledIcon IconName ButtonStyle
    | StyledIconWithTitle IconName ButtonStyle String


iconDelete : Icon
iconDelete =
    StyledIconWithTitle "icofont-trash" "btn-danger" "L??schen"


iconCreate : Icon
iconCreate =
    StyledIconWithTitle "icofont-plus" "btn-success" "Hinzuf??gen"


actionButton : Icon -> (() -> Msg) -> Html Msg
actionButton icon handleClick =
    let
        ( iconName, buttonStyle, title ) =
            case icon of
                Icon name ->
                    ( name, "btn-transparent", "" )

                StyledIcon name style ->
                    ( name, style, "" )

                StyledIconWithTitle name style title_ ->
                    ( name, style, title_ )
    in
    button [ class buttonStyle, onClick (handleClick ()), Attr.title title ]
        [ i [ class iconName ] [] ]


todoSelector : Model -> List (Html Msg)
todoSelector model =
    let
        isChildOf timeline todo =
            todo.parentTimeline == timeline.id

        todos =
            getSelectedTimeline model
                |> Maybe.map (\tl -> List.filter (isChildOf tl) model.todos)
                |> Maybe.withDefault []

        selectTodo todo =
            ProjectMsg (EditTodo todo)

        onCreate _ =
            getSelectedTimeline model
                |> Maybe.map (\tl -> MainMsg (CreateTodo tl.id))
                |> Maybe.withDefault (ProjectMsg Noop)
    in
    [ buttonSelector "Aufgabe" selectTodo (getSelectedTodo model) todos
    , actionButton iconCreate onCreate
    ]


type alias Item x =
    { x | id : Int, title : String }


buttonSelector : String -> (Item x -> Msg) -> Maybe (Item x) -> List (Item x) -> Html Msg
buttonSelector prefix handleClick selectedItem items =
    let
        fullWidth =
            style "width" "100%"

        itemTitle : Item x -> String
        itemTitle item =
            getName item

        buttonTitle =
            case selectedItem of
                Just item ->
                    prefix ++ ": " ++ itemTitle item

                _ ->
                    prefix

        toOption : Item x -> Html Msg
        toOption item =
            li [ class "menu-item" ]
                [ a [ href "#", onClick (handleClick item) ]
                    [ text <| itemTitle item ]
                ]
    in
    div
        [ class "list-dropdown", style "flex" "1" ]
        [ button [ class "btn-success btn-dropdown m-0", fullWidth ]
            [ span [] [ text buttonTitle ]
            , i [ class "icofont-caret-down" ] []
            ]
        , ul [ class "menu", fullWidth ] (List.map toOption items)
        ]


editorCard : String -> List (Html Msg) -> Html Msg -> Html Msg
editorCard _ children editor =
    section [ class "frame" ]
        [ div [ class "frame__header editor-card__header" ]
            children
        , div [ class "frame__body p-2" ] [ editor ]
        ]


breadcrumbs : List String -> Html Msg
breadcrumbs coll =
    div [ class "breadcrumbs" ]
        (coll
            |> List.map (\str -> span [ class "breadcrumbs-item" ] [ text str ])
            |> listIntersperse (span [ class "breadcrumbs-separator" ] [ text ">" ])
        )


editorView : Model -> Html Msg
editorView model =
    let
        baseClass =
            "editor-wrapper"

        getTitle : Maybe { a | title : String } -> String
        getTitle el =
            el |> Maybe.map (\e -> { title = e.title }) |> Maybe.withDefault { title = "" } |> getName
    in
    div
        [ classList [ ( "overlay-bg", True ), ( "overlay-bg--open", model.editorState /= EditorClosed ) ]
        , onClick (ProjectMsg CloseEditor)
        ]
        [ div
            [ classList
                [ ( baseClass ++ " bg-green-100 text-dark", True )
                , ( baseClass ++ "--open", model.editorState /= EditorClosed )
                ]
            , onClickStopPropagation (ProjectMsg Noop)
            ]
            [ div [ class "editor-content frame" ]
                [ div [ class "frame__header level" ]
                    [ button
                        [ class "btn-transparent icofont-close-line icofont-2x  level-item"
                        , onClick (ProjectMsg CloseEditor)
                        ]
                        []
                    , h5 [ class "frame__title level-item" ]
                        [ text "Projekte bearbeiten" ]
                    ]
                , div [ class "frame__body" ]
                    [ editorSelection model
                    , div
                        []
                        [ case model.editorState of
                            EditorProject id ->
                                let
                                    project =
                                        getProject id model
                                in
                                editorCard "Project"
                                    [ breadcrumbs [ getTitle project ] ]
                                    (projectEditor project)

                            EditorTimeline id ->
                                let
                                    timeline =
                                        getTimeline id model

                                    project =
                                        timeline |> Maybe.andThen (\tl -> getParentProject tl model)
                                in
                                editorCard "Phase"
                                    [ breadcrumbs [ getTitle project, getTitle timeline ] ]
                                    (timelineEditor timeline)

                            EditorTodo id ->
                                let
                                    todo =
                                        getTodo id model

                                    timeline =
                                        todo |> Maybe.andThen (\td -> getParentTimeline td model)

                                    project =
                                        timeline |> Maybe.andThen (\tl -> getParentProject tl model)
                                in
                                editorCard "Aufgabe"
                                    [ breadcrumbs [ getTitle project, getTitle timeline, getTitle todo ] ]
                                    (todoEditor <| getTodo id model)

                            _ ->
                                noHtml
                        ]
                    ]
                ]
            ]
        ]
