module Main exposing (..)

import Browser exposing (Document)
import Date
import Editors exposing (editorView)
import Fixtures
import Html exposing (Html, a, aside, button, div, i, li, main_, nav, section, span, text, ul)
import Html.Attributes exposing (class, classList, disabled, href)
import Html.Events exposing (onClick)
import Project exposing (..)
import Task
import Timeline exposing (timelineView)


allPagesFalse : ActivePage
allPagesFalse =
    { week = False
    , month = False
    , calendar = False
    , todoEditor = False
    , timelineEditor = False
    , projectEditor = False
    }


getActivePage : Model -> ActivePage
getActivePage { page } =
    case page of
        WeekView _ ->
            { allPagesFalse | week = True }

        MonthView _ ->
            { allPagesFalse | month = True }

        CalendarView _ ->
            { allPagesFalse | calendar = True }

        TodoEditor _ ->
            { allPagesFalse | todoEditor = True }

        TimelineEditor _ ->
            { allPagesFalse | timelineEditor = True }

        ProjectEditor _ ->
            { allPagesFalse | projectEditor = True }


type alias IconName =
    String


sidebarButton : IconName -> String -> Msg -> Html Msg
sidebarButton iconName title handleClick =
    li [ class "menu-item" ]
        [ button [ class "btn-transparent text-light", onClick handleClick ]
            [ i [ class iconName ] []
            , text title
            ]
        ]


viewSidebar : Model -> Html Msg
viewSidebar model =
    aside [ class "sidebar bg-green-800 text-light" ]
        [ div [ class "main-icon" ] [ i [ class "icofont-fox icofont-5x" ] [] ]

        -- div [ class "icofont-bars icofont-rotate-90 icofont-5x" ] []
        , ul [ class "menu" ]
            [ -- li [ class "divider" ] []
              li [ class "menu-title" ] [ text "Projekte" ]
            , sidebarButton "icofont-ui-edit" "Bearbeiten" (ProjectMsg <| EditTimeline (listLast model.timelines))
            , sidebarButton "icofont-ui-add" "Neues Projekt" (MainMsg CreateProject)
            , li [ class "menu-title" ] [ text "Plan" ]
            , sidebarButton "icofont-folder-open" "Öffnen" (ProjectMsg Noop)
            , li [ class "menu-item" ]
                [ button
                    [ classList [ ( "btn-transparent", not model.dirty ), ( "btn-success", model.dirty ) ]
                    , disabled (not model.dirty)
                    ]
                    [ i [ class "icofont-save" ] []
                    , text "Speichern"
                    ]
                ]
            , sidebarButton "icofont-settings" "Einstellungen" (ProjectMsg Noop)
            ]
        ]


viewNavbar : Model -> Html Msg
viewNavbar model =
    let
        isPageActive =
            getActivePage model

        navigationItem : String -> Bool -> Html Msg
        navigationItem title selected =
            li [ classList [ ( "selected", selected ) ] ] [ div [ class "tab-item-content" ] [ text title ] ]
    in
    nav [ class "navbar" ]
        [ div [ class "tab-container" ]
            [ ul []
                [ navigationItem "Woche" isPageActive.week
                , navigationItem "Monat" isPageActive.month
                , navigationItem "Kalender" isPageActive.calendar
                ]
            ]
        ]


viewMainContent : Model -> Html Msg
viewMainContent ({ page, displayPeriod } as model) =
    main_ [ class "main-content" ]
        [ case page of
            WeekView _ ->
                timelineView { viewType = Week, duration = displayPeriod } model

            _ ->
                Debug.todo "View not defined"
        ]


view : Model -> Document Msg
view model =
    { title = "PlanBar"
    , body =
        [ viewSidebar model
        , section [ class "wrapper" ]
            [ viewNavbar model
            , viewMainContent model
            ]
        , editorView model
        ]
    }


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


initialState : Model
initialState =
    let
        initialDate =
            Date.fromRataDie 0
    in
    { page = WeekView initialDate
    , projects = Fixtures.projects
    , timelines = List.concatMap .timelines Fixtures.projects
    , todos =
        Fixtures.projects
            |> List.concatMap .timelines
            |> List.concatMap .todos
    , today = initialDate
    , displayPeriod = { from = initialDate, to = initialDate }
    , editorState =
        { projectID = Nothing
        , timelineID = Nothing
        , todoID = Nothing
        }
    , dirty = False
    }


now : Cmd Msg
now =
    Task.perform (MainMsg << SetToday) Date.today


main : Program () Model Msg
main =
    Browser.application
        { init = \_ _ _ -> ( initialState, now )
        , subscriptions = subscriptions
        , update = update
        , view = view
        , onUrlChange = \_ -> ProjectMsg Noop
        , onUrlRequest = \_ -> ProjectMsg Noop
        }
