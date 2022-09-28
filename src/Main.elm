module Main exposing (..)

import Browser exposing (Document)
import Date exposing (Date)
import Editors exposing (projectEditor)
import Fixtures
import Html exposing (Html, aside, div, input, li, main_, nav, section, text, ul)
import Html.Attributes exposing (class, classList, type_)
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


viewSidebar : Model -> Html Msg
viewSidebar _ =
    aside [ class "sidebar bg-dark text-light" ] [ text "I am a sidebar" ]


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
            WeekView date ->
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
        , projectEditor model.editorState.project
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
    , today = initialDate
    , displayPeriod = { from = initialDate, to = initialDate }
    , editorState =
        { project = Nothing
        , timeline = Nothing
        , todo = Nothing
        }
    }


now =
    Task.perform (MainMsg << SetToday) Date.today


main : Program () Model Msg
main =
    Browser.application
        { init = \_ _ _ -> ( initialState, now )
        , subscriptions = subscriptions
        , update = update
        , view = view
        , onUrlChange = \_ -> Debug.todo "handle URL change"
        , onUrlRequest = \_ -> Debug.todo "handle URL request"
        }
