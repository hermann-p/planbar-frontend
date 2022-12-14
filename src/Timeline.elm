module Timeline exposing (..)

import Calendar
import Date exposing (Date)
import DateFormat exposing (de)
import Html exposing (Html, div, i, section, table, tbody, td, text, thead, tr)
import Html.Attributes exposing (class, classList, rowspan, style)
import Html.Events exposing (onClick)
import Project exposing (..)
import Time exposing (Weekday(..))


toProjectTree : Model -> List Project
toProjectTree model =
    List.map
        (\p ->
            { p
                | timelines =
                    model.timelines
                        |> List.filter (\tl -> tl.parentProject == p.id)
                        |> List.map
                            (\tl -> { tl | todos = List.filter (\todo -> todo.parentTimeline == tl.id) model.todos })
            }
        )
        model.projects


find : (a -> Bool) -> List a -> Maybe a
find pred xs =
    List.filter pred xs |> List.head


calendarConfig : Calendar.Config
calendarConfig =
    { startWeekday = Mon }


daysIn : Duration -> List Date
daysIn duration =
    List.range (Date.toRataDie <| duration.from) (Date.toRataDie <| duration.to)
        |> List.map Date.fromRataDie


isSectionStart : ViewType -> Date -> Bool
isSectionStart viewType day =
    case viewType of
        Week ->
            Date.weekday day == Mon

        Month ->
            Date.weekdayNumber day == 0


getDayClasses : ViewType -> Date -> String -> Date -> List ( String, Bool )
getDayClasses vt today base day =
    [ ( base, True )
    , ( base ++ "--today", day == today )
    , ( base ++ "--section-start", isSectionStart vt day )
    ]


timelineHeader : ViewType -> Date -> List Date -> Html Msg
timelineHeader viewtype today days =
    let
        baseClass =
            "timeline-header__item"

        columns =
            List.map
                (\day ->
                    td
                        [ classList <| getDayClasses viewtype today baseClass day
                        ]
                        (if isSectionStart viewtype day then
                            [ text <| Date.formatWithLanguage de "E ddd MMM" day ]

                         else
                            []
                        )
                )
                days

        emptyColumn =
            td [ class baseClass ] []
    in
    thead [ class "timeline-header" ]
        [ tr []
            (emptyColumn :: columns)
        ]


timelineEventItem : Project -> Maybe Todo -> Html Msg
timelineEventItem { color } todo =
    let
        baseName =
            "timeline-event"
    in
    case todo of
        Just event ->
            div
                [ class (baseName ++ "__container") ]
                [ div [ class (baseName ++ "__bar--left"), style "border-color" color ] []
                , div
                    [ classList [ ( baseName, True ), ( baseName ++ "--done", event.done ) ]
                    , style "border-color" color
                    , style "color" color
                    , onClick (ProjectMsg (EditTodo event))
                    ]
                    (if event.done then
                        [ i [ class "icofont-ui-check" ] [] ]

                     else
                        []
                    )
                , div [ class "event-actions" ]
                    [ div [ class "event-actions__title" ] [ text <| getName event ]
                    , div [ class "event-actions__button-area" ]
                        [ div [ class "event-actions__button" ]
                            [ i [ class "icofont-ui-edit", onClick (ProjectMsg <| EditTodo event) ] [] ]
                        , div [ class "event-actions__button" ]
                            [ i
                                [ class <|
                                    if event.done then
                                        "icofont-ui-close"

                                    else
                                        "icofont-ui-check"
                                , onClick (ProjectMsg <| SetTodo { event | done = not event.done })
                                ]
                                []
                            ]
                        ]
                    ]
                , div [ class (baseName ++ "__bar--right"), style "border-color" color ] []
                ]

        Nothing ->
            div [] []


isJust : Maybe a -> Bool
isJust m =
    case m of
        Nothing ->
            False

        _ ->
            True


projectTimelineItem : { timeline : Timeline, today : Date, project : Project } -> Date -> Html Msg
projectTimelineItem { timeline, today, project } day =
    let
        duration =
            durationFromTimeline timeline

        baseClass =
            "project-timeline__item"

        isActive =
            Date.isBetween duration.from duration.to day

        event =
            find (\todo -> todo.date == day) timeline.todos

        hasEvent =
            isJust event

        cssClasses =
            [ ( baseClass, True )
            , ( baseClass ++ "--today", day == today )
            , ( baseClass ++ "--active", isActive && not hasEvent )
            , ( baseClass ++ "--event", hasEvent )
            , ( baseClass ++ "--start", day == duration.from )
            , ( baseClass ++ "--end", day == duration.to )
            ]
    in
    td [ classList cssClasses ]
        (if hasEvent then
            [ timelineEventItem project event ]

         else if isActive then
            [ div
                [ class (baseClass ++ "__indicator hover-grow")
                , style "background-color" project.color
                , onClick (ProjectMsg (EditTimeline timeline))
                ]
                []
            ]

         else
            []
        )


projectTimeline : { days : List Date, project : Project, today : Date } -> Timeline -> Int -> Html Msg
projectTimeline { days, project, today } timeline idx =
    let
        baseClass =
            "project-timeline"

        timelineItems =
            List.map (projectTimelineItem { timeline = timeline, today = today, project = project }) days
    in
    tr [ class baseClass ]
        (if idx == 0 then
            projectViewHeader project :: timelineItems

         else
            timelineItems
        )


projectViewHeader : Project -> Html Msg
projectViewHeader project =
    td
        [ class "project-timeline__project-header"
        , rowspan <| List.length project.timelines
        , onClick << ProjectMsg <| EditProject project
        ]
        [ div [ class "project-timeline__project-header__title" ] [ text <| getName project ] ]


projectView : ViewType -> Date -> List Date -> Project -> List (Html Msg)
projectView _ today days project =
    let
        indices =
            List.range 0 <| List.length project.timelines
    in
    List.map2 (projectTimeline { days = days, project = project, today = today }) project.timelines indices


timelineView : { viewType : ViewType, duration : Duration } -> Model -> Html Msg
timelineView { viewType, duration } ({ today } as model) =
    let
        days =
            daysIn duration

        baseClass =
            "timeline-view"

        viewClass =
            baseClass
                ++ (if viewType == Week then
                        "--week"

                    else
                        "--month"
                   )
    in
    section [ class (baseClass ++ " " ++ viewClass) ]
        [ table [ class "timeline" ]
            [ timelineHeader viewType today days
            , tbody [ class "timeline-body" ]
                (List.concatMap (projectView viewType today days) (toProjectTree model))
            ]
        ]
