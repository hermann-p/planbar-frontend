module Timeline exposing (..)

import Calendar
import Date exposing (Date)
import DateFormat exposing (de)
import Html exposing (Html, div, section, table, tbody, td, text, thead, tr)
import Html.Attributes exposing (class, classList, colspan)
import Project exposing (..)
import Time exposing (Weekday(..))


find : (a -> Bool) -> List a -> Maybe a
find pred xs =
    List.filter pred xs |> List.head


calendarConfig : Calendar.Config
calendarConfig =
    { startWeekday = Mon }


calendarFromDate date =
    Calendar.fromDate (Just calendarConfig) date


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
    in
    thead [ class "timeline-header" ]
        [ tr []
            (List.map
                (\day ->
                    td
                        [ classList <| getDayClasses viewtype today baseClass day
                        ]
                        (if isSectionStart viewtype day then
                            [ text "yay" ]

                         else
                            []
                        )
                )
                days
            )
        ]


projectViewHeader : Project -> Html Msg
projectViewHeader project =
    tr [ class "timeline__project-header" ]
        [ td [ class "timeline__project-header__title", colspan 20 ] [ text project.title ]
        ]


timelineEventItem : Maybe Todo -> Html Msg
timelineEventItem todo =
    case todo of
        Just event ->
            text event.title

        Nothing ->
            div [] []


isJust : Maybe a -> Bool
isJust m =
    case m of
        Nothing ->
            False

        _ ->
            True


projectTimelineItem : Timeline -> Date -> Html Msg
projectTimelineItem timeline day =
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

        _ =
            Debug.log "day, active, event" ( day, isActive, hasEvent )

        cssClasses =
            [ ( baseClass, True )
            , ( baseClass ++ "--active", isActive && not hasEvent )
            , ( baseClass ++ "--event", hasEvent )
            , ( baseClass ++ "--start", day == duration.from )
            , ( baseClass ++ "--end", day == duration.to )
            ]
    in
    td [ classList cssClasses ]
        (if hasEvent then
            [ timelineEventItem event ]

         else if isActive then
            [ div [ class (baseClass ++ "__indicator") ] [] ]

         else
            []
        )


projectTimeline : ViewType -> Date -> List Date -> Timeline -> Html Msg
projectTimeline vt today days timeline =
    let
        baseClass =
            "project-timeline"
    in
    tr [ class baseClass ] (List.map (projectTimelineItem timeline) days)


projectView : ViewType -> Date -> List Date -> Project -> List (Html Msg)
projectView vt today days project =
    projectViewHeader project :: List.map (projectTimeline vt today days) project.timelines


timelineView : { viewType : ViewType, duration : Duration } -> Model -> Html Msg
timelineView { viewType, duration } { projects, today } =
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

        _ =
            Debug.log "time range" ( duration, days )
    in
    section [ class (baseClass ++ " " ++ viewClass) ]
        [ table [ class "timeline" ]
            [ timelineHeader viewType today days
            , tbody [ class "timeline-body" ] (List.concatMap (projectView viewType today days) projects)
            ]
        ]
