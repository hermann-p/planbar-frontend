module Editors exposing (..)

import Date exposing (Date)
import Html exposing (Html, a, button, div, footer, form, header, input, label, li, text, textarea, ul)
import Html.Attributes exposing (class, for, id, type_, value)
import Html.Events exposing (onClick, onInput)
import Project exposing (EditorMsg(..), Msg(..), Person, Project, Timeline, Todo, noHtml)


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
                    case project.comment of
                        Nothing ->
                            ""

                        Just text ->
                            text

                onDate : (Date -> EditorMsg) -> String -> Msg
                onDate msg isoDateString =
                    case Date.fromIsoString isoDateString of
                        Result.Ok date ->
                            ProjectMsg (msg date)

                        _ ->
                            ProjectMsg Noop
            in
            div [ class "modal project-editor shown", id "project-editor" ]
                [ div [ class "modal-content" ]
                    [ header [ class "modal-header level bg-gray-100" ]
                        [ div [ class "level-item" ] [ text project.title ]
                        , button [ class "btn-sm btn-close u-pull-right level-item", onClick << ProjectMsg <| EditProject Nothing ] []
                        ]
                    , div [ class "modal-body" ]
                        [ form []
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
                        , ul [] (List.map (\timeline -> li [] [ text timeline.title ]) project.timelines)
                        ]
                    , footer [ class "modal-footer u-text-right" ]
                        [ a [] [ button [ class "btn btn-cancel u-inline-block", onClick << ProjectMsg <| EditProject Nothing ] [ text "Abbrechen" ] ]
                        , a [] [ button [ class "btn btn-success u-inline-block" ] [ text "Ok" ] ]
                        ]
                    ]
                ]
