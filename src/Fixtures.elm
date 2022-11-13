module Fixtures exposing (..)

import Date
import Project exposing (Project, ProjectID, Timeline, TimelineID, Todo, TodoID)


type Color
    = Color String


projects : List Project
projects =
    [ mkProject 1 (Color "#ff7e00") "Märchen"
    , mkProject 2 (Color "#7eff00") "Urlaub"
    ]


timelines : List Timeline
timelines =
    [ mkTimeline 1 1 "Rumpelstilzchen"
    , mkTimeline 2 2 "Micha"
    , mkTimeline 2 3 "Hermann"
    ]


d =
    738473


todos : List Todo
todos =
    [ mkTodo 1 0 "Stroh zu Gold spinnen" (d - 10) True
    , mkTodo 1 1 "Backen" d True
    , mkTodo 1 2 "Brauen" (d + 1) False
    , mkTodo 1 3 "Der Königin ihr Kind holen" (d + 2) False
    , mkTodo 2 4 "Urlaubstag" 738456 False
    , mkTodo 2 5 "Brückentag" 738459 False
    , mkTodo 2 6 "Feiertag" 738460 False
    , mkTodo 3 7 "Wochenende" 738457 False
    , mkTodo 3 8 "Feiertag" 738460 False
    ]


mkProject : ProjectID -> Color -> String -> Project
mkProject id (Color color) title =
    { id = id, color = color, comment = Nothing, start = Date.fromRataDie 738415, end = Date.fromRataDie 738415, people = [], timelines = [], title = title }


mkTimeline : TimelineID -> ProjectID -> String -> Timeline
mkTimeline parentProject id title =
    { id = id, parentProject = parentProject, comment = Nothing, tags = [], people = [], title = title, todos = [] }


mkTodo : TimelineID -> TodoID -> String -> Int -> Bool -> Todo
mkTodo parentTimeline id title day done =
    { parentTimeline = parentTimeline, id = id, date = Date.fromRataDie day, done = done, title = title, comment = Nothing }
