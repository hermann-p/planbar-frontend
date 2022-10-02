module Fixtures exposing (..)

import Date
import Project exposing (Project, Timeline)


projects : List Project
projects =
    [ { id = 1
      , color = "bg-orange-400"
      , comment = Nothing
      , start = Date.fromRataDie 738415
      , end = Date.fromRataDie 738499
      , people = [ Project.Person "Alice" ]
      , timelines = timelines
      , title = "Yay, a project"
      }
    ]


timelines : List Timeline
timelines =
    [ { id = 1
      , parentProject = 1
      , comment = Nothing
      , people = [ Project.Person "Bob" ]
      , tags = [ Project.Tag "first", Project.Tag "last", Project.Tag "always" ]
      , title = "TL1"
      , todos =
            [ { parentTimeline = 1, id = 1, comment = Nothing, date = Date.fromRataDie 738420, done = True, title = "start" }
            , { parentTimeline = 1, id = 2, comment = Nothing, date = Date.fromRataDie 738423, done = False, title = "task" }
            , { parentTimeline = 1, id = 3, comment = Nothing, date = Date.fromRataDie 738427, done = False, title = "finish" }
            ]
      }
    , { id = 2
      , parentProject = 1
      , comment = Nothing
      , people = [ Project.Person "Bob" ]
      , tags = [ Project.Tag "first", Project.Tag "last", Project.Tag "always" ]
      , title = "TL2"
      , todos =
            [ { parentTimeline = 2, id = 4, comment = Nothing, date = Date.fromRataDie 738417, done = False, title = "go" }
            , { parentTimeline = 2, id = 5, comment = Nothing, date = Date.fromRataDie 738450, done = True, title = "done" }
            ]
      }
    ]
