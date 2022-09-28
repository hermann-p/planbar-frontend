module Fixtures exposing (..)

import Date
import Project exposing (Project, Timeline)


projects : List Project
projects =
    [ { color = "bg-orange-400"
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
    [ { comment = Nothing
      , people = [ Project.Person "Bob" ]
      , tags = [ Project.Tag "first", Project.Tag "last", Project.Tag "always" ]
      , title = "TL1"
      , todos =
            [ { comment = Nothing, date = Date.fromRataDie 738420, done = True, title = "start" }
            , { comment = Nothing, date = Date.fromRataDie 738427, done = False, title = "finish" }
            ]
      }
    , { comment = Nothing
      , people = [ Project.Person "Bob" ]
      , tags = [ Project.Tag "first", Project.Tag "last", Project.Tag "always" ]
      , title = "TL2"
      , todos =
            [ { comment = Nothing, date = Date.fromRataDie 738417, done = False, title = "go" }
            , { comment = Nothing, date = Date.fromRataDie 738450, done = True, title = "done" }
            ]
      }
    ]
