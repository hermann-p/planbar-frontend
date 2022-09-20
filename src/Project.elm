module Project exposing (..)

import Date exposing (Date)


type alias Duration =
    { from : Date
    , to : Date
    }


durationFromTodoList : List Todo -> Duration
durationFromTodoList todos =
    let
        intMax =
            2147483647

        compareDates : (Int -> Int -> Bool) -> Todo -> Date -> Date
        compareDates cmp next accum =
            if cmp (Date.toRataDie next.date) (Date.toRataDie accum) then
                next.date

            else
                accum

        latest =
            List.foldl (compareDates (>)) (Date.fromRataDie 0) todos

        earliest =
            List.foldl (compareDates (<)) (Date.fromRataDie intMax) todos
    in
    { from = earliest, to = latest }


durationFromTimeline : Timeline -> Duration
durationFromTimeline { todos } =
    durationFromTodoList todos


durationFromProject : Project -> Duration
durationFromProject project =
    let
        allTodos =
            List.foldl (\nextTimeline accum -> accum ++ nextTimeline.todos) [] project.timelines
    in
    durationFromTodoList allTodos


isIn : Duration -> Date -> Bool
isIn duration date =
    let
        startDay =
            Date.toRataDie duration.from

        endDay =
            Date.toRataDie duration.to

        day =
            Date.toRataDie date
    in
    startDay <= day && day <= endDay


type Tag
    = Tag String


type Person
    = Person String


type alias Todo =
    { title : String
    , date : Date
    , done : Bool
    , comment : Maybe String
    }


type alias Timeline =
    { people : List Person
    , tags : List Tag
    , todos : List Todo
    , comment : Maybe String
    }


type alias Project =
    { timelines : List Timeline
    , color : String
    , title : String
    , start : Date
    , end : Date
    , people : List Person
    , comment : Maybe String
    }


type Page
    = WeekView Date
    | MonthView Date
    | CalendarView Date
    | TodoEditor Todo
    | TimelineEditor Timeline
    | ProjectEditor Project


type alias Model =
    { page : Page
    , projects : List Project
    , today : Date
    , displayPeriod : Duration
    }


type alias ActivePage =
    { week : Bool, month : Bool, calendar : Bool, todoEditor : Bool, timelineEditor : Bool, projectEditor : Bool }


type Msg
    = SetActivePage Page
    | SetToday Date


type ViewType
    = Week
    | Month


getInitialDuration : ViewType -> Date -> Duration
getInitialDuration vt day =
    let
        rd =
            Date.toRataDie day
    in
    case vt of
        Week ->
            { from = Date.fromRataDie <| (rd - 7), to = Date.fromRataDie <| rd + 14 }

        Month ->
            { from = Date.fromRataDie <| rd - 30, to = Date.fromRataDie <| rd + 60 }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ page } as model) =
    case ( msg, page ) of
        ( SetActivePage page_, _ ) ->
            case page_ of
                WeekView date ->
                    ( { model | page = page_, displayPeriod = getInitialDuration Week model.today }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        ( SetToday date, WeekView _ ) ->
            ( { model | today = date, displayPeriod = getInitialDuration Week date }, Cmd.none )

        ( SetToday date, _ ) ->
            ( { model | today = date, displayPeriod = getInitialDuration Month date }, Cmd.none )
