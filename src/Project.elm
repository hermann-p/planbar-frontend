module Project exposing (..)

import Date exposing (Date)
import Html exposing (text)


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
    , title : String
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
    , editorState : EditorModel
    }


type alias ActivePage =
    { week : Bool, month : Bool, calendar : Bool, todoEditor : Bool, timelineEditor : Bool, projectEditor : Bool }


type EditorMsg
    = SetProjectTitle String
    | SetProjectColor String
    | SetProjectStart Date
    | SetProjectEnd Date
    | SetProjectPeople (List Person)
    | SetProjectTimelines (List Timeline)
    | SetProjectComment (Maybe String)
    | EditProject (Maybe Project)
    | Noop


type alias EditorModel =
    { project : Maybe Project
    , timeline : Maybe Timeline
    , todo : Maybe Todo
    }


type MainMsg
    = SetActivePage Page
    | SetToday Date


type Msg
    = ProjectMsg EditorMsg
    | MainMsg MainMsg


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
    case msg of
        MainMsg mainMsg ->
            case ( mainMsg, page ) of
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

        ProjectMsg projectMsg ->
            ( { model | editorState = updateEditor projectMsg model.editorState }, Cmd.none )


updateEditor : EditorMsg -> EditorModel -> EditorModel
updateEditor msg ({ project } as model) =
    case ( project, msg ) of
        ( _, Noop ) ->
            model

        ( _, EditProject prj ) ->
            { model | project = prj }

        ( Nothing, _ ) ->
            model

        ( Just prj, SetProjectColor color ) ->
            { model | project = Just { prj | color = color } }

        ( Just prj, SetProjectComment comment ) ->
            { model
                | project =
                    Just
                        { prj
                            | comment = comment
                        }
            }

        ( Just prj, SetProjectEnd date ) ->
            { model | project = Just { prj | end = date } }

        ( Just prj, SetProjectPeople people ) ->
            { model | project = Just { prj | people = people } }

        ( Just prj, SetProjectStart date ) ->
            { model | project = Just { prj | start = date } }

        ( Just prj, SetProjectTimelines timelines ) ->
            { model | project = Just { prj | timelines = timelines } }

        ( Just prj, SetProjectTitle text ) ->
            { model | project = Just { prj | title = text } }


noHtml =
    text ""
