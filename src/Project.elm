module Project exposing (..)

import Date exposing (Date)
import Dict exposing (update)
import Html exposing (text)
import Monocle.Lens exposing (Lens)


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
    { id : Int
    , parentTimeline : Int
    , title : String
    , date : Date
    , done : Bool
    , comment : Maybe String
    }


type alias Timeline =
    { id : Int
    , parentProject : Int
    , people : List Person
    , tags : List Tag
    , todos : List Todo
    , comment : Maybe String
    , title : String
    }


type alias Project =
    { id : Int
    , timelines : List Timeline
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
    | EditTimeline (Maybe Timeline)
    | EditTodo (Maybe Todo)
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
    | Batch (List Msg)


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
        Batch msgs ->
            msgs
                |> List.foldl
                    (\nxt ( accum, cmds ) ->
                        let
                            ( state, cmd ) =
                                update nxt accum
                        in
                        ( state, cmd :: cmds )
                    )
                    ( model, [] )
                |> Tuple.mapSecond Cmd.batch

        MainMsg mainMsg ->
            case ( mainMsg, page ) of
                ( SetActivePage page_, _ ) ->
                    case page_ of
                        WeekView date ->
                            ( { model | page = page_, displayPeriod = getInitialDuration Week date }, Cmd.none )

                        _ ->
                            ( model, Cmd.none )

                ( SetToday date, WeekView _ ) ->
                    ( { model | today = date, displayPeriod = getInitialDuration Week date }, Cmd.none )

                ( SetToday date, _ ) ->
                    ( { model | today = date, displayPeriod = getInitialDuration Month date }, Cmd.none )

        ProjectMsg projectMsg ->
            updateEditor projectMsg model


updateEditor : EditorMsg -> Model -> ( Model, Cmd Msg )
updateEditor msg ({ editorState } as model) =
    let
        { project, timeline, todo } =
            editorState

        editorLens =
            Lens .editorState (\v m -> { m | editorState = v })

        getFirstTimeline : Maybe Project -> Maybe Timeline
        getFirstTimeline prj =
            prj
                |> Maybe.andThen (\p -> Just p.timelines)
                |> Maybe.andThen List.head

        getFirstTodo : Maybe Timeline -> Maybe Todo
        getFirstTodo t =
            t
                |> Maybe.andThen (\td -> Just td.todos)
                |> Maybe.andThen List.head
    in
    case msg of
        Noop ->
            ( model, Cmd.none )

        EditProject prj ->
            let
                tl =
                    getFirstTimeline prj
            in
            ( editorLens.set { editorState | project = prj, timeline = tl, todo = getFirstTodo tl } model, Cmd.none )

        EditTimeline tl ->
            ( editorLens.set { editorState | timeline = tl, todo = getFirstTodo tl } model, Cmd.none )

        EditTodo t ->
            ( editorLens.set { editorState | todo = t } model, Cmd.none )

        SetProjectColor color ->
            ( editorLens.set { editorState | project = Maybe.andThen (\p -> Just { p | color = color }) project } model, Cmd.none )

        SetProjectComment comment ->
            ( editorLens.set
                { editorState
                    | project = Maybe.andThen (\p -> Just { p | comment = comment }) project
                }
                model
            , Cmd.none
            )

        SetProjectEnd date ->
            ( editorLens.set { editorState | project = Maybe.andThen (\p -> Just { p | end = date }) project } model, Cmd.none )

        SetProjectPeople people ->
            ( editorLens.set { editorState | project = Maybe.andThen (\p -> Just { p | people = people }) project } model, Cmd.none )

        SetProjectStart date ->
            ( editorLens.set { editorState | project = Maybe.andThen (\p -> Just { p | start = date }) project } model, Cmd.none )

        SetProjectTimelines timelines ->
            ( editorLens.set { editorState | project = Maybe.andThen (\p -> Just { p | timelines = timelines }) project } model, Cmd.none )

        SetProjectTitle text ->
            ( editorLens.set { editorState | project = Maybe.andThen (\p -> Just { p | title = text }) project } model, Cmd.none )


noHtml =
    text ""


listFind : (a -> Bool) -> List a -> Maybe a
listFind pred list =
    list
        |> List.filter pred
        |> List.head


getProject : List Project -> Int -> Maybe Project
getProject projects id =
    listFind (\p -> p.id == id) projects


getTimeline : List Project -> Int -> Maybe Timeline
getTimeline projects id =
    projects
        |> List.concatMap .timelines
        |> listFind (\tl -> tl.id == id)


getParentProject : List Project -> Timeline -> Maybe Project
getParentProject projects timeline =
    getProject projects timeline.parentProject


getTodo : List Project -> Int -> Maybe Todo
getTodo projects id =
    projects
        |> List.concatMap .timelines
        |> List.concatMap .todos
        |> listFind (\t -> t.id == id)


getParentTimeline : List Project -> Todo -> Maybe Timeline
getParentTimeline projects todo =
    getTimeline projects todo.parentTimeline
