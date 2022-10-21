module Project exposing (..)

import Date exposing (Date)
import Dict exposing (update)
import Html exposing (Html, text)
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


type alias TodoID =
    Int


type alias Todo =
    { id : TodoID
    , parentTimeline : TimelineID
    , title : String
    , date : Date
    , done : Bool
    , comment : Maybe String
    }


type alias TimelineID =
    Int


type alias Timeline =
    { id : TimelineID
    , parentProject : ProjectID
    , people : List Person
    , tags : List Tag
    , todos : List Todo
    , comment : Maybe String
    , title : String
    }


type alias ProjectID =
    Int


type alias Project =
    { id : ProjectID
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
    , timelines : List Timeline
    , todos : List Todo
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
    | SetProjectComment (Maybe String)
    | EditProject (Maybe Project)
    | EditTimeline (Maybe Timeline)
    | EditTodo (Maybe Todo)
    | Noop


type alias EditorModel =
    { projectID : Maybe ProjectID
    , timelineID : Maybe TimelineID
    , todoID : Maybe TodoID
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


listUpdateWhere : (a -> Bool) -> (a -> a) -> List a -> List a
listUpdateWhere pred f list =
    List.map
        (\item ->
            if pred item then
                f item

            else
                item
        )
        list


updateProject : Maybe ProjectID -> (Project -> Project) -> Model -> Model
updateProject id f model =
    { model | projects = listUpdateWhere (\p -> p.id == Maybe.withDefault -1 id) f model.projects }


updateTimeline : Maybe TimelineID -> (Timeline -> Timeline) -> Model -> Model
updateTimeline id f model =
    { model | timelines = listUpdateWhere (\t -> t.id == Maybe.withDefault -1 id) f model.timelines }


updateTodo : Maybe TodoID -> (Todo -> Todo) -> Model -> Model
updateTodo id f model =
    { model | todos = listUpdateWhere (\t -> t.id == Maybe.withDefault -1 id) f model.todos }


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
        { projectID, timelineID, todoID } =
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
            ( editorLens.set
                { editorState
                    | projectID = Maybe.map .id prj
                    , timelineID = Maybe.map .id tl
                    , todoID = Maybe.map .id <| getFirstTodo tl
                }
                model
            , Cmd.none
            )

        EditTimeline tl ->
            ( editorLens.set
                { editorState
                    | timelineID = Maybe.map .id tl
                    , todoID = Maybe.map .id <| getFirstTodo tl
                }
                model
            , Cmd.none
            )

        EditTodo t ->
            ( editorLens.set { editorState | todoID = Maybe.map .id t } model, Cmd.none )

        SetProjectColor color ->
            ( updateProject projectID (\p -> { p | color = color }) model, Cmd.none )

        SetProjectComment comment ->
            ( updateProject projectID (\p -> { p | comment = comment }) model, Cmd.none )

        SetProjectEnd date ->
            ( updateProject projectID (\p -> { p | end = date }) model, Cmd.none )

        SetProjectPeople people ->
            ( updateProject projectID (\p -> { p | people = people }) model, Cmd.none )

        SetProjectStart date ->
            ( updateProject projectID (\p -> { p | start = date }) model, Cmd.none )

        SetProjectTitle title ->
            ( updateProject projectID (\p -> { p | title = title }) model, Cmd.none )


noHtml : Html msg
noHtml =
    text ""


listFind : (a -> Bool) -> List a -> Maybe a
listFind pred list =
    list
        |> List.filter pred
        |> List.head


getProject : ProjectID -> Model -> Maybe Project
getProject id model =
    listFind (\p -> p.id == id) model.projects


getSelectedProject : Model -> Maybe Project
getSelectedProject model =
    getProject (Maybe.withDefault -1 model.editorState.projectID) model


getTimeline : TimelineID -> Model -> Maybe Timeline
getTimeline id model =
    listFind (\tl -> tl.id == id) model.timelines


getSelectedTimeline : Model -> Maybe Timeline
getSelectedTimeline model =
    getTimeline (Maybe.withDefault -1 model.editorState.timelineID) model


getParentProject : Timeline -> Model -> Maybe Project
getParentProject timeline model =
    getProject timeline.parentProject model


getTodo : TodoID -> Model -> Maybe Todo
getTodo id model =
    listFind (\t -> t.id == id) model.todos


getSelectedTodo : Model -> Maybe Todo
getSelectedTodo model =
    getTodo (Maybe.withDefault -1 model.editorState.todoID) model


getParentTimeline : Todo -> Model -> Maybe Timeline
getParentTimeline todo model =
    getTimeline todo.parentTimeline model
