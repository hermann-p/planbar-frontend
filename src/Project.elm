module Project exposing (..)

import Date exposing (Date)
import Dict exposing (update)
import Html exposing (Html, text)


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
    , editorState : EditorState
    , dirty : Bool
    }


listLast : List a -> Maybe a
listLast list =
    case list of
        [] ->
            Nothing

        x :: [] ->
            Just x

        _ :: xs ->
            listLast xs


getNewId : List { a | id : number } -> number
getNewId items =
    listLast items
        |> Maybe.map .id
        |> Maybe.map (\n -> n + 1)
        |> Maybe.withDefault 1


createProject : Model -> Project
createProject model =
    { id = getNewId model.projects
    , title = ""
    , color = "#dd0000"
    , comment = Nothing
    , start = model.today
    , end = model.today
    , timelines = []
    , people = []
    }


createTimeline : ProjectID -> Model -> Timeline
createTimeline parentId model =
    { id = getNewId model.timelines
    , parentProject = parentId
    , title = ""
    , comment = Nothing
    , people = []
    , tags = []
    , todos = []
    }


createTodo : TimelineID -> Model -> Todo
createTodo parentId model =
    { id = getNewId model.todos
    , parentTimeline = parentId
    , title = ""
    , comment = Nothing
    , date = model.today
    , done = False
    }


type alias ActivePage =
    { week : Bool, month : Bool, calendar : Bool, todoEditor : Bool, timelineEditor : Bool, projectEditor : Bool }


type EditorMsg
    = SetProject Project
    | SetTimeline Timeline
    | SetTodo Todo
    | EditProject Project
    | EditTimeline Timeline
    | EditTodo Todo
    | CloseEditor
    | Noop


type EditorState
    = EditorClosed
    | EditorTimeline TimelineID
    | EditorProject ProjectID
    | EditorTodo TodoID


type MainMsg
    = SetActivePage Page
    | SetToday Date
    | CreateProject
    | CreateTimeline ProjectID
    | CreateTodo TimelineID


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
    { model | projects = listUpdateWhere (\p -> p.id == Maybe.withDefault -1 id) f model.projects, dirty = True }


updateTimeline : Maybe TimelineID -> (Timeline -> Timeline) -> Model -> Model
updateTimeline id f model =
    { model | timelines = listUpdateWhere (\t -> t.id == Maybe.withDefault -1 id) f model.timelines, dirty = True }


updateTodo : Maybe TodoID -> (Todo -> Todo) -> Model -> Model
updateTodo id f model =
    { model | todos = listUpdateWhere (\t -> t.id == Maybe.withDefault -1 id) f model.todos, dirty = True }


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

                ( CreateProject, _ ) ->
                    let
                        project =
                            createProject model

                        timeline =
                            createTimeline project.id { model | projects = List.append model.projects [ project ] }
                    in
                    update
                        (ProjectMsg <| EditTimeline timeline)
                        { model
                            | projects = List.append model.projects [ project ]
                            , timelines = List.append model.timelines [ timeline ]
                        }

                ( CreateTimeline parentId, _ ) ->
                    let
                        timeline =
                            createTimeline parentId model
                    in
                    update
                        (ProjectMsg <| EditTimeline timeline)
                        { model | timelines = List.append model.timelines [ timeline ] }

                ( CreateTodo parentTimelineId, _ ) ->
                    let
                        todo =
                            createTodo parentTimelineId model
                    in
                    update
                        (ProjectMsg <| EditTodo todo)
                        { model | todos = List.append model.todos [ todo ] }

        ProjectMsg projectMsg ->
            updateEditor projectMsg model


updateEditor : EditorMsg -> Model -> ( Model, Cmd Msg )
updateEditor msg model =
    case msg of
        Noop ->
            ( model, Cmd.none )

        CloseEditor ->
            ( { model | editorState = EditorClosed }, Cmd.none )

        EditProject project ->
            ( { model | editorState = EditorProject project.id }
            , Cmd.none
            )

        EditTimeline timeline ->
            ( { model | editorState = EditorTimeline timeline.id }
            , Cmd.none
            )

        EditTodo todo ->
            ( { model | editorState = EditorTodo todo.id }
            , Cmd.none
            )

        SetProject project ->
            ( updateProject (Just project.id) (\_ -> project) model, Cmd.none )

        SetTimeline timeline ->
            ( updateTimeline (Just timeline.id) (\_ -> timeline) model, Cmd.none )

        SetTodo todo ->
            ( updateTodo (Just todo.id) (\_ -> todo) model, Cmd.none )


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
    case model.editorState of
        EditorProject id ->
            getProject id model

        _ ->
            Nothing


getTimeline : TimelineID -> Model -> Maybe Timeline
getTimeline id model =
    listFind (\tl -> tl.id == id) model.timelines


getSelectedTimeline : Model -> Maybe Timeline
getSelectedTimeline model =
    case model.editorState of
        EditorTimeline id ->
            getTimeline id model

        _ ->
            Nothing


getParentProject : Timeline -> Model -> Maybe Project
getParentProject timeline model =
    getProject timeline.parentProject model


getTodo : TodoID -> Model -> Maybe Todo
getTodo id model =
    listFind (\t -> t.id == id) model.todos


getSelectedTodo : Model -> Maybe Todo
getSelectedTodo model =
    case model.editorState of
        EditorTodo id ->
            getTodo id model

        _ ->
            Nothing


getParentTimeline : Todo -> Model -> Maybe Timeline
getParentTimeline todo model =
    getTimeline todo.parentTimeline model
