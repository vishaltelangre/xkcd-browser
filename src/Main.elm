module Main exposing (..)

import Html exposing (Html, text, div, img, h2, span, ul, li, label, pre, a, br, code)
import Html.Attributes exposing (src, class, title, href, target)
import Http
import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Keyboard
import Navigation exposing (Location)
import Random
import RemoteData exposing (WebData)
import UrlParser exposing (Parser, (</>), s, parseHash)


---- ROUTING ----


type Route
    = LatestComicRoute
    | RandomComicRoute
    | ComicRoute Int
    | NotFoundRoute


matchers : Parser (Route -> a) a
matchers =
    UrlParser.oneOf
        [ UrlParser.map LatestComicRoute UrlParser.top
        , UrlParser.map RandomComicRoute (s "comics" </> s "random")
        , UrlParser.map ComicRoute (s "comics" </> UrlParser.int)
        ]


parseLocation : Location -> Route
parseLocation location =
    case (parseHash matchers location) of
        Just route ->
            route

        Nothing ->
            NotFoundRoute



---- MODEL ----


type ComicQuery
    = Latest
    | LatestAndRequested
    | Random
    | WithNumber Int


type alias Comic =
    { number : Int
    , alternateText : String
    , title : String
    , safeTitle : String
    , day : String
    , month : String
    , year : String
    , imageLink : String
    , link : String
    , news : String
    , transcript : String
    }


type alias ComicPair =
    { latest : Comic
    , requested : Comic
    }


getComicPair : Model -> WebData ComicPair
getComicPair model =
    RemoteData.map2 ComicPair model.latest model.requested


type alias Model =
    { seed : Int
    , spinnerPath : String
    , route : Route
    , latest : WebData Comic
    , requested : WebData Comic
    }


initialModel : Int -> String -> Route -> Model
initialModel seed spinnerPath route =
    { seed = seed
    , spinnerPath = spinnerPath
    , route = route
    , latest = RemoteData.Loading
    , requested = RemoteData.Loading
    }


init : ProgramFlags -> Location -> ( Model, Cmd Msg )
init { seed, spinnerPath } location =
    let
        currentRoute =
            parseLocation location

        model =
            (initialModel seed spinnerPath currentRoute)
    in
        model ! [ routeChangeCmd currentRoute model ]


comicApiUrl : ComicQuery -> String
comicApiUrl comicQuery =
    case comicQuery of
        WithNumber number ->
            "/api/" ++ (toString number) ++ "/info.0.json"

        _ ->
            "/api/info.0.json"


comicUrl : Int -> String
comicUrl comicNumber =
    "https://xkcd.com/" ++ (toString comicNumber)


comicExplainUrl : Int -> String
comicExplainUrl comicNumber =
    "http://www.explainxkcd.com/wiki/index.php/" ++ (toString comicNumber)


comicPath : ComicQuery -> String
comicPath comicQuery =
    case comicQuery of
        WithNumber number ->
            "#/comics/" ++ (toString number)

        Random ->
            "#/comics/random"

        _ ->
            "#"



---- UPDATE ----


type Msg
    = OnComicLoad ComicQuery (WebData Comic)
    | LoadRequestedComic ComicQuery
    | OnLocationChange Location
    | Noop


fetchComic : ComicQuery -> Cmd Msg
fetchComic comicQuery =
    let
        url =
            comicApiUrl comicQuery
    in
        Http.get url comicDecoder |> RemoteData.sendRequest |> Cmd.map (OnComicLoad comicQuery)


comicDecoder : Decoder Comic
comicDecoder =
    decode Comic
        |> required "num" int
        |> optional "alt" string ""
        |> optional "title" string ""
        |> required "safe_title" string
        |> optional "day" string ""
        |> optional "month" string ""
        |> optional "year" string ""
        |> required "img" string
        |> optional "link" string ""
        |> optional "news" string ""
        |> optional "transcript" string ""


routeChangeCmd : Route -> Model -> Cmd Msg
routeChangeCmd route { latest } =
    case ( route, latest ) of
        ( ComicRoute comicNumber, RemoteData.Loading ) ->
            Cmd.batch [ fetchComic Latest, fetchComic (WithNumber comicNumber) ]

        ( ComicRoute comicNumber, _ ) ->
            fetchComic (WithNumber comicNumber)

        ( LatestComicRoute, _ ) ->
            fetchComic LatestAndRequested

        ( RandomComicRoute, RemoteData.Success comic ) ->
            Random.int 1 comic.number |> Random.generate (LoadRequestedComic << WithNumber)

        ( RandomComicRoute, RemoteData.Failure _ ) ->
            Navigation.modifyUrl (comicPath LatestAndRequested)

        ( RandomComicRoute, RemoteData.Loading ) ->
            fetchComic Random

        ( _, _ ) ->
            Cmd.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnComicLoad Latest response ->
            { model | latest = response } ! []

        OnComicLoad LatestAndRequested response ->
            { model | latest = response, requested = response } ! []

        OnComicLoad Random response ->
            { model | latest = response } ! [ routeChangeCmd model.route model ]

        OnComicLoad (WithNumber _) response ->
            { model | requested = response } ! []

        LoadRequestedComic comic ->
            model ! [ comicPath comic |> Navigation.modifyUrl ]

        OnLocationChange location ->
            let
                newRoute =
                    parseLocation location

                newModel =
                    { model | route = newRoute, requested = RemoteData.Loading }
            in
                case newRoute of
                    LatestComicRoute ->
                        if newModel.latest == RemoteData.Loading then
                            newModel ! [ routeChangeCmd newRoute newModel ]
                        else
                            { newModel | requested = newModel.latest } ! []

                    _ ->
                        newModel ! [ routeChangeCmd newRoute newModel ]

        Noop ->
            model ! []



---- VIEW ----


viewLoadingSpinner : String -> Html Msg
viewLoadingSpinner spinnerPath =
    img [ src spinnerPath ] []


viewHeading : Comic -> Html Msg
viewHeading { number, safeTitle } =
    div [ class "heading" ]
        [ h2 []
            [ span [ class "number" ] [ text ("#" ++ toString (number)) ]
            , span [ class "title" ] [ text safeTitle ]
            ]
        ]


isFirstComic : ComicPair -> Bool
isFirstComic { requested } =
    requested.number == 1


isLatestComic : ComicPair -> Bool
isLatestComic { requested, latest } =
    requested.number == latest.number


viewNavigation : Maybe ComicPair -> Html Msg
viewNavigation maybeComicPair =
    let
        navLink name shortcutIcon comicQuery =
            a [ comicPath comicQuery |> href, class "navlink" ]
                [ span [] [ text name ]
                , br [] []
                , code [] [ text shortcutIcon ]
                ]

        arrowLink shouldHideFunc name shortcutIcon incrementByNumber =
            Maybe.withDefault (text "") <|
                flip Maybe.map maybeComicPair <|
                    \({ requested } as comicPair) ->
                        if shouldHideFunc comicPair then
                            text ""
                        else
                            (+) requested.number incrementByNumber
                                |> WithNumber
                                |> navLink name shortcutIcon
    in
        div [ class "navigation" ]
            [ arrowLink isFirstComic "Previous" "←" -1
            , navLink "Latest" "L" LatestAndRequested
            , navLink "Random" "R" Random
            , arrowLink isLatestComic "Next" "→" 1
            ]


viewImage : Comic -> Html Msg
viewImage { imageLink, alternateText } =
    img [ src imageLink, title alternateText ] []


viewMeta : Comic -> Html Msg
viewMeta { number, alternateText, month, day, year, transcript, news } =
    let
        viewMetaItem label_ value =
            if String.isEmpty value then
                text ""
            else
                li []
                    [ label [] [ text label_ ]
                    , pre [] [ text value ]
                    ]
    in
        div [ class "meta" ]
            [ ul []
                [ li []
                    [ label [] [ text "Title Text:" ]
                    , span [] [ text alternateText ]
                    ]
                , li []
                    [ label [] [ text "Published on:" ]
                    , span [] [ text (month ++ "/" ++ day ++ "/" ++ year) ]
                    ]
                , li []
                    [ label [] [ text "XKCD Link:" ]
                    , a [ href (comicUrl number), target "_blank" ]
                        [ text (comicUrl number) ]
                    ]
                , li []
                    [ label [] [ text "Explain XKCD Link:" ]
                    , a [ href (comicExplainUrl number), target "_blank" ]
                        [ text (comicExplainUrl number) ]
                    ]
                , viewMetaItem "Transcript:" transcript
                , viewMetaItem "News:" news
                ]
            ]


viewComic : Model -> Html Msg
viewComic model =
    case getComicPair model of
        RemoteData.NotAsked ->
            viewError ""

        RemoteData.Loading ->
            h2 [] [ viewLoadingSpinner model.spinnerPath ]

        RemoteData.Success ({ latest, requested } as comicPair) ->
            div []
                [ viewHeading requested
                , viewNavigation <| Just comicPair
                , viewImage requested
                , viewMeta requested
                ]

        RemoteData.Failure error ->
            viewError (toString error)


viewError : String -> Html Msg
viewError errorMessage =
    div [] [ h2 [] [ text errorMessage ], viewNavigation Nothing ]


view : Model -> Html Msg
view model =
    case model.route of
        LatestComicRoute ->
            viewComic model

        ComicRoute comicNumber ->
            viewComic model

        RandomComicRoute ->
            viewLoadingSpinner model.spinnerPath

        NotFoundRoute ->
            viewError "Not Found"



---- SUBSCRIPTIONS ----


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        msgForKeyCode keyCode =
            case ( keyCode, getComicPair model ) of
                -- Left arrow
                ( 37, RemoteData.Success ({ requested } as comicPair) ) ->
                    if isFirstComic comicPair then
                        Noop
                    else
                        LoadRequestedComic <| WithNumber <| requested.number - 1

                -- Right arrow
                ( 39, RemoteData.Success ({ requested } as comicPair) ) ->
                    if isLatestComic comicPair then
                        Noop
                    else
                        LoadRequestedComic <| WithNumber <| requested.number + 1

                -- L key
                ( 76, _ ) ->
                    LoadRequestedComic <| LatestAndRequested

                -- R key
                ( 82, _ ) ->
                    LoadRequestedComic <| Random

                _ ->
                    Noop
    in
        Keyboard.downs msgForKeyCode



---- PROGRAM ----


type alias ProgramFlags =
    { seed : Int, spinnerPath : String }


main : Program ProgramFlags Model Msg
main =
    Navigation.programWithFlags OnLocationChange
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }
