module Main exposing (..)

import Html exposing (Html, text, div, img, h2, span, button, ul, li, label, pre, a)
import Html.Attributes exposing (src, class, title, href, target)
import Html.Events exposing (onClick)
import Http
import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Navigation exposing (Location)
import Random
import RemoteData exposing (WebData)
import UrlParser exposing (Parser, (</>), s, parseHash)


---- ROUTING ----


type Route
    = LatestComicRoute
    | ComicRoute Int
    | NotFoundRoute


matchers : Parser (Route -> a) a
matchers =
    UrlParser.oneOf
        [ UrlParser.map LatestComicRoute UrlParser.top
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
            "https://xkcd.com/" ++ (toString number) ++ "/info.0.json"

        _ ->
            "https://xkcd.com/info.0.json"


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

        _ ->
            "#"



---- UPDATE ----


type Msg
    = OnComicLoad ComicQuery (WebData Comic)
    | LoadRequestedComic Int
    | GenerateRandomComicNumber
    | OnLocationChange Location


fetchComic : ComicQuery -> Cmd Msg
fetchComic comicQuery =
    let
        url =
            "https://cors.io?" ++ (comicApiUrl comicQuery)
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
    let
        latestComicFetchCmd =
            case latest of
                RemoteData.Loading ->
                    fetchComic Latest

                _ ->
                    Cmd.none
    in
        case route of
            ComicRoute comicNumber ->
                Cmd.batch
                    [ latestComicFetchCmd
                    , fetchComic (WithNumber comicNumber)
                    ]

            LatestComicRoute ->
                fetchComic LatestAndRequested

            _ ->
                Cmd.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GenerateRandomComicNumber ->
            let
                cmd =
                    case model.latest of
                        RemoteData.Success comic ->
                            Random.int 1 comic.number
                                |> Random.generate LoadRequestedComic

                        _ ->
                            Cmd.none
            in
                model ! [ cmd ]

        OnComicLoad comicQuery response ->
            case comicQuery of
                Latest ->
                    { model | latest = response } ! []

                LatestAndRequested ->
                    { model | latest = response, requested = response } ! []

                WithNumber _ ->
                    { model | requested = response } ! []

        LoadRequestedComic number ->
            model ! [ comicPath (WithNumber number) |> Navigation.newUrl ]

        OnLocationChange location ->
            let
                newRoute =
                    parseLocation location

                newModel =
                    { model | route = newRoute, requested = RemoteData.Loading }
            in
                newModel ! [ routeChangeCmd newRoute newModel ]



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


viewNavigation : Int -> Int -> Html Msg
viewNavigation currentComicNumber totalComicCount =
    let
        previousLink =
            if currentComicNumber == 1 then
                text ""
            else
                a [ href (comicPath (WithNumber (currentComicNumber - 1))) ]
                    [ text "Previous" ]

        nextLink =
            if currentComicNumber == totalComicCount then
                text ""
            else
                a [ href (comicPath (WithNumber (currentComicNumber + 1))) ]
                    [ text "Next" ]
    in
        div [ class "navigation" ]
            [ previousLink
            , a [ href (comicPath LatestAndRequested) ] [ text "Latest" ]
            , a [ href "javascript:void(0)", onClick GenerateRandomComicNumber ]
                [ text "Random" ]
            , nextLink
            ]


viewImage : Comic -> Html Msg
viewImage { imageLink, alternateText } =
    img [ src imageLink, title alternateText ] []


viewMeta : Comic -> Html Msg
viewMeta { number, month, day, year, transcript, news } =
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
viewComic { latest, requested, spinnerPath } =
    case requested of
        RemoteData.NotAsked ->
            text ""

        RemoteData.Loading ->
            h2 [] [ viewLoadingSpinner spinnerPath ]

        RemoteData.Success requestedComic ->
            case latest of
                RemoteData.Success latestComic ->
                    div []
                        [ viewHeading requestedComic
                        , viewNavigation requestedComic.number latestComic.number
                        , viewImage requestedComic
                        , viewMeta requestedComic
                        ]

                RemoteData.Failure error ->
                    text (toString error)

                _ ->
                    h2 [] [ viewLoadingSpinner spinnerPath ]

        RemoteData.Failure error ->
            text (toString error)


view : Model -> Html Msg
view model =
    case model.route of
        LatestComicRoute ->
            viewComic model

        ComicRoute comicNumber ->
            viewComic model

        NotFoundRoute ->
            h2 [] [ text "Not Found" ]



---- PROGRAM ----


type alias ProgramFlags =
    { seed : Int, spinnerPath : String }


main : Program ProgramFlags Model Msg
main =
    Navigation.programWithFlags OnLocationChange
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
