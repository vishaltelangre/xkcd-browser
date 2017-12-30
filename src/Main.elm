module Main exposing (..)

import Html exposing (Html, text, div, img, h2, span, button, ul, li, label, pre, a)
import Html.Attributes exposing (src, class, title, href, target)
import Html.Events exposing (onClick)
import Http
import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Random


---- MODEL ----


type ComicQuery
    = Latest
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
    { seed : Int, latest : Comic, requested : Comic }


initCommic : Comic
initCommic =
    Comic 0 "" "" "" "" "" "" "" "" "" ""


init : Int -> ( Model, Cmd Msg )
init seed =
    { seed = seed
    , latest = initCommic
    , requested = initCommic
    }
        ! [ getComic Latest ]


comicApiUrl : ComicQuery -> String
comicApiUrl comicQuery =
    case comicQuery of
        Latest ->
            "https://xkcd.com/info.0.json"

        WithNumber number ->
            "https://xkcd.com/" ++ (toString number) ++ "/info.0.json"


comicUrl : Int -> String
comicUrl comicNumber =
    "https://xkcd.com/" ++ (toString comicNumber)


comicExplainUrl : Int -> String
comicExplainUrl comicNumber =
    "http://www.explainxkcd.com/wiki/index.php/" ++ (toString comicNumber)



---- UPDATE ----


type Msg
    = ComicLoaded ComicQuery (Result Http.Error Comic)
    | LoadRequestedComic Int
    | GenerateRandomComicNumber


getComic : ComicQuery -> Cmd Msg
getComic comicQuery =
    let
        url =
            "https://cors.io?" ++ (comicApiUrl comicQuery)
    in
        Http.get url comicDecoder |> Http.send (ComicLoaded comicQuery)


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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GenerateRandomComicNumber ->
            let
                generator =
                    Random.int 1 model.latest.number
            in
                model ! [ Random.generate LoadRequestedComic generator ]

        ComicLoaded comicQuery (Ok comic) ->
            case comicQuery of
                Latest ->
                    { model | latest = comic, requested = comic } ! []

                WithNumber _ ->
                    { model | requested = comic } ! []

        ComicLoaded _ (Err error) ->
            let
                _ =
                    Debug.log "Decoding error: " error
            in
                model ! []

        LoadRequestedComic number ->
            model ! [ WithNumber number |> getComic ]



---- VIEW ----


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
        previousButton =
            if currentComicNumber == 1 then
                text ""
            else
                button [ onClick (LoadRequestedComic (currentComicNumber - 1)) ]
                    [ text "Previous" ]

        nextButton =
            if currentComicNumber == totalComicCount then
                text ""
            else
                button [ onClick (LoadRequestedComic (currentComicNumber + 1)) ]
                    [ text "Next" ]
    in
        div [ class "navigation" ]
            [ previousButton
            , button [ onClick GenerateRandomComicNumber ] [ text "Random" ]
            , nextButton
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


view : Model -> Html Msg
view { latest, requested } =
    div []
        [ viewHeading requested
        , viewNavigation requested.number latest.number
        , viewImage requested
        , viewMeta requested
        ]



---- PROGRAM ----


main : Program Int Model Msg
main =
    Html.programWithFlags
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
