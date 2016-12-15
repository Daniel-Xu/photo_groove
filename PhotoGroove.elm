module PhotoGroove exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Array exposing (Array)
import Random


-- model


type alias Photo =
    { url : String }


type alias Model =
    { photos : List Photo
    , selectedUrl : Maybe String
    , loadingError : Maybe String
    , chosenSize : ThumbnailSize
    }


type ThumbnailSize
    = Small
    | Medium
    | Large


initialModel : Model
initialModel =
    { photos = []
    , selectedUrl = Nothing
    , loadingError = Nothing
    , chosenSize = Large
    }


urlPrefix : String
urlPrefix =
    "http://elm-in-action.com/"


viewThumbnail : Maybe String -> Photo -> Html Msg
viewThumbnail selectedUrl photo =
    img
        [ src (urlPrefix ++ photo.url)
        , classList [ ( "selected", selectedUrl == Just photo.url ) ]
        , onClick (SelectPhoto photo.url)
        ]
        []



-- update


type Msg
    = SelectPhoto String
    | Surprise
    | ChooseSize ThumbnailSize
    | SelectByIndex Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SelectPhoto url ->
            ( { model | selectedUrl = Just url }, Cmd.none )

        Surprise ->
            let
                randomPhotoPicker =
                    Random.int 0 (List.length model.photos - 1)
            in
                ( model, Random.generate SelectByIndex randomPhotoPicker )

        ChooseSize size ->
            ( { model | chosenSize = size }, Cmd.none )

        SelectByIndex index ->
            let
                newSelectedUrl : Maybe String
                newSelectedUrl =
                    model.photos
                        |> Array.fromList
                        |> Array.get index
                        |> Maybe.map .url
            in
                ( { model | selectedUrl = newSelectedUrl }, Cmd.none )



-- view


sizeToString : ThumbnailSize -> String
sizeToString size =
    case size of
        Small ->
            "small"

        Medium ->
            "medium"

        Large ->
            "large"


sizeToClass : ThumbnailSize -> String
sizeToClass size =
    case size of
        Small ->
            "small"

        Medium ->
            "med"

        Large ->
            "large"


viewSizeChooser : ThumbnailSize -> Html Msg
viewSizeChooser size =
    label []
        [ input
            [ type_ "radio"
            , name "size"
            , onClick (ChooseSize size)
            ]
            []
        , text (sizeToString size)
        ]


view : Model -> Html Msg
view model =
    div [ class "content" ]
        [ h1 [] [ text "Photo Groove" ]
        , button [ onClick Surprise ] [ text "Surprise Me" ]
        , h3 [] [ text "Thumbnail Size: " ]
        , div [ id "choose-size" ]
            (List.map viewSizeChooser [ Small, Medium, Large ])
        , div
            [ id "thumbnails", class (sizeToClass model.chosenSize) ]
            (List.map (viewThumbnail model.selectedUrl) model.photos)
        , viewLarge model.selectedUrl
        ]


viewLarge : Maybe String -> Html Msg
viewLarge maybeUrl =
    case maybeUrl of
        Nothing ->
            text ""

        Just url ->
            img
                [ class "large", src (urlPrefix ++ "large/" ++ url) ]
                []


main : Program Never Model Msg
main =
    Html.program
        { init = ( initialModel, Cmd.none )
        , view = view
        , update = update
        , subscriptions = (\model -> Sub.none)
        }
