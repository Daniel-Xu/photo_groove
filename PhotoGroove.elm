module PhotoGroove exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.App as App
import Array exposing (Array)


-- model


type alias Photo =
    { url : String }


type alias Model =
    { photos : List Photo
    , selectedUrl : String
    }


initialModel : Model
initialModel =
    { photos =
        [ { url = "1.jpeg" }
        , { url = "2.jpeg" }
        , { url = "3.jpeg" }
        ]
    , selectedUrl = "1.jpeg"
    }


urlPrefix : String
urlPrefix =
    "http://elm-in-action.com/"


viewThumbnail : String -> Photo -> Html Msg
viewThumbnail selectedUrl photo =
    img
        [ src (urlPrefix ++ photo.url)
        , classList [ ( "selected", selectedUrl == photo.url ) ]
        , onClick (SelectPhoto photo.url)
        ]
        []


photoArray : Array Photo
photoArray =
    Array.fromList initialModel.photos



-- update


type Msg
    = SelectPhoto String
    | Surprise String


update : Msg -> Model -> Model
update msg model =
    case msg of
        SelectPhoto url ->
            { model | selectedUrl = url }

        Surprise url ->
            { model | selectedUrl = url }



-- view


view : Model -> Html Msg
view model =
    div [ class "content" ]
        [ h1 [] [ text "Photo Groove" ]
        , button [ onClick (Surprise "2.jpeg") ] [ text "Suprise Me" ]
        , div
            [ id "tumbnails" ]
            (List.map (viewThumbnail model.selectedUrl) initialModel.photos)
        , img
            [ class "large"
            , src (urlPrefix ++ "large/" ++ model.selectedUrl)
            ]
            []
        ]


main : Program Never
main =
    App.beginnerProgram
        { model = initialModel
        , view = view
        , update = update
        }
