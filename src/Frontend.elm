module Frontend exposing (..)

import Browser exposing (UrlRequest(..))
import Browser.Dom as Dom
import Browser.Events
import Browser.Navigation as Nav
import Element exposing (..)
import Lamdera
import Mosaic exposing (Msg(..), Picture, defaultSizeConfig)
import Palette.Color exposing (..)
import Platform.Sub as Sub
import Task
import Types exposing (..)
import Url


app =
    Lamdera.frontend
        { init = init
        , onUrlRequest = UrlClicked
        , onUrlChange = UrlChanged
        , update = update
        , updateFromBackend = updateFromBackend
        , subscriptions = subscriptions
        , view = view
        }


init : Url.Url -> Nav.Key -> ( FrontendModel, Cmd FrontendMsg )
init _ key =
    ( { key = key
      , message = ""
      , maybeDevice = Nothing
      , deviceHeight = 0
      , deviceWidth = 0
      , mosaic = Mosaic.init defaultSizeConfig pictures
      }
    , Dom.getViewport
        |> Task.perform (\{ viewport } -> GotNewSize { width = round viewport.width, height = round viewport.height })
    )


update : FrontendMsg -> FrontendModel -> ( FrontendModel, Cmd FrontendMsg )
update msg model =
    case msg of
        UrlClicked urlRequest ->
            case urlRequest of
                Internal url ->
                    ( model
                    , Nav.pushUrl model.key (Url.toString url)
                    )

                External url ->
                    ( model
                    , Nav.load url
                    )

        UrlChanged _ ->
            ( model, Cmd.none )

        NoOpFrontendMsg ->
            ( model, Cmd.none )

        GotNewSize viewPort ->
            ( { model
                | maybeDevice = Just <| classifyDevice viewPort
                , deviceHeight = viewPort.height
                , deviceWidth = viewPort.width
                , mosaic =
                    model.mosaic
                        |> Mosaic.updateScreenSize { deviceHeight = viewPort.height, deviceWidth = viewPort.width }

                -- |> Mosaic.takeFullscreenSize
              }
            , Cmd.none
            )

        MosaicMsg subMsg ->
            ( { model | mosaic = Mosaic.update subMsg model.mosaic }, Cmd.none )


updateFromBackend : ToFrontend -> FrontendModel -> ( FrontendModel, Cmd FrontendMsg )
updateFromBackend msg model =
    case msg of
        NoOpToFrontend ->
            ( model, Cmd.none )


view : FrontendModel -> Browser.Document FrontendMsg
view model =
    { title = "NaunoKTM-elm-ui-mosaic"
    , body =
        [ layout
            [ width fill
            , height fill
            ]
          <|
            bodyView model
        ]
    }


bodyView : FrontendModel -> Element FrontendMsg
bodyView model =
    column
        [ width fill
        , height fill
        , inFront <| Element.map MosaicMsg <| Mosaic.displayModal model.mosaic
        ]
        [ column
            [ width fill
            , height fill
            , centerX
            , centerY
            , paddingEach { top = 32, bottom = 32, left = 32, right = 32 }
            ]
            [ Element.map MosaicMsg <|
                Mosaic.viewMosaic model.mosaic
            ]
        ]


subscriptions : FrontendModel -> Sub FrontendMsg
subscriptions model =
    Sub.batch
        [ Browser.Events.onResize (\w h -> GotNewSize { width = w, height = h })
        , Sub.map MosaicMsg <| Mosaic.subscriptions model.mosaic
        ]


pictures : List Picture
pictures =
    [ Picture "1.jpg" { width = 880, height = 609 }
    , Picture "2.jpg" { width = 1200, height = 800 }
    , Picture "3.jpg" { width = 845, height = 321 }
    , Picture "4.jpg" { width = 1080, height = 474 }
    , Picture "5.jpg" { width = 1024, height = 576 }
    , Picture "6.jpg" { width = 1080, height = 474 }
    , Picture "7.jpg" { width = 1024, height = 576 }
    , Picture "8.jpg" { width = 1280, height = 853 }
    ]
