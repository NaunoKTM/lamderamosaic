module Frontend exposing (..)

import Browser exposing (UrlRequest(..))
import Browser.Dom as Dom
import Browser.Events
import Browser.Navigation as Nav
import Element exposing (..)
import Element.Background as Background
import Element.Events exposing (onClick)
import Element.Font as Font
import Html.Attributes as HA
import Json.Decode as D
import Lamdera
import List
import List.Extra as List
import Mosaic exposing (DisplayConfig, KeyBoardKey, Modal(..), Msg(..), Picture, PictureSize)
import Palette.Color exposing (..)
import Platform.Sub as Sub
import Task
import Types exposing (..)
import Url
import Utils.Element exposing (edges)
import Utils.Input as Utils exposing (letClickThrough)


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
init url key =
    ( { key = key
      , message = ""
      , maybeDevice = Nothing
      , modal = Nothing
      , deviceHeight = 0
      , deviceWidth = 0
      , mosaic = Mosaic.init defaultSizeConfig firstList
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

        UrlChanged url ->
            ( model, Cmd.none )

        NoOpFrontendMsg ->
            ( model, Cmd.none )

        GotNewSize viewPort ->
            ( { model
                | maybeDevice = Just <| classifyDevice viewPort
                , deviceHeight = viewPort.height
                , deviceWidth = viewPort.width
                , mosaic = model.mosaic |> Mosaic.updateScreenSize { deviceHeight = viewPort.height, deviceWidth = viewPort.width }
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
    { title = "Responsive mosaic with modal"
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
        , Sub.map MosaicMsg (Mosaic.subscriptions model.mosaic)
        ]


firstList : List Picture
firstList =
    [ Picture "1.jpg" { width = 880, height = 609 }
    , Picture "2.jpg" { width = 1200, height = 800 }
    , Picture "3.jpg" { width = 845, height = 321 }
    , Picture "4.jpg" { width = 1080, height = 474 }
    , Picture "5.jpg" { width = 1024, height = 576 }
    , Picture "6.jpg" { width = 1080, height = 474 }
    , Picture "7.jpg" { width = 1024, height = 576 }
    , Picture "8.jpg" { width = 1280, height = 853 }
    ]


getListFromIndex : Int -> List Picture
getListFromIndex index =
    case index of
        0 ->
            sortByHeight firstList

        _ ->
            []


onePicture : Int -> Int -> Picture -> Int -> Int -> Element FrontendMsg
onePicture blockWidth blockHeight picture listIndex id =
    let
        attrs =
            if toFloat picture.size.height / toFloat blockHeight < toFloat picture.size.width / toFloat blockWidth then
                [ height <| px blockHeight ]

            else
                [ width <| px blockWidth ]
    in
    el [ width <| px blockWidth, height <| px blockHeight, clip ] <|
        el
            [ pointer
            , htmlAttribute <| HA.id <| picture.id
            , centerX
            , centerY
            , clip
            ]
        <|
            image
                (clip :: attrs)
                { description = picture.id, src = picture.id }


defaultSizeConfig : DisplayConfig
defaultSizeConfig =
    { baseWidth = 350
    , baseHeight = 500
    , spacingSize = 8
    }


makeItComp : DisplayConfig -> DisplayConfig
makeItComp displayConfig =
    let
        x =
            displayConfig.spacingSize

        y =
            displayConfig.baseHeight

        n =
            ((y - 2 * x) + 5) // 6

        newY =
            max y (6 * n + 2 * x)
    in
    { displayConfig | baseHeight = newY }


displayMosaic : DisplayConfig -> List Picture -> Int -> Element FrontendMsg
displayMosaic config pictures listIndex =
    let
        fixConfig =
            makeItComp config

        displaySizeOfList list =
            el [ width fill, height fill, Background.color blackNonOpaque, letClickThrough ] <|
                el [ centerY, centerX, Font.color white, Font.size 32 ] <|
                    text <|
                        ("+" ++ String.fromInt (List.length list))

        sortedPictures =
            sortByHeight pictures

        calculateHeight : Float -> Float -> Float
        calculateHeight totalItems spacingCount =
            (toFloat fixConfig.baseHeight - (spacingCount * toFloat fixConfig.spacingSize)) / totalItems

        layoutPictures : List Picture -> Element FrontendMsg
        layoutPictures pics =
            case pics of
                [] ->
                    none

                [ picture ] ->
                    onePicture fixConfig.baseWidth fixConfig.baseHeight picture listIndex 0

                [ picture1, picture2 ] ->
                    let
                        w =
                            round (toFloat fixConfig.baseWidth * 0.5)

                        h =
                            round (calculateHeight 2 1)
                    in
                    row [ spacing fixConfig.spacingSize ]
                        [ onePicture w h picture1 listIndex 0
                        , onePicture w h picture2 listIndex 1
                        ]

                [ picture1, picture2, picture3 ] ->
                    let
                        w =
                            round (toFloat fixConfig.baseWidth * 0.5)

                        h1 =
                            fixConfig.baseHeight

                        h2 =
                            round (calculateHeight 2 1)
                    in
                    row [ spacing fixConfig.spacingSize ]
                        [ column [ spacing fixConfig.spacingSize ]
                            [ onePicture w h1 picture1 listIndex 0 ]
                        , column [ spacing fixConfig.spacingSize ]
                            [ onePicture w h2 picture2 listIndex 1
                            , onePicture w h2 picture3 listIndex 2
                            ]
                        ]

                [ picture1, picture2, picture3, picture4 ] ->
                    let
                        w =
                            round (toFloat fixConfig.baseWidth * 0.5)

                        h =
                            round (calculateHeight 2 1)
                    in
                    row [ spacing fixConfig.spacingSize ]
                        [ column [ spacing fixConfig.spacingSize ]
                            [ onePicture w h picture1 listIndex 0
                            , onePicture w h picture2 listIndex 1
                            ]
                        , column [ spacing fixConfig.spacingSize ]
                            [ onePicture w h picture3 listIndex 2
                            , onePicture w h picture4 listIndex 3
                            ]
                        ]

                [ picture1, picture2, picture3, picture4, picture5 ] ->
                    let
                        w =
                            round (toFloat fixConfig.baseWidth * 0.5)

                        h1 =
                            round (calculateHeight 2 1)

                        h2 =
                            round (calculateHeight 3 2)
                    in
                    row [ spacing fixConfig.spacingSize ]
                        [ column [ spacing fixConfig.spacingSize ]
                            [ onePicture w h1 picture1 listIndex 0
                            , onePicture w h1 picture2 listIndex 1
                            ]
                        , column [ spacing fixConfig.spacingSize ]
                            [ onePicture w h2 picture3 listIndex 2
                            , onePicture w h2 picture4 listIndex 3
                            , onePicture w h2 picture5 listIndex 4
                            ]
                        ]

                picture1 :: picture2 :: picture3 :: picture4 :: picture5 :: restOfList ->
                    let
                        w =
                            round (toFloat fixConfig.baseWidth * 0.5)

                        h1 =
                            round (calculateHeight 2 1)

                        h2 =
                            round (calculateHeight 3 2)
                    in
                    row [ spacing fixConfig.spacingSize ]
                        [ column [ spacing fixConfig.spacingSize, alignTop ]
                            [ onePicture w h1 picture1 listIndex 0
                            , onePicture w h1 picture2 listIndex 1
                            ]
                        , column [ spacing fixConfig.spacingSize, alignTop ]
                            [ onePicture w h2 picture3 listIndex 2
                            , onePicture w h2 picture4 listIndex 3
                            , el [ inFront <| displaySizeOfList restOfList ] <|
                                onePicture w h2 picture5 listIndex 4
                            ]
                        ]
    in
    el [ centerX ] <| layoutPictures sortedPictures


getHeight : String -> List ( String, PictureSize ) -> Maybe Int
getHeight id sizes =
    List.head <|
        List.filterMap
            (\( imgId, size ) ->
                if imgId == id then
                    Just size.height

                else
                    Nothing
            )
            sizes


compareHeights : Picture -> Picture -> Order
compareHeights pic1 pic2 =
    Basics.compare pic2.size.height pic1.size.height


sortByHeight : List Picture -> List Picture
sortByHeight pictures =
    List.sortWith compareHeights pictures


nextPic : Int -> Int -> Element FrontendMsg
nextPic listIndex index =
    if index == List.length (getListFromIndex listIndex) - 1 then
        el [ width <| px 100, height fill ] none

    else
        el
            [ width <| px 100
            , paddingEach { edges | right = 30, left = 30 }
            , centerY
            , Font.color g400
            , Font.size 67
            , moveUp 3.5
            , pointer
            ]
        <|
            text ">"


previousPic : Int -> Int -> Element FrontendMsg
previousPic listIndex index =
    if index == 0 then
        el [ width <| px 100, height fill ] none

    else
        el
            [ width <| px 100
            , paddingEach { edges | right = 30, left = 30 }
            , centerY
            , Font.color g400
            , Font.size 67
            , moveUp 3.5
            , pointer
            ]
        <|
            text "<"


overlayEl : Element msg -> Element msg
overlayEl =
    el
        [ width fill
        , height fill
        , Background.color <| rgba255 0 0 0 0.7
        , htmlAttribute <| HA.style "overflow-y" "auto"
        , htmlAttribute <| HA.style "position" "fixed"
        , htmlAttribute <| HA.style "top" "0"
        , htmlAttribute <| HA.style "right" "0"
        , htmlAttribute <| HA.style "bottom" "0"
        , htmlAttribute <| HA.style "left" "0"
        ]


findPicture : List Picture -> Int -> Maybe Picture
findPicture pictures pictureId =
    List.getAt pictureId pictures
