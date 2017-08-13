module Main exposing (..)

import Html exposing (..)
import Html.Events exposing (onClick)
import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { posts : Maybe (List Post)
    }


initialModel : Model
initialModel =
    { posts = Nothing }


init : ( Model, Cmd Msg )
init =
    ( initialModel
    , Http.send FetchPosts getPosts
    )


type alias Post =
    { id : Int
    , title : String
    , author : String
    }



-- UPDATE


type Msg
    = FetchPosts (Result Http.Error (List Post))
    | CreatePost
    | CreatedPost (Result Http.Error Post)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FetchPosts (Ok posts) ->
            ( { model | posts = Just posts }, Cmd.none )

        FetchPosts (Err err) ->
            ( model, Cmd.none )

        CreatePost ->
            let
                posts =
                    model.posts

                cmd =
                    Http.send
                        CreatedPost
                        (createPost (Post (nextId posts) "Little Elixir Book" "Benjamin Tan"))
            in
                ( model, cmd )

        CreatedPost (Ok post) ->
            case model.posts of
                Just posts ->
                    ( { model | posts = Just ((post :: (List.reverse posts)) |> List.reverse) }, Cmd.none )

                Nothing ->
                    ( { model | posts = Just (post :: []) }, Cmd.none )

        CreatedPost (Err err) ->
            ( model, Cmd.none )


nextId : Maybe (List Post) -> Int
nextId posts =
    case posts of
        Just posts ->
            case posts |> List.reverse |> List.head of
                Just lastPost ->
                    lastPost.id + 1

                Nothing ->
                    1

        Nothing ->
            1



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick CreatePost ] [ text "Create Post" ]
        , text (toString model)
        ]



-- HTTP


postEncoder : Post -> Encode.Value
postEncoder post =
    Encode.object
        [ ( "id", Encode.int post.id )
        , ( "author", Encode.string post.author )
        , ( "title", Encode.string post.title )
        ]


postsDecoder : Decoder (List Post)
postsDecoder =
    Decode.list postDecoder


postDecoder : Decoder Post
postDecoder =
    Decode.map3
        Post
        (Decode.at [ "id" ] Decode.int)
        (Decode.at [ "title" ] Decode.string)
        (Decode.at [ "author" ] Decode.string)


fetchPosts : Cmd Msg
fetchPosts =
    Http.send FetchPosts getPosts


getPosts : Http.Request (List Post)
getPosts =
    Http.get postsUrl postsDecoder


createPost : Post -> Http.Request Post
createPost post =
    Http.post
        postsUrl
        (Http.stringBody "application/json" <| Encode.encode 0 <| postEncoder post)
        postDecoder


postsUrl : String
postsUrl =
    "http://localhost:3000/posts"
