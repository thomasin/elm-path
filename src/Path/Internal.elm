module Path.Internal exposing (Path(..), PathDetails, Platform(..), RootDetails, TailDetails, absoluteParser, makePathDetails, pathTailParser, posixSeparator, win32Separator)

import Parser exposing ((|.), (|=))
import String.Extra as String


type Platform
    = Win32
    | Posix


type Path
    = Path Platform PathDetails


type alias PathDetails =
    { root : Maybe String
    , dir : String
    , name : String
    }


posixSeparator : Char
posixSeparator =
    '/'


win32Separator : Char
win32Separator =
    '\\'


absoluteParser : Char -> Parser.Parser Bool
absoluteParser sep =
    Parser.succeed True
        |. Parser.symbol (String.fromChar sep)


type alias RootDetails =
    { root : String
    , isAbsolute : Bool
    }


type alias TailDetails =
    { dir : List String
    , name : String
    }


makePathDetails : Char -> RootDetails -> Maybe TailDetails -> PathDetails
makePathDetails sep root tail =
    { root = String.nonBlank root.root
    , dir =
        case tail of
            Just tail_ ->
                String.join (String.fromChar sep) tail_.dir

            Nothing ->
                if root.isAbsolute then
                    ""

                else
                    "."
    , name = Maybe.withDefault "" (Maybe.map .name tail)
    }


pathTailParser : Bool -> Char -> Parser.Parser (Maybe TailDetails)
pathTailParser allowAboveRoot sep =
    let
        handleNewPathPiece : List String -> String -> Parser.Step (List String) (Maybe TailDetails)
        handleNewPathPiece normalised piece =
            if piece == "." || piece == "" then
                Parser.Loop normalised

            else if piece == ".." then
                if not (List.isEmpty normalised) && List.head normalised /= Just ".." then
                    Parser.Loop (List.drop 1 normalised)

                else if allowAboveRoot then
                    Parser.Loop (".." :: normalised)

                else
                    Parser.Loop normalised

            else
                Parser.Loop (piece :: normalised)

        dirAndName : List String -> Maybe TailDetails
        dirAndName normalised =
            case normalised of
                name :: dir ->
                    Just { dir = List.reverse dir, name = name }

                [] ->
                    Nothing

        parserHelp : List String -> Parser.Parser (Parser.Step (List String) (Maybe TailDetails))
        parserHelp normalised =
            Parser.oneOf
                [ Parser.succeed (Parser.Done (dirAndName normalised))
                    |. Parser.end
                , Parser.succeed (handleNewPathPiece normalised)
                    |= Parser.getChompedString (Parser.chompUntilEndOr (String.fromChar sep))
                    |. Parser.chompWhile ((==) sep)
                ]
    in
    Parser.loop [] parserHelp
