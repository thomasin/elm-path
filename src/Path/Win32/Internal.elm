module Path.Win32.Internal exposing (parser)

import Parser exposing ((|.), (|=))
import Path.Internal as Internal


separator : String
separator =
    String.fromChar Internal.win32Separator


parser : Parser.Parser Internal.Path
parser =
    let
        rootParser : Parser.Parser Internal.RootDetails
        rootParser =
            Parser.oneOf
                [ Parser.backtrackable <|
                    Parser.succeed (\root -> { root = root, isAbsolute = True })
                        |= uncRootParser
                , Parser.succeed { root = separator, isAbsolute = True }
                    |. Internal.absoluteParser Internal.win32Separator
                , Parser.backtrackable <|
                    Parser.mapChompedString (\root isAbsolute -> { root = root, isAbsolute = isAbsolute })
                        deviceRootParser
                , Parser.succeed { root = "", isAbsolute = False }
                ]

        deviceRootParser : Parser.Parser Bool
        deviceRootParser =
            Parser.succeed identity
                |. Parser.chompIf Char.isAlpha
                |. Parser.symbol ":"
                |= Parser.oneOf
                    [ Internal.absoluteParser Internal.win32Separator
                    , Parser.succeed False
                    ]

        uncRootParser : Parser.Parser String
        uncRootParser =
            Parser.succeed (\machine server -> separator ++ separator ++ machine ++ separator ++ server ++ separator)
                |. Parser.token (separator ++ separator)
                |= Parser.getChompedString
                    (Parser.succeed ()
                        |. Parser.chompIf ((/=) Internal.win32Separator)
                        |. Parser.chompWhile ((/=) Internal.win32Separator)
                    )
                |. Parser.chompIf ((==) Internal.win32Separator)
                |. Parser.chompWhile ((==) Internal.win32Separator)
                |= Parser.getChompedString
                    (Parser.succeed ()
                        |. Parser.chompIf ((/=) Internal.win32Separator)
                        |. Parser.chompWhile ((/=) Internal.win32Separator)
                    )
                |. Parser.chompIf ((==) Internal.win32Separator)
                |. Parser.chompWhile ((==) Internal.win32Separator)
    in
    Parser.succeed (Internal.Path Internal.Win32)
        |= (rootParser
                |> Parser.andThen
                    (\root ->
                        Parser.succeed (Internal.makePathDetails Internal.win32Separator root)
                            |= Internal.pathTailParser (not root.isAbsolute) Internal.win32Separator
                    )
           )
