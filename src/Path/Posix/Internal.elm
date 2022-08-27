module Path.Posix.Internal exposing (parser)

import Parser exposing ((|.), (|=))
import Path.Internal as Internal


separator : String
separator =
    String.fromChar Internal.posixSeparator


parser : Parser.Parser Internal.Path
parser =
    let
        doubleSeparatorParser : Parser.Parser ()
        doubleSeparatorParser =
            Parser.succeed identity
                |. Parser.backtrackable (Parser.token (separator ++ separator))
                |= Parser.oneOf
                    [ Parser.map (\_ -> True) (Parser.backtrackable (Parser.chompIf ((==) Internal.posixSeparator)))
                    , Parser.succeed False
                    ]
                |> Parser.andThen
                    (\nextCharIsSeparator ->
                        if nextCharIsSeparator then
                            Parser.problem "Expecting only two forward slashes (e.g. `//foo`"

                        else
                            Parser.commit ()
                    )

        rootParser : Parser.Parser Internal.RootDetails
        rootParser =
            Parser.oneOf
                [ Parser.backtrackable <|
                    Parser.succeed { root = separator ++ separator, isAbsolute = True }
                        |. doubleSeparatorParser
                , Parser.succeed { root = separator, isAbsolute = True }
                    |. Internal.absoluteParser Internal.posixSeparator
                , Parser.succeed { root = "", isAbsolute = False }
                ]
    in
    Parser.succeed (Internal.Path Internal.Posix)
        |= (rootParser
                |> Parser.andThen
                    (\root ->
                        Parser.succeed (Internal.makePathDetails Internal.posixSeparator root)
                            |= Internal.pathTailParser (not root.isAbsolute) Internal.posixSeparator
                    )
           )
