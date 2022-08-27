module Path.Win32 exposing (delimiter, fromString, separator)

import Parser
import Path.Internal as Internal
import Path.Win32.Internal


separator : String
separator =
    String.fromChar Internal.win32Separator


delimiter : String
delimiter =
    ";"


fromString : String -> Result String Internal.Path
fromString path =
    if String.startsWith "\\\\?\\" path then
        Result.mapError Parser.deadEndsToString
            (Result.map
                (\(Internal.Path _ details) ->
                    Internal.Path Internal.Win32 { details | root = Just "\\\\?\\" }
                )
                (Parser.run Path.Win32.Internal.parser (String.dropLeft 3 path))
            )

    else if String.startsWith "\\\\.\\" path then
        Result.mapError Parser.deadEndsToString <|
            Result.map
                (\(Internal.Path _ details) ->
                    Internal.Path Internal.Win32 { details | root = Just "\\\\.\\" }
                )
                (Parser.run Path.Win32.Internal.parser (String.dropLeft 3 path))

    else
        Result.mapError Parser.deadEndsToString <|
            Parser.run Path.Win32.Internal.parser
                (String.replace (String.fromChar Internal.posixSeparator) separator path)
