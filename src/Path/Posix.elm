module Path.Posix exposing (delimiter, fromString, separator)

import Parser
import Path.Internal as Internal
import Path.Posix.Internal


separator : String
separator =
    String.fromChar Internal.posixSeparator


delimiter : String
delimiter =
    ":"


fromString : String -> Result String Internal.Path
fromString str =
    Parser.run Path.Posix.Internal.parser str
        |> Result.mapError Parser.deadEndsToString
