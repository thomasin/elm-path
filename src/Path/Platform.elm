module Path.Platform exposing (Platform, win32, posix, fromSeparator, separator, delimiter)

{-|


# Path.Platform

@docs Platform, win32, posix, fromSeparator, separator, delimiter

-}

import Path.Internal as Internal
import Path.Posix as Posix
import Path.Win32 as Win32


{-| Represents a platform used for parsing the strings, either Win32 or Posix.
Create or infer platforms with [`win32`](#win32), [`posix`](#posix) and [`fromSeparator`](#fromSeparator)
-}
type alias Platform =
    Internal.Platform


{-| Win32. Use when you want to hardcode platform into Path functions.
If you are unsure what platform you need, use [`fromSeparator`](#fromSeparator) to infer it.

    Path.Platform.fromString Path.Platform.win32 "c:/A/B/C.elm"

-}
win32 : Platform
win32 =
    Internal.Win32


{-| Posix. Use when you want to hardcode platform into Path functions.
If you are unsure what platform you need, use [`fromSeparator`](#fromSeparator) to infer it.

    Path.Platform.fromString Path.Platform.posix "/A/B/C.elm"

-}
posix : Platform
posix =
    Internal.Posix


{-| The separator of a platform.

    Path.Platform.separator Path.Platform.posix == "/"

    Path.Platform.separator Path.Platform.win32 == "\\"

-}
separator : Platform -> String
separator platform =
    case platform of
        Internal.Win32 ->
            Win32.separator

        Internal.Posix ->
            Posix.separator


{-| Get platform based on a separator. This will default to Posix!
You can find the separator using Node with `path.sep`,
in Python with `os.path.sep`, and pass it in to Elm.

    Path.Platform.fromSeparator "/" == Path.Platform.posix

    Path.Platform.fromSeparator "\\" == Path.Platform.win32

    Path.Platform.fromSeparator "lkjankljsAKSKJ" == Path.Platform.posix

-}
fromSeparator : String -> Platform
fromSeparator separator_ =
    if separator_ == Win32.separator then
        win32

    else
        posix


{-| Delimiter of a platform.

    Path.Platform.delimiter Path.Platform.posix == ":"

    Path.Platform.delimiter Path.Platform.win32 == ";"

-}
delimiter : Platform -> String
delimiter platform =
    case platform of
        Internal.Win32 ->
            Win32.delimiter

        Internal.Posix ->
            Posix.delimiter
