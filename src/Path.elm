module Path exposing
    ( Path, Platform, fromString, toString, fromList, toList
    , separator, delimiter, platform, dir, base, name, ext
    )

{-|


# Paths

@docs Path, Platform, fromString, toString, fromList, toList

@docs separator, delimiter, platform, dir, base, name, ext

-}

import List.Extra as List
import Path.Internal as Internal
import Path.Platform as Platform
import Path.Posix as Posix
import Path.Win32 as Win32
import String.Extra as String


{-| Represents a platform used for parsing the strings, either Win32 or Posix.
Create or infer platforms with [`Path.Platform`](Path-Platform)
-}
type alias Platform =
    Internal.Platform


{-| Parsed and normalised path.
-}
type alias Path =
    Internal.Path


{-| Get the separator of a path.

    Result.map Path.separator (Path.fromString Path.Platform.posix "/static/images/image.jpeg/" ]) == Ok "/"
    Result.map Path.separator (Path.fromString Path.Platform.posix "../..//.///foo") == Ok "/"
    Result.map Path.separator (Path.fromString Path.Platform.win32 "./x/b/..//b/c.prod.js") == Ok "\\"
    Result.map Path.separator (Path.fromString Path.Platform.posix "./x/b/..//b/.hidden") == Ok "/"

-}
separator : Path -> String
separator (Internal.Path platform_ _) =
    Platform.separator platform_


{-| Get the delimiter of a path.

    Result.map Path.delimiter (Path.fromString Path.Platform.posix "/static/images/image.jpeg/" ]) == Ok ":"
    Result.map Path.delimiter (Path.fromString Path.Platform.posix "../..//.///foo") == Ok ":"
    Result.map Path.delimiter (Path.fromString Path.Platform.win32 "./x/b/..//b/c.prod.js") == Ok ";"
    Result.map Path.delimiter (Path.fromString Path.Platform.posix "./x/b/..//b/.hidden") == Ok ":"

-}
delimiter : Path -> String
delimiter (Internal.Path platform_ _) =
    Platform.delimiter platform_


{-| Get the platform of a path.

    Result.map Path.platform (Path.fromString Path.Platform.posix "/static/images/image.jpeg/" ]) == Ok Path.Platform.posix
    Result.map Path.platform (Path.fromString Path.Platform.posix "../..//.///foo") == Ok Path.Platform.posix
    Result.map Path.platform (Path.fromString Path.Platform.win32 "./x/b/..//b/c.prod.js") == Ok Path.Platform.win32
    Result.map Path.platform (Path.fromString Path.Platform.posix "./x/b/..//b/.hidden") == Ok Path.Platform.posix

-}
platform : Path -> Platform
platform (Internal.Path platform_ _) =
    platform_


{-| Get the directory of a path.

    Result.map Path.dir (Path.fromString Path.Platform.posix "/static/images/image.jpeg/" ]) == Ok "/static/images"
    Result.map Path.dir (Path.fromString Path.Platform.posix "../..//.///foo") == Ok "../.."
    Result.map Path.dir (Path.fromString Path.Platform.posix "./x/b/..//b/c.prod.js") == Ok "x/b"
    Result.map Path.dir (Path.fromString Path.Platform.posix "./x/b/..//b/.hidden") == Ok "x/b"

-}
dir : Path -> String
dir (Internal.Path _ details) =
    case details.root of
        Just root ->
            root ++ details.dir

        Nothing ->
            details.dir


{-| Get the full file name or last directory name of a path.

    Result.map Path.base (Path.fromString Path.Platform.posix "/static/images/image.jpeg/" ]) == Ok "image.jpeg"
    Result.map Path.base (Path.fromString Path.Platform.posix "../..//.///foo") == Ok "foo"
    Result.map Path.base (Path.fromString Path.Platform.posix "./x/b/..//b/c.prod.js") == Ok "c.prod.js"
    Result.map Path.base (Path.fromString Path.Platform.posix "./x/b/..//b/.hidden") == Ok ".hidden"

-}
base : Path -> String
base (Internal.Path _ details) =
    details.name


{-| Get the file name (without extension) or last directory name of a path. Will take from the left of the last ".".

    Result.map Path.name (Path.fromString Path.Platform.posix "/static/images/image.jpeg/" ]) == Ok "image"
    Result.map Path.name (Path.fromString Path.Platform.posix "../..//.///foo") == Ok "foo"
    Result.map Path.name (Path.fromString Path.Platform.posix "./x/b/..//b/c.prod.js") == Ok "c.prod"
    Result.map Path.name (Path.fromString Path.Platform.posix "./x/b/..//b/.hidden") == Ok ".hidden"

-}
name : Path -> String
name ((Internal.Path _ details) as path) =
    String.dropRight (String.length (ext path)) details.name


{-| Get the file extension (if it exists) of a path. Will only take from the last ".".

    Result.map Path.ext (Path.fromString Path.Platform.posix "/static/images/image.jpeg/" ]) == Ok ".jpeg"
    Result.map Path.ext (Path.fromString Path.Platform.posix "../..//.///foo") == Ok ""
    Result.map Path.ext (Path.fromString Path.Platform.posix "./x/b/..//b/c.prod.js") == Ok ".js"
    Result.map Path.ext (Path.fromString Path.Platform.posix "./x/b/..//b/.hidden") == Ok ""

-}
ext : Path -> String
ext (Internal.Path _ details) =
    let
        withoutLeadingDots : String
        withoutLeadingDots =
            String.split "." details.name
                |> List.dropWhile String.isEmpty
                |> String.join "."
    in
    case String.rightOfBack "." withoutLeadingDots of
        "" ->
            ""

        str ->
            "." ++ str


{-| Create a path from a string. The path will be normalised and any trailing slashes will be dropped.
Note this will _not_ raise an error on invalid path characters

    Result.map Path.toString (Path.fromString Path.Platform.posix "/static/images/image.jpeg/" ]) == Ok "/static/images/image.jpeg"
    Result.map Path.toString (Path.fromString Path.Platform.posix "../..//.///foo") == Ok "../../foo"
    Result.map Path.toString (Path.fromString Path.Platform.posix "./x/b/..//b/c.js") == Ok "x/b/c.js"
    Result.map Path.toString (Path.fromString Path.Platform.win32 "a:\\b") == Ok "a:\\b"
    Result.map Path.toString (Path.fromString Path.Platform.win32 "\\\\machine\\server\\\\path") == Ok "\\\\machine\\server\\path"
    Result.map Path.toString (Path.fromString Path.Platform.posix "") == Ok "."
    Result.map Path.toString (Path.fromString Path.Platform.posix " foo") == Ok " foo"

-}
fromString : Platform -> String -> Result String Path
fromString platform_ str =
    case platform_ of
        Internal.Win32 ->
            Win32.fromString str

        Internal.Posix ->
            Posix.fromString str


{-| Create a path from a list of strings. Some notes:

  - If passed a path with a root, it will _not_ overwrite previous segments, it will add the root to the path. This is similar to Node path behaviour and different from Python os.path behaviour.

  - If passed a valid root as the first element in the list, will not add a separator between the first and second elements

  - Will ignore empty strings

  - Won't ignore spaces!

```elm
Result.map Path.toString (Path.fromList Path.Platform.posix [ "/static", "images", "/image.jpeg" ]) == Ok "/static/images/image.jpeg"
Result.map Path.toString (Path.fromList Path.Platform.posix [ "", "..", "..", "/foo" ]) == Ok "../../foo"
Result.map Path.toString (Path.fromList Path.Platform.posix [ ".", "x/b", "..", "/b/c.js" ]) == Ok "x/b/c.js"
Result.map Path.toString (Path.fromList Path.Platform.win32 [ "a:", "\\b" ][ "a:", "\b" ][ "a:", "\\b" ]) == Ok "a:\\b"
Result.map Path.toString (Path.fromList Path.Platform.win32 [ "a:", "b\\c" ][ "a:", "b\c" ][ "a:", "b\\c" ]) == Ok "a:b\\c"
Result.map Path.toString (Path.fromList Path.Platform.posix []) == Ok "."
Result.map Path.toString (Path.fromList Path.Platform.posix [ " ", "foo" ]) == Ok " /foo"
```

-}
fromList : Platform -> List String -> Result String Path
fromList platform_ strs =
    case platform_ of
        Internal.Win32 ->
            case List.filter (not << String.isEmpty) strs of
                [] ->
                    Win32.fromString "."

                firstStr :: restStrs ->
                    case Win32.fromString firstStr of
                        Ok (Internal.Path _ details) ->
                            if not (details.root == Nothing) && details.dir == "." && details.name == "" then
                                String.join Win32.separator restStrs
                                    |> (++) firstStr
                                    |> Win32.fromString

                            else
                                String.join Win32.separator restStrs
                                    |> (++) (firstStr ++ Win32.separator)
                                    |> Win32.fromString

                        Err err ->
                            Err err

        Internal.Posix ->
            List.filter (not << String.isEmpty) strs
                |> String.join (Posix.separator ++ Posix.separator)
                |> Posix.fromString


{-| Turn a path into a list of directory and file name segments. Includes the path root.

    Result.map Path.toList (Path.fromString "/static/images/image.jpeg") == Ok [ "/", "static", "images", "image.jpeg" ]

    Result.map Path.toList (Path.fromString "c:\\static\\images\\image.jpeg") == Ok [ "c:\\", "static", "images", "image.jpeg" ]

    Result.map Path.toList (Path.fromString "static/images/image.jpeg") == Ok [ "static", "images", "image.jpeg" ]

-}
toList : Path -> List String
toList (Internal.Path platform_ details) =
    case details.root of
        Just root ->
            root
                :: String.split (Platform.separator platform_) details.dir
                ++ [ details.name ]
                |> List.filter (not << String.isEmpty)

        Nothing ->
            String.split (Platform.separator platform_) details.dir
                ++ [ details.name ]
                |> List.filter (not << String.isEmpty)


{-| Turn a path into a string.

    Result.map Path.toString (Path.fromString Path.Platform.posix "/static/images/image.jpeg/") == Ok "/static/images/image.jpeg"

    Result.map Path.toString (Path.fromString Path.Platform.win32 "c:\\static\\images\\image.jpeg") == Ok "c:\\static\\images\\image.jpeg"

    Result.map Path.toString (Path.fromString Path.Platform.posix "static/////images///image.jpeg///////") == Ok "static/images/image.jpeg"

    Result.map Path.toString (Path.fromString Path.Platform.posix "") == Ok "."

    Result.map Path.toString (Path.fromString Path.Platform.posix "a/../b/./c/../") == Ok "b"

-}
toString : Path -> String
toString (Internal.Path platform_ details) =
    let
        dir_ : String
        dir_ =
            if String.isEmpty details.dir then
                ""

            else if String.isEmpty details.name then
                details.dir

            else
                details.dir ++ Platform.separator platform_
    in
    case details.root of
        Just root ->
            root ++ dir_ ++ details.name

        Nothing ->
            dir_ ++ details.name
