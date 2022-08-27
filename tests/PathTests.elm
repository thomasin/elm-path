module PathTests exposing (suite)

import Expect
import Path
import Path.Internal as Internal
import Path.Platform as Platform
import Path.Posix as Posix
import Path.Win32 as Win32
import Test exposing (Test, describe, test)


testToList : Path.Platform -> String -> Result String (List String)
testToList platform inputStr =
    Path.fromString platform inputStr
        |> Result.map Path.toList


testExt : String -> Result String String
testExt inputStr =
    Path.fromString Platform.posix inputStr
        |> Result.map Path.ext


testName : String -> Result String String
testName inputStr =
    Path.fromString Platform.posix inputStr
        |> Result.map Path.name


suite : Test
suite =
    describe "Path"
        [ describe "name"
            [ test "1" <| \() -> Expect.equal (Ok "image") (testName "/static/images/image.jpeg/")
            , test "2" <| \() -> Expect.equal (Ok "foo") (testName "../..//.///foo")
            , test "3" <| \() -> Expect.equal (Ok "c.prod") (testName "./x/b/..//b/c.prod.js")
            , test "4" <| \() -> Expect.equal (Ok ".hidden") (testName "./x/b/..//b/.hidden")
            ]
        , describe "ext"
            [ test "1" <| \() -> Expect.equal (Ok ".jpeg") (testExt "/static/images/image.jpeg/")
            , test "2" <| \() -> Expect.equal (Ok "") (testExt "../..//.///foo")
            , test "3" <| \() -> Expect.equal (Ok ".js") (testExt "./x/b/..//b/c.prod.js")
            , test "4" <| \() -> Expect.equal (Ok "") (testExt "./x/b/..//b/.hidden")
            ]
        , describe "toList"
            [ describe "win32"
                [ test "1" <| \() -> Expect.equal (Ok [ "c:\\", "foo", "bar" ]) (testToList Platform.win32 "c:\\foo\\bar")
                , test "2" <| \() -> Expect.equal (Ok [ "\\\\conky\\mountpoint\\", "foo", "bar" ]) (testToList Platform.win32 "\\\\conky\\mountpoint\\foo\\bar")
                , test "3" <| \() -> Expect.equal (Ok [ "c:\\" ]) (testToList Platform.win32 "c:\\")
                , test "4" <| \() -> Expect.equal (Ok [ "\\\\conky\\mountpoint\\" ]) (testToList Platform.win32 "\\\\conky\\mountpoint\\")
                , test "5" <| \() -> Expect.equal (Ok [ "c:\\" ]) (testToList Platform.win32 "c:/")
                , test "6" <| \() -> Expect.equal (Ok [ "\\\\conky\\mountpoint\\" ]) (testToList Platform.win32 "//conky/mountpoint/")
                ]
            , describe "posix"
                [ test "1" <| \() -> Expect.equal (Ok [ "/", "foo", "bar" ]) (testToList Platform.posix "/foo/bar")
                , test "2" <| \() -> Expect.equal (Ok [ "/" ]) (testToList Platform.posix "/")
                , test "3" <| \() -> Expect.equal (Ok [ "foo" ]) (testToList Platform.posix "foo")
                , test "4" <| \() -> Expect.equal (Ok [ "/", "foo" ]) (testToList Platform.posix "////foo")
                , test "5" <| \() -> Expect.equal (Ok [ "//", "foo", "bar" ]) (testToList Platform.posix "//foo//bar")
                , test "6" <| \() -> Expect.equal (Ok [ "/", "foo", "bar" ]) (testToList Platform.posix "/foo/bar")
                ]
            ]
        , describe "fromList"
            [ describe "win32"
                [ describe "auto" (crossPlatformJoinTest Platform.win32 (String.replace Posix.separator Win32.separator))
                , describe "ntpath"
                    [ test "1" <| \() -> Expect.equal (Ok ".") (join Platform.win32 [ "" ])
                    , test "2" <| \() -> Expect.equal (Ok ".") (join Platform.win32 [ "", "", "" ])
                    , test "3" <| \() -> Expect.equal (Ok "a") (join Platform.win32 [ "a" ])
                    , test "4" <| \() -> Expect.equal (Ok "\\a") (join Platform.win32 [ "/a" ])
                    , test "5" <| \() -> Expect.equal (Ok "\\a") (join Platform.win32 [ "\\a" ])
                    , test "6" <| \() -> Expect.equal (Ok "a:.") (join Platform.win32 [ "a:" ])
                    , test "7" <| \() -> Expect.equal (Ok "a:\\b") (join Platform.win32 [ "a:", "\\b" ])
                    , test "8" <| \() -> Expect.equal (Ok "a\\b") (join Platform.win32 [ "a", "\\b" ])
                    , test "9" <| \() -> Expect.equal (Ok "a\\b\\c") (join Platform.win32 [ "a", "b", "c" ])
                    , test "10" <| \() -> Expect.equal (Ok "a\\b\\c") (join Platform.win32 [ "a\\", "b", "c" ])
                    , test "11" <| \() -> Expect.equal (Ok "a\\b\\c") (join Platform.win32 [ "a", "b\\", "c" ])
                    , test "12" <| \() -> Expect.equal (Ok "a\\b\\c") (join Platform.win32 [ "a", "b", "\\c" ])
                    , test "13" <| \() -> Expect.equal (Ok "d:\\pleep") (join Platform.win32 [ "d:\\", "\\pleep" ])
                    , test "14" <| \() -> Expect.equal (Ok "d:\\a\\b") (join Platform.win32 [ "d:\\", "a", "b" ])
                    , test "15" <| \() -> Expect.equal (Ok "a") (join Platform.win32 [ "", "a" ])
                    , test "16" <| \() -> Expect.equal (Ok "a") (join Platform.win32 [ "", "", "", "", "a" ])
                    , test "17" <| \() -> Expect.equal (Ok "a") (join Platform.win32 [ "a", "" ])
                    , test "18" <| \() -> Expect.equal (Ok "a") (join Platform.win32 [ "a", "", "", "", "" ])
                    , test "19" <| \() -> Expect.equal (Ok "a") (join Platform.win32 [ "a\\", "" ])
                    , test "20" <| \() -> Expect.equal (Ok "a") (join Platform.win32 [ "a\\", "", "", "", "" ])
                    , test "21" <| \() -> Expect.equal (Ok "a") (join Platform.win32 [ "a/", "" ])
                    , test "22" <| \() -> Expect.equal (Ok "a\\b\\x\\y") (join Platform.win32 [ "a/b", "x/y" ])
                    , test "23" <| \() -> Expect.equal (Ok "\\a\\b\\x\\y") (join Platform.win32 [ "/a/b", "x/y" ])
                    , test "24" <| \() -> Expect.equal (Ok "\\a\\b\\x\\y") (join Platform.win32 [ "/a/b/", "x/y" ])
                    , test "25" <| \() -> Expect.equal (Ok "c:x\\y") (join Platform.win32 [ "c:", "x/y" ])
                    , test "26" <| \() -> Expect.equal (Ok "c:a\\b\\x\\y") (join Platform.win32 [ "c:a/b", "x/y" ])
                    , test "27" <| \() -> Expect.equal (Ok "c:a\\b\\x\\y") (join Platform.win32 [ "c:a/b/", "x/y" ])
                    , test "28" <| \() -> Expect.equal (Ok "c:\\x\\y") (join Platform.win32 [ "c:/", "x/y" ])
                    , test "29" <| \() -> Expect.equal (Ok "c:\\a\\b\\x\\y") (join Platform.win32 [ "c:/a/b", "x/y" ])
                    , test "30" <| \() -> Expect.equal (Ok "c:\\a\\b\\x\\y") (join Platform.win32 [ "c:/a/b/", "x/y" ])
                    , test "31" <| \() -> Expect.equal (Ok "\\\\computer\\share\\x\\y") (join Platform.win32 [ "//computer/share", "x/y" ])
                    , test "32" <| \() -> Expect.equal (Ok "\\\\computer\\share\\x\\y") (join Platform.win32 [ "//computer/share/", "x/y" ])
                    , test "33" <| \() -> Expect.equal (Ok "\\\\computer\\share\\a\\b\\x\\y") (join Platform.win32 [ "//computer/share/a/b", "x/y" ])
                    , test "34" <| \() -> Expect.equal (Ok "a\\b\\x\\y") (join Platform.win32 [ "a/b", "/x/y" ])
                    , test "35" <| \() -> Expect.equal (Ok "\\a\\b\\x\\y") (join Platform.win32 [ "/a/b", "/x/y" ])
                    , test "36" <| \() -> Expect.equal (Ok "c:\\x\\y") (join Platform.win32 [ "c:", "/x/y" ])
                    , test "37" <| \() -> Expect.equal (Ok "c:a\\b\\x\\y") (join Platform.win32 [ "c:a/b", "/x/y" ])
                    , test "38" <| \() -> Expect.equal (Ok "c:\\x\\y") (join Platform.win32 [ "c:/", "/x/y" ])
                    , test "39" <| \() -> Expect.equal (Ok "c:\\a\\b\\x\\y") (join Platform.win32 [ "c:/a/b", "/x/y" ])
                    , test "40" <| \() -> Expect.equal (Ok "\\\\computer\\share\\x\\y") (join Platform.win32 [ "//computer/share", "/x/y" ])
                    , test "41" <| \() -> Expect.equal (Ok "\\\\computer\\share\\x\\y") (join Platform.win32 [ "//computer/share/", "/x/y" ])
                    , test "42" <| \() -> Expect.equal (Ok "\\\\computer\\share\\a\\x\\y") (join Platform.win32 [ "//computer/share/a", "/x/y" ])

                    -- Not ideal behaviour. Other possibilites: allowing an option to follow Python os.path behaviour, where paths with a
                    -- root overwrite all of the previous segments i.e. `join Platform.win32 ["c:a/b", "C:x/y"] == Ok "C:x\\y"`.
                    -- Or throw an error when encountering invalid characters in a path (should probably do this anyway).
                    , test "43" <| \() -> Expect.equal (Ok "c:C:x\\y") (join Platform.win32 [ "c:", "C:x/y" ])
                    , test "44" <| \() -> Expect.equal (Ok "c:a\\b\\C:x\\y") (join Platform.win32 [ "c:a/b", "C:x/y" ])
                    , test "45" <| \() -> Expect.equal (Ok "c:\\C:x\\y") (join Platform.win32 [ "c:/", "C:x/y" ])
                    , test "46" <| \() -> Expect.equal (Ok "c:\\a\\b\\C:x\\y") (join Platform.win32 [ "c:/a/b", "C:x/y" ])
                    ]
                ]
            , describe "posix"
                (crossPlatformJoinTest Platform.posix identity)
            ]
        ]


join : Internal.Platform -> List String -> Result String String
join os strs =
    Result.map Path.toString (Path.fromList os strs)


crossPlatformJoinTest : Internal.Platform -> (String -> String) -> List Test
crossPlatformJoinTest os outputFunc =
    List.map
        (\( tester, input, output ) ->
            tester <| \() -> Expect.equal (Ok (outputFunc output)) (join os input)
        )
        [ ( test "1", [ ".", "x/b", "..", "/b/c.js" ], "x/b/c.js" )
        , ( test "2", [], "." )
        , ( test "3", [ "/.", "x/b", "..", "/b/c.js" ], "/x/b/c.js" )
        , ( test "4", [ "/foo", "../../../bar" ], "/bar" )
        , ( test "5", [ "foo", "../../../bar" ], "../../bar" )
        , ( test "6", [ "foo/", "../../../bar" ], "../../bar" )
        , ( test "7", [ "foo/x", "../../../bar" ], "../bar" )
        , ( test "8", [ "foo/x", "./bar" ], "foo/x/bar" )
        , ( test "9", [ "foo/x/", "./bar" ], "foo/x/bar" )
        , ( test "10", [ "foo/x/", ".", "bar" ], "foo/x/bar" )
        , ( test "11", [ "./" ], "." )
        , ( test "12", [ ".", "./" ], "." )
        , ( test "13", [ ".", ".", "." ], "." )
        , ( test "14", [ ".", "./", "." ], "." )
        , ( test "15", [ ".", "/./", "." ], "." )
        , ( test "16", [ ".", "/////./", "." ], "." )
        , ( test "17", [ "." ], "." )
        , ( test "18", [ "", "." ], "." )
        , ( test "19", [ "", "foo" ], "foo" )
        , ( test "20", [ "foo", "/bar" ], "foo/bar" )
        , ( test "21", [ "", "/foo" ], "/foo" )
        , ( test "22", [ "", "", "/foo" ], "/foo" )
        , ( test "23", [ "", "", "foo" ], "foo" )
        , ( test "24", [ "foo", "" ], "foo" )
        , ( test "25", [ "foo/", "" ], "foo" )
        , ( test "26", [ "foo", "", "/bar" ], "foo/bar" )
        , ( test "27", [ "./", "..", "/foo" ], "../foo" )
        , ( test "28", [ "./", "..", "..", "/foo" ], "../../foo" )
        , ( test "29", [ ".", "..", "..", "/foo" ], "../../foo" )
        , ( test "30", [ "", "..", "..", "/foo" ], "../../foo" )
        , ( test "31", [ "/" ], "/" )
        , ( test "32", [ "/", "." ], "/" )
        , ( test "33", [ "/", ".." ], "/" )
        , ( test "34", [ "/", "..", ".." ], "/" )
        , ( test "35", [ "" ], "." )
        , ( test "36", [ "", "" ], "." )
        , ( test "37", [ "", "/foo" ], "/foo" )
        , ( test "38", [ " ", "foo" ], " /foo" )
        , ( test "39", [ " ", "." ], " " )
        , ( test "40", [ " ", "/" ], " " ) -- This slash technically becomes a trailing slash so is dropped
        , ( test "41", [ " ", "" ], " " )
        , ( test "42", [ "/", "foo" ], "/foo" )
        , ( test "43", [ "/", "/foo" ], "/foo" )
        , ( test "44", [ "/", "//foo" ], "/foo" )
        , ( test "45", [ "/", "", "/foo" ], "/foo" )
        , ( test "46", [ "", "/", "foo" ], "/foo" )
        , ( test "47", [ "", "/", "/foo" ], "/foo" )
        ]
