module PathWin32Tests exposing (suite)

import Expect
import Path
import Path.Win32 as Win32
import Test exposing (Test, describe, test)


testFromString : String -> Result String String
testFromString input =
    Win32.fromString input
        |> Result.map Path.toString


suite : Test
suite =
    describe "Path.Win32"
        [ describe "fromString"
            [ test "1" <| \() -> Expect.equal (Ok "fixtures\\b\\c.js") (testFromString "./fixtures///b/../b/c.js")
            , test "2" <| \() -> Expect.equal (Ok "\\bar") (testFromString "/foo/../../../bar")
            , test "3" <| \() -> Expect.equal (Ok "a\\b") (testFromString "a//b//../b")
            , test "4" <| \() -> Expect.equal (Ok "a\\b\\c") (testFromString "a//b//./c")
            , test "5" <| \() -> Expect.equal (Ok "a\\b") (testFromString "a//b//.")
            , test "6" <| \() -> Expect.equal (Ok "\\\\server\\share\\dir\\file.ext") (testFromString "//server/share/dir/file.ext")
            , test "7" <| \() -> Expect.equal (Ok "\\x\\y\\z") (testFromString "/a/b/c/../../../x/y/z")
            , test "8" <| \() -> Expect.equal (Ok "C:.") (testFromString "C:")
            , test "9" <| \() -> Expect.equal (Ok "C:..\\abc") (testFromString "C:..\\abc")
            , test "10" <| \() -> Expect.equal (Ok "C:..\\..\\def") (testFromString "C:..\\..\\abc\\..\\def")
            , test "11" <| \() -> Expect.equal (Ok "C:\\") (testFromString "C:\\.")
            , test "12" <| \() -> Expect.equal (Ok "file:stream") (testFromString "file:stream")
            , test "13" <| \() -> Expect.equal (Ok "bar") (testFromString "bar\\foo..\\..\\")
            , test "14" <| \() -> Expect.equal (Ok "bar") (testFromString "bar\\foo..\\..")
            , test "15" <| \() -> Expect.equal (Ok "bar\\baz") (testFromString "bar\\foo..\\..\\baz")
            , test "16" <| \() -> Expect.equal (Ok "bar\\foo..") (testFromString "bar\\foo..\\")
            , test "17" <| \() -> Expect.equal (Ok "bar\\foo..") (testFromString "bar\\foo..")
            , test "18" <| \() -> Expect.equal (Ok "..\\..\\bar") (testFromString "..\\foo..\\..\\..\\bar")
            , test "20" <| \() -> Expect.equal (Ok "..\\..\\..\\..\\..\\bar") (testFromString "../../../foo/../../../bar")
            , test "21" <| \() -> Expect.equal (Ok "..\\..\\..\\..\\..\\..") (testFromString "../../../foo/../../../bar/../../")
            , test "22" <| \() -> Expect.equal (Ok "..\\..") (testFromString "../foobar/barfoo/foo/../../../bar/../../")
            , test "24" <| \() -> Expect.equal (Ok "foo\\bar\\baz") (testFromString "foo/bar\\baz")
            , test "25" <| \() -> Expect.equal (Ok "A\\B") (testFromString "A//////././//.//B")
            , test "26" <| \() -> Expect.equal (Ok "A\\B") (testFromString "A/./B")
            , test "27" <| \() -> Expect.equal (Ok "A\\B") (testFromString "A/foo/../B")
            , test "28" <| \() -> Expect.equal (Ok "C:A\\B") (testFromString "C:A//B")
            , test "29" <| \() -> Expect.equal (Ok "D:A\\B") (testFromString "D:A/./B")
            , test "30" <| \() -> Expect.equal (Ok "e:A\\B") (testFromString "e:A/foo/../B")
            , test "31" <| \() -> Expect.equal (Ok "C:\\A\\B") (testFromString "C:///A//B")
            , test "32" <| \() -> Expect.equal (Ok "D:\\A\\B") (testFromString "D:///A/./B")
            , test "33" <| \() -> Expect.equal (Ok "e:\\A\\B") (testFromString "e:///A/foo/../B")
            , test "34" <| \() -> Expect.equal (Ok "..") (testFromString "..")
            , test "35" <| \() -> Expect.equal (Ok ".") (testFromString ".")
            , test "36" <| \() -> Expect.equal (Ok ".") (testFromString "")
            , test "37" <| \() -> Expect.equal (Ok "\\") (testFromString "/")
            , test "38" <| \() -> Expect.equal (Ok "c:\\") (testFromString "c:/")
            , test "39" <| \() -> Expect.equal (Ok "\\") (testFromString "/../.././..")
            , test "40" <| \() -> Expect.equal (Ok "c:\\") (testFromString "c:/../../..")
            , test "41" <| \() -> Expect.equal (Ok "..\\..\\..") (testFromString "../.././..")
            , test "42" <| \() -> Expect.equal (Ok "K:..\\..\\..") (testFromString "K:../.././..")
            , test "43" <| \() -> Expect.equal (Ok "C:\\a\\b") (testFromString "C:////a/b")
            , test "44" <| \() -> Expect.equal (Ok "\\\\machine\\share\\a\\b") (testFromString "//machine/share//a/b")
            , test "45" <| \() -> Expect.equal (Ok "\\\\.\\NUL") (testFromString "\\\\.\\NUL")
            , test "46" <| \() -> Expect.equal (Ok "\\\\?\\D:/XY\\Z") (testFromString "\\\\?\\D:/XY\\Z")
            , test "47" <| \() -> Expect.equal (Ok "C:\\") (testFromString "C:\\\\\\\\")
            ]
        ]
