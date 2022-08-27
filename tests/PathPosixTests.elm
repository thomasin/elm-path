module PathPosixTests exposing (suite)

import Expect
import Path
import Path.Posix as Posix
import Test exposing (Test, describe, test)


testFromString : String -> Result String String
testFromString input =
    Posix.fromString input
        |> Result.map Path.toString


suite : Test
suite =
    describe "Path.Posix"
        [ describe "fromString"
            [ test "1" <| \() -> Expect.equal (Ok "fixtures/b/c.js") (testFromString "./fixtures///b/../b/c.js")
            , test "2" <| \() -> Expect.equal (Ok "/bar") (testFromString "/foo/../../../bar")
            , test "3" <| \() -> Expect.equal (Ok "a/b") (testFromString "a//b//../b")
            , test "4" <| \() -> Expect.equal (Ok "a/b/c") (testFromString "a//b//./c")
            , test "5" <| \() -> Expect.equal (Ok "a/b") (testFromString "a//b//.")
            , test "6" <| \() -> Expect.equal (Ok "/x/y/z") (testFromString "/a/b/c/../../../x/y/z")
            , test "7" <| \() -> Expect.equal (Ok "/foo/bar") (testFromString "///..//./foo/.//bar")
            , test "8" <| \() -> Expect.equal (Ok "bar") (testFromString "bar/foo../../")
            , test "9" <| \() -> Expect.equal (Ok "bar") (testFromString "bar/foo../..")
            , test "10" <| \() -> Expect.equal (Ok "bar/baz") (testFromString "bar/foo../../baz")
            , test "11" <| \() -> Expect.equal (Ok "bar/foo..") (testFromString "bar/foo../")
            , test "12" <| \() -> Expect.equal (Ok "bar/foo..") (testFromString "bar/foo..")
            , test "13" <| \() -> Expect.equal (Ok "../../bar") (testFromString "../foo../../../bar")
            , test "14" <| \() -> Expect.equal (Ok "../../bar") (testFromString "../.../.././.../../../bar")
            , test "15" <| \() -> Expect.equal (Ok "../../../../../bar") (testFromString "../../../foo/../../../bar")
            , test "16" <| \() -> Expect.equal (Ok "../../../../../..") (testFromString "../../../foo/../../../bar/../../")
            , test "17" <| \() -> Expect.equal (Ok "../..") (testFromString "../foobar/barfoo/foo/../../../bar/../../")
            , test "18" <| \() -> Expect.equal (Ok "../../../../baz") (testFromString "../.../../foobar/../../../bar/../../baz")
            , test "19" <| \() -> Expect.equal (Ok "foo/bar\\baz") (testFromString "foo/bar\\baz")
            , test "20" <| \() -> Expect.equal (Ok ".") (testFromString "")
            , test "21" <| \() -> Expect.equal (Ok "/") (testFromString "/")
            , test "22" <| \() -> Expect.equal (Ok "//") (testFromString "//")
            , test "23" <| \() -> Expect.equal (Ok "/") (testFromString "///")
            , test "24" <| \() -> Expect.equal (Ok "/foo/bar") (testFromString "///foo/.//bar//")
            , test "25" <| \() -> Expect.equal (Ok "/foo/baz") (testFromString "///foo/.//bar//.//..//.//baz")
            , test "26" <| \() -> Expect.equal (Ok "/foo/bar") (testFromString "///..//./foo/.//bar")
            , test "27" <| \() -> Expect.equal (Ok ".") (testFromString "")
            , test "28" <| \() -> Expect.equal (Ok "/") (testFromString "/")
            , test "29" <| \() -> Expect.equal (Ok "//") (testFromString "//")
            , test "30" <| \() -> Expect.equal (Ok "/") (testFromString "///")
            , test "31" <| \() -> Expect.equal (Ok "/foo/bar") (testFromString "///foo/.//bar//")
            , test "32" <| \() -> Expect.equal (Ok "/foo/baz") (testFromString "///foo/.//bar//.//..//.//baz")
            , test "33" <| \() -> Expect.equal (Ok "/foo/bar") (testFromString "///..//./foo/.//bar")
            , test "34" <| \() -> Expect.equal (Ok "//foo/bar") (testFromString "//foo//bar")
            ]
        ]
