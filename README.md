## File path parsing

Takes inspiration from both Python's os.path and Node's path modules.  
Turns strings and lists of strings into parsed and normalised paths.  
Supports Win32 and Posix, with drives, UNC paths, double backslash Posix paths, and normal absolute and relative paths.  
Quite minimal functionality, behaviour and docs so please raise issues and suggest improvements/additions (:  

```
Path.fromString (Path.Platform.fromSeparator "/") "/a/b/c/../../../x/y/z"
    |> Result.map Path.toString
    |> Expect.equal (Ok "/x/y/z")
```
