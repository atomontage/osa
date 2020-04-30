-- This is free and unencumbered software released into the public domain.

on show_finder(p as string)
-- Open a Finder window either showing the contents of p (p is a directory)
-- or showing the contents of its directory with p selected (p is a file).

    if p is "" then
        set p to POSIX path of (path to home folder)
    end if

    set p to (p as POSIX file)
    
    tell application "Finder"        
        if kind of (info for p without size) is "folder" then
            open p as text
        else
            reveal p as text
        end if
        activate
    end tell
    
    return (POSIX path of p) as text
end show_finder
