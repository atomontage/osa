-- This is free and unencumbered software released into the public domain.

on terminal(p as string)
-- Open a new Terminal.app window and cd to p (if p is a directory)
-- or cd to directory of p (if p is a file)

    tell application "Finder"
        try
            set p to (p as POSIX file)
            
            if kind of (info for p without size) is "folder" then
                set d to p
            else
                set p to p as alias
                set d to (get container of p) as text
            end if            
        on error
            set d to ""
        end try

        tell application "Terminal"
            if d is "" then               
                do script ""
            else
                do script "cd " & (quoted form of POSIX path of d)
            end if
            activate
        end tell
    end tell

    if d is "" then
        set d to (path to home folder)
    end if

    return (POSIX path of d) as text
end terminal
