package Getopt is
    function Option_Argument return String;
    Option_Index : Positive := 1;
    Option_Error : Boolean := True; -- True means to show default errors
    Option_Option : Character;

    -- Not POSIX compliant - returns NUL instead of -1 at end
    Option_End : constant Character := Character'First;
    Option_Long : constant Character := Character'Last;
    Option_Missing : constant Character := ':';
    Option_Unknown : constant Character := '?';
    function Get_Option (Option_String : String) return Character;
end Getopt;
