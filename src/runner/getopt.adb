with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO; use Ada.Text_IO;

package body Getopt is
    Option_Argument_Value : Unbounded_String;

    -- Which option of the current argument we are on -abc has a:1, b:2, c:3
    Current_Option : Positive := 1;

    function Option_Argument return String is
    begin
        return To_String (Option_Argument_Value);
    end Option_Argument;

    function Get_Option (Option_String : String) return Character is
        procedure Next_Option (Current : String) is
        begin
            if Current'First + Current_Option < Current'Last then
                Current_Option := Current_Option + 1;
            else
                Current_Option := 1;
                Option_Index := Option_Index + 1;
            end if;
        end Next_Option;
    begin
        if Argument_Count = 0 or Option_Index > Argument_Count then
            return Option_End;
        end if;

        declare
            Current : String := Argument (Option_Index);
        begin
            if Current'Length = 1 then
                return Option_End;
            end if;

            if Current (Current'First + 1) = '-' then
                if Current'Length = 2 then
                    Option_Index := Option_Index + 1;
                    return Option_End;
                else
                    return Option_Long;
                end if;
            end if;

            for I in Option_String'Range loop
                if
                    Option_String (I) =
                        Current (Current'First + Current_Option)
                then
                    if
                        I < Option_String'Length and then
                        Option_String (I + 1) = ':'
                    then
                        -- Has argument
                        if Current'First + Current_Option < Current'Last then
                            -- Glued argument -xvalue
                            Option_Argument_Value := To_Unbounded_String (
                                Current (
                                    Current'First + Current_Option + 1 ..
                                    Current'Last));
                            Option_Index := Option_Index + 1;
                            return Option_String (I);
                        elsif Option_Index < Argument_Count then
                            -- Separate argument -x value
                            Current_Option := 1;
                            Option_Index := Option_Index + 1;
                            Option_Argument_Value := To_Unbounded_String (
                                Argument (Option_Index));
                            Option_Index := Option_Index + 1;
                            return Option_String (I);
                        else
                            -- Missing argument
                            Option_Index := Option_Index + 1;
                            Option_Option := Option_String (I);

                            if
                                Option_Error and
                                Option_String (Option_String'First) /= ':'
                            then
                                Put_Line (Command_Name & " -" &
                                    Option_String (I .. I) &
                                    " requires an argument.");
                            end if;

                            if Option_String (Option_String'First) = ':' then
                                return ':';
                            else
                                return '?';
                            end if;
                        end if;
                    else
                        -- No argument
                        Next_Option (Current);
                        return Option_String (I);
                    end if;
                end if;
            end loop;

            Option_Option := Current (Current'First + Current_Option);
            Next_Option (Current);

            return '?';
        end;
    end Get_Option;
end Getopt;
