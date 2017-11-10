with Memory; use Memory;
with FS_Utils;

with Ada.Containers;
with Ada.Containers.Hashed_Maps;
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Hash;
with Ada.Strings.Maps.Constants;

with Ada.Text_IO;

package body Assembly is
    function Assemble (File : in String) return Memory.Memory_Access is
        Result : Memory.Memory_Access := new Memory.Memory_State;

        package String_Int_Maps is new Ada.Containers.Hashed_Maps (
            Key_Type => Unbounded_String,
            Element_Type => Address_Value,
            Hash => Ada.Strings.Unbounded.Hash,
            Equivalent_Keys => "=");
        Labels : String_Int_Maps.Map := String_Int_Maps.Empty_Map;

        type Missing_Label is record
            Address : Address_Value;
            Offset : Integer;
        end record;
        package Int_Vectors is new Ada.Containers.Vectors (
            Index_Type => Positive,
            Element_Type => Missing_Label);

        package String_Ints_Maps is new Ada.Containers.Hashed_Maps (
            Key_Type => Unbounded_String,
            Element_Type => Int_Vectors.Vector,
            Hash => Ada.Strings.Unbounded.Hash,
            Equivalent_Keys => "=",
            "=" => Int_Vectors."=");
        Missing_Labels : String_Ints_Maps.Map := String_Ints_Maps.Empty_Map;

        package String_String_Maps is new Ada.Containers.Hashed_Maps (
            Key_Type => Unbounded_String,
            Element_Type => Unbounded_String,
            Hash => Ada.Strings.Unbounded.Hash,
            Equivalent_Keys => "=");
        Substitutions : String_String_Maps.Map := String_String_Maps.Empty_Map;
        Unit : Unit_Type;

        ------------------------------------------------------------------------
        -- Lexer
        ------------------------------------------------------------------------
        type Terminal_Type is (
            SECTION, DIRECTIVE, IDENTIFIER, LITERAL, DOT, PLUS, MINUS,
            REGISTER_NAME, COLON, COMMA, EOF);

        type Terminal is record
            Kind : Terminal_Type;
            Data : Unbounded_String;
        end record;

        type File_State is record
            Name : Unbounded_String;
            Text : Unbounded_String;
            Index : Positive;
            Line : Positive;
            Column : Positive;
        end record;

        package FS_Vectors is new Ada.Containers.Vectors (
            Index_Type => Positive,
            Element_Type => File_State);
        Code : FS_Vectors.Vector := FS_Vectors.Empty_Vector;

        procedure Error (Comment : String) is
            Top_State : File_State;
        begin
            if FS_Vectors.Is_Empty (Code) then
                Ada.Text_IO.Put_Line ("error: <unknown location>| " & Comment);
            else
                Top_State := FS_Vectors.Last_Element (Code);
                Ada.Text_IO.Put_Line ("error: " &
                    To_String (Top_State.Name) & "|" &
                    Integer'Image (Top_State.Line) & " col " &
                    Integer'Image (Top_State.Column) & "| " & Comment);
            end if;
        end Error;

        procedure Open_File (Name : in String) is
            New_State : File_State;
        begin
            New_State.Name := To_Unbounded_String (Name);
            New_State.Text := To_Unbounded_String (
                FS_Utils.Read_Entire_File (Name));
            New_State.Index := 1;
            New_State.Line := 1;
            New_State.Column := 1;
            FS_Vectors.Append (Code, New_State);
        end Open_File;

        function Get_Next return Character is
            Result : Character;
            procedure Fetcher (What : in out File_State) is
            begin
                if What.Index <= Length (What.Text) then
                    Result := Element (What.Text, What.Index);
                    What.Index := What.Index + 1;
                else
                    Result := ASCII.NUL;
                end if;

                if Result = ASCII.LF then
                    What.Line := What.Line + 1;
                    What.Column := 1;
                else
                    What.Column := What.Column + 1;
                end if;
            end Fetcher;
        begin
            FS_Vectors.Update_Element (
                Code, FS_Vectors.Last (Code), Fetcher'Access);
            return Result;
        end Get_Next;

        function Check_Next return Character is
            Result : Character;
            procedure Fetcher (What : in out File_State) is
            begin
                if What.Index <= Length (What.Text) then
                    Result := Element (What.Text, What.Index);
                else
                    Result := ASCII.NUL;
                end if;
            end Fetcher;
        begin
            FS_Vectors.Update_Element (
                Code, FS_Vectors.Last (Code), Fetcher'Access);
            return Result;
        end Check_Next;

        function End_Of_Current return Boolean is
            Result : Boolean;
            procedure Fetcher (What : in out File_State) is
            begin
                if What.Index <= Length (What.Text) then
                    Result := False;
                else
                    Result := True;
                end if;
            end Fetcher;
        begin
            FS_Vectors.Update_Element (
                Code, FS_Vectors.Last (Code), Fetcher'Access);
            return Result;
        end End_Of_Current;

        Macros : String_String_Maps.Map := String_String_Maps.Empty_Map;

        procedure Lowerize (Str : in out Unbounded_String) is
        begin
            Str := Translate (Str, Ada.Strings.Maps.Constants.Lower_Case_Map);
        end Lowerize;

        -- Automatically removes comments, loads inline files, and applies
        -- 'macros'
        function Next_Terminal return Terminal is
            Next : Character;
            Str : Unbounded_String := To_Unbounded_String ("");
            Str2 : Unbounded_String := To_Unbounded_String ("");
        begin
            -- Keep reading until we have a terminal
            loop
                Str := To_Unbounded_String ("");
                Str2 := To_Unbounded_String ("");
                if FS_Vectors.Is_Empty (Code) then
                    return (EOF, To_Unbounded_String (""));
                end if;

                if End_Of_Current then
                    FS_Vectors.Delete_Last (Code);
                else
                    Next := Get_Next;
                    case Next is
                        when ASCII.LF | ' ' | ASCII.HT =>
                            null; -- Ignore newlines
                        when '.' =>
                            return (DOT, To_Unbounded_String ("."));
                        when '-' =>
                            return (MINUS, To_Unbounded_String ("-"));
                        when '+' =>
                            return (PLUS, To_Unbounded_String ("+"));
                        when '#' =>
                            loop
                                Next := Get_Next;
                                exit when Next = ASCII.LF;
                                Append (Str, Next);
                            end loop;
                            Open_File (To_String (Str));
                        when ';' =>
                            while Next /= ASCII.LF loop
                                Next := Get_Next;
                            end loop;
                        when ':' =>
                            return (COLON, To_Unbounded_String (":"));
                        when ',' =>
                            return (COMMA, To_Unbounded_String (","));
                        when '!' =>
                            loop
                                Next := Get_Next;

                                exit when not (
                                    (Next >= 'a' and Next <= 'z') or
                                    (Next >= 'A' and Next <= 'Z') or
                                    Next = '_');
                                Append (Str, Next);
                            end loop;

                            Lowerize (Str);

                            if
                                Str /= "captain" and
                                Str /= "mortar" and
                                Str /= "sniper" and
                                Str /= "engineer_ss" and
                                Str /= "engineer_fs" and
                                Str /= "mg_ss" and
                                Str /= "mg_fs" and
                                Str /= "scout_ss" and
                                Str /= "scout_fs" and
                                Str /= "rifleman_ss" and
                                Str /= "rifleman_fs"
                            then
                                Error ("unknown section " & To_String (Str));
                            else
                                return (SECTION, Str);
                            end if;
                        when '0' .. '9' =>
                            Append (Str, Next);
                            loop
                                Next := Check_Next;

                                exit when not (
                                    (Next >= '0' and Next <= '9') or
                                    (Next >= 'a' and Next <= 'f') or
                                    (Next >= 'A' and Next <= 'F') or
                                    Next = 'x' or Next = 'X');

                                Next := Get_Next;
                                Append (Str, Next);
                            end loop;

                            return (LITERAL, Str);
                        when 'a' .. 'z' | 'A' .. 'Z' | '_' =>
                            Append (Str, Next);
                            loop
                                Next := Check_Next;
                                exit when not (
                                    (Next >= 'a' and Next <= 'z') or
                                    (Next >= 'A' and Next <= 'Z') or
                                    (Next >= '0' and Next <= '9') or
                                    Next = '_');

                                Next := Get_Next;
                                Append (Str, Next);
                            end loop;

                            if Next = '=' then
                                Next := Get_Next;
                                loop
                                    Next := Check_Next;
                                    exit when not (
                                        (Next >= 'a' and Next <= 'z') or
                                        (Next >= 'A' and Next <= 'Z') or
                                        (Next >= '0' and Next <= '9') or
                                        Next = '_');

                                    Next := Get_Next;
                                    Append (Str2, Next);
                                end loop;

                                if String_String_Maps.Contains (Macros, Str)
                                then
                                    String_String_Maps.Replace (
                                        Macros, Str, Str2);
                                else
                                    String_String_Maps.Insert (
                                        Macros, Str, Str2);
                                end if;
                            else
                                -- Apply mappings
                                if String_String_Maps.Contains (Macros, Str)
                                then
                                    Str := String_String_Maps.Element (
                                        Macros, Str);
                                end if;

                                -- Determine if directive, identifier, literal,
                                -- or register.
                                if
                                    Element (Str, 1) >= '0' and
                                    Element (Str, 1) <= '9'
                                then
                                    return (LITERAL, Str);
                                else
                                    Lowerize (Str);
                                    if Is_Directive (To_String (Str), Unit) then
                                        return (DIRECTIVE, Str);
                                    elsif Is_Register (To_String (Str)) then
                                        return (REGISTER_NAME, Str);
                                    elsif Str = "raw" then
                                        return (DIRECTIVE, Str);
                                    else
                                        return (IDENTIFIER, Str);
                                    end if;
                                end if;
                            end if;
                        when others =>
                            Error ("unexpected symbol: " & Next);
                    end case;
                end if;
            end loop;
        end Next_Terminal;

        function Peek_Terminal return Terminal is
            Saved_State : FS_Vectors.Vector := Code;
            Ret : Terminal;
        begin
            Ret := Next_Terminal;
            Code := Saved_State;
            return Ret;
        end Peek_Terminal;

        ------------------------------------------------------------------------
        -- Recursive Descent Parser Functions
        ------------------------------------------------------------------------
        Current : Terminal;

        procedure Label is
            Label_Name : Unbounded_String := Current.Data;
        begin
            Current := Next_Terminal;
            if Current.Kind /= COLON then
                Error ("expected ':' to complete label.");
                return;
            end if;

            String_Int_Maps.Insert (Labels, Label_Name, Result.Lengths (Unit));
            if String_Ints_Maps.Contains (Missing_Labels, Label_Name) then
                for Cursor in Int_Vectors.Iterate (
                    String_Ints_Maps.Element (Missing_Labels, Label_Name)) loop
                    Set_Address (
                        Result.Data (Unit) (
                            Int_Vectors.Element (Cursor).Address),
                        Result.Lengths (Unit));
                end loop;
                String_Ints_Maps.Delete (
                    Missing_Labels,
                    Label_Name);
            end if;
            Current := Next_Terminal;
        end Label;

        function Modifier return Integer is
            Ret : Integer := 0;
        begin
            if Current.Kind = PLUS then
                Current := Next_Terminal;
                if Current.Kind /= LITERAL then
                    Error ("literal offset expected");
                else
                    Ret := Integer'Value (To_String (Current.Data));
                end if;
                Current := Next_Terminal;
            elsif Current.Kind = MINUS then
                Current := Next_Terminal;
                if Current.Kind /= LITERAL then
                    Error ("literal offset expected");
                else
                    Ret := -Integer'Value (To_String (Current.Data));
                end if;
                Current := Next_Terminal;
            end if;
            return Ret;
        end Modifier;

        procedure Set_Label (To : Unbounded_String) is
            -- Because for some reason To is passed by reference
            -- Thus Next_Terminal breaks it after keychecking
            Loc : String := To_String (To);
        begin
            if String_Int_Maps.Contains (Labels, To_Unbounded_String (Loc)) then
                Current := Next_Terminal;
                Set_Address (
                    Result.Data (Unit) (Result.Lengths (Unit)),
                    Address_Value (
                        Integer (String_Int_Maps.Element (
                            Labels, To_Unbounded_String (Loc))) + Modifier));
            else
                if String_Ints_Maps.Contains (Missing_Labels,
                    To_Unbounded_String (Loc))
                then
                    declare
                        procedure Update_Missing_Label (
                            Key : in Unbounded_String;
                            Element : in out Int_Vectors.Vector) is
                        begin
                            Int_Vectors.Append (
                                Element,
                                (Result.Lengths (Unit), Modifier));
                        end Update_Missing_Label;
                    begin
                        String_Ints_Maps.Update_Element (
                            Missing_Labels,
                            String_Ints_Maps.Find (
                                Missing_Labels, To_Unbounded_String (Loc)),
                            Update_Missing_Label'Access);
                    end;
                else
                    Current := Next_Terminal;
                    String_Ints_Maps.Insert (
                        Missing_Labels,
                        To_Unbounded_String (Loc),
                        Int_Vectors.To_Vector (
                            (Result.Lengths (Unit), Modifier), 1));
                end if;
            end if;
        end Set_Label;

        procedure Extra_Args (
            Value : in out Cell;
            Already_Immediated : Boolean := False) is
            Contents : Unbounded_String := Current.Data;
        begin
            if Current.Kind = LITERAL then
                Current := Next_Terminal;
                if Current.Kind = COMMA then
                    if Already_Immediated then
                        Error ("only one literal allowed per instruction");
                        Current := Next_Terminal;
                        return;
                    end if;
                    -- Small immediate
                    Set_Immediate (Value, String_To_Immediate (
                        To_String (Contents)));
                    Value.B := 12;
                    Current := Next_Terminal;
                    if Current.Kind = LITERAL then
                        Error ("only one literal allowed per instruction.");
                        return;
                    elsif Current.Kind = REGISTER_NAME then
                        Value.C := String_To_Register (
                            To_String (Current.Data));
                        Current := Next_Terminal;
                    end if;
                else
                    -- Address immediate
                    Set_Address (Value, String_To_Address (
                        To_String (Contents)));
                end if;
            elsif Current.Kind = REGISTER_NAME then
                Value.B := String_To_Register (To_String (Contents));
                Current := Next_Terminal;
                if Current.Kind /= COMMA then
                    Error ("only two arguments provided (need 1 or 3).");
                end if;
                Current := Next_Terminal;
                if Current.Kind = LITERAL then
                    if Already_Immediated then
                        Error ("only one literal allowed per instruction");
                        Current := Next_Terminal;
                        return;
                    end if;
                    Set_Immediate (Value, String_To_Immediate (
                        To_String (Current.Data)));
                    Value.C := 12;
                    Current := Next_Terminal;
                elsif Current.Kind = REGISTER_NAME then
                    Value.C := String_To_Register (To_String (Current.Data));
                    Current := Next_Terminal;
                else
                    Error ("expected argument type.");
                end if;
            elsif Current.Kind = IDENTIFIER then
                Set_Label (Current.Data);
            elsif Current.Kind = DOT then
                Current := Next_Terminal;
                Set_Address (Value,
                    Address_Value (
                        Integer (Result.Lengths (Unit)) + Modifier));
            else
                Error ("expected extra argument.");
            end if;
        end Extra_Args;

        procedure Register_Args (Value : in out Cell) is
        begin
            Value.A := String_To_Register (To_String (Current.Data));
            Current := Next_Terminal;

            if Current.Kind = COMMA then
                Current := Next_Terminal;
                Extra_Args (Value);
            end if;
        end Register_Args;

        procedure Args (Value : in out Cell) is
        begin
            if Current.Kind = REGISTER_NAME then
                Register_Args (Value);
            elsif Current.Kind = LITERAL then
                Set_Address (
                    Value,
                    String_To_Address (To_String (Current.Data)));
                Current := Next_Terminal;
                if Current.Kind = COMMA then
                    Current := Next_Terminal;
                    Extra_Args (Value, True);
                end if;
            elsif Current.Kind = IDENTIFIER then
                Set_Label (Current.Data);
            elsif Current.Kind = DOT then
                Current := Next_Terminal;
                Set_Address (Value,
                    Address_Value (
                        Integer (Result.Lengths (Unit)) + Modifier));
            else
                Error ("expected argument.");
            end if;
        end Args;

        procedure Data is
            Value : Cell := (0, 0, 0, 0, 0);
        begin
            if Current.Data = "raw" then
                Current := Next_Terminal;
                if Current.Kind /= LITERAL then
                    Error ("expected raw literal.");
                else
                    Result.Lengths (Unit) := Result.Lengths (Unit) + 1;
                    Result.Data (Unit) (Result.Lengths (Unit)) :=
                        String_To_Cell (To_String (Current.Data));
                end if;
            else
                Value.Instruction := String_To_Instruction (
                    To_String (Current.Data), Unit);
                Current := Next_Terminal;
                if Current.Kind = REGISTER_NAME or Current.Kind = LITERAL then
                    Args (Value);
                end if;

                -- Check if an identifier is empty directive + label or single
                -- argument labelled directive
                if Current.Kind = IDENTIFIER then
                    if Peek_Terminal.Kind /= COLON then
                        Args (Value);
                    end if;
                end if;

                Result.Lengths (Unit) := Result.Lengths (Unit) + 1;
                Result.Data (Unit) (Result.Lengths (Unit)) := Value;
            end if;
        end Data;

        procedure Line is
            Did_Something : Boolean := False;
        begin
            while Current.Kind = SECTION loop
                Unit := String_To_Unit (To_String (Current.Data));
                Current := Next_Terminal;
                Did_Something := True;
            end loop;

            if Current.Kind = IDENTIFIER then
                Label;
                Did_Something := True;
            end if;

            if Current.Kind = DIRECTIVE then
                Data;
                Did_Something := True;
            end if;

            if not Did_Something then
                Error ("got stuck on " & To_String (Current.Data) &
                    " attempting recovery...");
                Current := Next_Terminal;
            end if;
        end Line;

        procedure Program is
        begin
            Current := Next_Terminal;
            while Current.Kind /= EOF loop
                Line;
            end loop;
        end Program;

    begin
        Open_File (File);
        Result.Lengths := (others => 0);
        Program;
        for Cursor in String_Ints_Maps.Iterate (Missing_Labels) loop
            Error ("undefined reference to " &
                To_String (String_Ints_Maps.Key (Cursor)));
        end loop;
        return Result;
    end Assemble;
end Assembly;
