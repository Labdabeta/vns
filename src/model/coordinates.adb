with Ada.Text_IO;

package body Coordinates is
    function Get_Direction_Towards (
        Source, Destination : Coordinate)
        return Direction is
        DX : Integer := Integer (Destination.X) - Integer (Source.X);
        DY : Integer := Integer (Destination.Y) - Integer (Source.Y);
    begin
        if DX > 0 then
            if DY > 0 then
                return DIR_SOUTH_EAST;
            elsif DY < 0 then
                return DIR_NORTH_EAST;
            else
                return DIR_EAST;
            end if;
        elsif DX < 0 then
            if DY > 0 then
                return DIR_SOUTH_WEST;
            elsif DY < 0 then
                return DIR_NORTH_WEST;
            else
                return DIR_WEST;
            end if;
        else
            if DY > 0 then
                return DIR_SOUTH;
            elsif DY < 0 then
                return DIR_NORTH;
            else
                -- Default case is a pseudo-retreat
                return DIR_WEST;
            end if;
        end if;
    end Get_Direction_Towards;

    procedure Apply_Direction (Source : in out Coordinate; Dir : in Direction)
    is begin
        case Dir is
            when DIR_NORTH =>
                if Source.Y /= Y_Coordinate'First then
                    Source.Y := Source.Y - 1;
                end if;
            when DIR_NORTH_EAST =>
                if Source.Y /= Y_Coordinate'First then
                    Source.Y := Source.Y - 1;
                end if;
                if Source.X /= X_Coordinate'Last then
                    Source.X := Source.X + 1;
                end if;
            when DIR_EAST =>
                if Source.X /= X_Coordinate'Last then
                    Source.X := Source.X + 1;
                end if;
            when DIR_SOUTH_EAST =>
                if Source.Y /= Y_Coordinate'Last then
                    Source.Y := Source.Y + 1;
                end if;
                if Source.X /= X_Coordinate'Last then
                    Source.X := Source.X + 1;
                end if;
            when DIR_SOUTH =>
                if Source.Y /= Y_Coordinate'Last then
                    Source.Y := Source.Y + 1;
                end if;
            when DIR_SOUTH_WEST =>
                if Source.Y /= Y_Coordinate'Last then
                    Source.Y := Source.Y + 1;
                end if;
                if Source.X /= X_Coordinate'First then
                    Source.X := Source.X - 1;
                end if;
            when DIR_WEST =>
                if Source.X /= X_Coordinate'First then
                    Source.X := Source.X - 1;
                end if;
            when DIR_NORTH_WEST =>
                if Source.Y /= Y_Coordinate'First then
                    Source.Y := Source.Y - 1;
                end if;
                if Source.X /= X_Coordinate'First then
                    Source.X := Source.X - 1;
                end if;
        end case;
    end Apply_Direction;

    function Adjacent (
        Source, Destination : Coordinate)
        return Boolean is
        DX : Integer := Integer (Source.X) - Integer (Destination.X);
        DY : Integer := Integer (Source.Y) - Integer (Destination.Y);
    begin
        return (DX = 1 and DY = 0) or (DX = -1 and DY = 0) or
               (DX = 0 and DY = 1) or (DX = 0 and DY = -1) or
               (DX = 0 and DY = 0);
    end Adjacent;

    type Saved_Search_Path is
        record
            Path : Coordinate_Path (1 .. 511);
            Valid : Boolean;
        end record;
    Saved_Search_Paths : array (X_Coordinate, Y_Coordinate) of
        Saved_Search_Path := (
            others => (others => (
                Path => (others => (0, 0)),
                Valid => False)));
    function Generate_Search_Path (From : Coordinate) return Coordinate_Path is
        Result : Coordinate_Path (1 .. 511);
        Next : Positive := 1;
        Search_X, Search_Y : Integer;
        function Is_Valid (X, Y : Integer) return Boolean is
        begin
            return X >= 0 and Y >= 0 and X <= 31 and Y <= 15;
        end Is_Valid;

        Cached : Saved_Search_Path := Saved_Search_Paths (From.X, From.Y);
    begin
        if Cached.Valid then
            return Cached.Path;
        end if;
        for Iteration in Positive range 1 .. 33 loop
            Search_X := Integer (From.X) - Iteration;
            Search_Y := Integer (From.Y) - Iteration;

            -- Scan the top line
            for I in Natural range 0 .. Iteration * 2 loop
                if Is_Valid (Search_X + I, Search_Y) then
                    Result (Next).X := X_Coordinate (Search_X + I);
                    Result (Next).Y := Y_Coordinate (Search_Y);
                    Next := Next + 1;
                end if;
            end loop;

            -- Scan the right line
            for I in Positive range 1 .. Iteration * 2 loop
                if Is_Valid (Search_X + 2 * Iteration, Search_Y + I) then
                    Result (Next).X := X_Coordinate (Search_X + 2 * Iteration);
                    Result (Next).Y := Y_Coordinate (Search_Y + I);
                    Next := Next + 1;
                end if;
            end loop;

            -- Scan the bottom line
            for I in reverse Natural range 0 .. Iteration * 2 - 1 loop
                if Is_Valid (Search_X + I, Search_Y + 2 * Iteration) then
                    Result (Next).X := X_Coordinate (Search_X + I);
                    Result (Next).Y := Y_Coordinate (Search_Y + 2 * Iteration);
                    Next := Next + 1;
                end if;
            end loop;

            -- Scan the left line
            for I in reverse Positive range 1 .. Iteration * 2 - 1 loop
                if Is_Valid (Search_X, Search_Y + I) then
                    Result (Next).X := X_Coordinate (Search_X);
                    Result (Next).Y := Y_Coordinate (Search_Y + I);
                    Next := Next + 1;
                end if;
            end loop;
        end loop;

        Saved_Search_Paths (From.X, From.Y) := (Result, True);
        return Result;
    end Generate_Search_Path;

    function Get_Path_To (
        Source, Destination : Coordinate)
        return Coordinate_Path is
        X0 : Integer := Integer (Source.X);
        X1 : Integer := Integer (Destination.X);
        Y0 : Integer := Integer (Source.Y);
        Y1 : Integer := Integer (Destination.Y);
        Delta_X : Integer := abs (X1 - X0);
        Delta_Y : Integer := abs (Y1 - Y0);
        Step_X : Integer;
        Step_Y : Integer;
        Error : Integer;
        Error2 : Integer;
        Path_Length : Positive;
        Empty_Result : Coordinate_Path (1 .. 0);
    begin
        if Source = Destination then
            return Empty_Result;
        end if;

        if Source.X < Destination.X then
            Step_X := 1;
        else
            Step_X := -1;
        end if;

        if Source.Y < Destination.Y then
            Step_Y := 1;
        else
            Step_Y := -1;
        end if;

        if Delta_X > Delta_Y then
            Path_Length := Positive (Delta_X);
            Error := Delta_X / 2;
        else
            Path_Length := Positive (Delta_Y);
            Error := -Delta_Y / 2;
        end if;

        declare
            Result : Coordinate_Path (1 .. Path_Length + 1);
            Next : Positive := 1;
        begin
            loop
                Result (Next) := (X_Coordinate (X0), Y_Coordinate (Y0));
                Next := Next + 1;

                exit when X0 = X1 and Y0 = Y1;
                Error2 := Error;

                if Error2 > -Delta_X then
                    Error := Error - Delta_Y;
                    X0 := X0 + Step_X;
                end if;

                if Error2 < Delta_Y then
                    Error := Error + Delta_X;
                    Y0 := Y0 + Step_Y;
                end if;
            end loop;

            return Result (2 .. Result'Last);
        end;
    end Get_Path_To;
end Coordinates;
