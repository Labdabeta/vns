
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

    function Generate_Search_Path (From : Coordinate) return Coordinate_Path is
        Result : Coordinate_Path (1 .. 511);
        Next : Positive := 1;
        Search_X, Search_Y : Integer;
        function Is_Valid (X, Y : Integer) return Boolean is
        begin
            return X >= 0 and Y >= 0 and X <= 31 and Y <= 15;
        end Is_Valid;
    begin
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

        return Result;
    end Generate_Search_Path;

    function Get_Path_To (
        Source, Destination : Coordinate)
        return Coordinate_Path is
        Delta_X : Integer := Integer (Destination.X) - Integer (Source.X);
        Delta_Y : Integer := Integer (Destination.Y) - Integer (Source.Y);
        type Integer_Coord is record
            X, Y : Integer;
        end record;
        type Integer_Path is array (Positive range <>) of Integer_Coord;
        function Octant_Zero (X0, Y0, X1, Y1 : Integer)
            return Integer_Path is
            DX : Integer := X1 - X0;
            DY : Integer := Y1 - Y0;
            D : Integer := 2 * DY - DX;
            Y : Integer := Y0;
            Result : Integer_Path (1 .. DX);
        begin
            for X in Integer range X0 .. X1 loop
                if X > X0 then
                    Result (X - X0) := (X, Y);
                end if;

                if D > 0 then
                    Y := Y + 1;
                    D := D - 2 * DX;
                end if;
                D := D + 2 * DY;
            end loop;
            return Result;
        end Octant_Zero;
    begin
        if Delta_X > 0 then
            if Delta_Y > 0 then
                if Delta_X > Delta_Y then
                    -- O0
                    declare
                        Result : Integer_Path := Octant_Zero (
                        Integer (Source.X), Integer (Source.Y),
                        Integer (Destination.X), Integer (Destination.Y));
                        Ret : Coordinate_Path (Result'First .. Result'Last);
                    begin
                        for Index in Result'Range loop
                            Ret (Index) := (
                                X_Coordinate (Result (Index).X),
                                Y_Coordinate (Result (Index).Y));
                        end loop;
                        return Ret;
                    end;
                else
                    -- O1
                    declare
                        Result : Integer_Path := Octant_Zero (
                            Integer (Source.Y), Integer (Source.X),
                            Integer (Destination.Y), Integer (Destination.X));
                        Ret : Coordinate_Path (Result'First .. Result'Last);
                    begin
                        for Index in Result'Range loop
                            Ret (Index) := (
                                X_Coordinate (Result (Index).Y),
                                Y_Coordinate (Result (Index).X));
                        end loop;
                        return Ret;
                    end;
                end if;
            else
                if Delta_X > -Delta_Y then
                    -- O7
                    declare
                        Result : Integer_Path := Octant_Zero (
                            Integer (Source.X), -Integer (Source.Y),
                            Integer (Destination.X), -Integer (Destination.Y));
                        Ret : Coordinate_Path (Result'First .. Result'Last);
                    begin
                        for Index in Result'Range loop
                            Ret (Index) := (
                                X_Coordinate (Result (Index).X),
                                Y_Coordinate (-Result (Index).Y));
                        end loop;
                        return Ret;
                    end;
                else
                    -- O6
                    declare
                        Result : Integer_Path := Octant_Zero (
                            -Integer (Source.Y), Integer (Source.X),
                            -Integer (Destination.Y), Integer (Destination.X));
                        Ret : Coordinate_Path (Result'First .. Result'Last);
                    begin
                        for Index in Result'Range loop
                            Ret (Index) := (
                                X_Coordinate (Result (Index).Y),
                                Y_Coordinate (-Result (Index).X));
                        end loop;
                        return Ret;
                    end;
                end if;
            end if;
        else
            if Delta_Y > 0 then
                if -Delta_X > Delta_Y then
                    -- O3
                    declare
                        Result : Integer_Path := Octant_Zero (
                            -Integer (Source.X), Integer (Source.Y),
                            -Integer (Destination.X), Integer (Destination.Y));
                        Ret : Coordinate_Path (Result'First .. Result'Last);
                    begin
                        for Index in Result'Range loop
                            Ret (Index) := (
                                X_Coordinate (-Result (Index).X),
                                Y_Coordinate (Result (Index).Y));
                        end loop;
                        return Ret;
                    end;
                else
                    -- O2
                    declare
                        Result : Integer_Path := Octant_Zero (
                            Integer (Source.Y), -Integer (Source.X),
                            Integer (Destination.Y), -Integer (Destination.X));
                        Ret : Coordinate_Path (Result'First .. Result'Last);
                    begin
                        for Index in Result'Range loop
                            Ret (Index) := (
                                X_Coordinate (-Result (Index).Y),
                                Y_Coordinate (Result (Index).X));
                        end loop;
                        return Ret;
                    end;
                end if;
            else
                if -Delta_X > -Delta_Y then
                    -- O4
                    declare
                        Result : Integer_Path := Octant_Zero (
                            -Integer (Source.X), -Integer (Source.Y),
                            -Integer (Destination.X), -Integer (Destination.Y));
                        Ret : Coordinate_Path (Result'First .. Result'Last);
                    begin
                        for Index in Result'Range loop
                            Ret (Index) := (
                                X_Coordinate (-Result (Index).X),
                                Y_Coordinate (-Result (Index).Y));
                        end loop;
                        return Ret;
                    end;
                else
                    -- O5
                    declare
                        Result : Integer_Path := Octant_Zero (
                            -Integer (Source.Y), -Integer (Source.X),
                            -Integer (Destination.Y), -Integer (Destination.X));
                        Ret : Coordinate_Path (Result'First .. Result'Last);
                    begin
                        for Index in Result'Range loop
                            Ret (Index) := (
                                X_Coordinate (-Result (Index).Y),
                                Y_Coordinate (-Result (Index).X));
                        end loop;
                        return Ret;
                    end;
                end if;
            end if;
        end if;
    end Get_Path_To;
end Coordinates;
