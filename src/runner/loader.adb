with Boards; use Boards;
with Processors; use Processors;

with Ada.Sequential_IO;

package body Loader is
    package Register_IO is new Ada.Sequential_IO (Register_Type);
    use Register_IO;

    procedure Load_Game (
        The_Game : in out Games.Game;
        White : in String;
        Black : in String) is
        -- Loads the 'next' memory array from the file
        procedure Load_Memory_Array_From_File (
            File : in File_Type;
            Unit : in Unit_Type;
            Team : in Player_ID) is
            Length : Register_Type;
        begin
            Read (File, Length);

            declare
                Result : Memory_Array (0 .. Address_Type (Length));
            begin
                for I in Result'Range loop
                    Read (File, Result (I));
                end loop;

                Games.Load_Code (The_Game, Unit, Team, Result);
            end;
        end Load_Memory_Array_From_File;

        Current : File_Type;
    begin
        Open (Current, In_File, White);
        for U in Unit_Type'Range loop
            Load_Memory_Array_From_File (Current, U, T_WHITE);
        end loop;
        Close (Current);
        Open (Current, In_File, Black);
        for U in Unit_Type'Range loop
            Load_Memory_Array_From_File (Current, U, T_BLACK);
        end loop;
        Close (Current);
    end Load_Game;
end Loader;
