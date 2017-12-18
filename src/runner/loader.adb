with Boards; use Boards;
with Processors; use Processors;

with Ada.Sequential_IO;

package body Loader is
    package Register_IO is new Ada.Sequential_IO (Register_Type);
    use Register_IO;

    procedure Load_Team (
        The_Game : in out Games.Game;
        File : in String;
        Team : in Boards.Player_ID) is
        -- Loads the 'next' memory array from the file
        procedure Load_Memory_Array_From_File (
            File : in File_Type;
            Unit : in Unit_Type) is
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
        Open (Current, In_File, File);
        for U in Unit_Type'Range loop
            Load_Memory_Array_From_File (Current, U);
        end loop;
        Close (Current);
    end Load_Team;
end Loader;
