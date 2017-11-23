with Boards; use Boards;

package body Processors.Riflemen is
    procedure Set_Rifleman_Registers (
        Machines : in out Processor_Array;
        State : in Boards.Board) is
        procedure Set_Team_Side_Rifleman_Registers (
            Team : in Player_ID;
            Side : in Board_Side) is
            Pos : Location :=
                Get_Unit (State, Rifleman_IDs (Side), Team).Position;
        begin
            Machines (Team, Rifleman_IDs (Side)).Registers (20 .. 31) := (
                20 => Register_Type (
                    Count_Nearby_Enemies (State, Pos, Team, 1)),
                21 => Register_Type (
                    Count_Nearby_Enemies (State, Pos, Team, 2)),
                22 => Register_Type (
                    Count_Nearby_Enemies (State, Pos, Team, 3)),
                23 => Register_Type (
                    Count_Nearby_Enemies (State, Pos, Team, 4)),
                24 => Register_Type (
                    Count_Nearby_Enemies (State, Pos, Team, 5)),
                25 => Register_Type (
                    Count_Nearby_Enemies (State, Pos, Team, 6)),
                26 => Register_Type (
                    Count_Nearby_Enemies (State, Pos, Enemy_Of (Team), 1)),
                27 => Register_Type (
                    Count_Nearby_Enemies (State, Pos, Enemy_Of (Team), 2)),
                28 => Register_Type (
                    Count_Nearby_Enemies (State, Pos, Enemy_Of (Team), 3)),
                29 => Register_Type (
                    Count_Nearby_Enemies (State, Pos, Enemy_Of (Team), 4)),
                30 => Register_Type (
                    Count_Nearby_Enemies (State, Pos, Enemy_Of (Team), 5)),
                31 => Register_Type (
                    Count_Nearby_Enemies (State, Pos, Enemy_Of (Team), 6)));
        end Set_Team_Side_Rifleman_Registers;
    begin
        for T in Player_ID'Range loop
            for S in Board_Side'Range loop
                Set_Team_Side_Rifleman_Registers (T, S);
            end loop;
        end loop;
    end Set_Rifleman_Registers;
end Processors.Riflemen;
