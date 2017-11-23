with Boards; use Boards;
with Processors.Registers; use Processors.Registers;

package body Processors.Machinegunners is
    procedure Set_Machinegunner_Registers (
        Machines : in out Processor_Array;
        State : in Board) is
        procedure Set_Team_Side_Machinegunner_Registers (
            Team : in Player_ID;
            Side : in Board_Side) is
            Pos : Location :=
                Get_Unit (State, Machinegunner_IDs (Side), Team).Position;
            Them : Player_ID := Enemy_Of (Team);
            Nearest_Ally_Pos : Location :=
                Get_Nearest_Ally (State, Pos, Them).Position;
        begin
            Machines (Team, Machinegunner_IDs (Side)).Registers (16 .. 31) := (
                16 => From_Boolean (
                    Get_Unit (State, Machinegunner_IDs (Side), Team).Setup),
                17 => Register_Type (Nearest_Ally_Pos (Team).X),
                18 => Register_Type (Nearest_Ally_Pos (Team).Y),
                19 => Register_Type (Compute_Fire_Time (
                    State, Machinegunner_IDs (Side), Pos, Nearest_Ally_Pos)),
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
                    Count_Nearby_Enemies (State, Pos, Them, 1)),
                27 => Register_Type (
                    Count_Nearby_Enemies (State, Pos, Them, 2)),
                28 => Register_Type (
                    Count_Nearby_Enemies (State, Pos, Them, 3)),
                29 => Register_Type (
                    Count_Nearby_Enemies (State, Pos, Them, 4)),
                30 => Register_Type (
                    Count_Nearby_Enemies (State, Pos, Them, 5)),
                31 => Register_Type (
                    Count_Nearby_Enemies (State, Pos, Them, 6)));
        end Set_Team_Side_Machinegunner_Registers;
    begin
        for T in Player_ID'Range loop
            for S in Board_Side'Range loop
                Set_Team_Side_Machinegunner_Registers (T, S);
            end loop;
        end loop;
    end Set_Machinegunner_Registers;
end Processors.Machinegunners;
