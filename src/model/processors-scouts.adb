with Boards; use Boards;
with Coordinates; use Coordinates;
with Processors.Registers; use Processors.Registers;

package body Processors.Scouts is
    procedure Set_Scout_Registers (
        Machines : in out Processor_Array;
        State : in Boards.Board) is
        procedure Set_Team_Side_Scout_Registers (
            Team : in Player_ID;
            Side : in Board_Side) is
            Pos : Location := Get_Unit (State, Scout_IDs (Side), Team).Position;
            Them : Player_ID := Enemy_Of (Team);
            Nearest_Enemies : array (1 .. 5) of Coordinate := (
                1 => Get_Nearest_Ally (State, Pos, Them, 0).Position (Team),
                2 => Get_Nearest_Ally (State, Pos, Them, 1).Position (Team),
                3 => Get_Nearest_Ally (State, Pos, Them, 2).Position (Team),
                4 => Get_Nearest_Ally (State, Pos, Them, 3).Position (Team),
                5 => Get_Nearest_Ally (State, Pos, Them, 4).Position (Team));
            Sniper_Idx : Register_Index;
        begin
            if Side = SNIPER_SIDE then
                Sniper_Idx := 24;
            else
                Sniper_Idx := 26;
            end if;
            Machines (Team, Scout_IDs (Side)).Registers (16 .. 17) := (
                16 => From_Boolean (Get_Unit (State, UT_SNIPER, Team).Setup),
                17 => From_Boolean (
                    Get_Unit (State, UT_SNIPER, Enemy_Of (Team)).Setup));
            Machines (Team, Scout_IDs (Side)).Registers (20 .. 31) := (
                20 => Machines (Team, UT_SNIPER).Registers (Sniper_Idx),
                21 => Machines (Team, UT_SNIPER).Registers (Sniper_Idx + 1),
                22 => Register_Type (Nearest_Enemies (1).X),
                23 => Register_Type (Nearest_Enemies (1).Y),
                24 => Register_Type (Nearest_Enemies (2).X),
                25 => Register_Type (Nearest_Enemies (2).Y),
                26 => Register_Type (Nearest_Enemies (3).X),
                27 => Register_Type (Nearest_Enemies (3).Y),
                28 => Register_Type (Nearest_Enemies (4).X),
                29 => Register_Type (Nearest_Enemies (4).Y),
                30 => Register_Type (Nearest_Enemies (5).X),
                31 => Register_Type (Nearest_Enemies (5).Y));
        end Set_Team_Side_Scout_Registers;
    begin
        for T in Player_ID'Range loop
            for S in Board_Side'Range loop
                Set_Team_Side_Scout_Registers (T, S);
            end loop;
        end loop;
    end Set_Scout_Registers;
end Processors.Scouts;
