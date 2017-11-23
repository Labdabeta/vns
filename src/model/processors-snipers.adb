with Boards; use Boards;

package body Processors.Snipers is
    procedure Set_Sniper_Registers (
        Machines : in out Processor_Array;
        State : in Boards.Board) is
        procedure Set_Team_Sniper_Registers (Team : in Player_ID) is
            My_Pos : Location := Get_Unit (State, UT_SNIPER, Team).Position;
            Nearest_Enemy : Unit_State := Get_Nearest_Ally (
                State, My_Pos, Enemy_Of (Team));
            Enemy_Captain : Unit_State := Get_Unit (
                State, UT_CAPTAIN, Enemy_Of (Team));
        begin
            Machines (Team, UT_SNIPER).Registers (17 .. 23) := (
                17 => Register_Type (
                    Count_Nearby_Enemies (State, My_Pos, Team, 1)),
                18 => Register_Type (
                    Count_Nearby_Enemies (State, My_Pos, Team, 2)),
                19 => Register_Type (
                    Count_Nearby_Enemies (State, My_Pos, Team, 3)),
                20 => Machines (Team, UT_SCOUT_SS).Registers (18),
                21 => Machines (Team, UT_SCOUT_SS).Registers (19),
                22 => Machines (Team, UT_SCOUT_FS).Registers (18),
                23 => Machines (Team, UT_SCOUT_FS).Registers (19));
            Machines (Team, UT_SNIPER).Registers (28 .. 31) := (
                28 => Register_Type (Nearest_Enemy.Position (Team).X),
                29 => Register_Type (Nearest_Enemy.Position (Team).Y),
                30 => Register_Type (Enemy_Captain.Position (Team).X),
                31 => Register_Type (Enemy_Captain.Position (Team).Y));
        end Set_Team_Sniper_Registers;
    begin
        for T in Player_ID'Range loop
            Set_Team_Sniper_Registers (T);
        end loop;
    end Set_Sniper_Registers;
end Processors.Snipers;
