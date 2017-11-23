with Boards; use Boards;

package body Processors.Mortars is
    procedure Set_Mortar_Registers (
        Machines : in out Processor_Array;
        State : in Boards.Board) is
        procedure Set_Team_Mortar_Registers (Team : in Player_ID) is begin
            Machines (Team, UT_MORTAR).Registers (18 .. 31) := (
                18 => Register_Type (Get_Unit (
                    State, UT_SCOUT_SS, Enemy_Of (Team)).Position (Team).X),
                19 => Register_Type (Get_Unit (
                    State, UT_SCOUT_SS, Enemy_Of (Team)).Position (Team).Y),
                20 => Register_Type (Get_Unit (
                    State, UT_SCOUT_FS, Enemy_Of (Team)).Position (Team).X),
                21 => Register_Type (Get_Unit (
                    State, UT_SCOUT_FS, Enemy_Of (Team)).Position (Team).Y),
                22 => Register_Type (Get_Unit (
                    State, UT_CAPTAIN, Team).Position (Team).X),
                23 => Register_Type (Get_Unit (
                    State, UT_SNIPER, Team).Position (Team).X),
                24 => Register_Type (Get_Unit (
                    State, UT_ENGINEER_SS, Team).Position (Team).X),
                25 => Register_Type (Get_Unit (
                    State, UT_ENGINEER_FS, Team).Position (Team).X),
                26 => Register_Type (Get_Unit (
                    State, UT_MACHINEGUNNER_SS, Team).Position (Team).X),
                27 => Register_Type (Get_Unit (
                    State, UT_MACHINEGUNNER_FS, Team).Position (Team).X),
                28 => Register_Type (Get_Unit (
                    State, UT_SCOUT_SS, Team).Position (Team).X),
                29 => Register_Type (Get_Unit (
                    State, UT_SCOUT_FS, Team).Position (Team).X),
                30 => Register_Type (Get_Unit (
                    State, UT_RIFLEMAN_SS, Team).Position (Team).X),
                31 => Register_Type (Get_Unit (
                    State, UT_RIFLEMAN_FS, Team).Position (Team).X));
        end Set_Team_Mortar_Registers;
    begin
        for T in Player_ID'Range loop
            Set_Team_Mortar_Registers (T);
        end loop;
    end Set_Mortar_Registers;
end Processors.Mortars;
