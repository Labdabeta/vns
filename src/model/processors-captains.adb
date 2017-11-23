with Boards; use Boards;

package body Processors.Captains is
    procedure Set_Captain_Registers (
        Machines : in out Processor_Array;
        State : in Boards.Board) is

        procedure Set_Team_Captain_Registers (Team : in Player_ID) is
            Nearest_Ally : Unit_State := Get_Nearest_Ally (
                State, Get_Unit (State, UT_CAPTAIN, Team).Position, Team);
            function Compute_Life_Value (Unit : in Unit_Type)
                return Register_Type is
                Result : Register_Type := 0;
            begin
                if Get_Unit (State, Unit, Team).Alive then
                    Result := Result + 1;
                end if;
                if Get_Unit (State, Unit, Enemy_Of (Team)).Alive then
                    Result := Result + 2;
                end if;
                return Result;
            end Compute_Life_Value;
        begin
            Machines (Team, UT_CAPTAIN).Registers (16 .. 29) := (
                16 => Register_Type (Get_Points (State, Team)),
                17 => Register_Type (Get_Points (State, Enemy_Of (Team))),
                18 => Register_Type (Nearest_Ally.Position (Team).X),
                19 => Register_Type (Nearest_Ally.Position (Team).Y),
                20 => Compute_Life_Value (UT_MORTAR),
                21 => Compute_Life_Value (UT_SNIPER),
                22 => Compute_Life_Value (UT_ENGINEER_SS),
                23 => Compute_Life_Value (UT_ENGINEER_FS),
                24 => Compute_Life_Value (UT_MACHINEGUNNER_SS),
                25 => Compute_Life_Value (UT_MACHINEGUNNER_FS),
                26 => Compute_Life_Value (UT_SCOUT_SS),
                27 => Compute_Life_Value (UT_SCOUT_FS),
                28 => Compute_Life_Value (UT_RIFLEMAN_SS),
                29 => Compute_Life_Value (UT_RIFLEMAN_FS));
        end Set_Team_Captain_Registers;
    begin
        for T in Player_ID'Range loop
            Set_Team_Captain_Registers (T);
        end loop;
    end Set_Captain_Registers;
end Processors.Captains;
