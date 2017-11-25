with Boards; use Boards;
with Processors.Registers; use Processors.Registers;

package body Processors.Captains is
    -- Instruction names
    CAPTAIN_ASK : constant Instruction_ID := 96;
    CAPTAIN_PLZ : constant Instruction_ID := 97;
    CAPTAIN_BEG : constant Instruction_ID := 98;
    CAPTAIN_GVL : constant Instruction_ID := 99;
    CAPTAIN_KMK : constant Instruction_ID := 100;
    CAPTAIN_SAC : constant Instruction_ID := 101;
    CAPTAIN_BOM : constant Instruction_ID := 102;
    CAPTAIN_AIR : constant Instruction_ID := 103;
    CAPTAIN_SUM : constant Instruction_ID := 104;
    CAPTAIN_HAK : constant Instruction_ID := 105;
    CAPTAIN_EMP : constant Instruction_ID := 106;
    CAPTAIN_ALL : constant Instruction_ID := 107;
    CAPTAIN_RMT : constant Instruction_ID := 108;
    CAPTAIN_RSN : constant Instruction_ID := 109;
    CAPTAIN_RES : constant Instruction_ID := 110;
    CAPTAIN_REF : constant Instruction_ID := 111;
    CAPTAIN_RMS : constant Instruction_ID := 112;
    CAPTAIN_RMF : constant Instruction_ID := 113;
    CAPTAIN_RSS : constant Instruction_ID := 114;
    CAPTAIN_RSF : constant Instruction_ID := 115;
    CAPTAIN_RRS : constant Instruction_ID := 116;
    CAPTAIN_RRF : constant Instruction_ID := 117;
    CAPTAIN_WMT : constant Instruction_ID := 118;
    CAPTAIN_WSN : constant Instruction_ID := 119;
    CAPTAIN_WES : constant Instruction_ID := 120;
    CAPTAIN_WEF : constant Instruction_ID := 121;
    CAPTAIN_WMS : constant Instruction_ID := 122;
    CAPTAIN_WMF : constant Instruction_ID := 123;
    CAPTAIN_WSS : constant Instruction_ID := 124;
    CAPTAIN_WSF : constant Instruction_ID := 125;
    CAPTAIN_WRS : constant Instruction_ID := 126;
    CAPTAIN_WRF : constant Instruction_ID := 127;

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

    function Captain_Time (
        Op : in Instruction_ID;
        A : in Register_Type;
        Team : in Boards.Player_ID;
        State : in out Boards.Board)
        return Natural is
    begin
        case Op is
            when CAPTAIN_KMK | CAPTAIN_SAC | CAPTAIN_ALL | CAPTAIN_RMT |
                CAPTAIN_RSN | CAPTAIN_RES | CAPTAIN_REF | CAPTAIN_RMS |
                CAPTAIN_RMF | CAPTAIN_RSS | CAPTAIN_RSF | CAPTAIN_RRS |
                CAPTAIN_RRF | CAPTAIN_WMT | CAPTAIN_WSN | CAPTAIN_WES |
                CAPTAIN_WEF | CAPTAIN_WMS | CAPTAIN_WMF | CAPTAIN_WSS |
                CAPTAIN_WSF | CAPTAIN_WRS | CAPTAIN_WRF =>
                return 1;
            when CAPTAIN_ASK => return 4;
            when CAPTAIN_PLZ | CAPTAIN_SUM => return 8;
            when CAPTAIN_BEG | CAPTAIN_HAK => return 16;
            when CAPTAIN_GVL => return 32;
            when CAPTAIN_BOM | CAPTAIN_EMP => return 256;
            when CAPTAIN_AIR => return 128;
            when others => return 0; -- Not a special operation
        end case;
    end Captain_Time;

    procedure Captain_Instruction (
        Op : in Instruction_ID;
        Team : in Boards.Player_ID;
        Immediate : in Address_Type;
        A : in out Register_Type;
        State : in out Boards.Board;
        Shared : in out Shared_Memory;
        Radios : in out Communications) is
    begin
        case Op is
            when CAPTAIN_ASK =>
                Increment_Points (State, Team);
            when CAPTAIN_PLZ =>
                Increment_Points (State, Team, 4);
            when CAPTAIN_BEG =>
                Increment_Points (State, Team, 16);
            when CAPTAIN_GVL =>
                Increment_Points (State, Team, 64);
            when CAPTAIN_KMK =>
                Increment_Points (State, Team, 1024);
                Kill_Unit (State, Team, UT_CAPTAIN);
            when CAPTAIN_SAC =>
                if Get_Unit (State, To_Unit (A), Team).Alive then
                    Increment_Points (State, Team, 1024);
                    Kill_Unit (State, Team, To_Unit (A));
                end if;
            when CAPTAIN_BOM =>
                Bomb_Water (State, Team);
            when CAPTAIN_AIR =>
                Bomb_Beach (State, Team);
            when CAPTAIN_SUM =>
                Set_Unit_Summon (State, Team, Unit_Type'Val (A - 1), True);
            when CAPTAIN_HAK =>
                Shared (Enemy_Of (Team), Immediate) := A;
            when CAPTAIN_EMP =>
                for Index in Shared'Range (2) loop
                    Shared (Enemy_Of (Team), Index) := A;
                end loop;
            when CAPTAIN_ALL =>
                for Index in Unit_Type'Range loop
                    Radios (Team, Index, Immediate) := A;
                end loop;
            when CAPTAIN_RMT =>
                A := Radios (Team, UT_MORTAR, Immediate);
            when CAPTAIN_RSN =>
                A := Radios (Team, UT_SNIPER, Immediate);
            when CAPTAIN_RES =>
                A := Radios (Team, UT_ENGINEER_SS, Immediate);
            when CAPTAIN_REF =>
                A := Radios (Team, UT_ENGINEER_FS, Immediate);
            when CAPTAIN_RMS =>
                A := Radios (Team, UT_MACHINEGUNNER_SS, Immediate);
            when CAPTAIN_RMF =>
                A := Radios (Team, UT_MACHINEGUNNER_FS, Immediate);
            when CAPTAIN_RSS =>
                A := Radios (Team, UT_SCOUT_SS, Immediate);
            when CAPTAIN_RSF =>
                A := Radios (Team, UT_SCOUT_FS, Immediate);
            when CAPTAIN_RRS =>
                A := Radios (Team, UT_RIFLEMAN_SS, Immediate);
            when CAPTAIN_RRF =>
                A := Radios (Team, UT_RIFLEMAN_FS, Immediate);
            when CAPTAIN_WMT =>
                Radios (Team, UT_MORTAR, Immediate) := A;
            when CAPTAIN_WSN =>
                Radios (Team, UT_SNIPER, Immediate) := A;
            when CAPTAIN_WES =>
                Radios (Team, UT_ENGINEER_SS, Immediate) := A;
            when CAPTAIN_WEF =>
                Radios (Team, UT_ENGINEER_FS, Immediate) := A;
            when CAPTAIN_WMS =>
                Radios (Team, UT_MACHINEGUNNER_SS, Immediate) := A;
            when CAPTAIN_WMF =>
                Radios (Team, UT_MACHINEGUNNER_FS, Immediate) := A;
            when CAPTAIN_WSS =>
                Radios (Team, UT_SCOUT_SS, Immediate) := A;
            when CAPTAIN_WSF =>
                Radios (Team, UT_SCOUT_FS, Immediate) := A;
            when CAPTAIN_WRS =>
                Radios (Team, UT_RIFLEMAN_SS, Immediate) := A;
            when CAPTAIN_WRF =>
                Radios (Team, UT_RIFLEMAN_FS, Immediate) := A;
            when others => null;
        end case;
    end Captain_Instruction;
end Processors.Captains;
