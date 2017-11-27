with Boards; use Boards;
with Coordinates; use Coordinates;

with Processors.Asks; use Processors.Asks;
with Processors.Floats; use Processors.Floats;

package body Processors.Mortars is
    -- Instruction names
    MORTAR_WTG : constant Instruction_ID := 96;
    MORTAR_RTG : constant Instruction_ID := 97;
    MORTAR_WSG : constant Instruction_ID := 98;
    MORTAR_RSG : constant Instruction_ID := 99;
    MORTAR_WFG : constant Instruction_ID := 100;
    MORTAR_RFG : constant Instruction_ID := 101;
    MORTAR_MLE : constant Instruction_ID := 118;
    MORTAR_SET : constant Instruction_ID := 119;
    MORTAR_CSS : constant Instruction_ID := 120;
    MORTAR_CFS : constant Instruction_ID := 121;
    MORTAR_WSS : constant Instruction_ID := 122;
    MORTAR_WFS : constant Instruction_ID := 123;
    MORTAR_BOM : constant Instruction_ID := 124;
    MORTAR_AIR : constant Instruction_ID := 125;
    MORTAR_GUP : constant Instruction_ID := 126;
    MORTAR_SUP : constant Instruction_ID := 127;

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

    function Mortar_Time (Op : in Instruction_ID) return Natural is
    begin
        if Is_Float_Op (Op) then
            return Float_Time (Op);
        end if;

        case Op is
            when MORTAR_WTG | MORTAR_RTG | MORTAR_WSG | MORTAR_RSG |
                MORTAR_WFG | MORTAR_RFG =>
                return 4;
            when MORTAR_CSS | MORTAR_CFS | MORTAR_WSS | MORTAR_WFS |
                MORTAR_BOM | MORTAR_AIR =>
                return 8;
            when MORTAR_MLE =>
                return 16;
            when MORTAR_SET | MORTAR_GUP =>
                return 128;
            when others =>
                return 0;
        end case;
    end Mortar_Time;

    procedure Mortar_Instruction (
        Op : in Instruction_ID;
        Team : in Boards.Player_ID;
        Immediate : in Address_Type;
        State : in out Boards.Board;
        A : in out Register_Type;
        B : in out Register_Type;
        C : in out Register_Type;
        Tactical : in out Shared_Grid;
        Support : in out Shared_Grid;
        Flag : in out Shared_Grid;
        Machines : in out Processor_Array) is
    begin
        if Is_Float_Op (Op) then
            Float_Instruction (Op, A, B, C, Immediate);
        end if;

        case Op is
            when MORTAR_WTG =>
                Tactical (Team, X_Coordinate (B), Y_Coordinate (C)) := A;
            when MORTAR_RTG =>
                A := Tactical (Team, X_Coordinate (B), Y_Coordinate (C));
            when MORTAR_WSG =>
                Support (Team, X_Coordinate (B), Y_Coordinate (C)) := A;
            when MORTAR_RSG =>
                A := Support (Team, X_Coordinate (B), Y_Coordinate (C));
            when MORTAR_WFG =>
                Flag (Team, X_Coordinate (B), Y_Coordinate (C)) := A;
            when MORTAR_RFG =>
                A := Flag (Team, X_Coordinate (B), Y_Coordinate (C));
            when MORTAR_MLE => Do_Melee (State, Team, UT_MORTAR);
            when MORTAR_SET => Set_Setup (State, Team, UT_MORTAR, True);
            when MORTAR_GUP => Set_Setup (State, Team, UT_MORTAR, False);
            when MORTAR_CSS | MORTAR_CFS | MORTAR_WSS | MORTAR_WFS |
                MORTAR_BOM | MORTAR_AIR | MORTAR_SUP =>
                Ask_Instruction (Op, Team, B, C, Immediate, State, A, Machines);
            when others => null;
        end case;
    end Mortar_Instruction;
end Processors.Mortars;
