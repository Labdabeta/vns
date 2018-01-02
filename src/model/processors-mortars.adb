with Boards; use Boards;
with Coordinates; use Coordinates;

with Processors.Asks; use Processors.Asks;
with Processors.Floats; use Processors.Floats;
with Processors.Registers; use Processors.Registers;

package body Processors.Mortars is
    -- Instruction names
    MORTAR_QHI : constant Instruction_ID := 96;
    MORTAR_QRS : constant Instruction_ID := 97;
    MORTAR_QSH : constant Instruction_ID := 98;
    MORTAR_QPR : constant Instruction_ID := 99;
    MORTAR_QSU : constant Instruction_ID := 100;
    MORTAR_QMV : constant Instruction_ID := 101;
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
            when MORTAR_QSU | MORTAR_QMV | MORTAR_QSH | MORTAR_QPR |
                MORTAR_QHI | MORTAR_QRS =>
                return 1;
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
        Team : in Boards.Player_ID;
        State : in out Boards.Board;
        Machines : in out Processor_Array) is
        Me : Unit_Processor renames Machines (Team, UT_MORTAR);
        A : Register_Type renames Me.Registers (Me.RA);
        B : Register_Type renames Me.Registers (Me.RB);
        C : Register_Type renames Me.Registers (Me.RC);
        Enemy : Player_ID := Enemy_Of (Team);
    begin
        if Is_Float_Op (Me.Op) then
            Float_Instruction (Me);
        end if;

        case Me.Op is
            when MORTAR_QSU =>
                B := From_Boolean (
                    Get_Unit (State, To_Unit (Me.A), Team).Setup);
                C := From_Boolean (
                    Get_Unit (State, To_Unit (Me.A), Enemy).Setup);
            when MORTAR_QMV =>
                B := From_Boolean (
                    Get_Unit (State, To_Unit (Me.A), Team).Moving);
                C := From_Boolean (
                    Get_Unit (State, To_Unit (Me.A), Enemy).Moving);
            when MORTAR_QSH =>
                B := From_Boolean (
                    Get_Unit (State, To_Unit (Me.A), Team).Shooting);
                C := From_Boolean (
                    Get_Unit (State, To_Unit (Me.A), Enemy).Shooting);
            when MORTAR_QPR =>
                B := From_Boolean (
                    Get_Unit (State, To_Unit (Me.A), Team).Prone);
                C := From_Boolean (
                    Get_Unit (State, To_Unit (Me.A), Enemy).Prone);
            when MORTAR_QHI =>
                B := From_Boolean (
                    Get_Unit (State, To_Unit (Me.A), Team).Hidden);
                C := From_Boolean (
                    Get_Unit (State, To_Unit (Me.A), Enemy).Hidden);
            when MORTAR_QRS =>
                B := From_Boolean (
                    Get_Unit (State, To_Unit (Me.A), Team).Summoned or
                    Get_Unit (State, To_Unit (Me.A), Team).Retreating);
                C := From_Boolean (
                    Get_Unit (State, To_Unit (Me.A), Enemy).Summoned or
                    Get_Unit (State, To_Unit (Me.A), Enemy).Retreating);
            when MORTAR_MLE => Do_Melee (State, Team, UT_MORTAR);
            when MORTAR_SET => Set_Setup (State, Team, UT_MORTAR, True);
            when MORTAR_GUP => Set_Setup (State, Team, UT_MORTAR, False);
            when MORTAR_CSS | MORTAR_CFS | MORTAR_WSS | MORTAR_WFS |
                MORTAR_BOM | MORTAR_AIR | MORTAR_SUP =>
                Ask_Instruction (Team, UT_MORTAR, State, Machines);
            when others => null;
        end case;
    end Mortar_Instruction;
end Processors.Mortars;
