with Boards; use Boards;
with Coordinates; use Coordinates;

with Processors.Floats; use Processors.Floats;
with Processors.Asks; use Processors.Asks;
with Processors.Registers; use Processors.Registers;

package body Processors.Riflemen is
    RIFLEMAN_QHI : constant Instruction_ID := 96;
    RIFLEMAN_QRS : constant Instruction_ID := 97;
    RIFLEMAN_QSH : constant Instruction_ID := 98;
    RIFLEMAN_QPR : constant Instruction_ID := 99;
    RIFLEMAN_QSU : constant Instruction_ID := 100;
    RIFLEMAN_QMV : constant Instruction_ID := 101;
    RIFLEMAN_LIE : constant Instruction_ID := 118;
    RIFLEMAN_GUP : constant Instruction_ID := 119;
    RIFLEMAN_CSS : constant Instruction_ID := 120;
    RIFLEMAN_CFS : constant Instruction_ID := 121;
    RIFLEMAN_WSS : constant Instruction_ID := 122;
    RIFLEMAN_WFS : constant Instruction_ID := 123;
    RIFLEMAN_BOM : constant Instruction_ID := 124;
    RIFLEMAN_AIR : constant Instruction_ID := 125;
    RIFLEMAN_MOR : constant Instruction_ID := 126;
    RIFLEMAN_SUP : constant Instruction_ID := 127;

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

    function Rifleman_Time (Op : in Instruction_ID) return Natural is
    begin
        if Is_Float_Op (Op) then
            return Float_Time (Op);
        end if;

        case Op is
            when RIFLEMAN_QSU | RIFLEMAN_QMV | RIFLEMAN_QSH | RIFLEMAN_QPR |
                RIFLEMAN_QHI | RIFLEMAN_QRS =>
                return 1;
            when RIFLEMAN_CSS | RIFLEMAN_CFS | RIFLEMAN_WSS | RIFLEMAN_WFS |
                RIFLEMAN_BOM | RIFLEMAN_AIR | RIFLEMAN_MOR | RIFLEMAN_SUP =>
                return 8;
            when RIFLEMAN_GUP =>
                return 64;
            when others =>
                return 0;
        end case;
    end Rifleman_Time;

    procedure Rifleman_Instruction (
        Op : in Instruction_ID;
        Team : in Boards.Player_ID;
        Unit : in Boards.Unit_Type;
        Immediate : in Address_Type;
        State : in out Boards.Board;
        A : in out Register_Type;
        B : in out Register_Type;
        C : in out Register_Type;
        Machines : in out Processor_Array) is
        Enemy : Player_ID := Enemy_Of (Team);
    begin
        if Is_Float_Op (Op) then
            Float_Instruction (Op, A, B, C, Immediate);
        end if;

        case Op is
            when RIFLEMAN_QSU =>
                B := From_Boolean (Get_Unit (State, To_Unit (A), Team).Setup);
                C := From_Boolean (Get_Unit (State, To_Unit (A), Enemy).Setup);
            when RIFLEMAN_QMV =>
                B := From_Boolean (Get_Unit (State, To_Unit (A), Team).Moving);
                C := From_Boolean (Get_Unit (State, To_Unit (A), Enemy).Moving);
            when RIFLEMAN_QSH =>
                B := From_Boolean (
                    Get_Unit (State, To_Unit (A), Team).Shooting);
                C := From_Boolean (
                    Get_Unit (State, To_Unit (A), Enemy).Shooting);
            when RIFLEMAN_QPR =>
                B := From_Boolean (Get_Unit (State, To_Unit (A), Team).Prone);
                C := From_Boolean (Get_Unit (State, To_Unit (A), Enemy).Prone);
            when RIFLEMAN_QHI =>
                B := From_Boolean (Get_Unit (State, To_Unit (A), Team).Hidden);
                C := From_Boolean (Get_Unit (State, To_Unit (A), Enemy).Hidden);
            when RIFLEMAN_QRS =>
                B := From_Boolean (
                    Get_Unit (State, To_Unit (A), Team).Summoned or
                    Get_Unit (State, To_Unit (A), Team).Retreating);
                C := From_Boolean (
                    Get_Unit (State, To_Unit (A), Enemy).Summoned or
                    Get_Unit (State, To_Unit (A), Enemy).Retreating);
            when RIFLEMAN_LIE => Set_Prone (State, Team, Unit, True);
            when RIFLEMAN_GUP => Set_Prone (State, Team, Unit, False);
            when RIFLEMAN_CSS | RIFLEMAN_CFS | RIFLEMAN_WSS | RIFLEMAN_WFS |
                RIFLEMAN_BOM | RIFLEMAN_AIR | RIFLEMAN_MOR | RIFLEMAN_SUP =>
                Ask_Instruction (Op, Team, B, C, Immediate, State, A, Machines);
            when others => null;
        end case;
    end Rifleman_Instruction;
end Processors.Riflemen;
