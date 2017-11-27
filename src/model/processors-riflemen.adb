with Boards; use Boards;
with Coordinates; use Coordinates;

with Processors.Floats; use Processors.Floats;
with Processors.Asks; use Processors.Asks;

package body Processors.Riflemen is
    RIFLEMAN_WTG : constant Instruction_ID := 96;
    RIFLEMAN_RTG : constant Instruction_ID := 97;
    RIFLEMAN_WSG : constant Instruction_ID := 98;
    RIFLEMAN_RSG : constant Instruction_ID := 99;
    RIFLEMAN_WFG : constant Instruction_ID := 100;
    RIFLEMAN_RFG : constant Instruction_ID := 101;
    RIFLEMAN_ITF : constant Instruction_ID := 102;
    RIFLEMAN_FAD : constant Instruction_ID := 103;
    RIFLEMAN_FSU : constant Instruction_ID := 104;
    RIFLEMAN_FMU : constant Instruction_ID := 105;
    RIFLEMAN_FDV : constant Instruction_ID := 106;
    RIFLEMAN_CEL : constant Instruction_ID := 107;
    RIFLEMAN_FLR : constant Instruction_ID := 108;
    RIFLEMAN_SIN : constant Instruction_ID := 109;
    RIFLEMAN_COS : constant Instruction_ID := 110;
    RIFLEMAN_TAN : constant Instruction_ID := 111;
    RIFLEMAN_POW : constant Instruction_ID := 112;
    RIFLEMAN_ASN : constant Instruction_ID := 113;
    RIFLEMAN_ACS : constant Instruction_ID := 114;
    RIFLEMAN_ATN : constant Instruction_ID := 115;
    RIFLEMAN_LOG : constant Instruction_ID := 116;
    RIFLEMAN_FCP : constant Instruction_ID := 117;
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
        case Op is
            when RIFLEMAN_WTG | RIFLEMAN_RTG | RIFLEMAN_WSG | RIFLEMAN_RSG |
                RIFLEMAN_WFG | RIFLEMAN_RFG =>
                return 4;
            when RIFLEMAN_ITF | RIFLEMAN_FAD | RIFLEMAN_FSU | RIFLEMAN_CEL |
                RIFLEMAN_FLR | RIFLEMAN_FCP | RIFLEMAN_CSS | RIFLEMAN_CFS |
                RIFLEMAN_WSS | RIFLEMAN_WFS | RIFLEMAN_BOM | RIFLEMAN_AIR |
                RIFLEMAN_MOR | RIFLEMAN_SUP =>
                return 8;
            when RIFLEMAN_FMU =>
                return 32;
            when RIFLEMAN_FDV | RIFLEMAN_SIN | RIFLEMAN_COS | RIFLEMAN_TAN |
                RIFLEMAN_POW | RIFLEMAN_ASN | RIFLEMAN_ACS | RIFLEMAN_ATN |
                RIFLEMAN_LOG | RIFLEMAN_LIE | RIFLEMAN_GUP =>
                return 64;
            when others =>
                return 0;
        end case;
    end Rifleman_Time;

    procedure Rifleman_Instruction (
        Op : in Instruction_ID;
        Team : in Boards.Player_ID;
        Unit : in Boards.Unit_Type;
        B : in out Register_Type;
        C : in out Register_Type;
        Immediate : in Address_Type;
        State : in out Boards.Board;
        A : in out Register_Type;
        Tactical : in out Shared_Grid;
        Support : in out Shared_Grid;
        Flags : in out Shared_Grid;
        Machines : in out Processor_Array) is
    begin
        case Op is
            when RIFLEMAN_WTG =>
                Tactical (Team, X_Coordinate (B), Y_Coordinate (C)) := A;
            when RIFLEMAN_RTG =>
                A := Tactical (Team, X_Coordinate (B), Y_Coordinate (C));
            when RIFLEMAN_WSG =>
                Support (Team, X_Coordinate (B), Y_Coordinate (C)) := A;
            when RIFLEMAN_RSG =>
                A := Support (Team, X_Coordinate (B), Y_Coordinate (C));
            when RIFLEMAN_WFG =>
                Support (Team, X_Coordinate (B), Y_Coordinate (C)) := A;
            when RIFLEMAN_RFG =>
                A := Support (Team, X_Coordinate (B), Y_Coordinate (C));
            when RIFLEMAN_LIE => Set_Prone (State, Team, Unit, True);
            when RIFLEMAN_GUP => Set_Prone (State, Team, Unit, False);
            when RIFLEMAN_ITF | RIFLEMAN_FAD | RIFLEMAN_FSU | RIFLEMAN_FMU |
                RIFLEMAN_FDV | RIFLEMAN_CEL | RIFLEMAN_FLR | RIFLEMAN_SIN |
                RIFLEMAN_COS | RIFLEMAN_TAN | RIFLEMAN_POW | RIFLEMAN_ASN |
                RIFLEMAN_ACS | RIFLEMAN_ATN | RIFLEMAN_LOG | RIFLEMAN_FCP =>
                Float_Instruction (Op, B, C, Immediate, A);
            when RIFLEMAN_CSS | RIFLEMAN_CFS | RIFLEMAN_WSS | RIFLEMAN_WFS |
                RIFLEMAN_BOM | RIFLEMAN_AIR | RIFLEMAN_MOR | RIFLEMAN_SUP =>
                Ask_Instruction (Op, Team, B, C, Immediate, State, A, Machines);
            when others => null;
        end case;
    end Rifleman_Instruction;
end Processors.Riflemen;
