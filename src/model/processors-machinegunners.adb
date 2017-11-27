with Boards; use Boards;
with Processors.Registers; use Processors.Registers;
with Coordinates; use Coordinates;

with Processors.Floats; use Processors.Floats;
with Processors.Asks; use Processors.Asks;

package body Processors.Machinegunners is
    MG_SET : constant Instruction_ID := 96;
    MG_GUP : constant Instruction_ID := 97;
    MG_WSG : constant Instruction_ID := 98;
    MG_RSG : constant Instruction_ID := 99;
    MG_WFG : constant Instruction_ID := 100;
    MG_RFG : constant Instruction_ID := 101;
    MG_ITF : constant Instruction_ID := 102;
    MG_FAD : constant Instruction_ID := 103;
    MG_FSU : constant Instruction_ID := 104;
    MG_FMU : constant Instruction_ID := 105;
    MG_FDV : constant Instruction_ID := 106;
    MG_CEL : constant Instruction_ID := 107;
    MG_FLR : constant Instruction_ID := 108;
    MG_SIN : constant Instruction_ID := 109;
    MG_COS : constant Instruction_ID := 110;
    MG_TAN : constant Instruction_ID := 111;
    MG_POW : constant Instruction_ID := 112;
    MG_ASN : constant Instruction_ID := 113;
    MG_ACS : constant Instruction_ID := 114;
    MG_ATN : constant Instruction_ID := 115;
    MG_LOG : constant Instruction_ID := 116;
    MG_FCP : constant Instruction_ID := 117;
    MG_MLE : constant Instruction_ID := 118;
    MG_MLF : constant Instruction_ID := 119;
    MG_CSS : constant Instruction_ID := 120;
    MG_CFS : constant Instruction_ID := 121;
    MG_WSS : constant Instruction_ID := 122;
    MG_WFS : constant Instruction_ID := 123;
    MG_BOM : constant Instruction_ID := 124;
    MG_AIR : constant Instruction_ID := 125;
    MG_MOR : constant Instruction_ID := 126;
    MG_SUP : constant Instruction_ID := 127;

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

    function Machinegunner_Time (Op : in Instruction_ID) return Natural is
    begin
        case Op is
            when MG_WSG | MG_RSG | MG_WFG | MG_RFG | MG_MLF =>
                return 4;
            when MG_ITF | MG_FAD | MG_FSU | MG_CEL | MG_FLR | MG_FCP |
                MG_CSS | MG_CFS | MG_WSS | MG_WFS | MG_BOM | MG_AIR |
                MG_MOR | MG_SUP =>
                return 8;
            when MG_MLE =>
                return 16;
            when MG_FMU =>
                return 32;
            when MG_FDV | MG_SIN | MG_COS | MG_TAN | MG_POW | MG_ASN |
                MG_ACS | MG_ATN | MG_LOG =>
                return 64;
            when MG_SET | MG_GUP =>
                return 128;
            when others =>
                return 0;
        end case;
    end Machinegunner_Time;

    procedure Machinegunner_Instruction (
        Op : in Instruction_ID;
        Team : in Boards.Player_ID;
        Unit : in Boards.Unit_Type;
        B : in out Register_Type;
        C : in out Register_Type;
        Immediate : in Address_Type;
        State : in out Boards.Board;
        A : in out Register_Type;
        Support : in out Shared_Grid;
        Flags : in out Shared_Grid;
        Machines : in out Processor_Array) is
    begin
        case Op is
            when MG_SET => Set_Setup (State, Team, Unit, True);
            when MG_GUP => Set_Setup (State, Team, Unit, False);
            when MG_WSG =>
                Support (Team, X_Coordinate (B), Y_Coordinate (C)) := A;
            when MG_RSG =>
                A := Support (Team, X_Coordinate (B), Y_Coordinate (C));
            when MG_WFG =>
                Flags (Team, X_Coordinate (B), Y_Coordinate (C)) := A;
            when MG_RFG =>
                A := Flags (Team, X_Coordinate (B), Y_Coordinate (C));
            when MG_MLE =>
                Do_Melee (State, Team, Unit);
            when MG_MLF =>
                Do_Hit (State, Team, Unit, To_Direction (A));
            when MG_ITF | MG_FAD | MG_FSU | MG_FMU | MG_FDV | MG_CEL |
                MG_FLR | MG_SIN | MG_COS | MG_TAN | MG_POW | MG_ASN |
                MG_ACS | MG_ATN | MG_LOG | MG_FCP =>
                Float_Instruction (Op, B, C, Immediate, A);
            when MG_CSS | MG_CFS | MG_WSS | MG_WFS |
                MG_BOM | MG_AIR | MG_MOR | MG_SUP =>
                Ask_Instruction (Op, Team, B, C, Immediate, State, A, Machines);
            when others => null;
        end case;
    end Machinegunner_Instruction;
end Processors.Machinegunners;
