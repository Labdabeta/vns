with Boards; use Boards;
with Coordinates; use Coordinates;

with Processors.Registers; use Processors.Registers;
with Processors.Floats; use Processors.Floats;
with Processors.Asks; use Processors.Asks;

package body Processors.Engineers is
    ENGINEER_WIR : constant Instruction_ID := 96;
    ENGINEER_CUT : constant Instruction_ID := 97;
    ENGINEER_SND : constant Instruction_ID := 98;
    ENGINEER_DIG : constant Instruction_ID := 99;
    ENGINEER_WFG : constant Instruction_ID := 100;
    ENGINEER_RFG : constant Instruction_ID := 101;
    ENGINEER_ITF : constant Instruction_ID := 102;
    ENGINEER_FAD : constant Instruction_ID := 103;
    ENGINEER_FSU : constant Instruction_ID := 104;
    ENGINEER_FMU : constant Instruction_ID := 105;
    ENGINEER_FDV : constant Instruction_ID := 106;
    ENGINEER_CEL : constant Instruction_ID := 107;
    ENGINEER_FLR : constant Instruction_ID := 108;
    ENGINEER_SIN : constant Instruction_ID := 109;
    ENGINEER_COS : constant Instruction_ID := 110;
    ENGINEER_TAN : constant Instruction_ID := 111;
    ENGINEER_POW : constant Instruction_ID := 112;
    ENGINEER_ASN : constant Instruction_ID := 113;
    ENGINEER_ACS : constant Instruction_ID := 114;
    ENGINEER_ATN : constant Instruction_ID := 115;
    ENGINEER_LOG : constant Instruction_ID := 116;
    ENGINEER_FCP : constant Instruction_ID := 117;
    ENGINEER_LIE : constant Instruction_ID := 118;
    ENGINEER_GUP : constant Instruction_ID := 119;
    ENGINEER_CSS : constant Instruction_ID := 120;
    ENGINEER_CFS : constant Instruction_ID := 121;
    ENGINEER_WSS : constant Instruction_ID := 122;
    ENGINEER_WFS : constant Instruction_ID := 123;
    ENGINEER_BOM : constant Instruction_ID := 124;
    ENGINEER_AIR : constant Instruction_ID := 125;
    ENGINEER_MOR : constant Instruction_ID := 126;
    ENGINEER_SUP : constant Instruction_ID := 127;

    procedure Set_Engineer_Registers (
        Machines : in out Processor_Array;
        State : in Board) is
        procedure Set_Team_Side_Engineer_Registers (
            Team : in Player_ID;
            Side : in Board_Side) is
            Pos : Coordinate :=
                Get_Unit (State, Engineer_IDs (Side), Team).Position (Team);
            Them : Player_ID := Enemy_Of (Team);
        begin
            Machines (Team, Engineer_IDs (Side)).Registers (20 .. 31) := (
                20 => Register_Type (Get_Nearest_Terrain (
                    State, To_Location (Pos, Team), Team, TT_SAND).X),
                21 => Register_Type (Get_Nearest_Terrain (
                    State, To_Location (Pos, Team), Team, TT_SAND).Y),
                22 => Register_Type (Get_Nearest_Terrain (
                    State, To_Location (Pos, Team), Team, TT_WIRE).X),
                23 => Register_Type (Get_Nearest_Terrain (
                    State, To_Location (Pos, Team), Team, TT_WIRE).Y),
                24 => Register_Type (Get_Nearest_Ally (
                    State, To_Location (Pos, Team), Team).Position (Team).X),
                25 => Register_Type (Get_Nearest_Ally (
                    State, To_Location (Pos, Team), Team).Position (Team).Y),
                26 => Register_Type (Get_Nearest_Ally (
                    State, To_Location (Pos, Team), Them).Position (Team).X),
                27 => Register_Type (Get_Nearest_Ally (
                    State, To_Location (Pos, Team), Them).Position (Team).Y),
                28 => Register_Type (Get_Nearest_Terrain (
                    State, To_Location (Pos, Team), Team, TT_WATER).X),
                29 => Register_Type (Get_Nearest_Terrain (
                    State, To_Location (Pos, Team), Team, TT_WATER).Y),
                30 => Register_Type (Distance_To_Base (
                    State, To_Location (Pos, Team), Team)),
                31 => Register_Type (Distance_To_Base (
                    State, To_Location (Pos, Team), Them)));
        end Set_Team_Side_Engineer_Registers;
    begin
        for T in Player_ID'Range loop
            for S in Board_Side'Range loop
                Set_Team_Side_Engineer_Registers (T, S);
            end loop;
        end loop;
    end Set_Engineer_Registers;

    function Engineer_Time (Op : in Instruction_ID) return Natural is
    begin
        case Op is
            when ENGINEER_WFG | ENGINEER_RFG =>
                return 4;
            when ENGINEER_WIR | ENGINEER_CUT | ENGINEER_SND | ENGINEER_DIG |
                ENGINEER_ITF | ENGINEER_FAD | ENGINEER_FSU | ENGINEER_CEL |
                ENGINEER_FLR | ENGINEER_FCP | ENGINEER_CSS | ENGINEER_CFS |
                ENGINEER_WSS | ENGINEER_WFS | ENGINEER_BOM | ENGINEER_AIR |
                ENGINEER_MOR | ENGINEER_SUP =>
                return 8;
            when ENGINEER_FMU =>
                return 32;
            when ENGINEER_FDV | ENGINEER_SIN | ENGINEER_COS | ENGINEER_TAN |
                ENGINEER_POW | ENGINEER_ASN | ENGINEER_ACS | ENGINEER_ATN |
                ENGINEER_LOG | ENGINEER_LIE | ENGINEER_GUP =>
                return 64;
            when others =>
                return 0;
        end case;
    end Engineer_Time;

    procedure Engineer_Instruction (
        Op : in Instruction_ID;
        Team : in Boards.Player_ID;
        Unit : in Boards.Unit_Type;
        B : in Register_Type;
        C : in Register_Type;
        Immediate : in Address_Type;
        State : in out Boards.Board;
        A : in out Register_Type;
        Flags : in out Shared_Grid;
        Machines : in out Processor_Array) is
    begin
        case Op is
            when ENGINEER_WIR =>
                Plant_Wire (State, Team, Unit, To_Direction (A));
            when ENGINEER_CUT =>
                Plant_Wire (State, Team, Unit, To_Direction (A), True);
            when ENGINEER_SND =>
                Plant_Cover (State, Team, Unit, To_Direction (A));
            when ENGINEER_DIG =>
                Plant_Cover (State, Team, Unit, To_Direction (A), True);
            when ENGINEER_WFG =>
                Flags (Team, X_Coordinate (B), Y_Coordinate (C)) := A;
            when ENGINEER_RFG =>
                A := Flags (Team, X_Coordinate (B), Y_Coordinate (C));
            when ENGINEER_LIE => Set_Prone (State, Team, Unit, True);
            when ENGINEER_GUP => Set_Prone (State, Team, Unit, False);
            when ENGINEER_ITF | ENGINEER_FAD | ENGINEER_FSU | ENGINEER_FMU |
                ENGINEER_FDV | ENGINEER_CEL | ENGINEER_FLR | ENGINEER_SIN |
                ENGINEER_COS | ENGINEER_TAN | ENGINEER_POW | ENGINEER_ASN |
                ENGINEER_ACS | ENGINEER_ATN | ENGINEER_LOG | ENGINEER_FCP =>
                Float_Instruction (Op, B, C, Immediate, A);
            when ENGINEER_CSS | ENGINEER_CFS | ENGINEER_WSS | ENGINEER_WFS |
                ENGINEER_BOM | ENGINEER_AIR | ENGINEER_MOR | ENGINEER_SUP =>
                Ask_Instruction (Op, Team, B, C, Immediate, State, A, Machines);
            when others => null;
        end case;
    end Engineer_Instruction;
end Processors.Engineers;
