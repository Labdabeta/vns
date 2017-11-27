with Boards; use Boards;
with Coordinates; use Coordinates;

with Processors.Registers; use Processors.Registers;
with Processors.Floats; use Processors.Floats;
with Processors.Asks; use Processors.Asks;

package body Processors.Scouts is
    SCOUT_RUN : constant Instruction_ID := 96;
    SCOUT_HIT : constant Instruction_ID := 97;
    SCOUT_WSG : constant Instruction_ID := 98;
    SCOUT_RSG : constant Instruction_ID := 99;
    SCOUT_WFG : constant Instruction_ID := 100;
    SCOUT_RFG : constant Instruction_ID := 101;
    SCOUT_ITF : constant Instruction_ID := 102;
    SCOUT_FAD : constant Instruction_ID := 103;
    SCOUT_FSU : constant Instruction_ID := 104;
    SCOUT_FMU : constant Instruction_ID := 105;
    SCOUT_FDV : constant Instruction_ID := 106;
    SCOUT_CEL : constant Instruction_ID := 107;
    SCOUT_FLR : constant Instruction_ID := 108;
    SCOUT_SIN : constant Instruction_ID := 109;
    SCOUT_COS : constant Instruction_ID := 110;
    SCOUT_TAN : constant Instruction_ID := 111;
    SCOUT_POW : constant Instruction_ID := 112;
    SCOUT_ASN : constant Instruction_ID := 113;
    SCOUT_ACS : constant Instruction_ID := 114;
    SCOUT_ATN : constant Instruction_ID := 115;
    SCOUT_LOG : constant Instruction_ID := 116;
    SCOUT_FCP : constant Instruction_ID := 117;
    SCOUT_LIE : constant Instruction_ID := 118;
    SCOUT_GUP : constant Instruction_ID := 119;
    SCOUT_CSS : constant Instruction_ID := 120;
    SCOUT_CFS : constant Instruction_ID := 121;
    SCOUT_WSS : constant Instruction_ID := 122;
    SCOUT_WFS : constant Instruction_ID := 123;
    SCOUT_BOM : constant Instruction_ID := 124;
    SCOUT_AIR : constant Instruction_ID := 125;
    SCOUT_MOR : constant Instruction_ID := 126;
    SCOUT_SUP : constant Instruction_ID := 127;

    procedure Set_Scout_Registers (
        Machines : in out Processor_Array;
        State : in Boards.Board) is
        procedure Set_Team_Side_Scout_Registers (
            Team : in Player_ID;
            Side : in Board_Side) is
            Pos : Location := Get_Unit (State, Scout_IDs (Side), Team).Position;
            Them : Player_ID := Enemy_Of (Team);
            Nearest_Enemies : array (1 .. 5) of Coordinate := (
                1 => Get_Nearest_Ally (State, Pos, Them, 0).Position (Team),
                2 => Get_Nearest_Ally (State, Pos, Them, 1).Position (Team),
                3 => Get_Nearest_Ally (State, Pos, Them, 2).Position (Team),
                4 => Get_Nearest_Ally (State, Pos, Them, 3).Position (Team),
                5 => Get_Nearest_Ally (State, Pos, Them, 4).Position (Team));
            Sniper_Idx : Register_Index;
        begin
            if Side = SNIPER_SIDE then
                Sniper_Idx := 24;
            else
                Sniper_Idx := 26;
            end if;
            Machines (Team, Scout_IDs (Side)).Registers (16 .. 17) := (
                16 => From_Boolean (Get_Unit (State, UT_SNIPER, Team).Setup),
                17 => From_Boolean (
                    Get_Unit (State, UT_SNIPER, Enemy_Of (Team)).Setup));
            Machines (Team, Scout_IDs (Side)).Registers (20 .. 31) := (
                20 => Machines (Team, UT_SNIPER).Registers (Sniper_Idx),
                21 => Machines (Team, UT_SNIPER).Registers (Sniper_Idx + 1),
                22 => Register_Type (Nearest_Enemies (1).X),
                23 => Register_Type (Nearest_Enemies (1).Y),
                24 => Register_Type (Nearest_Enemies (2).X),
                25 => Register_Type (Nearest_Enemies (2).Y),
                26 => Register_Type (Nearest_Enemies (3).X),
                27 => Register_Type (Nearest_Enemies (3).Y),
                28 => Register_Type (Nearest_Enemies (4).X),
                29 => Register_Type (Nearest_Enemies (4).Y),
                30 => Register_Type (Nearest_Enemies (5).X),
                31 => Register_Type (Nearest_Enemies (5).Y));
        end Set_Team_Side_Scout_Registers;
    begin
        for T in Player_ID'Range loop
            for S in Board_Side'Range loop
                Set_Team_Side_Scout_Registers (T, S);
            end loop;
        end loop;
    end Set_Scout_Registers;

    function Scout_Time (Op : in Instruction_ID) return Natural is
    begin
        case Op is
            when SCOUT_RUN =>
                return 1;
            when SCOUT_WSG | SCOUT_RSG | SCOUT_WFG | SCOUT_RFG =>
                return 4;
            when SCOUT_ITF | SCOUT_FAD | SCOUT_FSU | SCOUT_CEL | SCOUT_FLR |
                SCOUT_FCP | SCOUT_CSS | SCOUT_CFS | SCOUT_WSS | SCOUT_WFS |
                SCOUT_BOM | SCOUT_AIR | SCOUT_MOR | SCOUT_SUP =>
                return 8;
            when SCOUT_HIT =>
                return 16;
            when SCOUT_FMU =>
                return 32;
            when SCOUT_FDV | SCOUT_SIN | SCOUT_COS | SCOUT_TAN | SCOUT_POW |
                SCOUT_ASN | SCOUT_ACS | SCOUT_ATN | SCOUT_LOG | SCOUT_LIE |
                SCOUT_GUP =>
                return 64;
            when others =>
                return 0;
        end case;
    end Scout_Time;

    procedure Scout_Instruction (
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
            when SCOUT_RUN => Do_Move (State, Team, Unit, To_Direction (A));
            when SCOUT_HIT => Do_Hit (State, Team, Unit, To_Direction (A));
            when SCOUT_WSG =>
                Support (Team, X_Coordinate (B), Y_Coordinate (C)) := A;
            when SCOUT_RSG =>
                A := Support (Team, X_Coordinate (B), Y_Coordinate (C));
            when SCOUT_WFG =>
                Flags (Team, X_Coordinate (B), Y_Coordinate (C)) := A;
            when SCOUT_RFG =>
                A := Flags (Team, X_Coordinate (B), Y_Coordinate (C));
            when SCOUT_LIE => Set_Prone (State, Team, Unit, True);
            when SCOUT_GUP => Set_Prone (State, Team, Unit, False);
            when SCOUT_ITF | SCOUT_FAD | SCOUT_FSU | SCOUT_FMU |
                SCOUT_FDV | SCOUT_CEL | SCOUT_FLR | SCOUT_SIN |
                SCOUT_COS | SCOUT_TAN | SCOUT_POW | SCOUT_ASN |
                SCOUT_ACS | SCOUT_ATN | SCOUT_LOG | SCOUT_FCP =>
                Float_Instruction (Op, B, C, Immediate, A);
            when SCOUT_CSS | SCOUT_CFS | SCOUT_WSS | SCOUT_WFS |
                SCOUT_BOM | SCOUT_AIR | SCOUT_MOR | SCOUT_SUP =>
                Ask_Instruction (Op, Team, B, C, Immediate, State, A, Machines);
            when others => null;
        end case;
    end Scout_Instruction;
end Processors.Scouts;
