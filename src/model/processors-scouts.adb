with Boards; use Boards;
with Coordinates; use Coordinates;

with Processors.Registers; use Processors.Registers;
with Processors.Floats; use Processors.Floats;
with Processors.Asks; use Processors.Asks;

package body Processors.Scouts is
    SCOUT_RUN : constant Instruction_ID := 96;
    SCOUT_HIT : constant Instruction_ID := 97;
    SCOUT_QSH : constant Instruction_ID := 98;
    SCOUT_QPR : constant Instruction_ID := 99;
    SCOUT_QSU : constant Instruction_ID := 100;
    SCOUT_QMV : constant Instruction_ID := 101;
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

            -- Check if the nearest allies are wrong and set to -1, -1 if so
            if Get_Nearest_Ally (State, Pos, Them, 0) = Null_Unit then
                Machines (Team, Scout_IDs (Side)).Registers (22) := -1;
                Machines (Team, Scout_IDs (Side)).Registers (23) := -1;
            end if;

            if Get_Nearest_Ally (State, Pos, Them, 1) = Null_Unit then
                Machines (Team, Scout_IDs (Side)).Registers (24) := -1;
                Machines (Team, Scout_IDs (Side)).Registers (25) := -1;
            end if;

            if Get_Nearest_Ally (State, Pos, Them, 2) = Null_Unit then
                Machines (Team, Scout_IDs (Side)).Registers (26) := -1;
                Machines (Team, Scout_IDs (Side)).Registers (27) := -1;
            end if;

            if Get_Nearest_Ally (State, Pos, Them, 3) = Null_Unit then
                Machines (Team, Scout_IDs (Side)).Registers (28) := -1;
                Machines (Team, Scout_IDs (Side)).Registers (29) := -1;
            end if;

            if Get_Nearest_Ally (State, Pos, Them, 4) = Null_Unit then
                Machines (Team, Scout_IDs (Side)).Registers (30) := -1;
                Machines (Team, Scout_IDs (Side)).Registers (31) := -1;
            end if;
        end Set_Team_Side_Scout_Registers;
    begin
        for T in Player_ID'Range loop
            for S in Board_Side'Range loop
                Set_Team_Side_Scout_Registers (T, S);
            end loop;
        end loop;
    end Set_Scout_Registers;

    function Scout_Time (
        Op : in Instruction_ID;
        State : in out Board;
        Team : in Player_ID;
        Unit : in Unit_Type;
        RA : in Register_Type) return Natural is
    begin
        if Is_Float_Op (Op) then
            return Float_Time (Op);
        end if;

        case Op is
            when SCOUT_RUN =>
                if Get_Unit (State, Unit, Team).Prone then
                    return 0;
                else
                    Prepare_Move (State, Team, Unit, To_Direction (RA));
                    return 1;
                end if;
            when SCOUT_QSU | SCOUT_QMV | SCOUT_QSH | SCOUT_QPR =>
                return 1;
            when SCOUT_CSS | SCOUT_CFS | SCOUT_WSS | SCOUT_WFS |
                SCOUT_BOM | SCOUT_AIR | SCOUT_MOR | SCOUT_SUP =>
                return 8;
            when SCOUT_HIT =>
                return 16;
            when SCOUT_LIE | SCOUT_GUP =>
                return 64;
            when others =>
                return 0;
        end case;
    end Scout_Time;

    procedure Scout_Instruction (
        Team : in Boards.Player_ID;
        Unit : in Boards.Unit_Type;
        State : in out Boards.Board;
        Machines : in out Processor_Array) is
        Me : Unit_Processor renames Machines (Team, Unit);
        A : Register_Type renames Me.Registers (Me.RA);
        B : Register_Type renames Me.Registers (Me.RB);
        C : Register_Type renames Me.Registers (Me.RC);
        Us : Unit_State := Get_Unit (State, Unit, Team);
        Enemy : Player_ID := Enemy_Of (Team);
    begin
        case Me.Op is
            when SCOUT_RUN =>
                if not Get_Unit (State, Unit, Team).Prone then
                    Do_Move (State, Team, Unit, To_Direction (Me.A));
                    B := Register_Type (Us.Position (Team).X);
                    C := Register_Type (Us.Position (Team).Y);
                end if;
            when SCOUT_HIT => Do_Hit (State, Team, Unit, To_Direction (Me.A));
            when SCOUT_QSU =>
                B := From_Boolean (
                    Get_Unit (State, To_Unit (Me.A), Team).Setup);
                C := From_Boolean (
                    Get_Unit (State, To_Unit (Me.A), Enemy).Setup);
            when SCOUT_QMV =>
                B := From_Boolean (
                    Get_Unit (State, To_Unit (Me.A), Team).Moving);
                C := From_Boolean (
                    Get_Unit (State, To_Unit (Me.A), Enemy).Moving);
            when SCOUT_QSH =>
                B := From_Boolean (
                    Get_Unit (State, To_Unit (Me.A), Team).Shooting);
                C := From_Boolean (
                    Get_Unit (State, To_Unit (Me.A), Enemy).Shooting);
            when SCOUT_QPR =>
                B := From_Boolean (
                    Get_Unit (State, To_Unit (Me.A), Team).Prone);
                C := From_Boolean (
                    Get_Unit (State, To_Unit (Me.A), Enemy).Prone);
            when SCOUT_LIE => Set_Prone (State, Team, Unit, True);
            when SCOUT_GUP => Set_Prone (State, Team, Unit, False);
            when SCOUT_CSS | SCOUT_CFS | SCOUT_WSS | SCOUT_WFS |
                SCOUT_BOM | SCOUT_AIR | SCOUT_MOR | SCOUT_SUP =>
                Ask_Instruction (Team, Unit, State, Machines);
            when others => null;
        end case;
    end Scout_Instruction;
end Processors.Scouts;
