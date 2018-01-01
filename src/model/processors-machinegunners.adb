with Boards; use Boards;
with Processors.Registers; use Processors.Registers;
with Coordinates; use Coordinates;

with Processors.Floats; use Processors.Floats;
with Processors.Asks; use Processors.Asks;

package body Processors.Machinegunners is
    MG_SET : constant Instruction_ID := 96;
    MG_GUP : constant Instruction_ID := 97;
    MG_QSH : constant Instruction_ID := 98;
    MG_QPR : constant Instruction_ID := 99;
    MG_QSU : constant Instruction_ID := 100;
    MG_QMV : constant Instruction_ID := 101;
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

            -- Check if the nearest enemy is wrong and set to -1, -1 if so
            if Get_Nearest_Ally (State, Pos, Them) = Null_Unit then
                Machines (Team, Machinegunner_IDs (Side)).Registers (17) := -1;
                Machines (Team, Machinegunner_IDs (Side)).Registers (18) := -1;
                Machines (Team, Machinegunner_IDs (Side)).Registers (19) :=
                    Register_Type'Last;
            end if;
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
        if Is_Float_Op (Op) then
            return Float_Time (Op);
        end if;

        case Op is
            when MG_QSU | MG_QMV | MG_QSH | MG_QPR =>
                return 1;
            when MG_CSS | MG_CFS | MG_WSS | MG_WFS | MG_BOM |
                MG_AIR | MG_MOR | MG_SUP =>
                return 8;
            when MG_MLE =>
                return 16;
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
            when MG_SET => Set_Setup (State, Team, Unit, True);
            when MG_GUP => Set_Setup (State, Team, Unit, False);
            when MG_QSU =>
                B := From_Boolean (Get_Unit (State, To_Unit (A), Team).Setup);
                C := From_Boolean (Get_Unit (State, To_Unit (A), Enemy).Setup);
            when MG_QMV =>
                B := From_Boolean (Get_Unit (State, To_Unit (A), Team).Moving);
                C := From_Boolean (Get_Unit (State, To_Unit (A), Enemy).Moving);
            when MG_QSH =>
                B := From_Boolean (
                    Get_Unit (State, To_Unit (A), Team).Shooting);
                C := From_Boolean (
                    Get_Unit (State, To_Unit (A), Enemy).Shooting);
            when MG_QPR =>
                B := From_Boolean (Get_Unit (State, To_Unit (A), Team).Prone);
                C := From_Boolean (Get_Unit (State, To_Unit (A), Enemy).Prone);
            when MG_MLE =>
                Do_Melee (State, Team, Unit);
            when MG_MLF =>
                Do_Hit (State, Team, Unit, To_Direction (A));
            when MG_CSS | MG_CFS | MG_WSS | MG_WFS |
                MG_BOM | MG_AIR | MG_MOR | MG_SUP =>
                Ask_Instruction (Op, Team, B, C, Immediate, State, A, Machines);
            when others => null;
        end case;
    end Machinegunner_Instruction;
end Processors.Machinegunners;
