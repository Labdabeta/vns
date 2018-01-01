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
    ENGINEER_QSU : constant Instruction_ID := 100;
    ENGINEER_QMV : constant Instruction_ID := 101;
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
            Nearest_Ally : Unit_State := Get_Nearest_Ally (
                State, To_Location (Pos, Team), Team);
            Nearest_Enemy : Unit_State := Get_Nearest_Ally (
                State, To_Location (Pos, Team), Enemy_Of (Team));
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
                24 => Register_Type (Nearest_Ally.Position (Team).X),
                25 => Register_Type (Nearest_Ally.Position (Team).Y),
                26 => Register_Type (Nearest_Enemy.Position (Team).X),
                27 => Register_Type (Nearest_Enemy.Position (Team).Y),
                28 => Register_Type (Get_Nearest_Terrain (
                    State, To_Location (Pos, Team), Team, TT_WATER).X),
                29 => Register_Type (Get_Nearest_Terrain (
                    State, To_Location (Pos, Team), Team, TT_WATER).Y),
                30 => Register_Type (Distance_To_Base (
                    State, To_Location (Pos, Team), Team)),
                31 => Register_Type (Distance_To_Base (
                    State, To_Location (Pos, Team), Them)));

            -- Check if the nearest ally/enemy is wrong and set to -1, -1 if so
            if Nearest_Ally = Null_Unit then
                Machines (Team, Engineer_IDs (Side)).Registers (24) := -1;
                Machines (Team, Engineer_IDs (Side)).Registers (25) := -1;
            end if;

            if Nearest_Enemy = Null_Unit then
                Machines (Team, Engineer_IDs (Side)).Registers (26) := -1;
                Machines (Team, Engineer_IDs (Side)).Registers (27) := -1;
            end if;
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
        if Is_Float_Op (Op) then
            return Float_Time (Op);
        end if;

        case Op is
            when ENGINEER_QSU | ENGINEER_QMV =>
                return 1;
            when ENGINEER_WIR | ENGINEER_CUT | ENGINEER_SND | ENGINEER_DIG |
                ENGINEER_CSS | ENGINEER_CFS | ENGINEER_WSS | ENGINEER_WFS |
                ENGINEER_BOM | ENGINEER_AIR | ENGINEER_MOR | ENGINEER_SUP =>
                return 8;
            when ENGINEER_LIE | ENGINEER_GUP =>
                return 64;
            when others =>
                return 0;
        end case;
    end Engineer_Time;

    procedure Engineer_Instruction (
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
            when ENGINEER_WIR =>
                Plant_Wire (State, Team, Unit, To_Direction (A));
                B := Register_Type (Count_Wire (State));
                C := Register_Type (Count_Cover (State));
            when ENGINEER_CUT =>
                Plant_Wire (State, Team, Unit, To_Direction (A), True);
                B := Register_Type (Count_Wire (State));
                C := Register_Type (Count_Cover (State));
            when ENGINEER_SND =>
                Plant_Cover (State, Team, Unit, To_Direction (A));
                B := Register_Type (Count_Wire (State));
                C := Register_Type (Count_Cover (State));
            when ENGINEER_DIG =>
                Plant_Cover (State, Team, Unit, To_Direction (A), True);
                B := Register_Type (Count_Wire (State));
                C := Register_Type (Count_Cover (State));
            when ENGINEER_QSU =>
                B := From_Boolean (Get_Unit (State, To_Unit (A), Team).Setup);
                C := From_Boolean (Get_Unit (State, To_Unit (A), Enemy).Setup);
            when ENGINEER_QMV =>
                B := From_Boolean (Get_Unit (State, To_Unit (A), Team).Moving);
                C := From_Boolean (Get_Unit (State, To_Unit (A), Enemy).Moving);
            when ENGINEER_LIE => Set_Prone (State, Team, Unit, True);
            when ENGINEER_GUP => Set_Prone (State, Team, Unit, False);
            when ENGINEER_CSS | ENGINEER_CFS | ENGINEER_WSS | ENGINEER_WFS |
                ENGINEER_BOM | ENGINEER_AIR | ENGINEER_MOR | ENGINEER_SUP =>
                Ask_Instruction (Op, Team, B, C, Immediate, State, A, Machines);
            when others => null;
        end case;
    end Engineer_Instruction;
end Processors.Engineers;
