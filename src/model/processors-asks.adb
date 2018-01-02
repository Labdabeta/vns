with Boards; use Boards;

with Processors.Registers; use Processors.Registers;

package body Processors.Asks is
    ASK_CSS : constant Instruction_ID := 120;
    ASK_CFS : constant Instruction_ID := 121;
    ASK_WSS : constant Instruction_ID := 122;
    ASK_WFS : constant Instruction_ID := 123;
    ASK_BOM : constant Instruction_ID := 124;
    ASK_AIR : constant Instruction_ID := 125;
    ASK_MOR : constant Instruction_ID := 126;
    ASK_SUP : constant Instruction_ID := 127;

    procedure Ask_Instruction (
        Team : in Boards.Player_ID;
        Unit : in Boards.Unit_Type;
        State : in Boards.Board;
        Machines : in out Processor_Array) is
        Me : Unit_Processor renames Machines (Team, Unit);
        A : Register_Type renames Me.Registers (Me.RA);
        B : Register_Type renames Me.Registers (Me.RB);
        C : Register_Type renames Me.Registers (Me.RC);
    begin
        case Me.Op is
            when ASK_CSS =>
                Machines (Team, UT_ENGINEER_SS).Registers (16) := Me.B;
                Machines (Team, UT_ENGINEER_SS).Registers (17) := Me.C;
                A :=
                    From_Boolean (Get_Unit (State, UT_ENGINEER_SS, Team).Alive);
            when ASK_CFS =>
                Machines (Team, UT_ENGINEER_FS).Registers (16) := Me.B;
                Machines (Team, UT_ENGINEER_FS).Registers (17) := Me.C;
                A :=
                    From_Boolean (Get_Unit (State, UT_ENGINEER_FS, Team).Alive);
            when ASK_WSS =>
                Machines (Team, UT_ENGINEER_SS).Registers (18) := Me.B;
                Machines (Team, UT_ENGINEER_SS).Registers (19) := Me.C;
                A :=
                    From_Boolean (Get_Unit (State, UT_ENGINEER_SS, Team).Alive);
            when ASK_WFS =>
                Machines (Team, UT_ENGINEER_FS).Registers (18) := Me.B;
                Machines (Team, UT_ENGINEER_FS).Registers (19) := Me.C;
                A :=
                    From_Boolean (Get_Unit (State, UT_ENGINEER_FS, Team).Alive);
            when ASK_BOM =>
                Machines (Team, UT_CAPTAIN).Registers (30) :=
                    Machines (Team, UT_CAPTAIN).Registers (30) +
                        Register_Type (Me.Immediate);
                A := From_Boolean (Get_Unit (State, UT_CAPTAIN, Team).Alive);
            when ASK_AIR =>
                Machines (Team, UT_CAPTAIN).Registers (31) :=
                    Machines (Team, UT_CAPTAIN).Registers (31) +
                        Register_Type (Me.Immediate);
                A := From_Boolean (Get_Unit (State, UT_CAPTAIN, Team).Alive);
            when ASK_MOR =>
                Machines (Team, UT_MORTAR).Registers (16) := Me.B;
                Machines (Team, UT_MORTAR).Registers (17) := Me.C;
                A := From_Boolean (Get_Unit (State, UT_MORTAR, Team).Alive);
            when ASK_SUP =>
                Machines (Team, UT_RIFLEMAN_SS).Registers (16) := Me.B;
                Machines (Team, UT_RIFLEMAN_SS).Registers (17) := Me.C;
                Machines (Team, UT_RIFLEMAN_FS).Registers (16) := Me.B;
                Machines (Team, UT_RIFLEMAN_FS).Registers (17) := Me.C;
                Machines (Enemy_Of (Team), UT_RIFLEMAN_SS).Registers (18) :=
                    Me.B;
                Machines (Enemy_Of (Team), UT_RIFLEMAN_SS).Registers (19) :=
                    Me.C;
                Machines (Enemy_Of (Team), UT_RIFLEMAN_FS).Registers (18) :=
                    Me.B;
                Machines (Enemy_Of (Team), UT_RIFLEMAN_FS).Registers (19) :=
                    Me.C;
            when others => null;
        end case;
    end Ask_Instruction;
end Processors.Asks;
