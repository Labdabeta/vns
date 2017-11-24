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
        Op : in Instruction_ID;
        Team : in Boards.Player_ID;
        B : in Register_Type;
        C : in Register_Type;
        Immediate : in Address_Type;
        State : in Boards.Board;
        A : in out Register_Type;
        Machines : in out Processor_Array) is
    begin
        case Op is
            when ASK_CSS =>
                Machines (Team, UT_ENGINEER_SS).Registers (16) := B;
                Machines (Team, UT_ENGINEER_SS).Registers (17) := C;
                A :=
                    From_Boolean (Get_Unit (State, UT_ENGINEER_SS, Team).Alive);
            when ASK_CFS =>
                Machines (Team, UT_ENGINEER_FS).Registers (16) := B;
                Machines (Team, UT_ENGINEER_FS).Registers (17) := C;
                A :=
                    From_Boolean (Get_Unit (State, UT_ENGINEER_FS, Team).Alive);
            when ASK_WSS =>
                Machines (Team, UT_ENGINEER_SS).Registers (18) := B;
                Machines (Team, UT_ENGINEER_SS).Registers (19) := C;
                A :=
                    From_Boolean (Get_Unit (State, UT_ENGINEER_SS, Team).Alive);
            when ASK_WFS =>
                Machines (Team, UT_ENGINEER_FS).Registers (18) := B;
                Machines (Team, UT_ENGINEER_FS).Registers (19) := C;
                A :=
                    From_Boolean (Get_Unit (State, UT_ENGINEER_FS, Team).Alive);
            when ASK_BOM =>
                Machines (Team, UT_CAPTAIN).Registers (30) :=
                    Machines (Team, UT_CAPTAIN).Registers (30) +
                        Register_Type (Immediate);
                A := From_Boolean (Get_Unit (State, UT_CAPTAIN, Team).Alive);
            when ASK_AIR =>
                Machines (Team, UT_CAPTAIN).Registers (31) :=
                    Machines (Team, UT_CAPTAIN).Registers (31) +
                        Register_Type (Immediate);
                A := From_Boolean (Get_Unit (State, UT_CAPTAIN, Team).Alive);
            when ASK_MOR =>
                Machines (Team, UT_MORTAR).Registers (16) := B;
                Machines (Team, UT_MORTAR).Registers (17) := C;
                A := From_Boolean (Get_Unit (State, UT_MORTAR, Team).Alive);
            when ASK_SUP =>
                Machines (Team, UT_RIFLEMAN_SS).Registers (16) := B;
                Machines (Team, UT_RIFLEMAN_SS).Registers (17) := C;
                Machines (Team, UT_RIFLEMAN_FS).Registers (16) := B;
                Machines (Team, UT_RIFLEMAN_FS).Registers (17) := C;
                Machines (Enemy_Of (Team), UT_RIFLEMAN_SS).Registers (18) := B;
                Machines (Enemy_Of (Team), UT_RIFLEMAN_SS).Registers (19) := C;
                Machines (Enemy_Of (Team), UT_RIFLEMAN_FS).Registers (18) := B;
                Machines (Enemy_Of (Team), UT_RIFLEMAN_FS).Registers (19) := C;
            when others => null;
        end case;
    end Ask_Instruction;
end Processors.Asks;
