with Boards;

private package Processors.Riflemen is
    procedure Set_Rifleman_Registers (
        Machines : in out Processor_Array;
        State : in Boards.Board);
    function Rifleman_Time (Op : in Instruction_ID) return Natural;
    procedure Rifleman_Instruction (
        Op : in Instruction_ID;
        Team : in Boards.Player_ID;
        Unit : in Boards.Unit_Type;
        B : in Register_Type;
        C : in Register_Type;
        Immediate : in Address_Type;
        State : in out Boards.Board;
        A : in out Register_Type;
        Tactical : in out Shared_Grid;
        Support : in out Shared_Grid;
        Flags : in out Shared_Grid;
        Machines : in out Processor_Array);
end Processors.Riflemen;