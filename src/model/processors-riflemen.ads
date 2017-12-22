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
        Immediate : in Address_Type;
        State : in out Boards.Board;
        A : in out Register_Type;
        B : in out Register_Type;
        C : in out Register_Type;
        Machines : in out Processor_Array);
end Processors.Riflemen;
