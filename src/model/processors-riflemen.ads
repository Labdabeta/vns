with Boards;

private package Processors.Riflemen is
    procedure Set_Rifleman_Registers (
        Machines : in out Processor_Array;
        State : in Boards.Board);
    function Rifleman_Time (Op : in Instruction_ID) return Natural;
    procedure Rifleman_Instruction (
        Team : in Boards.Player_ID;
        Unit : in Boards.Unit_Type;
        State : in out Boards.Board;
        Machines : in out Processor_Array);
end Processors.Riflemen;
