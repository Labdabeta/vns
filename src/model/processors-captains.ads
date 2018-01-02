with Boards;

private package Processors.Captains is
    procedure Set_Captain_Registers (
        Machines : in out Processor_Array;
        State : in Boards.Board);
    function Captain_Time (
        Op : in Instruction_ID;
        A : in Register_Type;
        Team : in Boards.Player_ID;
        State : in out Boards.Board)
        return Natural;
    procedure Captain_Instruction (
        Team : in Boards.Player_ID;
        Me : in out Unit_Processor;
        State : in out Boards.Board;
        Shared : in out Shared_Memory;
        Radios : in out Communications);
end Processors.Captains;
