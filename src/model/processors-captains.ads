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
        Op : in Instruction_ID;
        Team : in Boards.Player_ID;
        Immediate : in Address_Type;
        A : in out Register_Type;
        State : in out Boards.Board;
        Shared : in out Shared_Memory;
        Radios : in out Communications);
end Processors.Captains;
