with Boards;

private package Processors.Engineers is
    procedure Set_Engineer_Registers (
        Machines : in out Processor_Array;
        State : in Boards.Board);
    function Engineer_Time (Op : in Instruction_ID) return Natural;
    procedure Engineer_Instruction (
        Team : in Boards.Player_ID;
        Unit : in Boards.Unit_Type;
        State : in out Boards.Board;
        Machines : in out Processor_Array);
end Processors.Engineers;
