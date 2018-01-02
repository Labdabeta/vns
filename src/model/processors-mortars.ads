with Boards;

private package Processors.Mortars is
    procedure Set_Mortar_Registers (
        Machines : in out Processor_Array;
        State : in Boards.Board);
    function Mortar_Time (Op : in Instruction_ID) return Natural;
    procedure Mortar_Instruction (
        Team : in Boards.Player_ID;
        State : in out Boards.Board;
        Machines : in out Processor_Array);
end Processors.Mortars;
