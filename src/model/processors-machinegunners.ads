with Boards;

private package Processors.Machinegunners is
    procedure Set_Machinegunner_Registers (
        Machines : in out Processor_Array;
        State : in Boards.Board);
    function Machinegunner_Time (Op : in Instruction_ID) return Natural;
    procedure Machinegunner_Instruction (
        Team : in Boards.Player_ID;
        Unit : in Boards.Unit_Type;
        State : in out Boards.Board;
        Machines : in out Processor_Array);
end Processors.Machinegunners;
