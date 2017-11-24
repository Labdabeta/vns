with Boards;

private package Processors.Machinegunners is
    procedure Set_Machinegunner_Registers (
        Machines : in out Processor_Array;
        State : in Boards.Board);
    function Machinegunner_Time (Op : in Instruction_ID) return Natural;
    procedure Machinegunner_Instruction (
        Op : in Instruction_ID;
        Team : in Boards.Player_ID;
        Unit : in Boards.Unit_Type;
        B : in Register_Type;
        C : in Register_Type;
        Immediate : in Address_Type;
        State : in out Boards.Board;
        A : in out Register_Type;
        Support : in out Shared_Grid;
        Flags : in out Shared_Grid;
        Machines : in out Processor_Array);
end Processors.Machinegunners;
