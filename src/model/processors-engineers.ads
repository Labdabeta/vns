with Boards;

private package Processors.Engineers is
    procedure Set_Engineer_Registers (
        Machines : in out Processor_Array;
        State : in Boards.Board);
    function Engineer_Time (Op : in Instruction_ID) return Natural;
    procedure Engineer_Instruction (
        Op : in Instruction_ID;
        Team : in Boards.Player_ID;
        Unit : in Boards.Unit_Type;
        Immediate : in Address_Type;
        State : in out Boards.Board;
        A : in out Register_Type;
        B : in out Register_Type;
        C : in out Register_Type;
        Machines : in out Processor_Array);
end Processors.Engineers;
