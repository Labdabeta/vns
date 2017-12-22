with Boards;

private package Processors.Scouts is
    procedure Set_Scout_Registers (
        Machines : in out Processor_Array;
        State : in Boards.Board);
    function Scout_Time (
        Op : in Instruction_ID;
        State : in out Boards.Board;
        Team : in Boards.Player_ID;
        Unit : in Boards.Unit_Type;
        RA : in Register_Type) return Natural;
    procedure Scout_Instruction (
        Op : in Instruction_ID;
        Team : in Boards.Player_ID;
        Unit : in Boards.Unit_Type;
        Immediate : in Address_Type;
        State : in out Boards.Board;
        A : in out Register_Type;
        B : in out Register_Type;
        C : in out Register_Type;
        Machines : in out Processor_Array);
end Processors.Scouts;
