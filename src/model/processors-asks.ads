with Boards;

private package Processors.Asks is
    -- TODO: same as float ops
    procedure Ask_Instruction (
        Op : in Instruction_ID;
        Team : in Boards.Player_ID;
        B : in Register_Type;
        C : in Register_Type;
        Immediate : in Address_Type;
        State : in Boards.Board;
        A : in out Register_Type;
        Machines : in out Processor_Array);
end Processors.Asks;
