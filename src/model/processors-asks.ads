with Boards;

private package Processors.Asks is
    procedure Ask_Instruction (
        Team : in Boards.Player_ID;
        Unit : in Boards.Unit_Type;
        State : in Boards.Board;
        Machines : in out Processor_Array);
end Processors.Asks;
