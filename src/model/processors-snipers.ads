with Boards;

private package Processors.Snipers is
    procedure Set_Sniper_Registers (
        Machines : in out Processor_Array;
        State : in Boards.Board);
    function Sniper_Time (
        Op : in Instruction_ID;
        B : in Register_Type;
        C : in Register_Type;
        Position : in Boards.Location;
        Team : in Boards.Player_ID;
        State : in out Boards.Board)
        return Natural;
    procedure Sniper_Instruction (
        Team : in Boards.Player_ID;
        State : in out Boards.Board;
        Machines : in out Processor_Array);
end Processors.Snipers;
