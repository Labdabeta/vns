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
        Op : in Instruction_ID;
        Team : in Boards.Player_ID;
        Immediate : in Address_Type;
        State : in out Boards.Board;
        A : in out Register_Type;
        B : in out Register_Type;
        C : in out Register_Type;
        Machines : in out Processor_Array);
end Processors.Snipers;
