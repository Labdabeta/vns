with Boards;

private package Processors.Mortars is
    procedure Set_Mortar_Registers (
        Machines : in out Processor_Array;
        State : in Boards.Board);
    function Mortar_Time (Op : in Instruction_ID) return Natural;
    procedure Mortar_Instruction (
        Op : in Instruction_ID;
        Team : in Boards.Player_ID;
        B : in Register_Type;
        C : in Register_Type;
        Immediate : in Address_Type;
        State : in out Boards.Board;
        A : in out Register_Type;
        Tactical : in out Shared_Grid;
        Support : in out Shared_Grid;
        Flag : in out Shared_Grid;
        Machines : in out Processor_Array);
end Processors.Mortars;
