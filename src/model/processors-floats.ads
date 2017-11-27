private package Processors.Floats is
    function Is_Float_Op (Op : in Instruction_ID) return Boolean;
    function Float_Time (Op : in Instruction_ID) return Natural;
    procedure Float_Instruction (
        Op : in Instruction_ID;
        A : in out Register_Type;
        B : in out Register_Type;
        C : in out Register_Type;
        Immediate : in Address_Type);
end Processors.Floats;
