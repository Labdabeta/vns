private package Processors.Floats is
    -- TODO: refactor timing/etc from each unit
    procedure Float_Instruction (
        Op : in Instruction_ID;
        B : in out Register_Type;
        C : in out Register_Type;
        Immediate : in Address_Type;
        A : in out Register_Type);
end Processors.Floats;
