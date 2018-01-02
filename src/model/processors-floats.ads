private package Processors.Floats is
    function Is_Float_Op (Op : in Instruction_ID) return Boolean;
    function Float_Time (Op : in Instruction_ID) return Natural;
    procedure Float_Instruction (Me : in out Unit_Processor);
end Processors.Floats;
