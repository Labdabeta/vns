package body Processors.Instructions is
    function Is_Taken (
        Which : in Instruction_ID;
        Value : in Register_Type) return Boolean is
    begin
        case Which is
            when 15 | 21 => return Value = 0;
            when 16 | 22 => return Value /= 0;
            when 17 | 23 => return Value > 0;
            when 18 | 24 => return Value < 0;
            when 19 | 25 => return Value >= 0;
            when 20 | 26 => return Value <= 0;
            when others => return False;
        end case;
    end Is_Taken;
end Processors.Instructions;
