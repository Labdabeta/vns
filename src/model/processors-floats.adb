with Ada.Numerics.Generic_Elementary_Functions;
with Ada.Unchecked_Conversion;

package body Processors.Floats is
    -- Instruction Names
    FLOAT_ITF : constant Instruction_ID := 102;
    FLOAT_FAD : constant Instruction_ID := 103;
    FLOAT_FSU : constant Instruction_ID := 104;
    FLOAT_FMU : constant Instruction_ID := 105;
    FLOAT_FDV : constant Instruction_ID := 106;
    FLOAT_CEL : constant Instruction_ID := 107;
    FLOAT_RTF : constant Instruction_ID := 108;
    FLOAT_SIN : constant Instruction_ID := 109;
    FLOAT_COS : constant Instruction_ID := 110;
    FLOAT_TAN : constant Instruction_ID := 111;
    FLOAT_POW : constant Instruction_ID := 112;
    FLOAT_ASN : constant Instruction_ID := 113;
    FLOAT_ACS : constant Instruction_ID := 114;
    FLOAT_ATN : constant Instruction_ID := 115;
    FLOAT_LOG : constant Instruction_ID := 116;
    FLOAT_FCP : constant Instruction_ID := 117;

    function Is_Float_Op (Op : in Instruction_ID) return Boolean is
    begin
        return Op >= FLOAT_ITF and Op <= FLOAT_FCP;
    end Is_Float_Op;

    function Float_Time (Op : in Instruction_ID) return Natural is
    begin
        case Op is
            when FLOAT_ITF | FLOAT_FAD | FLOAT_FSU | FLOAT_CEL | FLOAT_RTF |
                FLOAT_FCP =>
                return 8;
            when FLOAT_FMU =>
                return 32;
            when FLOAT_FDV | FLOAT_SIN | FLOAT_COS | FLOAT_TAN | FLOAT_POW |
                FLOAT_ASN | FLOAT_ACS | FLOAT_ATN | FLOAT_LOG =>
                return 64;
            when others =>
                return 0;
        end case;
    end Float_Time;

    procedure Float_Instruction (
        Op : in Instruction_ID;
        A : in out Register_Type;
        B : in out Register_Type;
        C : in out Register_Type;
        Immediate : in Address_Type) is
        package Value_Functions is new
            Ada.Numerics.Generic_Elementary_Functions (Float);
        use Value_Functions;
        function To_R is new Ada.Unchecked_Conversion (
            Float, Register_Type);
        function To_F is new Ada.Unchecked_Conversion (
            Register_Type, Float);
    begin
        case Op is
            when FLOAT_ITF => A := To_R (Float (Immediate));
            when FLOAT_FAD => A := To_R (To_F (B) + To_F (C));
            when FLOAT_FSU => A := To_R (To_F (B) - To_F (C));
            when FLOAT_FMU => A := To_R (To_F (B) * To_F (C));
            when FLOAT_FDV => A := To_R (To_F (B) / To_F (C));
            when FLOAT_CEL =>
                B := Register_Type (Float'Ceiling (To_F (A)));
                C := Register_Type (Float'Floor (To_F (A)));
            when FLOAT_RTF => A := Register_Type (Float'Floor (
                To_F (A) / Float (Immediate)));
            when FLOAT_SIN => A := To_R (Sin (To_F (A)));
            when FLOAT_COS => A := To_R (Cos (To_F (A)));
            when FLOAT_TAN => A := To_R (Tan (To_F (A)));
            when FLOAT_POW => A := To_R (To_F (B) ** To_F (C));
            when FLOAT_ASN => A := To_R (Arcsin (To_F (A)));
            when FLOAT_ACS => A := To_R (Arccos (To_F (A)));
            when FLOAT_ATN => A := To_R (Arctan (To_F (A)));
            when FLOAT_LOG => A := To_R (Log (To_F (B), To_F (C)));
            when FLOAT_FCP =>
                if To_F (B) > To_F (C) then
                    A := 1;
                elsif To_F (B) < To_F (C) then
                    A := -1;
                else
                    A := 0;
                end if;
            when others => null;
        end case;
    end Float_Instruction;
end Processors.Floats;
