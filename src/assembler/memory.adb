with Ada.Streams;
with Interfaces; use Interfaces;

with Ada.Unchecked_Deallocation;

package body Memory is
    procedure Free_Memory (What : in out Memory_Access) is
        procedure Free is new Ada.Unchecked_Deallocation (
            Memory_State, Memory_Access);
    begin
        Free (What);
    end Free_Memory;

    procedure Set_Address (Which : in out Cell; Value : in Address_Value) is
    begin
        Which.B := Register_Name (Value / (2 ** 15));
        Which.C := Register_Name ((Value / (2 ** 10)) and 16#1F#);
        Which.Small := Small_Immediate (Value and 2#1111111111#);
    end Set_Address;

    function Get_Address (Which : in Cell) return Address_Value is
    begin
        return Address_Value (Which.B) * (2 ** 15) or
            Address_Value (Which.C) * (2 ** 10) or
            Address_Value (Which.Small);
    end Get_Address;

    procedure Set_Immediate (Which : in out Cell; Value : in Immediate_Value) is
    begin
        if Value < 0 then
            Which.Small := 2#1000000000# or (not Small_Immediate (-Value + 1));
        else
            Which.Small := Small_Immediate (Value);
        end if;
    end Set_Immediate;

    function Get_Immediate (Which : in Cell) return Immediate_Value is
        Result : Immediate_Value := 0;
    begin
        if (Which.Small and 2#1000000000#) /= 0 then
            Result := Immediate_Value'First;
        end if;

        Result := Result + Immediate_Value (Which.Small and 2#111111111#);
        return Result;
    end Get_Immediate;

    procedure Write (
        Stream : not null access Ada.Streams.Root_Stream_Type'Class;
        Item : in Cell) is
        Value : Unsigned_32;
    begin
        Value :=
            Unsigned_32 (Item.Instruction) * (2 ** 25) or
            Unsigned_32 (Item.A) * (2 ** 20) or
            Unsigned_32 (Item.B) * (2 ** 15) or
            Unsigned_32 (Item.C) * (2 ** 10) or
            Unsigned_32 (Item.Small);
        Unsigned_32'Write (Stream, Value);
    end Write;

    procedure Read (
        Stream : not null access Ada.Streams.Root_Stream_Type'Class;
        Item : out Cell) is
        Value : Unsigned_32;
    begin
        Unsigned_32'Read (Stream, Value);
        Item.Instruction := Instruction_Code (Value / (2 ** 25));
        Item.A := Register_Name ((Value / (2 ** 20)) and Unsigned_32 (16#1F#));
        Item.B := Register_Name ((Value / (2 ** 15)) and Unsigned_32 (16#1F#));
        Item.C := Register_Name ((Value / (2 ** 10)) and Unsigned_32 (16#1F#));
        Item.Small := Small_Immediate (Value and Unsigned_32 (2#1111111111#));
    end Read;

    procedure Output (
        Stream : not null access Ada.Streams.Root_Stream_Type'Class;
        Item : in Cell) is
    begin
        Write (Stream, Item);
    end Output;

    function Input (
        Stream : not null access Ada.Streams.Root_Stream_Type'Class)
        return Cell is
        Item : Cell;
    begin
        Read (Stream, Item);
        return Item;
    end Input;

    ----------------------------------------------------------------------------
    -- String conversions
    ----------------------------------------------------------------------------

    type Directive_Listing is array (Instruction_Code) of String (1 .. 3);
    Directive_Names : constant array (Unit_Type) of Directive_Listing := (
        UT_CAPTAIN => (
            -- Common
            "add", "sub", "mul", "div", "and", "orr", "xor", "nan",
            "clz", "cnt", "lsr", "lsl", "abs", "rnd", "cmp", "jiz",
            "jnz", "jgz", "jlz", "jge", "jle", "biz", "bnz", "bgz",
            "blz", "bge", "ble", "blx", "ldr", "str", "pop", "psh",
            -- Combat
            "who", "wht", "qcs", "qct", "qbp", "qck", "gnd", "whr",
            "dst", "cvr", "ded", "sht", "dir", "wlk", "crl", "swm",
            "cap", "rtt", "hid", "say", "rad", "yel", "ear", "die",
            "nrt", "nre", "est", "soe", "sot", "sow", "wst", "nrw",
            -- Upgrade
            "wcs", "wct", "wbp", "wcl", "tcs", "tct", "tbp", "tcl",
            "pnt", "ccs", "cct", "cbp", "ccl", "ucs", "uct", "ubp",
            "ucl", "dcs", "dct", "dbp", "dcl", "mcs", "mct", "mbp",
            "mcl", "rcs", "rct", "rbp", "rcl", "tim", "dly", "adv",
            -- Specific
            "ask", "plz", "beg", "gvl", "kmk", "sac", "bom", "air",
            "sum", "hak", "emp", "all", "rmt", "rsn", "res", "ref",
            "rms", "rmf", "rss", "rsf", "rrs", "rrf", "wmt", "wsn",
            "wes", "wef", "wms", "wmf", "wss", "wsf", "wrs", "wrf"),
        UT_MORTAR => (
            -- Common
            "add", "sub", "mul", "div", "and", "orr", "xor", "nan",
            "clz", "cnt", "lsr", "lsl", "abs", "rnd", "cmp", "jiz",
            "jnz", "jgz", "jlz", "jge", "jle", "biz", "bnz", "bgz",
            "blz", "bge", "ble", "blx", "ldr", "str", "pop", "psh",
            -- Combat
            "who", "wht", "qcs", "qct", "qbp", "qck", "gnd", "whr",
            "dst", "cvr", "ded", "sht", "dir", "wlk", "crl", "swm",
            "cap", "rtt", "hid", "say", "rad", "yel", "ear", "die",
            "nrt", "nre", "est", "soe", "sot", "sow", "wst", "nrw",
            -- Upgrade
            "wcs", "wct", "wbp", "wcl", "tcs", "tct", "tbp", "tcl",
            "pnt", "ccs", "cct", "cbp", "ccl", "ucs", "uct", "ubp",
            "ucl", "dcs", "dct", "dbp", "dcl", "mcs", "mct", "mbp",
            "mcl", "rcs", "rct", "rbp", "rcl", "tim", "dly", "adv",
            -- Specific
            "wtg", "rtg", "wsg", "rsg", "wfg", "rfg", "itf", "fad",
            "fsu", "fmu", "fdv", "cel", "flr", "sin", "cos", "tan",
            "pow", "asn", "acs", "atn", "log", "fcp", "mle", "set",
            "css", "cfs", "wss", "wfs", "bom", "air", "gup", "sup"),
        UT_SNIPER => (
            -- Common
            "add", "sub", "mul", "div", "and", "orr", "xor", "nan",
            "clz", "cnt", "lsr", "lsl", "abs", "rnd", "cmp", "jiz",
            "jnz", "jgz", "jlz", "jge", "jle", "biz", "bnz", "bgz",
            "blz", "bge", "ble", "blx", "ldr", "str", "pop", "psh",
            -- Combat
            "who", "wht", "qcs", "qct", "qbp", "qck", "gnd", "whr",
            "dst", "cvr", "ded", "sht", "dir", "wlk", "crl", "swm",
            "cap", "rtt", "hid", "say", "rad", "yel", "ear", "die",
            "nrt", "nre", "est", "soe", "sot", "sow", "wst", "nrw",
            -- Upgrade
            "wcs", "wct", "wbp", "wcl", "tcs", "tct", "tbp", "tcl",
            "pnt", "ccs", "cct", "cbp", "ccl", "ucs", "uct", "ubp",
            "ucl", "dcs", "dct", "dbp", "dcl", "mcs", "mct", "mbp",
            "mcl", "rcs", "rct", "rbp", "rcl", "tim", "dly", "adv",
            -- Specific
            "cam", "unc", "hip", "nmr", "ist", "tgt", "itf", "fad",
            "fsu", "fmu", "fdv", "cel", "flr", "sin", "cos", "tan",
            "pow", "asn", "acs", "atn", "log", "fcp", "lie", "gup",
            "css", "cfs", "wss", "wfs", "bom", "air", "mor", "sup"),
        UT_ENGINEER_SS | UT_ENGINEER_FS => (
            -- Common
            "add", "sub", "mul", "div", "and", "orr", "xor", "nan",
            "clz", "cnt", "lsr", "lsl", "abs", "rnd", "cmp", "jiz",
            "jnz", "jgz", "jlz", "jge", "jle", "biz", "bnz", "bgz",
            "blz", "bge", "ble", "blx", "ldr", "str", "pop", "psh",
            -- Combat
            "who", "wht", "qcs", "qct", "qbp", "qck", "gnd", "whr",
            "dst", "cvr", "ded", "sht", "dir", "wlk", "crl", "swm",
            "cap", "rtt", "hid", "say", "rad", "yel", "ear", "die",
            "nrt", "nre", "est", "soe", "sot", "sow", "wst", "nrw",
            -- Upgrade
            "wcs", "wct", "wbp", "wcl", "tcs", "tct", "tbp", "tcl",
            "pnt", "ccs", "cct", "cbp", "ccl", "ucs", "uct", "ubp",
            "ucl", "dcs", "dct", "dbp", "dcl", "mcs", "mct", "mbp",
            "mcl", "rcs", "rct", "rbp", "rcl", "tim", "dly", "adv",
            -- Specific
            "wir", "cut", "snd", "dig", "wfg", "rfg", "itf", "fad",
            "fsu", "fmu", "fdv", "cel", "flr", "sin", "cos", "tan",
            "pow", "asn", "acs", "atn", "log", "fcp", "lie", "gup",
            "css", "cfs", "wss", "wfs", "bom", "air", "mor", "sup"),
        UT_MACHINEGUNNER_SS | UT_MACHINEGUNNER_FS => (
            -- Common
            "add", "sub", "mul", "div", "and", "orr", "xor", "nan",
            "clz", "cnt", "lsr", "lsl", "abs", "rnd", "cmp", "jiz",
            "jnz", "jgz", "jlz", "jge", "jle", "biz", "bnz", "bgz",
            "blz", "bge", "ble", "blx", "ldr", "str", "pop", "psh",
            -- Combat
            "who", "wht", "qcs", "qct", "qbp", "qck", "gnd", "whr",
            "dst", "cvr", "ded", "sht", "dir", "wlk", "crl", "swm",
            "cap", "rtt", "hid", "say", "rad", "yel", "ear", "die",
            "nrt", "nre", "est", "soe", "sot", "sow", "wst", "nrw",
            -- Upgrade
            "wcs", "wct", "wbp", "wcl", "tcs", "tct", "tbp", "tcl",
            "pnt", "ccs", "cct", "cbp", "ccl", "ucs", "uct", "ubp",
            "ucl", "dcs", "dct", "dbp", "dcl", "mcs", "mct", "mbp",
            "mcl", "rcs", "rct", "rbp", "rcl", "tim", "dly", "adv",
            -- Specific
            "set", "gup", "wsg", "rsg", "wfg", "rfg", "itf", "fad",
            "fsu", "fmu", "fdv", "cel", "flr", "sin", "cos", "tan",
            "pow", "asn", "acs", "atn", "log", "fcp", "mle", "mlf",
            "css", "cfs", "wss", "wfs", "bom", "air", "mor", "sup"),
        UT_SCOUT_SS | UT_SCOUT_FS => (
            -- Common
            "add", "sub", "mul", "div", "and", "orr", "xor", "nan",
            "clz", "cnt", "lsr", "lsl", "abs", "rnd", "cmp", "jiz",
            "jnz", "jgz", "jlz", "jge", "jle", "biz", "bnz", "bgz",
            "blz", "bge", "ble", "blx", "ldr", "str", "pop", "psh",
            -- Combat
            "who", "wht", "qcs", "qct", "qbp", "qck", "gnd", "whr",
            "dst", "cvr", "ded", "sht", "dir", "wlk", "crl", "swm",
            "cap", "rtt", "hid", "say", "rad", "yel", "ear", "die",
            "nrt", "nre", "est", "soe", "sot", "sow", "wst", "nrw",
            -- Upgrade
            "wcs", "wct", "wbp", "wcl", "tcs", "tct", "tbp", "tcl",
            "pnt", "ccs", "cct", "cbp", "ccl", "ucs", "uct", "ubp",
            "ucl", "dcs", "dct", "dbp", "dcl", "mcs", "mct", "mbp",
            "mcl", "rcs", "rct", "rbp", "rcl", "tim", "dly", "adv",
            -- Specific
            "run", "hit", "wsg", "rsg", "wfg", "rfg", "itf", "fad",
            "fsu", "fmu", "fdv", "cel", "flr", "sin", "cos", "tan",
            "pow", "asn", "acs", "atn", "log", "fcp", "lie", "gup",
            "css", "cfs", "wss", "wfs", "bom", "air", "mor", "sup"),
        UT_RIFLEMAN_SS | UT_RIFLEMAN_FS => (
            -- Common
            "add", "sub", "mul", "div", "and", "orr", "xor", "nan",
            "clz", "cnt", "lsr", "lsl", "abs", "rnd", "cmp", "jiz",
            "jnz", "jgz", "jlz", "jge", "jle", "biz", "bnz", "bgz",
            "blz", "bge", "ble", "blx", "ldr", "str", "pop", "psh",
            -- Combat
            "who", "wht", "qcs", "qct", "qbp", "qck", "gnd", "whr",
            "dst", "cvr", "ded", "sht", "dir", "wlk", "crl", "swm",
            "cap", "rtt", "hid", "say", "rad", "yel", "ear", "die",
            "nrt", "nre", "est", "soe", "sot", "sow", "wst", "nrw",
            -- Upgrade
            "wcs", "wct", "wbp", "wcl", "tcs", "tct", "tbp", "tcl",
            "pnt", "ccs", "cct", "cbp", "ccl", "ucs", "uct", "ubp",
            "ucl", "dcs", "dct", "dbp", "dcl", "mcs", "mct", "mbp",
            "mcl", "rcs", "rct", "rbp", "rcl", "tim", "dly", "adv",
            -- Specific
            "wtg", "rtg", "wsg", "rsg", "wfg", "rfg", "itf", "fad",
            "fsu", "fmu", "fdv", "cel", "flr", "sin", "cos", "tan",
            "pow", "asn", "acs", "atn", "log", "fcp", "lie", "gup",
            "css", "cfs", "wss", "wfs", "bom", "air", "mor", "sup"));

    function String_To_Cell (Which : in String) return Cell is
        Value : Unsigned_32;
        Item : Cell;
    begin
        Value := Unsigned_32'Value (Which);

        Item.Instruction := Instruction_Code (Value / (2 ** 25));
        Item.A := Register_Name ((Value / (2 ** 20)) and Unsigned_32 (16#1F#));
        Item.B := Register_Name ((Value / (2 ** 15)) and Unsigned_32 (16#1F#));
        Item.C := Register_Name ((Value / (2 ** 10)) and Unsigned_32 (16#1F#));
        Item.Small := Small_Immediate (Value and Unsigned_32 (2#1111111111#));

        return Item;
    end String_To_Cell;

    function String_To_Address (Which : in String) return Address_Value is
    begin
        return Address_Value'Value (Which);
    end String_To_Address;

    function String_To_Immediate (Which : in String) return Immediate_Value is
    begin
        return Immediate_Value'Value (Which);
    end String_To_Immediate;

    function String_To_Instruction (Which : in String; Unit : in Unit_Type)
        return Instruction_Code is
    begin
        for Index in Directive_Names (Unit)'Range loop
            if Which = Directive_Names (Unit) (Index) then
                return Index;
            end if;
        end loop;

        return 0;
    end String_To_Instruction;

    function String_To_Register (Which : in String) return Register_Name is
    begin -- Just chop off the r
        return Register_Name'Value (Which (Which'First + 1 .. Which'Last));
    end String_To_Register;

    function String_To_Unit (Which : in String) return Unit_Type is
    begin
        if Which = "captain" then
            return UT_CAPTAIN;
        elsif Which = "mortar" then
            return UT_MORTAR;
        elsif Which = "sniper" then
            return UT_SNIPER;
        elsif Which = "engineer_ss" then
            return UT_ENGINEER_SS;
        elsif Which = "engineer_fs" then
            return UT_ENGINEER_FS;
        elsif Which = "mg_ss" then
            return UT_MACHINEGUNNER_SS;
        elsif Which = "mg_fs" then
            return UT_MACHINEGUNNER_FS;
        elsif Which = "scout_ss" then
            return UT_SCOUT_SS;
        elsif Which = "scout_fs" then
            return UT_SCOUT_FS;
        elsif Which = "rifleman_ss" then
            return UT_RIFLEMAN_SS;
        elsif Which = "rifleman_fs" then
            return UT_RIFLEMAN_FS;
        else
            return UT_CAPTAIN;
        end if;
    end String_To_Unit;

    function Is_Directive (Which : in String; Unit : in Unit_Type)
        return Boolean is
    begin
        for Index in Directive_Names (Unit)'Range loop
            if Directive_Names (Unit) (Index) = Which then
                return True;
            end if;
        end loop;
        return False;
    end Is_Directive;

    function Is_Register (Which : in String) return Boolean is
        Id_Part : String := Which (Which'First + 1 .. Which'Last);
        Val : Integer;
    begin
        if Which (Which'First) /= 'r' and Which (Which'First) /= 'R' then
            return False;
        end if;

        Val := Integer'Value (Id_Part);
        return Val >= Integer (Register_Name'First) and
               Val <= Integer (Register_Name'Last);
    end Is_Register;

end Memory;
