with Ada.Streams;

package Memory is
    type Instruction_Code is mod 2 ** 7;
    type Register_Name is mod 2 ** 5;
    type Small_Immediate is mod 2 ** 10;
    type Immediate_Value is range -(2 ** 9) .. (2 ** 9 - 1);
    type Address_Value is mod 2 ** 20;

    type Cell is record
        Instruction : Instruction_Code;
        A, B, C : Register_Name;
        Small : Small_Immediate;
    end record;

    -- NOTE: Temporarily limited to 100000 cells per unit of code, otherwise
    -- stack is too big
    type Memory_Array is array (Address_Value range 0 .. 100000) of Cell;

    type Unit_Type is (UT_CAPTAIN, UT_MORTAR, UT_SNIPER, UT_ENGINEER_SS,
        UT_ENGINEER_FS, UT_MACHINEGUNNER_SS, UT_MACHINEGUNNER_FS, UT_SCOUT_SS,
        UT_SCOUT_FS, UT_RIFLEMAN_SS, UT_RIFLEMAN_FS);
    type Memory_Array_List is array (Unit_Type) of Memory_Array;
    type Memory_Size_List is array (Unit_Type) of Address_Value;
    type Memory_State is record
        Data : Memory_Array_List;
        Lengths : Memory_Size_List;
    end record;
    type Memory_Access is access Memory_State;
    procedure Free_Memory (What : in out Memory_Access);

    procedure Set_Address (Which : in out Cell; Value : in Address_Value);
    function Get_Address (Which : in Cell) return Address_Value;

    procedure Set_Immediate (Which : in out Cell; Value : in Immediate_Value);
    function Get_Immediate (Which : in Cell) return Immediate_Value;

    -- Converts a string integer into an entire cell! (for RAW ops)
    function String_To_Cell (Which : in String) return Cell;
    function String_To_Address (Which : in String) return Address_Value;
    function String_To_Immediate (Which : in String) return Immediate_Value;
    function String_To_Instruction (Which : in String; Unit : in Unit_Type)
        return Instruction_Code;
    function String_To_Register (Which : in String) return Register_Name;
    function String_To_Unit (Which : in String) return Unit_Type;

    function Is_Directive (Which : in String; Unit : in Unit_Type)
        return Boolean;
    function Is_Register (Which : in String) return Boolean;

    procedure Write (
        Stream : not null access Ada.Streams.Root_Stream_Type'Class;
        Item : in Cell);

    procedure Read (
        Stream : not null access Ada.Streams.Root_Stream_Type'Class;
        Item : out Cell);

    procedure Output (
        Stream : not null access Ada.Streams.Root_Stream_Type'Class;
        Item : in Cell);

    function Input (
        Stream : not null access Ada.Streams.Root_Stream_Type'Class)
        return Cell;

    for Cell'Write use Write;
    for Cell'Read use Read;
    for Cell'Output use Output;
    for Cell'Input use Input;
end Memory;
