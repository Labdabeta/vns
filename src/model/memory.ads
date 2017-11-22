with Processors;
with Boards;
with Ada.Streams;

package Memory is
    type Small_Immediate is mod 2 ** 10;
    type Cell is record
        Instruction : Processors.Instruction_ID;
        A, B, C : Processors.Register_Index;
        Small : Small_Immediate;
    end record;

    type Memory_Array is array (Processors.Address_Type range 0 .. 100000) of
        Cell;

    type Memory_Array_List is array (Boards.Unit_Type) of Memory_Array;
    type Memory_Size_List is array (Boards.Unit_Type) of
        Processors.Address_Type;
    type Memory_State is record
        Data : Memory_Array_List;
        Lengths : Memory_Size_List;
    end record;
    type Memory_Access is access Memory_State;
    procedure Free_Memory (What : in out Memory_Access);

    procedure Set_Address (
        Which : in out Cell;
        Value : in Processors.Address_Type);
    function Get_Address (Which : in Cell) return Processors.Address_Type;

    procedure Set_Immediate (
        Which : in out Cell;
        Value : in Processors.Small_Immediate_Type);
    function Get_Immediate (Which : in Cell)
        return Processors.Small_Immediate_Type;

    -- Converts a string integer into an entire cell! (for RAW ops)
    function String_To_Cell (Which : in String) return Cell;
    function String_To_Address (Which : in String)
        return Processors.Address_Type;
    function String_To_Immediate (Which : in String)
        return Processors.Small_Immediate_Type;
    function String_To_Instruction (
        Which : in String;
        Unit : in Boards.Unit_Type)
        return Processors.Instruction_ID;
    function String_To_Register (Which : in String)
        return Processors.Register_Index;
    function String_To_Unit (Which : in String) return Boards.Unit_Type;

    function Is_Directive (Which : in String; Unit : in Boards.Unit_Type)
        return Boolean;
    function Is_Register (Which : in String) return Boolean;

    function To_String (
        Code : in Processors.Instruction_ID;
        Unit : in Boards.Unit_Type) return String;

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
