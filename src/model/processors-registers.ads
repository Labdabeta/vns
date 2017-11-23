with Interfaces;
with Ada.Unchecked_Conversion;
with Coordinates;

package Processors.Registers is
    function From_Boolean (Val : in Boolean) return Register_Type;

    function To_U32 is new Ada.Unchecked_Conversion (
        Register_Type, Interfaces.Unsigned_32);
    function From_U32 is new Ada.Unchecked_Conversion (
        Interfaces.Unsigned_32, Register_Type);

    function To_Direction (X : in Register_Type) return Coordinates.Direction;
end Processors.Registers;
