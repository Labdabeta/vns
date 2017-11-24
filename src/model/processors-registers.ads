with Interfaces;
with Ada.Unchecked_Conversion;
with Coordinates;
with Boards;

package Processors.Registers is
    function From_Boolean (Val : in Boolean) return Register_Type;

    function To_U32 is new Ada.Unchecked_Conversion (
        Register_Type, Interfaces.Unsigned_32);
    function From_U32 is new Ada.Unchecked_Conversion (
        Interfaces.Unsigned_32, Register_Type);

    function To_Direction (X : in Register_Type) return Coordinates.Direction;
    function To_Coordinate (
        X : in Register_Type;
        Y : in Register_Type)
        return Coordinates.Coordinate;

    function To_Unit (X : in Register_Type) return Boards.Unit_Type;
    function From_Unit (X : in Boards.Unit_Type) return Register_Type;

    function Count_Leading_Zeroes (X : in Register_Type) return Register_Type;
    function Count_Leading_Ones (X : in Register_Type) return Register_Type;
    function Count_Ones (X : in Register_Type) return Register_Type;
    function Count_Zeroes (X : in Register_Type) return Register_Type;
    function Shift_Right (X, By : in Register_Type) return Register_Type;
    function Shift_Left (X, By : in Register_Type) return Register_Type;
end Processors.Registers;
