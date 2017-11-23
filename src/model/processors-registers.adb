package body Processors.Registers is

    function From_Boolean (Val : in Boolean) return Register_Type is
    begin
        if Val then
            return 1;
        else
            return 0;
        end if;
    end From_Boolean;

    function To_Direction (X : in Register_Type) return Coordinates.Direction is
    begin
        return Coordinates.Direction'Val (X - 1);
    end To_Direction;
end Processors.Registers;
