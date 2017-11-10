package Coordinates is
    Width : constant := 32;
    Height : constant := 16;
    type X_Coordinate is mod Width;
    type Y_Coordinate is mod Height;
    type Coordinate is record
        X : X_Coordinate;
        Y : Y_Coordinate;
    end record;
    type Direction is (DIR_NORTH, DIR_NORTH_EAST, DIR_EAST, DIR_SOUTH_EAST,
        DIR_SOUTH, DIR_SOUTH_WEST, DIR_WEST, DIR_NORTH_WEST);

    type Coordinate_Path is array (Positive range <>) of Coordinate;
    type Coordinate_Path_Access is access all Coordinate_Path;

    function Get_Direction_Towards (
        Source, Destination : Coordinate)
        return Direction;

    procedure Apply_Direction (Source : in out Coordinate; Dir : in Direction);

    function Adjacent (
        Source, Destination : Coordinate)
        return Boolean;

    function Generate_Search_Path (From : Coordinate) return Coordinate_Path;

    --  The returned line does NOT include the source, but DOES include the
    --  destination
    function Get_Path_To (
        Source, Destination : Coordinate)
        return Coordinate_Path;

end Coordinates;
