with SDL;

-- This package converts from relative grid-based coordinates to real SDL values
package Grid is
    type Grid_X is range 1 .. 32;
    type Grid_Y is range 1 .. 29;

    function Rect (
        X : Grid_X;
        Y : Grid_Y)
        return SDL.Rectangle;

    function Small_Rect (
        X : Grid_X;
        Y : Grid_Y)
        return SDL.Rectangle;

    function Left_Rect (
        X : Grid_X;
        Y : Grid_Y)
        return SDL.Rectangle;

    function Right_Rect (
        X : Grid_X;
        Y : Grid_Y)
        return SDL.Rectangle;

    function Center (
        X : Grid_X;
        Y : Grid_Y)
        return SDL.Coordinate;

    type Coordinate_Pair is
        record
            Start : SDL.Coordinate;
            Finish : SDL.Coordinate;
        end record;
    type Line_List is array (Positive range <>) of Coordinate_Pair;
    function Grid_Lines (
        Min_X : Grid_X;
        Max_X : Grid_X;
        Min_Y : Grid_Y;
        Max_Y : Grid_Y)
        return Line_List;

    procedure Crop (
        Width : Grid_X;
        Height : Grid_Y);
end Grid;
