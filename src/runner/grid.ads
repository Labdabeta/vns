with SDL;

-- This package converts from relative grid-based coordinates to real SDL values
package Grid is
    type Grid_X is range 1 .. 32;
    type Grid_Y is range 1 .. 28;

    function Rect (
        X : Grid_X;
        Y : Grid_Y)
        return SDL.Rectangle;

    function Center (
        X : Grid_X;
        Y : Grid_Y)
        return SDL.Coordinate;
end Grid;
