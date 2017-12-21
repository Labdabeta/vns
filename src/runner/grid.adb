with SDL; use SDL;

package body Grid is
    function Rect (
        X : Grid_X;
        Y : Grid_Y)
        return SDL.Rectangle is
        W32 : Integer := Integer (State.Window.Width) / 32;
        H29 : Integer := Integer (State.Window.Height) / 29;
    begin
        return (
            Left => (Integer (X) - 1) * W32,
            Top => (Integer (Y) - 1) * H29,
            Width => W32,
            Height => H29);
    end Rect;

    function Small_Rect (
        X : Grid_X;
        Y : Grid_Y)
        return SDL.Rectangle is
        R : Rectangle := Rect (X, Y);
        W32 : Integer := Integer (State.Window.Width) / 32;
        H29 : Integer := Integer (State.Window.Height) / 29;
    begin
        return (
            Left => R.Left + W32 / 8,
            Top => R.Top + H29 / 8,
            Width => 6 * W32 / 8,
            Height => 6 * H29 / 8);
    end Small_Rect;

    function Center (
        X : Grid_X;
        Y : Grid_Y)
        return SDL.Coordinate is
        R : Rectangle := Rect (X, Y);
    begin
        return (R.Left + R.Width / 2, R.Top + R.Height / 2);
    end Center;
end Grid;
