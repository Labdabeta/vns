with SDL; use SDL;

package body Grid is
    function Rect (
        X : Grid_X;
        Y : Grid_Y)
        return SDL.Rectangle is
        W32 : Integer := Integer (State.Window.Width) / 32;
        H28 : Integer := Integer (State.Window.Height) / 28;
    begin
        return (
            Left => (Integer (X) - 1) * W32,
            Top => (Integer (Y) - 1) * H28,
            Width => W32,
            Height => H28);
    end Rect;

    function Center (
        X : Grid_X;
        Y : Grid_Y)
        return SDL.Coordinate is
        R : Rectangle := Rect (X, Y);
    begin
        return (R.Left + R.Width / 2, R.Top + R.Height / 2);
    end Center;
end Grid;
