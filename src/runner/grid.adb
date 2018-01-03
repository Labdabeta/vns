with SDL; use SDL;

package body Grid is
    Width_Division : Integer := 32;
    Height_Division : Integer := 29;

    function Rect (
        X : Grid_X;
        Y : Grid_Y)
        return SDL.Rectangle is
        W32 : Integer := Integer (State.Window.Width) / Width_Division;
        H29 : Integer := Integer (State.Window.Height) / Height_Division;
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
        W32 : Integer := Integer (State.Window.Width) / Width_Division;
        H29 : Integer := Integer (State.Window.Height) / Height_Division;
    begin
        return (
            Left => R.Left + W32 / 8,
            Top => R.Top + H29 / 8,
            Width => 6 * W32 / 8,
            Height => 6 * H29 / 8);
    end Small_Rect;

    function Left_Rect (
        X : Grid_X;
        Y : Grid_Y)
        return SDL.Rectangle is
        R : Rectangle := Rect (X, Y);
    begin
        return (
            Left => R.Left,
            Top => R.Top,
            Width => R.Width / 2,
            Height => R.Height);
    end Left_Rect;

    function Right_Rect (
        X : Grid_X;
        Y : Grid_Y)
        return SDL.Rectangle is
        R : Rectangle := Rect (X, Y);
    begin
        return (
            Left => R.Left + R.Width / 2,
            Top => R.Top,
            Width => R.Width / 2,
            Height => R.Height);
    end Right_Rect;

    function Center (
        X : Grid_X;
        Y : Grid_Y)
        return SDL.Coordinate is
        R : Rectangle := Rect (X, Y);
    begin
        return (R.Left + R.Width / 2, R.Top + R.Height / 2);
    end Center;

    function Grid_Lines (
        Min_X : Grid_X;
        Max_X : Grid_X;
        Min_Y : Grid_Y;
        Max_Y : Grid_Y)
        return Line_List is
        W32 : Integer := Integer (State.Window.Width) / Width_Division;
        H29 : Integer := Integer (State.Window.Height) / Height_Division;

        function Vertical_Lines return Line_List is
            Result : Line_List (1 .. Positive (Max_X - Min_X) + 2);
        begin
            for X in Grid_X range Min_X .. Max_X loop
                Result (Positive (X)) := (
                    Start => (
                        X => W32 * (Integer (X) - 1),
                        Y => H29 * (Integer (Min_Y) - 1)),
                    Finish => (
                        X => W32 * (Integer (X) - 1),
                        Y => H29 * Integer (Max_Y)));
            end loop;

            return Result;
        end Vertical_Lines;

        function Horizontal_Lines return Line_List is
            Result : Line_List (1 .. Positive (Max_Y - Min_Y) + 2);
        begin
            for Y in Grid_Y range Min_Y .. Max_Y loop
                Result (Positive (Y)) := (
                    Start => (
                        X => W32 * (Integer (Min_X) - 1),
                        Y => H29 * (Integer (Y) - 1)),
                    Finish => (
                        X => W32 * Integer (Max_X),
                        Y => H29 * (Integer (Y) - 1)));
            end loop;

            return Result;
        end Horizontal_Lines;
    begin
        return Vertical_Lines & Horizontal_Lines;
    end Grid_Lines;

    procedure Crop (
        Width : Grid_X;
        Height : Grid_Y) is
    begin
        Width_Division := Integer (Width);
        Height_Division := Integer (Height);
    end Crop;
end Grid;
