with SDL; use SDL;

with Sprite_Sheet;

with Ada.Command_Line;
with Ada.Text_IO;

procedure Runner is
    Sprite : SDL.Image;
begin
    if Ada.Command_Line.Argument_Count /= 2 then
        Ada.Text_IO.Put_Line (
            Ada.Command_Line.Command_Name & " [white] [black]");
        Ada.Text_IO.Put_Line (
            ASCII.HT & "[white] - The assembled team white binary.");
        Ada.Text_IO.Put_Line (
            ASCII.HT & "[black] - The assembled team black binary.");
        Ada.Text_IO.Put_Line ("");
        Ada.Text_IO.Put_Line ("Runs a game of Von Neumann Standing.");
        Ada.Text_IO.Put_Line ("");
        Ada.Text_IO.Put_Line ("An assembled binary consists of the 11 " &
        "executables for the team, each prefixed with their word length " &
        "across 4 bytes.");
        return;
    end if;

    if not SDL.Initialize ("TEST", 448, 448) then
        Ada.Text_IO.Put_Line ("SDL ERROR!");
        return;
    end if;

    Sprite := SDL.Create_Image (Sprite_Sheet.Raw_Data (1)'Access, 448, 384);
    SDL.Begin_Draw;
    Draw_Image (Sprite, (0, 0, 448, 448));
    SDL.End_Draw;

    while SDL.Step.Kind /= SDL.QUIT_EVENT loop
        null;
    end loop;

    SDL.Finalize;
end Runner;
