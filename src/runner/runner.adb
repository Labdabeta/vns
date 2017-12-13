with Traceback;
with Logger;

with SDL; use SDL;
with Viewer;

with Coordinates; use Coordinates;
with Games;
with Processors;
with Boards; use Boards;
with Loader;

with Ada.Command_Line;
with Ada.Text_IO;
with Ada.Real_Time; use Ada.Real_Time;

procedure Runner is
    Max_Frame_Rate : constant := 60;
    The_Game : Games.Game_Access := new Games.Game;
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

    if not SDL.Initialize ("Von Neumann Standing", 1024, 640) then
        Ada.Text_IO.Put_Line ("SDL ERROR!");
        return;
    end if;

    Games.Initialize (The_Game.all);

    Loader.Load_Game (
        The_Game.all,
        Ada.Command_Line.Argument (1),
        Ada.Command_Line.Argument (2));

    Viewer.Initialize;

    while Games.Winner (The_Game.all) = Boards.T_NONE loop
        declare
            E : Event := SDL.Step;
        begin
            if E.Kind = QUIT_EVENT then
                exit;
            end if;

            Viewer.Step_Game (The_Game, E);
            SDL.Begin_Draw ((255, 127, 0, 255));
            Viewer.Draw_Game (The_Game);
            SDL.End_Draw;

            delay To_Duration (Microseconds (1000 / Max_Frame_Rate));
        end;
    end loop;

    Ada.Text_IO.Put_Line (
        Team_ID'Image (Games.Winner (The_Game.all)) & " won!");

    SDL.Finalize;
    Games.Free_Game (The_Game);
end Runner;
