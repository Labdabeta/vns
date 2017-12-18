with Traceback;
with Logger;
with Getopt; use Getopt;

with SDL; use SDL;
with Viewer;

with Coordinates; use Coordinates;
with Games;
with Processors;
with Boards; use Boards;
with Loader;

with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Real_Time; use Ada.Real_Time;

procedure Runner is
    Max_Frame_Rate : constant := 60;
    The_Game : Games.Game_Access := new Games.Game;
    Next_Option : Character;
    Option_String : constant String := "hdqvw:b:x:f";

    procedure Show_Help is
    begin
        Put_Line ("Usage: " & Command_Name & " [OPTION]... -w WHITE -b BLACK");
        Put_Line ("Run a game of Von Neumann Standing.");
        Put_Line (ASCII.HT & "-w FILE" & ASCII.HT &
            "load FILE as team white binary");
        Put_Line (ASCII.HT & "-b FILE" & ASCII.HT &
            "load FILE as team black binary");
        Put_Line (ASCII.HT & "-h, --help" & ASCII.HT &
            "show this help message");
        Put_Line (ASCII.HT & "-d" & ASCII.HT & "set debug mode (DIE->LOG)");
        Put_Line (ASCII.HT & "-q" & ASCII.HT & "set quiet mode");
        Put_Line (ASCII.HT & "-v" & ASCII.HT & "set verbose mode");
        Put_Line (ASCII.HT & "-x" & ASCII.HT & "set start frame");
        Put_Line (ASCII.HT & "--version" & ASCII.HT & "print version number");
    end Show_Help;

    procedure Show_Version is
    begin
        Put_Line ("v0.1 POC");
    end Show_Version;

    Break_Frame : Natural := 0;
    White_Loaded : Boolean := False;
    Black_Loaded : Boolean := False;
begin
    if Argument_Count = 0 then
        Show_Help;
        return;
    end if;

    Games.Initialize (The_Game.all);
    loop
        Next_Option := Get_Option (Option_String);

        exit when Next_Option = Option_End;

        case Next_Option is
            when 'h' => Show_Help;
            when 'd' => Games.Set_Debug_Mode (True);
            when 'q' => Logger.Set_Verbosity (Logger.LOG_QUIET);
            when 'v' => Logger.Set_Verbosity (Logger.LOG_VERBOSE);
            when 'w' =>
                Loader.Load_Team (The_Game.all, Option_Argument, T_WHITE);
                White_Loaded := True;
            when 'b' =>
                Loader.Load_Team (The_Game.all, Option_Argument, T_BLACK);
                Black_Loaded := True;
            when 'x' =>
                Break_Frame := Natural'Value (Option_Argument);
            when Option_Long =>
                if Option_Argument = "--version" then
                    Show_Version;
                elsif Option_Argument = "--help" then
                    Show_Help;
                else
                    Show_Help;
                end if;
            when others => Show_Help;
        end case;
    end loop;

    if not White_Loaded or not Black_Loaded then
        return;
    end if;

    if not SDL.Initialize ("Von Neumann Standing", 1024, 640) then
        Ada.Text_IO.Put_Line ("SDL ERROR!");
        return;
    end if;

    Viewer.Initialize;

    for I in Natural range 1 .. Break_Frame loop
        Games.Step_Game (The_Game.all);
    end loop;

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
        Team_ID'Image (Games.Winner (The_Game.all)) & " won after " &
        Natural'Image (The_Game.Clock) & "UT!");

    SDL.Finalize;
    Games.Free_Game (The_Game);
end Runner;
