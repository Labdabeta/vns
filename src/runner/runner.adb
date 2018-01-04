with Traceback;
with Logger;
with Getopt; use Getopt;

with Version;
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
    Option_String : constant String := "hdqvw:b:s:x:f";
    Help_Shown : Boolean := False;

    procedure Show_Help is
    begin
        if Help_Shown then
            return;
        else
            Help_Shown := True;
        end if;
        Put_Line ("Usage: " & Command_Name & " [OPTION]... WHITE BLACK");
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
        Put_Line (ASCII.HT & "-s SEED" & ASCII.HT & "set random seed (must be" &
            " non-zero");
        Put_Line (ASCII.HT & "-x FRAME" & ASCII.HT & "set start frame");
        Put_Line (ASCII.HT & "--version" & ASCII.HT & "print version number");
    end Show_Help;

    procedure Show_Version is
    begin
        Put_Line ("Von Neumann Standing Version " & Version.Tag);
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
    Processors.Initialize_Package;

    loop
        Next_Option := Get_Option (Option_String);

        exit when Option_Index > Argument_Count;

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
                if Break_Frame = 0 then
                    Put_Line ("Starting on frame 0. Probably misparsed -x" &
                    " argument.");
                end if;
            when 's' =>
                Processors.Set_Seed (Integer'Value (Option_Argument));
            when Option_Long =>
                if Argument (Option_Index) = "--version" then
                    Show_Version;
                elsif Argument (Option_Index) = "--help" then
                    Show_Help;
                else
                    Put_Line ("Unknown option: " & Argument (Option_Index));
                end if;
                Option_Index := Option_Index + 1;
            when Option_End =>
                if not White_Loaded then
                    -- Try to load white
                    if Option_Index > Argument_Count then
                        Put_Line ("Team white not specified.");
                    end if;
                    Loader.Load_Team (
                        The_Game.all, Argument (Option_Index), T_WHITE);
                    White_Loaded := True;
                    Option_Index := Option_Index + 1;
                end if;

                if not Black_Loaded then
                    -- Try to load black
                    if Option_Index > Argument_Count then
                        Put_Line ("Team black not specified.");
                    end if;
                    Loader.Load_Team (
                        The_Game.all, Argument (Option_Index), T_BLACK);
                    Black_Loaded := True;
                    Option_Index := Option_Index + 1;
                end if;
            when others =>
                Put_Line ("Unknown option: -" & Option_Option);
        end case;
    end loop;

    if not White_Loaded or not Black_Loaded then
        Put_Line ("Not all teams specified!");
        return;
    end if;

    if not SDL.Initialize ("Von Neumann Standing", 1024, 928) then
        Ada.Text_IO.Put_Line ("SDL ERROR!");
        return;
    end if;

    Viewer.Initialize;

    for I in Natural range 1 .. Break_Frame loop
        Games.Step_Game (The_Game.all);
    end loop;
    Put_Line ("Ready");

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
