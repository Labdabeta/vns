with SDL; use SDL;
with Games;
with Logger;
with Coordinates; use Coordinates;
with Boards; use Boards;
with Sprite_Sheet;
with Font_Sheet;
with Ada.Real_Time; use Ada.Real_Time;
with Clips; use Clips;
with Drawer;

package body Viewer is
    -- 0 = paused, Natural'Last = as fast as possible
    Frame_Rate : Natural := 0;
    Old_Rate : Natural := 5;
    Next : Time;

    procedure Initialize is
    begin
        Drawer.Initialize (Frame_Rate);
    end Initialize;

    procedure Finalize is
    begin
        Drawer.Finalize;
    end Finalize;

    procedure Draw_Game (Which : in Games.Game_Access) is
    begin
        Drawer.Draw_Game (Which);
    end Draw_Game;

    procedure Step_Game (
        Which : in out Games.Game_Access;
        What : in SDL.Event) is
        procedure Set_Next is
        begin
            if Frame_Rate > 0 then
                Next := Clock + Milliseconds (1000 / Frame_Rate);
            else
                Next := Clock;
            end if;
            Drawer.Set_Frame_Rate (Frame_Rate);
        end Set_Next;
    begin
        if Frame_Rate = 0 then
            case What.Kind is
                when SDL.KEY_DOWN_EVENT =>
                    case What.Key is
                        when SDL.KEY_DOT =>
                            Games.Step_Game (Which.all);
                        when SDL.KEY_SPACE =>
                            Frame_Rate := Old_Rate;
                            Set_Next;
                        when SDL.KEY_UP =>
                            Frame_Rate := 1;
                            Set_Next;
                        when SDL.KEY_ENTER =>
                            Drawer.Toggle_Team;
                        when others => null;
                    end case;
                when others => null;
            end case;
        else
            case What.Kind is
                when SDL.NO_EVENT =>
                    -- Force the maxed frame rate
                    delay until Next;
                    Games.Step_Game (Which.all);
                    Set_Next;
                when SDL.KEY_DOWN_EVENT =>
                    case What.Key is
                        when SDL.KEY_DOT =>
                            if Frame_Rate /= Natural'Last then
                                Old_Rate := Frame_Rate;
                                Frame_Rate := Natural'Last;
                                Set_Next;
                            end if;
                        when SDL.KEY_UP =>
                            Frame_Rate := Frame_Rate + 1;
                            Set_Next;
                        when SDL.KEY_DOWN =>
                            Frame_Rate := Frame_Rate - 1;
                            Set_Next;
                        when SDL.KEY_SPACE =>
                            Old_Rate := Frame_Rate;
                            Frame_Rate := 0;
                            Set_Next;
                        when SDL.KEY_ENTER =>
                            Drawer.Toggle_Team;
                        when others => null;
                    end case;
                when SDL.KEY_UP_EVENT =>
                    if What.Key = SDL.KEY_DOT then
                        Frame_Rate := Old_Rate;
                        Set_Next;
                    end if;
                when others => null;
            end case;
        end if;
    end Step_Game;
end Viewer;
