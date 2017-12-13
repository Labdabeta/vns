with Games;
with SDL;

package Viewer is
    procedure Initialize;
    procedure Finalize;
    procedure Draw_Game (Which : in Games.Game_Access);

    -- Still wants NO_EVENT events so it can force framerate
    procedure Step_Game (
        Which : in out Games.Game_Access;
        What : in SDL.Event);
end Viewer;
