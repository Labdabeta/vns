with Games;
with Boards;

package Loader is
    procedure Load_Team (
        The_Game : in out Games.Game;
        File : in String;
        Team : in Boards.Player_ID);
end Loader;
