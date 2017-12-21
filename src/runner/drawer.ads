with Games;

package Drawer is
    procedure Initialize;
    procedure Finalize;

    procedure Draw_Game (Which : in Games.Game_Access);
end Drawer;
