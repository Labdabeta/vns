with Games;

package Drawer is
    procedure Initialize (UTPS : in Natural);
    procedure Finalize;

    procedure Toggle_Team;
    procedure Set_Frame_Rate (UTPS : in Natural);

    procedure Draw_Game (Which : in Games.Game_Access);
end Drawer;
