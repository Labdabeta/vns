with SDL; use SDL;
with Grid; use Grid;
with Boards; use Boards;

package body Drawer is
    procedure Initialize;
    procedure Finalize;

    procedure Draw_Game (Which : in Games.Game_Access);
end Drawer;
