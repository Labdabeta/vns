with Games;
with Boards;

package Processors.Instructions is
    function Compute_Time (
        Which : in out Games.Game;
        Unit : in Boards.Unit_Type;
        Team : in Boards.Player_ID) return Natural;
    procedure Do_Instruction (
        Which : in out Games.Game;
        Unit : in Boards.Unit_Type;
        Team : in Boards.Player_ID);
end Processors.Instructions;
