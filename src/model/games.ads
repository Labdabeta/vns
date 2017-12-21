with Boards;
with Processors;

with Ada.Numerics.Discrete_Random;

package Games is
    type Game is record
        Machines : Processors.Processor_Array;
        Radios : Processors.Communications;
        Shared : Processors.Shared_Memory;
        State : Boards.Board;
        Clock : Natural;
        Debug : Boolean;
    end record;
    type Game_Access is access Game;
    subtype Not_Null_Game_Access is not null Game_Access;
    procedure Free_Game (This : in out Game_Access);

    procedure Initialize (This : out Game);
    procedure Load_Code (
        This : in out Game;
        Unit : in Boards.Unit_Type;
        Team : in Boards.Player_ID;
        Code : in Processors.Memory_Array);
    procedure Set_Debug_Mode (Debug : in Boolean);
    procedure Step_Game (This : in out Game);
    function Winner (This : in Game) return Boards.Team_ID;
private
    package Random_Units is new Ada.Numerics.Discrete_Random (Boards.Unit_Type);
    Unit_Generator : Random_Units.Generator;

    package Random_Players is new Ada.Numerics.Discrete_Random (
        Boards.Player_ID);
    Player_Generator : Random_Players.Generator;
end Games;
