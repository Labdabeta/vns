with Boards; use Boards;
with Processors; use Processors;

with Ada.Unchecked_Deallocation;
with Logger;

package body Games is
    procedure Free_Game (This : in out Game_Access) is
        procedure Free is new Ada.Unchecked_Deallocation (Game, Game_Access);
    begin
        Free (This);
    end Free_Game;

    procedure Initialize (This : out Game) is begin
        for P in This.Machines'Range (1) loop
            for U in This.Machines'Range (2) loop
                Initialize (This.Machines (P, U));
            end loop;
        end loop;

        This.Radios := (others => (others => (others => 0)));
        This.Shared := (others => (others => 0));
        Initialize (This.State);
        This.Clock := 0;
        This.Debug := False;

        Random_Units.Reset (Unit_Generator);
        Random_Players.Reset (Player_Generator);
    end Initialize;

    procedure Load_Code (
        This : in out Game;
        Unit : in Boards.Unit_Type;
        Team : in Boards.Player_ID;
        Code : in Processors.Memory_Array) is
    begin
        Load_Code (This.Machines (Team, Unit), Code);
    end Load_Code;

    procedure Set_Debug_Mode (Debug : in Boolean) is
    begin
        Processors.Set_Debug_Mode (Debug);
    end Set_Debug_Mode;

    procedure Step_Game (This : in out Game) is
        Unit_Order : array (Unit_Type) of Unit_Type;
        Temp_Unit, Current_Unit : Unit_Type;

        function Random_Unit (Max : Unit_Type) return Unit_Type is
            Sample : Unit_Type := Random_Units.Random (Unit_Generator);
        begin
            return Unit_Type'Val (
                Unit_Type'Pos (Sample) mod Unit_Type'Pos (Unit_Type'Last));
        end Random_Unit;
    begin
        -- Initialize Unit_Order
        for Index in Unit_Order'Range loop
            Unit_Order (Index) := Index;
        end loop;

        -- Shuffle Unit_Order
        for Index in reverse Unit_Order'Range loop
            Current_Unit := Random_Unit (Index);
            Temp_Unit := Unit_Order (Current_Unit);
            Unit_Order (Current_Unit) := Unit_Order (Index);
            Unit_Order (Index) := Temp_Unit;
        end loop;

        Increment_Points (This.State);
        Set_Registers (This.Machines, This.State);

        for Index in Unit_Order'Range loop
            if Random_Players.Random (Player_Generator) = T_BLACK then
                Step_Processor (
                    This, Unit_Order (Index), T_BLACK);
                Step_Processor (
                    This, Unit_Order (Index), T_WHITE);
            else
                Step_Processor (
                    This, Unit_Order (Index), T_WHITE);
                Step_Processor (
                    This, Unit_Order (Index), T_BLACK);
            end if;
        end loop;

        This.Clock := This.Clock + 1;
        Logger.Log_UT (This.Clock,
            Boards.Get_Points (This.State, T_WHITE),
            Boards.Get_Points (This.State, T_BLACK));
    end Step_Game;

    function Winner (This : in Game) return Boards.Team_ID is
    begin
        return Boards.Winner (This.State);
    end Winner;
end Games;
