with Boards; use Boards;
with Coordinates; use Coordinates;

package body Processors.Engineers is
    procedure Set_Engineer_Registers (
        Machines : in out Processor_Array;
        State : in Board) is
        procedure Set_Team_Side_Engineer_Registers (
            Team : in Player_ID;
            Side : in Board_Side) is
            Pos : Coordinate :=
                Get_Unit (State, Engineer_IDs (Side), Team).Position (Team);
            Them : Player_ID := Enemy_Of (Team);
        begin
            Machines (Team, Engineer_IDs (Side)).Registers (20 .. 31) := (
                20 => Register_Type (Get_Nearest_Terrain (
                    State, To_Location (Pos, Team), Team, TT_SAND).X),
                21 => Register_Type (Get_Nearest_Terrain (
                    State, To_Location (Pos, Team), Team, TT_SAND).Y),
                22 => Register_Type (Get_Nearest_Terrain (
                    State, To_Location (Pos, Team), Team, TT_WIRE).X),
                23 => Register_Type (Get_Nearest_Terrain (
                    State, To_Location (Pos, Team), Team, TT_WIRE).Y),
                24 => Register_Type (Get_Nearest_Ally (
                    State, To_Location (Pos, Team), Team).Position (Team).X),
                25 => Register_Type (Get_Nearest_Ally (
                    State, To_Location (Pos, Team), Team).Position (Team).Y),
                26 => Register_Type (Get_Nearest_Ally (
                    State, To_Location (Pos, Team), Them).Position (Team).X),
                27 => Register_Type (Get_Nearest_Ally (
                    State, To_Location (Pos, Team), Them).Position (Team).Y),
                28 => Register_Type (Get_Nearest_Terrain (
                    State, To_Location (Pos, Team), Team, TT_WATER).X),
                29 => Register_Type (Get_Nearest_Terrain (
                    State, To_Location (Pos, Team), Team, TT_WATER).Y),
                30 => Register_Type (Distance_To_Base (
                    State, To_Location (Pos, Team), Team)),
                31 => Register_Type (Distance_To_Base (
                    State, To_Location (Pos, Team), Them)));
        end Set_Team_Side_Engineer_Registers;
    begin
        for T in Player_ID'Range loop
            for S in Board_Side'Range loop
                Set_Team_Side_Engineer_Registers (T, S);
            end loop;
        end loop;
    end Set_Engineer_Registers;
end Processors.Engineers;
