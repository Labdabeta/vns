with Games; use Games;
with Boards; use Boards;
with Coordinates; use Coordinates;
with Interfaces;

with Ada.Numerics.Generic_Elementary_Functions;
with Ada.Unchecked_Conversion;
with Ada.Exceptions;

with Logger;

package body Processors is
    function BTR (Val : in Boolean) return Register_Type is
    begin
        if Val then
            return 1;
        else
            return 0;
        end if;
    end BTR;

    function To_U32 is new Ada.Unchecked_Conversion (
        Register_Type, Interfaces.Unsigned_32);
    function From_U32 is new Ada.Unchecked_Conversion (
        Interfaces.Unsigned_32, Register_Type);

    function Is_Taken (
        Which : in Instruction_ID;
        Value : in Register_Type) return Boolean is
    begin
        case Which is
            when 15 | 21 => return Value = 0;
            when 16 | 22 => return Value /= 0;
            when 17 | 23 => return Value > 0;
            when 18 | 24 => return Value < 0;
            when 19 | 25 => return Value >= 0;
            when 20 | 26 => return Value <= 0;
            when others => return False;
        end case;
    end Is_Taken;

    -- Returns true if the address is in the cache.
    function Check_Cache (
        Which : in Cache_Contents;
        Address : in Address_Type) return Boolean is
    begin
        if Which.Size = CS_NONE then
            return False;
        end if;

        for Index in Which.Data'Range loop
            if Which.Data (Index).Address = Address and
                Which.Data (Index).Is_Loaded
            then
                return True;
            end if;
        end loop;
        return False;
    end Check_Cache;

    -- Adds the Address:Value pair to the cache, possibly flushing it
    procedure Add_To_Cache (
        Kind : in Cache_Type;
        Which : in out Cache_Contents;
        Address : in Address_Type;
        Memory : in Memory_Array) is
        New_Entry : Cache_Entry := (
            Address => Address,
            Value => Memory (Address),
            Is_Loaded => True,
            Age => 0);

        type Address_Options_Array is array (Address_Type range <>) of
            Address_Type;
        procedure Replace_Oldest_Entry (Spots : in Address_Options_Array) is
            Oldest_Entry : Address_Type;
            Age : Natural := Natural'Last; -- -1 is an empty slot
        begin
            for Index in Spots'Range loop
                if not Which.Data (Spots (Index)).Is_Loaded then
                    Which.Data (Spots (Index)) := New_Entry;
                    return;
                end if;

                if Which.Data (Spots (Index)).Age < Age then
                    Oldest_Entry := Spots (Index);
                    Age := Which.Data (Spots (Index)).Age;
                end if;
            end loop;

            Which.Data (Oldest_Entry) := New_Entry;
        end Replace_Oldest_Entry;

        -- Number of spots a given address could occupy
        function Cache_Size_Index return Address_Type is
        begin
            case Kind is
                when CT_NONE => return 1;
                when CT_TWO_WAY => return 2;
                when CT_FOUR_WAY => return 4;
                when CT_EIGHT_WAY => return 8;
                when CT_FULLY => return Which.Length;
            end case;
        end Cache_Size_Index;
    begin
        if Which.Size = CS_NONE then
            return;
        end if;

        for Index in Address_Type range 0 .. Which.Length loop
            if Which.Data (Index).Address = Address then
                Which.Data (Index) := New_Entry;
                return;
            end if;
        end loop;

        for Index in Address_Type range 0 .. Which.Length loop
            if Which.Data (Index).Is_Loaded then
                Which.Data (Index).Age := Which.Data (Index).Age + 1;
            end if;
        end loop;

        declare
            Spots : Address_Options_Array (1 .. Cache_Size_Index);
        begin
            for Index in Spots'Range loop
                Spots (Index) := (Address + Index) mod Which.Length;
            end loop;

            Replace_Oldest_Entry (Spots);
        end;
    end Add_To_Cache;

    -- Returns true if the branch should probably be taken, false otherwise
    function Predict_Branch (
        Kind : in Branch_Type;
        Which : in Branch_State) return Boolean is
    begin
        case Kind is
            when BT_NONE | BT_PERFECT => return False;
            when BT_ONE_BIT => return Which.Last_Direction = DIR_TAKEN;
            when BT_TWO_BIT => return Which.Prediction.Direction = DIR_TAKEN;
            when BT_TWO_LEVEL_TWO_BIT =>
                return Which.Prediction_History (
                    Which.Even_Earlier_Direction,
                    Which.Last_Direction).Direction = DIR_TAKEN;
        end case;
    end Predict_Branch;

    -- Register a 'TAKEN' with a 2-state predictor
    procedure Advance_Prediction (Which : in out Two_State_Branch) is begin
        if Which.Strength = WEAKLY then
            if Which.Direction = DIR_TAKEN then
                Which.Strength := STRONGLY;
            else
                Which.Direction := DIR_TAKEN;
            end if;
        else
            if Which.Direction /= DIR_TAKEN then
                Which.Strength := WEAKLY;
            end if;
        end if;
    end Advance_Prediction;

    -- Register a 'NOT TAKEN' with a 2-state predictor
    procedure Retreat_Prediction (Which : in out Two_State_Branch) is begin
        if Which.Strength = WEAKLY then
            if Which.Direction = DIR_TAKEN then
                Which.Direction := DIR_NOT_TAKEN;
            else
                Which.Strength := STRONGLY;
            end if;
        else
            if Which.Direction = DIR_TAKEN then
                Which.Strength := WEAKLY;
            end if;
        end if;
    end Retreat_Prediction;

    -- Updates all predictors, even though at any point only one updates
    procedure Update_Prediction (
        Which : in out Branch_State;
        Taken : in Boolean) is
    begin
        if Taken then
            Advance_Prediction (Which.Prediction);
            Advance_Prediction (Which.Prediction_History (
                Which.Even_Earlier_Direction, Which.Last_Direction));
        else
            Retreat_Prediction (Which.Prediction);
            Retreat_Prediction (Which.Prediction_History (
                Which.Even_Earlier_Direction, Which.Last_Direction));
        end if;
        Which.Even_Earlier_Direction := Which.Last_Direction;
        if Taken then
            Which.Last_Direction := DIR_TAKEN;
        else
            Which.Last_Direction := DIR_NOT_TAKEN;
        end if;
    end Update_Prediction;

    function To_Direction (X : in Register_Type) return Direction is begin
        return Direction'Val (X - 1);
    end To_Direction;

    procedure Initialize (This : out Unit_Processor) is begin
        This.Memory := (others => 0);
        This.Registers := (others => 0);
        This.Registers (13) := Register_Type (Address_Type'Last);
        This.ICounter := 0; -- Set to first instruction runtime if clock is 0
        This.CCounter := 0;
        This.Clock := 0;
        This.Advanced := False;
    end Initialize;

    procedure Load_Code (This : in out Unit_Processor; Code : in Memory_Array)
    is begin
        This.Memory (Code'Range) := Code;
    end Load_Code;

    procedure Set_Registers (
        Machines : in out Processor_Array;
        State : in Board) is

        procedure Set_Captain_Registers (Team : in Player_ID) is
            Nearest_Ally : Unit_State := Get_Nearest_Ally (
                State, Get_Unit (State, UT_CAPTAIN, Team).Position, Team);
            function Compute_Life_Value (Unit : in Unit_Type)
                return Register_Type is
                Result : Register_Type := 0;
            begin
                if Get_Unit (State, Unit, Team).Alive then
                    Result := Result + 1;
                end if;
                if Get_Unit (State, Unit, Enemy_Of (Team)).Alive then
                    Result := Result + 2;
                end if;
                return Result;
            end Compute_Life_Value;
        begin
            Machines (Team, UT_CAPTAIN).Registers (16 .. 29) := (
                16 => Register_Type (Get_Points (State, Team)),
                17 => Register_Type (Get_Points (State, Enemy_Of (Team))),
                18 => Register_Type (Nearest_Ally.Position (Team).X),
                19 => Register_Type (Nearest_Ally.Position (Team).Y),
                20 => Compute_Life_Value (UT_MORTAR),
                21 => Compute_Life_Value (UT_SNIPER),
                22 => Compute_Life_Value (UT_ENGINEER_SS),
                23 => Compute_Life_Value (UT_ENGINEER_FS),
                24 => Compute_Life_Value (UT_MACHINEGUNNER_SS),
                25 => Compute_Life_Value (UT_MACHINEGUNNER_FS),
                26 => Compute_Life_Value (UT_SCOUT_SS),
                27 => Compute_Life_Value (UT_SCOUT_FS),
                28 => Compute_Life_Value (UT_RIFLEMAN_SS),
                29 => Compute_Life_Value (UT_RIFLEMAN_FS));
        end Set_Captain_Registers;

        procedure Set_Mortar_Registers (Team : in Player_ID) is begin
            Machines (Team, UT_MORTAR).Registers (18 .. 31) := (
                18 => Register_Type (Get_Unit (
                    State, UT_SCOUT_SS, Enemy_Of (Team)).Position (Team).X),
                19 => Register_Type (Get_Unit (
                    State, UT_SCOUT_SS, Enemy_Of (Team)).Position (Team).Y),
                20 => Register_Type (Get_Unit (
                    State, UT_SCOUT_FS, Enemy_Of (Team)).Position (Team).X),
                21 => Register_Type (Get_Unit (
                    State, UT_SCOUT_FS, Enemy_Of (Team)).Position (Team).Y),
                22 => Register_Type (Get_Unit (
                    State, UT_CAPTAIN, Team).Position (Team).X),
                23 => Register_Type (Get_Unit (
                    State, UT_SNIPER, Team).Position (Team).X),
                24 => Register_Type (Get_Unit (
                    State, UT_ENGINEER_SS, Team).Position (Team).X),
                25 => Register_Type (Get_Unit (
                    State, UT_ENGINEER_FS, Team).Position (Team).X),
                26 => Register_Type (Get_Unit (
                    State, UT_MACHINEGUNNER_SS, Team).Position (Team).X),
                27 => Register_Type (Get_Unit (
                    State, UT_MACHINEGUNNER_FS, Team).Position (Team).X),
                28 => Register_Type (Get_Unit (
                    State, UT_SCOUT_SS, Team).Position (Team).X),
                29 => Register_Type (Get_Unit (
                    State, UT_SCOUT_FS, Team).Position (Team).X),
                30 => Register_Type (Get_Unit (
                    State, UT_RIFLEMAN_SS, Team).Position (Team).X),
                31 => Register_Type (Get_Unit (
                    State, UT_RIFLEMAN_FS, Team).Position (Team).X));
        end Set_Mortar_Registers;

        procedure Set_Sniper_Registers (Team : in Player_ID) is
            My_Pos : Location := Get_Unit (State, UT_SNIPER, Team).Position;
            Nearest_Enemy : Unit_State := Get_Nearest_Ally (
                State, My_Pos, Enemy_Of (Team));
            Enemy_Captain : Unit_State := Get_Unit (
                State, UT_CAPTAIN, Enemy_Of (Team));
        begin
            Machines (Team, UT_SNIPER).Registers (17 .. 23) := (
                17 => Register_Type (
                    Count_Nearby_Enemies (State, My_Pos, Team, 1)),
                18 => Register_Type (
                    Count_Nearby_Enemies (State, My_Pos, Team, 2)),
                19 => Register_Type (
                    Count_Nearby_Enemies (State, My_Pos, Team, 3)),
                20 => Machines (Team, UT_SCOUT_SS).Registers (18),
                21 => Machines (Team, UT_SCOUT_SS).Registers (19),
                22 => Machines (Team, UT_SCOUT_FS).Registers (18),
                23 => Machines (Team, UT_SCOUT_FS).Registers (19));
            Machines (Team, UT_SNIPER).Registers (28 .. 31) := (
                28 => Register_Type (Nearest_Enemy.Position (Team).X),
                29 => Register_Type (Nearest_Enemy.Position (Team).Y),
                30 => Register_Type (Enemy_Captain.Position (Team).X),
                31 => Register_Type (Enemy_Captain.Position (Team).Y));
        end Set_Sniper_Registers;

        procedure Set_Engineer_Registers (
            Team : in Player_ID;
            Side : in Board_Side) is
            Pos : Coordinate :=
                Get_Unit (State, Engineers (Side), Team).Position (Team);
            Them : Player_ID := Enemy_Of (Team);
        begin
            Machines (Team, Engineers (Side)).Registers (20 .. 31) := (
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
        end Set_Engineer_Registers;

        procedure Set_Machinegunner_Registers (
            Team : in Player_ID;
            Side : in Board_Side) is
            Pos : Location :=
                Get_Unit (State, Machinegunners (Side), Team).Position;
            Them : Player_ID := Enemy_Of (Team);
            Nearest_Ally_Pos : Location :=
                Get_Nearest_Ally (State, Pos, Them).Position;
        begin
            Machines (Team, Machinegunners (Side)).Registers (16 .. 31) := (
                16 => BTR (Get_Unit (State, Machinegunners (Side), Team).Setup),
                17 => Register_Type (Nearest_Ally_Pos (Team).X),
                18 => Register_Type (Nearest_Ally_Pos (Team).Y),
                19 => Register_Type (Compute_Fire_Time (
                    State, Machinegunners (Side), Pos, Nearest_Ally_Pos)),
                20 => Register_Type (
                    Count_Nearby_Enemies (State, Pos, Team, 1)),
                21 => Register_Type (
                    Count_Nearby_Enemies (State, Pos, Team, 2)),
                22 => Register_Type (
                    Count_Nearby_Enemies (State, Pos, Team, 3)),
                23 => Register_Type (
                    Count_Nearby_Enemies (State, Pos, Team, 4)),
                24 => Register_Type (
                    Count_Nearby_Enemies (State, Pos, Team, 5)),
                25 => Register_Type (
                    Count_Nearby_Enemies (State, Pos, Team, 6)),
                26 => Register_Type (
                    Count_Nearby_Enemies (State, Pos, Them, 1)),
                27 => Register_Type (
                    Count_Nearby_Enemies (State, Pos, Them, 2)),
                28 => Register_Type (
                    Count_Nearby_Enemies (State, Pos, Them, 3)),
                29 => Register_Type (
                    Count_Nearby_Enemies (State, Pos, Them, 4)),
                30 => Register_Type (
                    Count_Nearby_Enemies (State, Pos, Them, 5)),
                31 => Register_Type (
                    Count_Nearby_Enemies (State, Pos, Them, 6)));
        end Set_Machinegunner_Registers;

        procedure Set_Scout_Registers (
            Team : in Player_ID;
            Side : in Board_Side) is
            Pos : Location := Get_Unit (State, Scouts (Side), Team).Position;
            Them : Player_ID := Enemy_Of (Team);
            Nearest_Enemies : array (1 .. 5) of Coordinate := (
                1 => Get_Nearest_Ally (State, Pos, Them, 0).Position (Team),
                2 => Get_Nearest_Ally (State, Pos, Them, 1).Position (Team),
                3 => Get_Nearest_Ally (State, Pos, Them, 2).Position (Team),
                4 => Get_Nearest_Ally (State, Pos, Them, 3).Position (Team),
                5 => Get_Nearest_Ally (State, Pos, Them, 4).Position (Team));
            Sniper_Idx : Register_Index;
        begin
            if Side = SNIPER_SIDE then
                Sniper_Idx := 24;
            else
                Sniper_Idx := 26;
            end if;
            Machines (Team, Scouts (Side)).Registers (16 .. 17) := (
                16 => BTR (Get_Unit (State, UT_SNIPER, Team).Setup),
                17 => BTR (Get_Unit (State, UT_SNIPER, Enemy_Of (Team)).Setup));
            Machines (Team, Scouts (Side)).Registers (20 .. 31) := (
                20 => Machines (Team, UT_SNIPER).Registers (Sniper_Idx),
                21 => Machines (Team, UT_SNIPER).Registers (Sniper_Idx + 1),
                22 => Register_Type (Nearest_Enemies (1).X),
                23 => Register_Type (Nearest_Enemies (1).Y),
                24 => Register_Type (Nearest_Enemies (2).X),
                25 => Register_Type (Nearest_Enemies (2).Y),
                26 => Register_Type (Nearest_Enemies (3).X),
                27 => Register_Type (Nearest_Enemies (3).Y),
                28 => Register_Type (Nearest_Enemies (4).X),
                29 => Register_Type (Nearest_Enemies (4).Y),
                30 => Register_Type (Nearest_Enemies (5).X),
                31 => Register_Type (Nearest_Enemies (5).Y));
        end Set_Scout_Registers;

        procedure Set_Rifleman_Registers (
            Team : in Player_ID;
            Side : in Board_Side) is
            Pos : Location :=
                Get_Unit (State, Riflemen (Side), Team).Position;
        begin
            Machines (Team, Riflemen (Side)).Registers (20 .. 31) := (
                20 => Register_Type (
                    Count_Nearby_Enemies (State, Pos, Team, 1)),
                21 => Register_Type (
                    Count_Nearby_Enemies (State, Pos, Team, 2)),
                22 => Register_Type (
                    Count_Nearby_Enemies (State, Pos, Team, 3)),
                23 => Register_Type (
                    Count_Nearby_Enemies (State, Pos, Team, 4)),
                24 => Register_Type (
                    Count_Nearby_Enemies (State, Pos, Team, 5)),
                25 => Register_Type (
                    Count_Nearby_Enemies (State, Pos, Team, 6)),
                26 => Register_Type (
                    Count_Nearby_Enemies (State, Pos, Enemy_Of (Team), 1)),
                27 => Register_Type (
                    Count_Nearby_Enemies (State, Pos, Enemy_Of (Team), 2)),
                28 => Register_Type (
                    Count_Nearby_Enemies (State, Pos, Enemy_Of (Team), 3)),
                29 => Register_Type (
                    Count_Nearby_Enemies (State, Pos, Enemy_Of (Team), 4)),
                30 => Register_Type (
                    Count_Nearby_Enemies (State, Pos, Enemy_Of (Team), 5)),
                31 => Register_Type (
                    Count_Nearby_Enemies (State, Pos, Enemy_Of (Team), 6)));
        end Set_Rifleman_Registers;
    begin
        for T in Player_ID'Range loop
            Set_Captain_Registers (T);
            Machines (T, UT_CAPTAIN).Registers (0) := 0;
            Set_Mortar_Registers (T);
            Machines (T, UT_MORTAR).Registers (0) := 0;
            Set_Sniper_Registers (T);
            Machines (T, UT_SNIPER).Registers (0) := 0;
            for S in Board_Side'Range loop
                Set_Engineer_Registers (T, S);
                Machines (T, Engineers (S)).Registers (0) := 0;
                Set_Machinegunner_Registers (T, S);
                Machines (T, Machinegunners (S)).Registers (0) := 0;
                Set_Scout_Registers (T, S);
                Machines (T, Scouts (S)).Registers (0) := 0;
                Set_Rifleman_Registers (T, S);
                Machines (T, Riflemen (S)).Registers (0) := 0;
            end loop;
        end loop;
    end Set_Registers;

    procedure Step_Processor (
        Which : in out Game;
        Unit : in Unit_Type;
        Team : in Player_ID) is
        use Interfaces;
        State : Board renames Which.State;
        Machines : Processor_Array renames Which.Machines;
        Radios : Communications renames Which.Radios;
        Shared : Shared_Memory renames Which.Shared;
        Clock : Natural renames Which.Clock;
        Tactical : Shared_Grid renames Which.Tactical;
        Support : Shared_Grid renames Which.Support;
        Flag : Shared_Grid renames Which.Flag;
        Reset_Counter : array (CPU_Speed) of Natural := (
            CPUS_EIGHT_FRAMES => 7,
            CPUS_SIX_FRAMES => 5,
            CPUS_FOUR_FRAMES => 3,
            CPUS_TWO_FRAMES => 1,
            CPUS_EVERY_FRAME => 0);
        Us : Unit_State := Get_Unit (State, Unit, Team);
        Me : Unit_Processor renames Machines (Team, Unit);
        PCVal : Interfaces.Unsigned_32 :=
            To_U32 (Me.Memory (Address_Type (Me.Registers (15))));
        Instruction : Instruction_ID := Instruction_ID (
            Interfaces.Shift_Right (PCVal, 25));
        A : Register_Index := Register_Index (
            Interfaces.Shift_Right (PCVal, 20) and 2#11111#);
        B : Register_Index := Register_Index (
            Interfaces.Shift_Right (PCVal, 15) and 2#11111#);
        C : Register_Index := Register_Index (
            Interfaces.Shift_Right (PCVal, 10) and 2#11111#);
        Small : Small_Immediate_Type := Small_Immediate_Type (
            PCVal and 2#1111111111#);
        Immediate : Address_Type := Address_Type (
            PCVal and 2#11111111111111111111#);

        -- This function also alerts the board of upcoming updates
        function Compute_Time return Natural is

            function Jump_Time return Natural is begin
                if Us.Branch_Predictor = BT_PERFECT then
                    return 1;
                end if;
                if Is_Taken (Instruction, Me.Registers (A)) then
                    return 1;
                else
                    return 8;
                end if;
            end Jump_Time;

            function Branch_Time return Natural is begin
                if Us.Branch_Predictor = BT_PERFECT then
                    return 1;
                end if;

                if Us.Branch_Predictor = BT_NONE then
                    return 8;
                end if;

                if Is_Taken (Instruction, Me.Registers (A)) =
                    Predict_Branch (Us.Branch_Predictor,
                        Me.Predictor (Address_Type (Me.Registers (15))))
                then
                    return 1;
                else
                    return 8;
                end if;
            end Branch_Time;

            function Cache_Time (Address : in Address_Type) return Natural is
            begin
                if Check_Cache (Me.Cache, Address) then
                    return 1;
                else
                    return 8;
                end if;
            end Cache_Time;

            function Captain_Time return Natural is begin
                case Instruction is
                    when 100 | 101 | 107 .. 127 => return 1;
                    when 96 => return 4;
                    when 97 => return 8;
                    when 98 | 105 => return 16;
                    when 99 => return 32;
                    when 102 | 106 => return 256;
                    when 103 => return 128;
                    -- Unblocked by arrival or death of summoned unit
                    when 104 =>
                        Set_Unit_Summon (State, Team, Unit_Type'Val (
                            Me.Registers (A) - 1), True);
                        return Natural'Last;
                    when others => return 0; -- Not a special operation
                end case;
            end Captain_Time;

            function Mortar_Time return Natural is begin
                case Instruction is
                    when 96 .. 101 => return 4;
                    when 102 .. 104 | 107 .. 108 | 117 | 120 .. 125 | 127 =>
                        return 8;
                    when 118 => return 16;
                    when 105 => return 32;
                    when 106 | 109 .. 116 => return 64;
                    when 119 | 126 => return 128;
                    when others => return 0;
                end case;
            end Mortar_Time;

            function Sniper_Time return Natural is begin
                case Instruction is
                    when 97 | 99 => return 1;
                    when 100 | 102 .. 104 | 107 .. 108 | 117 | 120 .. 127 =>
                        return 8;
                    when 101 => return 16;
                    when 105 => return 32;
                    when 96 | 106 | 109 .. 116 | 118 .. 119 => return 64;
                    when 98 =>
                        Prepare_Shoot (State, Team, Unit,
                            (X_Coordinate (Me.Registers (B)),
                             Y_Coordinate (Me.Registers (C))));
                        return Compute_Fire_Time (State, UT_RIFLEMAN_SS,
                            Us.Position, To_Location (
                                (X_Coordinate (Me.Registers (B)),
                                 Y_Coordinate (Me.Registers (C))), Team));
                    when others => return 0;
                end case;
            end Sniper_Time;

            function Engineer_Time return Natural is begin
                case Instruction is
                    when 100 | 101 => return 4;
                    when 96 .. 99 | 102 .. 104 | 107 | 108 | 117 | 120 .. 127 =>
                        return 8;
                    when 105 => return 32;
                    when 106 | 109 .. 116 | 118 .. 119 => return 64;
                    when others => return 0;
                end case;
            end Engineer_Time;

            function Machinegunner_Time return Natural is begin
                case Instruction is
                    when 98 .. 101 | 119 => return 4;
                    when 102 .. 104 | 107 | 108 | 117 | 120 .. 127 => return 8;
                    when 118 => return 16;
                    when 105 => return 32;
                    when 106 | 109 .. 116 => return 64;
                    when 96 | 97 => return 128;
                    when others => return 0;
                end case;
            end Machinegunner_Time;

            function Scout_Time return Natural is begin
                case Instruction is
                    when 96 => return 1;
                    when 98 .. 101 => return 4;
                    when 102 .. 104 | 107 | 108 | 117 | 120 .. 127 => return 8;
                    when 97 => return 16;
                    when 105 => return 32;
                    when 106 | 109 .. 116 | 118 | 119 => return 64;
                    when others => return 0;
                end case;
            end Scout_Time;

            function Rifleman_Time return Natural is begin
                case Instruction is
                    when 96 .. 101 => return 4;
                    when 102 .. 104 | 107 | 108 | 117 | 120 .. 127 => return 8;
                    when 105 => return 32;
                    when 106 | 109 .. 116 | 118 | 119 => return 64;
                    when others => return 0;
                end case;
            end Rifleman_Time;
        begin
            if Us.Summoned then
                Prepare_Move (State, Team, Unit, Get_Direction_Towards (
                    Us.Position (Team),
                    Get_Unit (State, UT_CAPTAIN, Team).Position (Team)));
                return 1;
            end if;

            if Us.Retreating then
                Prepare_Move (State, Team, Unit, Get_Direction_Towards (
                    Us.Position (Team), Home_Base));
                return 1;
            end if;

            Me.Registers (12) := Register_Type (Small);

            Logger.Log_Prep (Unit, Team, Instruction);

            case Instruction is
                when 49 | 55 => return 0;
                when 0 | 1 | 4 .. 14 | 27 | 32 | 33 | 38 | 39 | 42 | 44 |
                    64 .. 94 =>
                    return 1;
                when 2 | 40 | 56 .. 63 | 95 => return 4;
                when 3 | 34 .. 37 | 41 | 51 | 52 => return 8;
                when 53 | 54 => return 16;
                when 15 .. 20 => return Jump_Time;
                when 21 .. 26 => return Branch_Time;
                when 28 | 29 => return Cache_Time (Immediate);
                when 30 | 31 => return Cache_Time (Address_Type (
                    Me.Registers (13)));
                when 43 =>
                    Prepare_Shoot (State, Team, Unit,
                        (X_Coordinate (Me.Registers (B)),
                         Y_Coordinate (Me.Registers (C))));
                    return Compute_Fire_Time (State, Unit, Us.Position,
                        To_Location (
                            (X_Coordinate (Me.Registers (B)),
                             Y_Coordinate (Me.Registers (C))), Team));
                when 45 =>
                    if Us.Setup and Unit /= UT_SNIPER then
                        return 0;
                    end if;
                    Prepare_Move (State, Team, Unit,
                        To_Direction (Me.Registers (A)));
                    if Us.Setup then
                        if Us.Prone then
                            return 32;
                        end if;
                        return 16;
                    end if;
                    if Us.Prone then
                        return 16;
                    end if;
                    return 8;
                when 46 =>
                    Prepare_Move (State, Team, Unit,
                        To_Direction (Me.Registers (A)));
                    return 64;
                when 47 =>
                    Prepare_Move (State, Team, Unit,
                        To_Direction (Me.Registers (A)));
                    return 64;
                when 48 =>
                    if Is_On_Target (State, Team, Unit) then
                        return 256;
                    else
                        return 0;
                    end if;
                when 50 =>
                    Set_Unit_Visibility (State, Team, Unit, False);
                    return Natural (
                        Me.Registers (A) + Register_Type (Immediate));
                when others =>
                    case Unit is
                        when UT_CAPTAIN => return Captain_Time;
                        when UT_MORTAR => return Mortar_Time;
                        when UT_SNIPER => return Sniper_Time;
                        when UT_ENGINEER_SS | UT_ENGINEER_FS =>
                            return Engineer_Time;
                        when UT_MACHINEGUNNER_SS | UT_MACHINEGUNNER_FS =>
                            return Machinegunner_Time;
                        when UT_SCOUT_SS | UT_SCOUT_FS => return Scout_Time;
                        when UT_RIFLEMAN_SS | UT_RIFLEMAN_FS =>
                            return Rifleman_Time;
                    end case;
            end case;
        exception
            --  Any invalid configuration freezes a unit!
            when others =>
                return Natural'Last;
        end Compute_Time;

        procedure Do_Instruction is
            function Count_Leading_Zeroes (Val : in Register_Type)
            return Register_Type is
                use Interfaces;
                X : Unsigned_32 := To_U32 (Val);
                T : Unsigned_32;
                R : Register_Type;
            begin
                if X = 0 then
                    return 32;
                end if;

                T := 1;
                R := 31;
                while (X and T) = 0 loop
                    T := Shift_Left (T, 1);
                    R := R - 1;
                end loop;

                return R;
            end Count_Leading_Zeroes;

            function Count_Leading_Ones (Val : in Register_Type)
            return Register_Type is
            begin
                return Count_Leading_Zeroes (From_U32 (not To_U32 (Val)));
            end Count_Leading_Ones;

            function Count_Ones (Val : in Register_Type)
            return Register_Type is
                use Interfaces;
                K5555 : constant Unsigned_32 := 16#55555555#;
                K3333 : constant Unsigned_32 := 16#33333333#;
                K0f0f : constant Unsigned_32 := 16#0f0f0f0f#;
                K0101 : constant Unsigned_32 := 16#01010101#;
                X : Unsigned_32 := To_U32 (Val);
            begin
                X := X - (Shift_Right (X, 1) and K5555);
                X := (X and K3333) + (Shift_Right (X, 2) and K3333);
                X := (X + Shift_Right (X, 4) and K0f0f);
                X := Shift_Right (X * K0101, 56);
                return Register_Type (X);
            end Count_Ones;

            function Count_Zeroes (Val : in Register_Type)
            return Register_Type is begin
                return 32 - Count_Ones (Val);
            end Count_Zeroes;

            function Shift_Right (Val, Amount : in Register_Type)
            return Register_Type is
            begin
                return From_U32 (Interfaces.Shift_Right (
                    To_U32 (Val), Natural (Amount)));
            end Shift_Right;

            function Shift_Left (Val, Amount : in Register_Type)
            return Register_Type is
            begin
                return From_U32 (Interfaces.Shift_Left (
                    To_U32 (Val), Natural (Amount)));
            end Shift_Left;

            function BC return Location is begin
                return To_Location ((
                    X_Coordinate (Me.Registers (B)),
                    Y_Coordinate (Me.Registers (C))), Team);
            end BC;

            procedure Do_Float_Op is
                package Value_Functions is new
                    Ada.Numerics.Generic_Elementary_Functions (Float);
                use Value_Functions;
                function To_R is new Ada.Unchecked_Conversion (
                    Float, Register_Type);
                function To_F is new Ada.Unchecked_Conversion (
                    Register_Type, Float);
            begin
                case Instruction is
                    when 102 =>
                        Me.Registers (A) := To_R (Float (Immediate));
                    when 103 =>
                        Me.Registers (A) := To_R (
                            To_F (Me.Registers (B)) + To_F (Me.Registers (C)));
                    when 104 =>
                        Me.Registers (A) := To_R (
                            To_F (Me.Registers (B)) - To_F (Me.Registers (C)));
                    when 105 =>
                        Me.Registers (A) := To_R (
                            To_F (Me.Registers (B)) * To_F (Me.Registers (C)));
                    when 106 =>
                        Me.Registers (A) := To_R (
                            To_F (Me.Registers (B)) / To_F (Me.Registers (C)));
                    when 107 =>
                        Me.Registers (A) := Register_Type (
                            Float'Ceiling (To_F (Me.Registers (A))));
                    when 108 =>
                        Me.Registers (A) := Register_Type (
                            Float'Floor (To_F (Me.Registers (A))));
                    when 109 =>
                        Me.Registers (A) := To_R (
                            Sin (To_F (Me.Registers (A))));
                    when 110 =>
                        Me.Registers (A) := To_R (
                            Cos (To_F (Me.Registers (A))));
                    when 111 =>
                        Me.Registers (A) := To_R (
                            Tan (To_F (Me.Registers (A))));
                    when 112 =>
                        Me.Registers (A) := To_R (
                            To_F (Me.Registers (B)) ** To_F (Me.Registers (C)));
                    when 113 =>
                        Me.Registers (A) := To_R (
                            Arcsin (To_F (Me.Registers (A))));
                    when 114 =>
                        Me.Registers (A) := To_R (
                            Arccos (To_F (Me.Registers (A))));
                    when 115 =>
                        Me.Registers (A) := To_R (
                            Arctan (To_F (Me.Registers (A))));
                    when 116 =>
                        Me.Registers (A) := To_R (
                            Log (To_F (Me.Registers (B)),
                                To_F (Me.Registers (C))));
                    when 117 =>
                        if To_F (Me.Registers (B)) > To_F (Me.Registers (C))
                        then
                            Me.Registers (A) := 1;
                        elsif To_F (Me.Registers (B)) > To_F (Me.Registers (C))
                        then
                            Me.Registers (A) := -1;
                        else
                            Me.Registers (A) := 0;
                        end if;
                    when others => null;
                end case;
            end Do_Float_Op;

            procedure Do_Ask_Op is begin
                case Instruction is
                    when 120 =>
                        Machines (Team, UT_ENGINEER_SS).Registers (16) :=
                            Me.Registers (B);
                        Machines (Team, UT_ENGINEER_SS).Registers (17) :=
                            Me.Registers (C);
                        if Get_Unit (State, UT_ENGINEER_SS, Team).Alive then
                            Me.Registers (A) := 1;
                        else
                            Me.Registers (A) := 0;
                        end if;
                    when 121 =>
                        Machines (Team, UT_ENGINEER_FS).Registers (16) :=
                            Me.Registers (B);
                        Machines (Team, UT_ENGINEER_FS).Registers (17) :=
                            Me.Registers (C);
                        if Get_Unit (State, UT_ENGINEER_FS, Team).Alive then
                            Me.Registers (A) := 1;
                        else
                            Me.Registers (A) := 0;
                        end if;
                    when 122 =>
                        Machines (Team, UT_ENGINEER_SS).Registers (18) :=
                            Me.Registers (B);
                        Machines (Team, UT_ENGINEER_SS).Registers (19) :=
                            Me.Registers (C);
                        if Get_Unit (State, UT_ENGINEER_SS, Team).Alive then
                            Me.Registers (A) := 1;
                        else
                            Me.Registers (A) := 0;
                        end if;
                    when 123 =>
                        Machines (Team, UT_ENGINEER_FS).Registers (18) :=
                            Me.Registers (B);
                        Machines (Team, UT_ENGINEER_FS).Registers (19) :=
                            Me.Registers (C);
                        if Get_Unit (State, UT_ENGINEER_FS, Team).Alive then
                            Me.Registers (A) := 1;
                        else
                            Me.Registers (A) := 0;
                        end if;
                    when 124 =>
                        Machines (Team, UT_CAPTAIN).Registers (30) :=
                            Machines (Team, UT_CAPTAIN).Registers (30) +
                            Register_Type (Immediate);
                        if Get_Unit (State, UT_CAPTAIN, Team).Alive then
                            Me.Registers (A) := 1;
                        else
                            Me.Registers (A) := 0;
                        end if;
                    when 125 =>
                        Machines (Team, UT_CAPTAIN).Registers (31) :=
                            Machines (Team, UT_CAPTAIN).Registers (31) +
                            Register_Type (Immediate);
                        if Get_Unit (State, UT_CAPTAIN, Team).Alive then
                            Me.Registers (A) := 1;
                        else
                            Me.Registers (A) := 0;
                        end if;
                    when 126 =>
                        Machines (Team, UT_MORTAR).Registers (16) :=
                            Me.Registers (B);
                        Machines (Team, UT_MORTAR).Registers (17) :=
                            Me.Registers (C);
                        if Get_Unit (State, UT_MORTAR, Team).Alive then
                            Me.Registers (A) := 1;
                        else
                            Me.Registers (A) := 0;
                        end if;
                    when 127 =>
                        Machines (Team, UT_RIFLEMAN_SS).Registers (16) :=
                            Me.Registers (B);
                        Machines (Team, UT_RIFLEMAN_SS).Registers (17) :=
                            Me.Registers (C);
                        Machines (Team, UT_RIFLEMAN_FS).Registers (16) :=
                            Me.Registers (B);
                        Machines (Team, UT_RIFLEMAN_FS).Registers (17) :=
                            Me.Registers (C);
                        Machines (Enemy_Of (Team),
                            UT_RIFLEMAN_SS).Registers (18) := Me.Registers (B);
                        Machines (Enemy_Of (Team),
                            UT_RIFLEMAN_SS).Registers (19) := Me.Registers (C);
                        Machines (Enemy_Of (Team),
                            UT_RIFLEMAN_FS).Registers (18) := Me.Registers (B);
                        Machines (Enemy_Of (Team),
                            UT_RIFLEMAN_FS).Registers (19) := Me.Registers (C);
                    when others => null;
                end case;
            end Do_Ask_Op;

            procedure Captain_Instruction is begin
                case Instruction is
                    when 96 => Increment_Points (State, Team);
                    when 97 => Increment_Points (State, Team, 4);
                    when 98 => Increment_Points (State, Team, 16);
                    when 99 => Increment_Points (State, Team, 64);
                    when 100 =>
                        Increment_Points (State, Team, 1024);
                        Kill_Unit (State, Team, Unit);
                    when 101 =>
                        Increment_Points (State, Team, 1024);
                        Kill_Unit (State, Team, Unit_Type'Val (
                            Me.Registers (A) - 1));
                    when 102 => Bomb_Water (State, Team);
                    when 103 => Bomb_Beach (State, Team);
                    when 104 => null; -- Just get re-awakened
                    when 105 =>
                        Shared (Enemy_Of (Team), Immediate) := Me.Registers (A);
                    when 106 =>
                        for Index in Shared'Range (2) loop
                            Shared (Enemy_Of (Team), Index) := 0;
                        end loop;
                    when 107 =>
                        for Index in Unit_Type'Range loop
                            Radios (Team, Index, Immediate) := Me.Registers (A);
                        end loop;
                    when 108 =>
                        Me.Registers (A) := Radios (Team, UT_MORTAR, Immediate);
                    when 109 =>
                        Me.Registers (A) := Radios (Team, UT_SNIPER, Immediate);
                    when 110 =>
                        Me.Registers (A) :=
                            Radios (Team, UT_ENGINEER_SS, Immediate);
                    when 111 =>
                        Me.Registers (A) :=
                            Radios (Team, UT_ENGINEER_FS, Immediate);
                    when 112 =>
                        Me.Registers (A) :=
                            Radios (Team, UT_MACHINEGUNNER_SS, Immediate);
                    when 113 =>
                        Me.Registers (A) :=
                            Radios (Team, UT_MACHINEGUNNER_FS, Immediate);
                    when 114 =>
                        Me.Registers (A) :=
                            Radios (Team, UT_SCOUT_SS, Immediate);
                    when 115 =>
                        Me.Registers (A) :=
                            Radios (Team, UT_SCOUT_FS, Immediate);
                    when 116 =>
                        Me.Registers (A) :=
                            Radios (Team, UT_RIFLEMAN_SS, Immediate);
                    when 117 =>
                        Me.Registers (A) :=
                            Radios (Team, UT_RIFLEMAN_FS, Immediate);
                    when 118 =>
                        Radios (Team, UT_MORTAR, Immediate) := Me.Registers (A);
                    when 119 =>
                        Radios (Team, UT_SNIPER, Immediate) := Me.Registers (A);
                    when 120 =>
                        Radios (Team, UT_ENGINEER_SS, Immediate) :=
                            Me.Registers (A);
                    when 121 =>
                        Radios (Team, UT_ENGINEER_FS, Immediate) :=
                            Me.Registers (A);
                    when 122 =>
                        Radios (Team, UT_MACHINEGUNNER_SS, Immediate) :=
                            Me.Registers (A);
                    when 123 =>
                        Radios (Team, UT_MACHINEGUNNER_FS, Immediate) :=
                            Me.Registers (A);
                    when 124 =>
                        Radios (Team, UT_SCOUT_SS, Immediate) :=
                            Me.Registers (A);
                    when 125 =>
                        Radios (Team, UT_SCOUT_FS, Immediate) :=
                            Me.Registers (A);
                    when 126 =>
                        Radios (Team, UT_RIFLEMAN_SS, Immediate) :=
                            Me.Registers (A);
                    when 127 =>
                        Radios (Team, UT_RIFLEMAN_FS, Immediate) :=
                            Me.Registers (A);
                    when others => null;
                end case;
            end Captain_Instruction;

            procedure Mortar_Instruction is begin
                case Instruction is
                    when 96 =>
                        Tactical (Team,
                            X_Coordinate (Me.Registers (B)),
                            Y_Coordinate (Me.Registers (C))) :=
                            Me.Registers (A);
                    when 97 =>
                        Me.Registers (A) := Tactical (
                            Team,
                            X_Coordinate (Me.Registers (B)),
                            Y_Coordinate (Me.Registers (C)));
                    when 98 =>
                        Support (Team,
                            X_Coordinate (Me.Registers (B)),
                            Y_Coordinate (Me.Registers (C))) :=
                            Me.Registers (A);
                    when 99 =>
                        Me.Registers (A) := Support (
                            Team,
                            X_Coordinate (Me.Registers (B)),
                            Y_Coordinate (Me.Registers (C)));
                    when 100 =>
                        Flag (Team,
                            X_Coordinate (Me.Registers (B)),
                            Y_Coordinate (Me.Registers (C))) :=
                            Me.Registers (A);
                    when 101 =>
                        Me.Registers (A) := Flag (
                            Team,
                            X_Coordinate (Me.Registers (B)),
                            Y_Coordinate (Me.Registers (C)));
                    when 102 .. 117 => Do_Float_Op;
                    when 118 => Do_Melee (State, Team, Unit);
                    when 119 => Set_Setup (State, Team, Unit, True);
                    when 120 .. 125 | 127 => Do_Ask_Op;
                    when 126 => Set_Setup (State, Team, Unit, False);
                    when others => null;
                end case;
            end Mortar_Instruction;

            procedure Sniper_Instruction is begin
                case Instruction is
                    when 96 => Set_Setup (State, Team, Unit, True);
                    when 97 => Set_Setup (State, Team, Unit, False);
                    when 98 =>
                        if Do_Shoot (State, Team, Unit, BC) then
                            Me.Registers (A) := 1;
                        else
                            Me.Registers (A) := 0;
                        end if;
                    when 99 =>
                        declare
                            My_Valid : Boolean;
                            My_Where : Location;
                            My_Time : Natural;
                            My_ICount : Natural;
                            My_CCount : Natural;
                            Your_Valid : Boolean;
                            Your_Where : Location;
                            Your_Time : Natural;
                            Your_ICount : Natural;
                            Your_CCount : Natural;
                        begin
                            My_Where := Target_Of (
                                State, Team, UT_MORTAR, My_Valid);
                            if My_Valid then
                                My_ICount := Machines (
                                    Team, UT_MORTAR).ICounter;
                                My_CCount := Machines (
                                    Team, UT_MORTAR).CCounter;
                                case Get_Unit (State, UT_MORTAR, Team).Speed is
                                    when CPUS_EIGHT_FRAMES =>
                                        My_Time := My_ICount * 8 -
                                            (8 - My_CCount);
                                    when CPUS_SIX_FRAMES =>
                                        My_Time := My_ICount * 6 -
                                            (6 - My_CCount);
                                    when CPUS_FOUR_FRAMES =>
                                        My_Time := My_ICount * 4 -
                                            (4 - My_CCount);
                                    when CPUS_TWO_FRAMES =>
                                        My_Time := My_ICount * 2 -
                                            (2 - My_CCount);
                                    when CPUS_EVERY_FRAME =>
                                        My_Time := My_ICount;
                                end case;
                            end if;
                            Your_Where := Target_Of (
                                State, Team, UT_MORTAR, Your_Valid);
                            if Your_Valid then
                                Your_ICount := Machines (
                                    Team, UT_MORTAR).ICounter;
                                Your_CCount := Machines (
                                    Team, UT_MORTAR).CCounter;
                                case Get_Unit (State, UT_MORTAR, Team).Speed is
                                    when CPUS_EIGHT_FRAMES =>
                                        Your_Time := Your_ICount * 8 -
                                            (8 - Your_CCount);
                                    when CPUS_SIX_FRAMES =>
                                        Your_Time := Your_ICount * 6 -
                                            (6 - Your_CCount);
                                    when CPUS_FOUR_FRAMES =>
                                        Your_Time := Your_ICount * 4 -
                                            (4 - Your_CCount);
                                    when CPUS_TWO_FRAMES =>
                                        Your_Time := Your_ICount * 2 -
                                            (2 - Your_CCount);
                                    when CPUS_EVERY_FRAME =>
                                        Your_Time := Your_ICount;
                                end case;
                            end if;

                            if My_Valid then
                                if Your_Valid then
                                    if My_Time < Your_Time then
                                        Me.Registers (A) :=
                                            Register_Type (My_Time);
                                        Me.Registers (B) :=
                                            Register_Type (My_Where (Team).X);
                                        Me.Registers (C) :=
                                            Register_Type (My_Where (Team).Y);
                                    else
                                        Me.Registers (A) :=
                                            Register_Type (Your_Time);
                                        Me.Registers (B) :=
                                            Register_Type (Your_Where (Team).X);
                                        Me.Registers (C) :=
                                            Register_Type (Your_Where (Team).Y);
                                    end if;
                                else
                                    Me.Registers (A) :=
                                        Register_Type (My_Time);
                                    Me.Registers (B) :=
                                        Register_Type (My_Where (Team).X);
                                    Me.Registers (C) :=
                                        Register_Type (My_Where (Team).Y);
                                end if;
                            else
                                if Your_Valid then
                                    Me.Registers (A) :=
                                        Register_Type (Your_Time);
                                    Me.Registers (B) :=
                                        Register_Type (Your_Where (Team).X);
                                    Me.Registers (C) :=
                                        Register_Type (Your_Where (Team).Y);
                                else
                                    Me.Registers (A) := -1;
                                    Me.Registers (B) := -1;
                                    Me.Registers (C) := -1;
                                end if;
                            end if;
                        end;
                    when 100 =>
                        if Is_Targeted (State, BC) then
                            if Targeting_Team (State, BC) = Team then
                                Me.Registers (A) := Register_Type (UID_OF (
                                    Targeting_Unit (State, BC)));
                            else
                                Me.Registers (A) := Register_Type (-UID_OF (
                                    Targeting_Unit (State, BC)));
                            end if;
                        else
                            Me.Registers (A) := 0;
                        end if;
                    when 101 =>
                        declare
                            Where : Location;
                            Valid : Boolean;
                        begin
                            if Me.Registers (A) < 0 then
                                Where := Target_Of (State, Enemy_Of (Team),
                                    Unit_Type'Val (-(Me.Registers (A) + 1)),
                                    Valid);
                            else
                                Where := Target_Of (State, Team,
                                    Unit_Type'Val (Me.Registers (A) - 1),
                                    Valid);
                            end if;

                            if Valid then
                                Me.Registers (B) :=
                                    Register_Type (Where (Team).X);
                                Me.Registers (C) :=
                                    Register_Type (Where (Team).Y);
                            else
                                Me.Registers (B) := -1;
                                Me.Registers (C) := -1;
                            end if;
                        end;
                    when 102 .. 117 => Do_Float_Op;
                    when 118 => Set_Prone (State, Team, Unit, True);
                    when 119 => Set_Prone (State, Team, Unit, False);
                    when 120 .. 127 => Do_Ask_Op;
                    when others => null;
                end case;
            end Sniper_Instruction;

            procedure Engineer_Instruction is begin
                case Instruction is
                    when 96 => Plant_Wire (State, Team, Unit,
                        To_Direction (Me.Registers (A)));
                    when 97 => Plant_Wire (State, Team, Unit,
                        To_Direction (Me.Registers (A)), True);
                    when 98 => Plant_Cover (State, Team, Unit,
                        To_Direction (Me.Registers (A)));
                    when 99 => Plant_Cover (State, Team, Unit,
                        To_Direction (Me.Registers (A)), True);
                    when 100 =>
                        Flag (Team,
                            X_Coordinate (Me.Registers (B)),
                            Y_Coordinate (Me.Registers (C))) :=
                            Me.Registers (A);
                    when 101 =>
                        Me.Registers (A) := Flag (
                            Team,
                            X_Coordinate (Me.Registers (B)),
                            Y_Coordinate (Me.Registers (C)));
                    when 102 .. 117 => Do_Float_Op;
                    when 118 => Set_Prone (State, Team, Unit, True);
                    when 119 => Set_Prone (State, Team, Unit, False);
                    when 120 .. 127 => Do_Ask_Op;
                    when others => null;
                end case;
            end Engineer_Instruction;

            procedure Machinegunner_Instruction is begin
                case Instruction is
                    when 96 => Set_Setup (State, Team, Unit, True);
                    when 97 => Set_Setup (State, Team, Unit, False);
                    when 98 =>
                        Support (Team,
                        X_Coordinate (Me.Registers (B)),
                        Y_Coordinate (Me.Registers (C))) :=
                            Me.Registers (A);
                    when 99 =>
                        Me.Registers (A) := Support (
                            Team,
                            X_Coordinate (Me.Registers (B)),
                            Y_Coordinate (Me.Registers (C)));
                    when 100 =>
                        Flag (Team,
                            X_Coordinate (Me.Registers (B)),
                            Y_Coordinate (Me.Registers (C))) :=
                            Me.Registers (A);
                    when 101 =>
                        Me.Registers (A) := Flag (
                            Team,
                            X_Coordinate (Me.Registers (B)),
                            Y_Coordinate (Me.Registers (C)));
                    when 102 .. 117 => Do_Float_Op;
                    when 118 => Do_Melee (State, Team, Unit);
                    when 119 => Do_Hit (State, Team, Unit, To_Direction (
                        Me.Registers (A)));
                    when 120 .. 127 => Do_Ask_Op;
                    when others => null;
                end case;
            end Machinegunner_Instruction;

            procedure Scout_Instruction is begin
                case Instruction is
                    when 96 => Do_Move (State, Team, Unit, To_Direction (
                        Me.Registers (A)));
                    when 97 => Do_Hit (State, Team, Unit, To_Direction (
                        Me.Registers (A)));
                    when 98 =>
                        Support (Team,
                            X_Coordinate (Me.Registers (B)),
                            Y_Coordinate (Me.Registers (C))) :=
                            Me.Registers (A);
                    when 99 =>
                        Me.Registers (A) := Support (
                            Team,
                            X_Coordinate (Me.Registers (B)),
                            Y_Coordinate (Me.Registers (C)));
                    when 100 =>
                        Flag (Team,
                            X_Coordinate (Me.Registers (B)),
                            Y_Coordinate (Me.Registers (C))) :=
                            Me.Registers (A);
                    when 101 =>
                        Me.Registers (A) := Flag (
                            Team,
                            X_Coordinate (Me.Registers (B)),
                            Y_Coordinate (Me.Registers (C)));
                    when 102 .. 117 => Do_Float_Op;
                    when 118 => Set_Prone (State, Team, Unit, True);
                    when 119 => Set_Prone (State, Team, Unit, False);
                    when 120 .. 127 => Do_Ask_Op;
                    when others => null;
                end case;
            end Scout_Instruction;

            procedure Rifleman_Instruction is begin
                case Instruction is
                    when 96 =>
                        Tactical (Team,
                            X_Coordinate (Me.Registers (B)),
                            Y_Coordinate (Me.Registers (C))) :=
                            Me.Registers (A);
                    when 97 =>
                        Me.Registers (A) := Tactical (
                            Team,
                            X_Coordinate (Me.Registers (B)),
                            Y_Coordinate (Me.Registers (C)));
                    when 98 =>
                        Support (Team,
                            X_Coordinate (Me.Registers (B)),
                            Y_Coordinate (Me.Registers (C))) :=
                            Me.Registers (A);
                    when 99 =>
                        Me.Registers (A) := Support (
                            Team,
                            X_Coordinate (Me.Registers (B)),
                            Y_Coordinate (Me.Registers (C)));
                    when 100 =>
                        Flag (Team,
                            X_Coordinate (Me.Registers (B)),
                            Y_Coordinate (Me.Registers (C))) :=
                            Me.Registers (A);
                    when 101 =>
                        Me.Registers (A) := Flag (
                            Team,
                            X_Coordinate (Me.Registers (B)),
                            Y_Coordinate (Me.Registers (C)));
                    when 102 .. 117 => Do_Float_Op;
                    when 118 => Set_Prone (State, Team, Unit, True);
                    when 119 => Set_Prone (State, Team, Unit, False);
                    when 120 .. 127 => Do_Ask_Op;
                    when others => null;
                end case;
            end Rifleman_Instruction;
            New_Log : Logger.Log_Entry;
        begin
            if Us.Summoned then
                Do_Move (State, Team, Unit, Get_Direction_Towards (
                    Us.Position (Team),
                    Get_Unit (State, UT_CAPTAIN, Team).Position (Team)));
                if Adjacent (
                    Us.Position (Team),
                    Get_Unit (State, UT_CAPTAIN, Team).Position (Team))
                then
                    Set_Unit_Summon (State, Team, Unit, False);
                    Machines (Team, UT_CAPTAIN).ICounter := 0;
                end if;
                return;
            end if;

            if Us.Retreating then
                Do_Move (State, Team, Unit, Get_Direction_Towards (
                    Us.Position (Team), Home_Base));
                if Adjacent (Us.Position (Team), Home_Base) then
                    Set_Unit_Retreat (State, Team, Unit, False);
                end if;
                return;
            end if;

            Me.Registers (12) := Register_Type (Small);
            Me.Registers (15) := Me.Registers (15) + 1;

            New_Log.Pre.Registers := Me.Registers;
            New_Log.Pre.State := Get_Unit (State, Unit, Team);
            New_Log.Unit := Unit;
            New_Log.Team := Team;
            New_Log.Operation := Instruction;
            New_Log.A := A;
            New_Log.B := B;
            New_Log.C := C;
            New_Log.Small := Small;
            New_Log.Immediate := Immediate;

            case Instruction is
                when 0 =>
                    Me.Registers (A) := Me.Registers (B) + Me.Registers (C);
                when 1 =>
                    Me.Registers (A) := Me.Registers (B) - Me.Registers (C);
                when 2 =>
                    Me.Registers (A) := Me.Registers (B) * Me.Registers (C);
                when 3 =>
                    Me.Registers (A) := Me.Registers (B) / Me.Registers (C);
                when 4 =>
                    Me.Registers (A) := From_U32 (
                        To_U32 (Me.Registers (B)) and
                        To_U32 (Me.Registers (C)));
                when 5 =>
                    Me.Registers (A) := From_U32 (
                        To_U32 (Me.Registers (B)) or
                        To_U32 (Me.Registers (C)));
                when 6 =>
                    Me.Registers (A) := From_U32 (
                        To_U32 (Me.Registers (B)) xor
                        To_U32 (Me.Registers (C)));
                when 7 =>
                    Me.Registers (A) := From_U32 (not (
                        To_U32 (Me.Registers (B)) and
                        To_U32 (Me.Registers (C))));
                when 8 =>
                    Me.Registers (B) := Count_Leading_Zeroes (Me.Registers (A));
                    Me.Registers (C) := Count_Leading_Ones (Me.Registers (A));
                when 9 =>
                    Me.Registers (B) := Count_Zeroes (Me.Registers (A));
                    Me.Registers (C) := Count_Ones (Me.Registers (A));
                when 10 =>
                    Me.Registers (A) := Shift_Right (
                        Me.Registers (B), Me.Registers (C));
                when 11 =>
                    Me.Registers (A) := Shift_Left (
                        Me.Registers (B), Me.Registers (C));
                when 12 =>
                    if Me.Registers (A) > 0 then
                        Me.Registers (B) := Me.Registers (A);
                        Me.Registers (C) := -Me.Registers (A);
                    else
                        Me.Registers (B) := -Me.Registers (A);
                        Me.Registers (C) := Me.Registers (A);
                    end if;
                when 13 =>
                    Me.Registers (A) :=
                        Random_Registers.Random (Register_Generator);
                    Me.Registers (B) :=
                        Random_Registers.Random (Register_Generator);
                    Me.Registers (C) :=
                        Random_Registers.Random (Register_Generator);
                when 14 =>
                    if Me.Registers (B) > Me.Registers (C) then
                        Me.Registers (A) := 1;
                    elsif Me.Registers (B) < Me.Registers (C) then
                        Me.Registers (A) := -1;
                    else
                        Me.Registers (A) := 0;
                    end if;
                when 15 | 21 =>
                    if Me.Registers (A) = 0 then
                        Me.Registers (15) := Register_Type (Immediate);
                    end if;
                when 16 | 22 =>
                    if Me.Registers (A) /= 0 then
                        Me.Registers (15) := Register_Type (Immediate);
                    end if;
                when 17 | 23 =>
                    if Me.Registers (A) > 0 then
                        Me.Registers (15) := Register_Type (Immediate);
                    end if;
                when 18 | 24 =>
                    if Me.Registers (A) < 0 then
                        Me.Registers (15) := Register_Type (Immediate);
                    end if;
                when 19 | 25 =>
                    if Me.Registers (A) >= 0 then
                        Me.Registers (15) := Register_Type (Immediate);
                    end if;
                when 20 | 26 =>
                    if Me.Registers (A) <= 0 then
                        Me.Registers (15) := Register_Type (Immediate);
                    end if;
                when 27 =>
                    Me.Registers (14) := Me.Registers (15);
                    Me.Registers (15) :=
                        Me.Registers (A) + Register_Type (Immediate);
                when 28 =>
                    Me.Registers (A) := Me.Memory (Address_Type (
                        Me.Registers (B) + Me.Registers (C)));
                when 29 =>
                    Me.Memory (Address_Type (
                        Me.Registers (B) + Me.Registers (C))) :=
                        Me.Registers (A);
                when 30 =>
                    Me.Registers (13) := Me.Registers (13) + 1;
                    Me.Registers (A) := Me.Memory (
                        Address_Type (Me.Registers (13)));
                when 31 =>
                    Me.Memory (Address_Type (Me.Registers (13))) :=
                        Me.Registers (A);
                    Me.Registers (13) := Me.Registers (13) - 1;
                when 32 =>
                    if Team_Of (State, BC) = Team then
                        Me.Registers (A) := 1;
                    elsif Team_Of (State, BC) = Enemy_Of (Team) then
                        Me.Registers (A) := -1;
                    else
                        Me.Registers (A) := 0;
                    end if;
                when 33 =>
                    if Team_Of (State, BC) = Team then
                        Me.Registers (A) := Register_Type (UID_OF (
                            Unit_Of (State, BC)));
                    elsif Team_Of (State, BC) = Enemy_Of (Team) then
                        Me.Registers (A) := Register_Type (-UID_OF (
                            Unit_Of (State, BC)));
                    else
                        Me.Registers (A) := 0;
                    end if;
                when 34 =>
                    if Team_Of (State, BC) = T_NONE then
                        Me.Registers (A) := -1;
                    else
                        Me.Registers (A) := Cache_Size'Pos (Get_Unit (
                            State, Unit_Of (State, BC),
                            Team_Of (State, BC)).Cache_Space);
                    end if;
                when 35 =>
                    if Team_Of (State, BC) = T_NONE then
                        Me.Registers (A) := -1;
                    else
                        Me.Registers (A) := Cache_Type'Pos (Get_Unit (
                            State, Unit_Of (State, BC),
                            Team_Of (State, BC)).Cache_Kind);
                    end if;
                when 36 =>
                    if Team_Of (State, BC) = T_NONE then
                        Me.Registers (A) := -1;
                    else
                        Me.Registers (A) := Branch_Type'Pos (Get_Unit (
                            State, Unit_Of (State, BC),
                            Team_Of (State, BC)).Branch_Predictor);
                    end if;
                when 37 =>
                    if Team_Of (State, BC) = T_NONE then
                        Me.Registers (A) := -1;
                    else
                        Me.Registers (A) := CPU_Speed'Pos (Get_Unit (
                            State, Unit_Of (State, BC),
                            Team_Of (State, BC)).Speed);
                    end if;
                when 38 =>
                    Me.Registers (A) := Terrain_Type'Pos (
                        Terrain_Of (State, BC)) + 1;
                when 39 =>
                    if Me.Registers (A) > 0 then
                        declare
                            The_Unit : Unit_State := Get_Unit (State,
                            Unit_Type'Val (Me.Registers (A) - 1), Team);
                        begin
                            if The_Unit.Alive then
                                Me.Registers (B) := Register_Type (
                                    The_Unit.Position (Team).X);
                                Me.Registers (C) := Register_Type (
                                    The_Unit.Position (Team).Y);
                            else
                                Me.Registers (B) := -1;
                                Me.Registers (C) := -1;
                            end if;
                        end;
                    else
                        declare
                            The_Type : Unit_Type :=
                                Unit_Type'Val (-(Me.Registers (A) + 1));
                            The_Unit : Unit_State := Get_Unit (
                                State, The_Type, Enemy_Of (Team));
                        begin
                            if (The_Type = UT_SNIPER and The_Unit.Setup) or
                                (not The_Unit.Alive) or The_Unit.Hidden
                            then
                                Me.Registers (B) := -1;
                                Me.Registers (C) := -1;
                            else
                                Me.Registers (B) := Register_Type (
                                    The_Unit.Position (Team).X);
                                Me.Registers (C) := Register_Type (
                                    The_Unit.Position (Team).Y);
                            end if;
                        end;
                    end if;
                when 40 =>
                    Me.Registers (A) := Get_Path_To (
                        Us.Position (Team), BC (Team))'Length;
                when 41 =>
                    Me.Registers (A) := Register_Type (
                        Cover_Between (State, Team, Unit, BC));
                when 42 =>
                    if Get_Unit (State, Unit_Type'Val (Me.Registers (A) - 1),
                        Team).Alive
                    then
                        Me.Registers (B) := 0;
                    else
                        Me.Registers (B) := 1;
                    end if;

                    if Get_Unit (State, Unit_Type'Val (Me.Registers (A) - 1),
                        Enemy_Of (Team)).Alive
                    then
                        Me.Registers (C) := 0;
                    else
                        Me.Registers (C) := 1;
                    end if;
                when 43 =>
                    if Do_Shoot (State, Team, Unit, BC) then
                        Me.Registers (A) := 1;
                    else
                        Me.Registers (A) := 0;
                    end if;
                when 44 =>
                    Me.Registers (A) := Direction'Pos (Get_Direction_Towards (
                        Us.Position (Team),
                        (X_Coordinate (Me.Registers (B)),
                        Y_Coordinate (Me.Registers (C))))) + 1;
                when 45 =>
                    Do_Move (
                        State, Team, Unit, To_Direction (Me.Registers (A)));
                    Me.Registers (B) := Register_Type (Us.Position (Team).X);
                    Me.Registers (C) := Register_Type (Us.Position (Team).Y);
                when 46 =>
                    Do_Crawl (
                        State, Team, Unit, To_Direction (Me.Registers (A)));
                    Me.Registers (B) := Register_Type (Us.Position (Team).X);
                    Me.Registers (C) := Register_Type (Us.Position (Team).Y);
                when 47 =>
                    Do_Swim (
                        State, Team, Unit, To_Direction (Me.Registers (A)));
                    Me.Registers (B) := Register_Type (Us.Position (Team).X);
                    Me.Registers (C) := Register_Type (Us.Position (Team).Y);
                when 48 =>
                    if Is_On_Target (State, Team, Unit) then
                        Win_Game (State, Team);
                    end if;
                when 49 =>
                    Set_Unit_Retreat (State, Team, Unit, True);
                when 50 =>
                    Set_Unit_Visibility (State, Team, Unit, True);
                when 51 =>
                    Radios (Team, Unit, Immediate) := Me.Registers (A);
                when 52 =>
                    Me.Registers (A) := Radios (Team, Unit, Immediate);
                when 53 =>
                    Shared (Team, Immediate) := Me.Registers (A);
                when 54 =>
                    Me.Registers (A) := Shared (Team, Immediate);
                when 55 =>  -- TODO: DEBUG MODE!
                    Kill_Unit (State, Team, Unit);
                when 56 .. 63 =>
                    declare
                        Dest : Coordinate;
                    begin
                        if Find_Unit (State, Team, Unit,
                            Direction'Val (Instruction - 56), Dest)
                        then
                            Me.Registers (B) := Register_Type (Dest.X);
                            Me.Registers (C) := Register_Type (Dest.Y);
                            if Team_Of (State, BC) = Team then
                                Me.Registers (A) := Register_Type (
                                    UID_OF (Unit_Of (State, BC)));
                            elsif Team_Of (State, BC) = Enemy_Of (Team) then
                                Me.Registers (A) := Register_Type (
                                    -UID_OF (Unit_Of (State, BC)));
                            else
                                Me.Registers (A) := 0;
                            end if;
                        end if;
                    end;
                when 64 =>
                    Me.Registers (A) := Cache_Size'Pos (Us.Cache_Space);
                when 65 =>
                    Me.Registers (A) := Cache_Type'Pos (Us.Cache_Kind);
                when 66 =>
                    Me.Registers (A) := Branch_Type'Pos (Us.Branch_Predictor);
                when 67 =>
                    Me.Registers (A) := CPU_Speed'Pos (Us.Speed);
                when 68 =>
                    Me.Registers (A) := Cache_Size'Pos (Get_Unit (
                        State, Unit_Type'Val (Me.Registers (A) - 1), Team)
                            .Cache_Space);
                when 69 =>
                    Me.Registers (A) := Cache_Type'Pos (Get_Unit (
                        State,
                        Unit_Type'Val (Me.Registers (A) - 1), Team).Cache_Kind);
                when 70 =>
                    Me.Registers (A) := Branch_Type'Pos (Get_Unit (
                        State,
                        Unit_Type'Val (Me.Registers (A) - 1), Team)
                            .Branch_Predictor);
                when 71 =>
                    Me.Registers (A) := CPU_Speed'Pos (Get_Unit (
                        State,
                        Unit_Type'Val (Me.Registers (A) - 1), Team).Speed);
                when 72 =>
                    Me.Registers (A) :=
                        Register_Type (Get_Points (State, Team));
                when 73 =>
                    Me.Registers (A) := Register_Type (
                        Cache_Size_Cost (Us.Cache_Space));
                when 74 =>
                    Me.Registers (A) := Register_Type (
                        Cache_Type_Cost (Us.Cache_Kind));
                when 75 =>
                    Me.Registers (A) := Register_Type (
                        Branch_Type_Cost (Us.Branch_Predictor));
                when 76 =>
                    Me.Registers (A) := Register_Type (
                        Speed_Cost (Us.Speed));
                when 77 =>
                    if Try_Upgrade_Cache_Size (State, Team, Unit) then
                        Me.Registers (A) := 1;
                    else
                        Me.Registers (A) := 0;
                    end if;
                when 78 =>
                    if Try_Upgrade_Cache_Type (State, Team, Unit) then
                        Me.Registers (A) := 1;
                    else
                        Me.Registers (A) := 0;
                    end if;
                when 79 =>
                    if Try_Upgrade_Branch_Type (State, Team, Unit) then
                        Me.Registers (A) := 1;
                    else
                        Me.Registers (A) := 0;
                    end if;
                when 80 =>
                    if Try_Upgrade_CPU_Speed (State, Team, Unit) then
                        Me.Registers (A) := 1;
                    else
                        Me.Registers (A) := 0;
                    end if;
                when 81 =>
                    if Try_Upgrade_Cache_Size (State, Team, Unit, True) then
                        Me.Registers (A) := 1;
                    else
                        Me.Registers (A) := 0;
                    end if;
                when 82 =>
                    if Try_Upgrade_Cache_Type (State, Team, Unit, True) then
                        Me.Registers (A) := 1;
                    else
                        Me.Registers (A) := 0;
                    end if;
                when 83 =>
                    if Try_Upgrade_Branch_Type (State, Team, Unit, True) then
                        Me.Registers (A) := 1;
                    else
                        Me.Registers (A) := 0;
                    end if;
                when 84 =>
                    if Try_Upgrade_CPU_Speed (State, Team, Unit, True) then
                        Me.Registers (A) := 1;
                    else
                        Me.Registers (A) := 0;
                    end if;
                when 85 =>
                    if Try_Max_Cache_Size (State, Team, Unit) then
                        Me.Registers (A) := 1;
                    else
                        Me.Registers (A) := 0;
                    end if;
                when 86 =>
                    if Try_Max_Cache_Type (State, Team, Unit) then
                        Me.Registers (A) := 1;
                    else
                        Me.Registers (A) := 0;
                    end if;
                when 87 =>
                    if Try_Max_Branch_Type (State, Team, Unit) then
                        Me.Registers (A) := 1;
                    else
                        Me.Registers (A) := 0;
                    end if;
                when 88 =>
                    if Try_Max_CPU_Speed (State, Team, Unit) then
                        Me.Registers (A) := 1;
                    else
                        Me.Registers (A) := 0;
                    end if;
                when 89 =>
                    if Try_Max_Cache_Size (State, Team, Unit, True) then
                        Me.Registers (A) := 1;
                    else
                        Me.Registers (A) := 0;
                    end if;
                when 90 =>
                    if Try_Max_Cache_Type (State, Team, Unit, True) then
                        Me.Registers (A) := 1;
                    else
                        Me.Registers (A) := 0;
                    end if;
                when 91 =>
                    if Try_Max_Branch_Type (State, Team, Unit, True) then
                        Me.Registers (A) := 1;
                    else
                        Me.Registers (A) := 0;
                    end if;
                when 92 =>
                    if Try_Max_CPU_Speed (State, Team, Unit, True) then
                        Me.Registers (A) := 1;
                    else
                        Me.Registers (A) := 0;
                    end if;
                when 93 =>
                    Me.Registers (A) := Register_Type (Clock);
                    Me.Registers (B) := Register_Type (Me.Clock);
                    Me.Registers (C) := 0;
                    for Index in Unit_Type'Range loop
                        Me.Registers (C) := Me.Registers (C) +
                            Register_Type (Machines (Team, Index).Clock);
                    end loop;
                when 94 =>
                    Me.Behind := True;
                when 95 =>
                    Me.Advanced := True;
                when others =>
                    case Unit is
                        when UT_CAPTAIN => Captain_Instruction;
                        when UT_MORTAR => Mortar_Instruction;
                        when UT_SNIPER => Sniper_Instruction;
                        when UT_ENGINEER_SS | UT_ENGINEER_FS =>
                            Engineer_Instruction;
                        when UT_MACHINEGUNNER_SS | UT_MACHINEGUNNER_FS =>
                            Machinegunner_Instruction;
                        when UT_SCOUT_SS | UT_SCOUT_FS => Scout_Instruction;
                        when UT_RIFLEMAN_SS | UT_RIFLEMAN_FS =>
                            Rifleman_Instruction;
                    end case;
            end case;
            New_Log.Post.Registers := Me.Registers;
            New_Log.Post.State := Get_Unit (State, Unit, Team);
            Logger.Log (New_Log);
        exception
            when E : others =>
                Logger.Log_Error (Unit, Team,
                    Ada.Exceptions.Exception_Information (E));
                Kill_Unit (State, Team, Unit);
        end Do_Instruction;

        procedure Execute_Step is begin
            if Machines (Team, Unit).Clock = 0 then
                Machines (Team, Unit).ICounter := Compute_Time;
            end if;

            Machines (Team, Unit).Clock := Machines (Team, Unit).Clock + 1;

            if Machines (Team, Unit).ICounter = 0 then
                while Machines (Team, Unit).ICounter = 0 loop
                    Do_Instruction;
                    Machines (Team, Unit).ICounter := Compute_Time;
                end loop;
                Machines (Team, Unit).ICounter :=
                    Machines (Team, Unit).ICounter - 1;
            else
                Machines (Team, Unit).ICounter :=
                    Machines (Team, Unit).ICounter - 1;
                Logger.Log_IWait (Unit, Team, Machines (Team, Unit).ICounter);
            end if;
        end Execute_Step;
    begin
        if Us.Alive then
            if Machines (Team, Unit).CCounter = 0 then
                Execute_Step;
                Machines (Team, Unit).CCounter := Reset_Counter (
                    Get_Unit (State, Unit, Team).Speed);
                if Machines (Team, Unit).Advanced then
                    Machines (Team, Unit).CCounter :=
                        Machines (Team, Unit).CCounter - 1;
                    Machines (Team, Unit).Advanced := False;
                end if;
                if Machines (Team, Unit).Behind then
                    Machines (Team, Unit).CCounter :=
                        Machines (Team, Unit).CCounter + 1;
                    Machines (Team, Unit).Behind := False;
                end if;
            else
                Machines (Team, Unit).CCounter :=
                    Machines (Team, Unit).CCounter - 1;
                Logger.Log_CWait (Unit, Team, Machines (Team, Unit).CCounter);
            end if;
        end if;
    end Step_Processor;
end Processors;
