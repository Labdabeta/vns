with Coordinates; use Coordinates;

package body Boards is
    Unit_Coords : array (Unit_Type) of Coordinate := (
        UT_CAPTAIN => (1, 7),
        UT_MORTAR => (1, 8),
        UT_SNIPER => (0, 3),
        UT_ENGINEER_SS => (0, 6),
        UT_ENGINEER_FS => (0, 9),
        UT_MACHINEGUNNER_SS => (1, 6),
        UT_MACHINEGUNNER_FS => (1, 9),
        UT_SCOUT_SS => (2, 7),
        UT_SCOUT_FS => (2, 8),
        UT_RIFLEMAN_SS => (2, 5),
        UT_RIFLEMAN_FS => (2, 10));
    River_Coords : array (1 .. 64) of Coordinate := (
        (7, 0), (8, 0), (9, 0), (10, 0),
        (8, 1), (9, 1), (10, 1), (11, 1),
        (9, 2), (10, 2), (11, 2), (12, 2),
        (10, 3), (11, 3), (12, 3), (13, 3),
        (11, 4), (12, 4), (13, 4), (14, 4),
        (12, 5), (13, 5), (14, 5), (15, 5),
        (13, 6), (14, 6), (15, 6), (16, 6),
        (14, 7), (15, 7), (16, 7), (17, 7),
        (14, 8), (15, 8), (16, 8), (17, 8),
        (15, 9), (16, 9), (17, 9), (18, 9),
        (16, 10), (17, 10), (18, 10), (19, 10),
        (17, 11), (18, 11), (19, 11), (20, 11),
        (18, 12), (19, 12), (20, 12), (21, 12),
        (19, 13), (20, 13), (21, 13), (22, 13),
        (20, 14), (21, 14), (22, 14), (23, 14),
        (21, 15), (22, 15), (23, 15), (24, 15));
    Beach_Coords : array (Player_ID, 1 .. 30) of Coordinate := (
        T_WHITE => (
            (6, 0), (6, 1), (7, 1), (7, 2), (8, 2), (8, 3), (9, 3), (9, 4),
            (10, 4), (10, 5), (11, 5), (11, 6), (12, 6), (12, 7), (13, 7),
            (13, 8), (13, 9), (14, 9), (14, 10), (15, 10), (15, 11), (16, 11),
            (16, 12), (17, 12), (17, 13), (18, 13), (18, 14), (19, 14),
            (19, 15), (20, 15)),
        T_BLACK => (
            (11, 0), (12, 0), (12, 1), (13, 1), (13, 2), (14, 2), (14, 3),
            (15, 3), (15, 4), (16, 4), (16, 5), (17, 5), (17, 6), (18, 6),
            (18, 7), (18, 8), (19, 8), (19, 9), (20, 9), (20, 10), (21, 10),
            (21, 11), (22, 11), (22, 12), (23, 12), (23, 13), (24, 13),
            (24, 14), (25, 14), (25, 15)));
    Base_Coords : array (Player_ID) of Coordinate := (
        T_WHITE => (0, 7), T_BLACK => (31, 8));

    function To_Location (
        Where : in Coordinates.Coordinate;
        Team : in Player_ID)
        return Location is
        Result : Location;
    begin
        Result (Team) := Where;
        Result (Enemy_Of (Team)) := (
            X_Coordinate'Last - Where.X,
            Y_Coordinate'Last - Where.Y);
        return Result;
    end To_Location;

    function To_Team (
        Where : in Coordinates.Coordinate;
        Team : in Player_ID)
        return Coordinate is
        Result : Location;
    begin
        Result (T_WHITE) := Where;
        Result (T_BLACK) := (
            X_Coordinate'Last - Where.X,
            Y_Coordinate'Last - Where.Y);
        return Result (Team);
    end To_Team;

    function To_Team (
        Way : in Coordinates.Direction;
        Team : in Player_ID)
        return Direction is
    begin
        if Team = T_BLACK then
            case Way is
                when DIR_NORTH => return DIR_SOUTH;
                when DIR_NORTH_EAST => return DIR_SOUTH_WEST;
                when DIR_EAST => return DIR_WEST;
                when DIR_SOUTH_EAST => return DIR_NORTH_WEST;
                when DIR_SOUTH => return DIR_NORTH;
                when DIR_SOUTH_WEST => return DIR_NORTH_EAST;
                when DIR_WEST => return DIR_EAST;
                when DIR_NORTH_WEST => return DIR_SOUTH_EAST;
            end case;
        else
            return Way;
        end if;
    end To_Team;

    procedure Initialize (This : out Board) is begin
        for P in This.Units'Range (1) loop
            for U in This.Units'Range (2) loop
                This.Units (P, U) := (
                    Cache_Space => CS_NONE,
                    Cache_Kind => CT_NONE,
                    Branch_Predictor => BT_NONE,
                    Speed => CPUS_EIGHT_FRAMES,
                    Position => To_Location (Unit_Coords (U), P),
                    Destination => To_Location (Unit_Coords (U), P),
                    Hidden => False,
                    Alive => True,
                    Summoned => False,
                    Retreating => False,
                    Setup => False,
                    Moving => False,
                    Shooting => False,
                    Prone => False);
            end loop;
        end loop;

        This.Terrain := (others => (others => TT_OPEN));

        for Index in River_Coords'Range loop
            This.Terrain (
                River_Coords (Index).X,
                River_Coords (Index).Y) := TT_WATER;
        end loop;

        for P in Beach_Coords'Range (1) loop
            for Index in Beach_Coords'Range (2) loop
                This.Terrain (
                    Beach_Coords (P, Index).X,
                    Beach_Coords (P, Index).Y) := TT_BEACH;
            end loop;
        end loop;

        for P in Base_Coords'Range loop
            This.Terrain (Base_Coords (P).X, Base_Coords (P).Y) := TT_BASE;
        end loop;

        This.Winner := T_NONE;
    end Initialize;

    function Get_Unit (
        This : in Board;
        Unit : in Unit_Type;
        Team : in Player_ID) return Unit_State is
    begin
        return This.Units (Team, Unit);
    end Get_Unit;

    function Get_Nearest_Ally (
        This : in Board;
        Where : in Location;
        Team : in Player_ID;
        Skip : in Natural := 0) return Unit_State is
        Search_Path : Coordinate_Path := Generate_Search_Path (Where (T_WHITE));
        Skip_Left : Natural := Skip;
    begin
        for Index in Search_Path'Range loop
            for U in This.Units'Range (2) loop
                if This.Units (Team, U).Position (T_WHITE) = Search_Path (Index)
                then
                    if Skip_Left > 0 then
                        Skip_Left := Skip_Left - 1;
                    else
                        return This.Units (Team, U);
                    end if;
                end if;
            end loop;
        end loop;

        return This.Units (Team, UT_CAPTAIN);
    end Get_Nearest_Ally;

    function Count_Nearby_Enemies (
        This : in Board;
        Where : in Location;
        Team : in Player_ID;
        Distance : in Positive) return Natural is
        Result : Natural := 0;
    begin
        for U in This.Units'Range (2) loop
            if Get_Path_To (Where (T_WHITE),
                This.Units (Enemy_Of (Team), U).Position (T_WHITE))'Length <=
                Distance and This.Units (Enemy_Of (Team), U).Alive
            then
                Result := Result + 1;
            end if;
        end loop;

        return Result;
    end Count_Nearby_Enemies;

    function Get_Nearest_Terrain (
        This : in Board;
        Where : in Location;
        Team : in Player_ID;
        Terrain : in Terrain_Type) return Coordinates.Coordinate is
        Search_Path : Coordinate_Path := Generate_Search_Path (Where (T_WHITE));
    begin
        for Index in Search_Path'Range loop
            if This.Terrain (Search_Path (Index).X, Search_Path (Index).Y) =
                Terrain
            then
                return To_Team (Search_Path (Index), Team);
            end if;
        end loop;

        return (0, 0);
    end Get_Nearest_Terrain;

    function Distance_To_Base (
        This : in Board;
        Where : in Location;
        Team : in Player_ID) return Natural is
    begin
        return Get_Path_To (Where (Team), Home_Base)'Length;
    end Distance_To_Base;

    function Compute_Fire_Time (
        State : in Board;
        Unit : in Unit_Type;
        Source : in Location;
        Destination : in Location) return Natural is
        B, P, D, A : Natural; -- The constants
        C : Integer;
        Path : Coordinate_Path := Get_Path_To (
            Source (T_WHITE), Destination (T_WHITE));
    begin
        case Unit is
            when UT_CAPTAIN => B := 1; P := 16; A := 64;
            when UT_MORTAR => B := 512; P := 0; A := 0;
            when UT_SNIPER => B := 64; P := 2; A := 0;
            when UT_ENGINEER_SS | UT_ENGINEER_FS => B := 8; P := 8; A := 32;
            when UT_MACHINEGUNNER_SS | UT_MACHINEGUNNER_FS =>
                B := 1; P := 8; A := 64;
            when UT_SCOUT_SS | UT_SCOUT_FS => B := 8; P := 16; A := 4;
            when UT_RIFLEMAN_SS | UT_RIFLEMAN_FS => B := 4; P := 4; A := 16;
        end case;

        D := Path'Length;
        C := 0;

        -- Don't count cover on first tile
        for Index in Positive range Path'First + 1 .. Path'Last loop
            case State.Terrain (Path (Index).X, Path (Index).Y) is
                when TT_WATER | TT_BEACH => D := D - 1;
                when TT_SAND => C := C + 1;
                when others => null;
            end case;
        end loop;

        return B + P * D + A * C;
    end Compute_Fire_Time;

    function Is_On_Target (
        State : in Board;
        Team : in Player_ID;
        Unit : in Unit_Type) return Boolean is
    begin
        return State.Units (Team, Unit).Position (T_WHITE) = Home_Base;
    end Is_On_Target;

    function Team_Of (
        This : in Board;
        Where : in Location) return Team_ID is
    begin
        for P in This.Units'Range (1) loop
            for U in This.Units'Range (2) loop
                if This.Units (P, U).Position = Where then
                    return P;
                end if;
            end loop;
        end loop;

        return T_NONE;
    end Team_Of;

    function Unit_Of (
        This : in Board;
        Where : in Location) return Unit_Type is
    begin
        for P in This.Units'Range (1) loop
            for U in This.Units'Range (2) loop
                if This.Units (P, U).Position = Where then
                    return U;
                end if;
            end loop;
        end loop;

        return UT_CAPTAIN;
    end Unit_Of;

    function Terrain_Of (
        This : in Board;
        Where : in Location) return Terrain_Type is
    begin
        return This.Terrain (Where (T_WHITE).X, Where (T_WHITE).Y);
    end Terrain_Of;

    function Cover_Between (
        This : in Board;
        Team : in Player_ID;
        Unit : in Unit_Type;
        Destination : in Location) return Integer is
        Path : Coordinate_Path := Get_Path_To (
            This.Units (Team, Unit).Position (T_WHITE), Destination (T_WHITE));
        Cover : Integer := 0;
    begin
        for Index in Positive range Path'First + 1 .. Path'Last loop
            case This.Terrain (Path (Index).X, Path (Index).Y) is
                when TT_WATER | TT_BEACH => Cover := Cover - 1;
                when TT_SAND => Cover := Cover + 1;
                when others => null;
            end case;
        end loop;

        return Cover;
    end Cover_Between;

    function Find_Unit (
        This : in out Board;
        Team : in Player_ID;
        Unit : in Unit_Type;
        Which_Way : in Coordinates.Direction;
        Result : out Coordinates.Coordinate) return Boolean is
        Space : Coordinate := This.Units (Team, Unit).Position (T_WHITE);
    begin
        loop
            Apply_Direction (Space, To_Team (Which_Way, Team));
            if Team_Of (This, To_Location (Space, Team)) /= T_NONE then
                Result := Space;
                return True;
            end if;
        end loop;
    exception
        when others =>
            return False;
    end Find_Unit;

    function Get_Points (This : in Board; Team : in Player_ID)
        return Resource_Points is
    begin
        return This.Points (Team);
    end Get_Points;

    function Is_Targeted (
        This : in Board;
        Where : in Location)
        return Boolean is
    begin
        for P in This.Units'Range (1) loop
            for U in This.Units'Range (2) loop
                if This.Units (P, U).Shooting and
                    This.Units (P, U).Destination = Where
                then
                    return True;
                end if;
            end loop;
        end loop;

        return False;
    end Is_Targeted;

    function Targeting_Team (
        This : in Board;
        Where : in Location)
        return Team_ID is
    begin
        for P in This.Units'Range (1) loop
            for U in This.Units'Range (2) loop
                if This.Units (P, U).Shooting and
                    This.Units (P, U).Destination = Where
                then
                    return P;
                end if;
            end loop;
        end loop;

        return T_NONE;
    end Targeting_Team;

    function Targeting_Unit (
        This : in Board;
        Where : in Location)
        return Unit_Type is
    begin
        for P in This.Units'Range (1) loop
            for U in This.Units'Range (2) loop
                if This.Units (P, U).Shooting and
                    This.Units (P, U).Destination = Where
                then
                    return U;
                end if;
            end loop;
        end loop;

        return UT_CAPTAIN;
    end Targeting_Unit;

    function Target_Of (
        This : in Board;
        Team : in Player_ID;
        Unit : in Unit_Type;
        Valid : out Boolean) return Location is
    begin
        if This.Units (Team, Unit).Shooting then
            Valid := True;
        else
            Valid := False;
        end if;

        return This.Units (Team, Unit).Destination;
    end Target_Of;

    procedure Prepare_Move (
        This : in out Board;
        Team : in Player_ID;
        Unit : in Unit_Type;
        Which_Way : in Coordinates.Direction) is
        Target : Coordinate := This.Units (Team, Unit).Position (T_WHITE);
    begin
        This.Units (Team, Unit).Moving := True;
        Apply_Direction (Target, To_Team (Which_Way, Team));
        This.Units (Team, Unit).Destination := To_Location (Target, T_WHITE);
    exception
        -- If we're stuck on the edge, just bump into it
        when others => return;
    end Prepare_Move;

    procedure Prepare_Shoot (
        This : in out Board;
        Team : in Player_ID;
        Unit : in Unit_Type;
        Target : in Coordinates.Coordinate) is
    begin
        This.Units (Team, Unit).Shooting := True;
        This.Units (Team, Unit).Destination := To_Location (Target, Team);
    end Prepare_Shoot;

    procedure Do_Move (
        This : in out Board;
        Team : in Player_ID;
        Unit : in Unit_Type;
        Which_Way : in Coordinates.Direction) is
        Target : Coordinate := This.Units (Team, Unit).Position (T_WHITE);
    begin
        if (This.Terrain (Target.X, Target.Y) = TT_OPEN or
            This.Terrain (Target.X, Target.Y) = TT_BEACH or
            This.Terrain (Target.X, Target.Y) = TT_BASE) and
            Team_Of (This, To_Location (Target, T_WHITE)) = T_NONE
        then
            Apply_Direction (Target, To_Team (Which_Way, Team));
            This.Units (Team, Unit).Position := To_Location (Target, T_WHITE);
        end if;
    end Do_Move;

    procedure Do_Crawl (
        This : in out Board;
        Team : in Player_ID;
        Unit : in Unit_Type;
        Which_Way : in Coordinates.Direction) is
        Target : Coordinate := This.Units (Team, Unit).Position (T_WHITE);
    begin
        if (This.Terrain (Target.X, Target.Y) = TT_OPEN or
            This.Terrain (Target.X, Target.Y) = TT_BEACH or
            This.Terrain (Target.X, Target.Y) = TT_WIRE or
            This.Terrain (Target.X, Target.Y) = TT_BASE) and
            Team_Of (This, To_Location (Target, T_WHITE)) = T_NONE
        then
            Apply_Direction (Target, To_Team (Which_Way, Team));
            This.Units (Team, Unit).Position := To_Location (Target, T_WHITE);
        end if;
    end Do_Crawl;

    procedure Do_Swim (
        This : in out Board;
        Team : in Player_ID;
        Unit : in Unit_Type;
        Which_Way : in Coordinates.Direction) is
        Target : Coordinate := This.Units (Team, Unit).Position (T_WHITE);
    begin
        if This.Terrain (Target.X, Target.Y) = TT_WATER and
            Team_Of (This, To_Location (Target, T_WHITE)) = T_NONE
        then
            Apply_Direction (Target, To_Team (Which_Way, Team));
            This.Units (Team, Unit).Position := To_Location (Target, T_WHITE);
        end if;
    end Do_Swim;

    function Do_Shoot (
        This : in out Board;
        Team : in Player_ID;
        Unit : in Unit_Type;
        Target : in Location) return Boolean is
        Path : Coordinate_Path := Get_Path_To (
            This.Units (Team, Unit).Position (T_WHITE), Target (T_WHITE));
        Cost : Resource_Points;
        Result : Boolean;
    begin
        case Unit is
            when UT_CAPTAIN => Cost := 32;
            when UT_MORTAR => Cost := 256;
            when UT_SNIPER => Cost := 64;
            when UT_ENGINEER_SS | UT_ENGINEER_FS => Cost := 32;
            when UT_MACHINEGUNNER_SS | UT_MACHINEGUNNER_FS => Cost := 64;
            when UT_SCOUT_SS | UT_SCOUT_FS => Cost := 64;
            when UT_RIFLEMAN_SS | UT_RIFLEMAN_FS => Cost := 32;
        end case;

        if This.Points (Team) < Cost then
            return False;
        end if;

        if (Unit = UT_MORTAR or Unit = UT_MACHINEGUNNER_SS or Unit =
            UT_MACHINEGUNNER_FS) and not This.Units (Team, Unit).Setup
        then
            return False;
        end if;

        This.Points (Team) := This.Points (Team) - Cost;

        if Unit = UT_MORTAR then
            Result := False;
            for P in This.Units'Range (1) loop
                for U in This.Units'Range (2) loop
                    if Get_Path_To (
                        Target (T_WHITE),
                        This.Units (P, U).Position (T_WHITE))'Length <= 1
                    then
                        This.Units (P, U).Alive := False;
                        Result := True;
                    end if;
                end loop;
            end loop;

            return Result;
        end if;

        for Index in Positive range Path'First + 1 .. Path'Length loop
            for P in This.Units'Range (1) loop
                for U in This.Units'Range (2) loop
                    if This.Units (P, U).Position (T_WHITE) = Path (Index) and
                        (not This.Units (P, U).Prone or Index = Path'Last)
                    then
                        This.Units (P, U).Alive := False;
                        return True;
                    end if;
                end loop;
            end loop;
        end loop;

        return False;
    end Do_Shoot;

    procedure Do_Melee (
        This : in out Board;
        Team : in Player_ID;
        Unit : in Unit_Type) is
    begin
        for P in This.Units'Range (1) loop
            for U in This.Units'Range (2) loop
                if Get_Path_To (
                    This.Units (Team, Unit).Position (T_WHITE),
                    This.Units (P, U).Position (T_WHITE))'Length <= 1 and
                    (P /= Team or U /= Unit)
                then
                    This.Units (P, U).Alive := False;
                end if;
            end loop;
        end loop;
    end Do_Melee;

    procedure Do_Hit (
        This : in out Board;
        Team : in Player_ID;
        Unit : in Unit_Type;
        Which_Way : in Coordinates.Direction) is
        Target : Coordinate := This.Units (Team, Unit).Position (T_WHITE);
    begin
        Apply_Direction (Target, Which_Way);
        for P in This.Units'Range (1) loop
            for U in This.Units'Range (2) loop
                if This.Units (P, U).Position (T_WHITE) = Target then
                    This.Units (P, U).Alive := False;
                end if;
            end loop;
        end loop;
    exception
        -- Hitting out of bounds doesn't do any harm
        when others => return;
    end Do_Hit;

    procedure Set_Setup (
        This : in out Board;
        Team : in Player_ID;
        Unit : in Unit_Type;
        Setup : in Boolean) is
    begin
        This.Units (Team, Unit).Setup := Setup;
        This.Units (Team, Unit).Prone := False;
    end Set_Setup;

    procedure Set_Prone (
        This : in out Board;
        Team : in Player_ID;
        Unit : in Unit_Type;
        Prone : in Boolean) is
    begin
        This.Units (Team, Unit).Prone := Prone;
    end Set_Prone;

    procedure Plant_Wire (
        This : in out Board;
        Team : in Player_ID;
        Unit : in Unit_Type;
        Way : in Coordinates.Direction;
        Cut : in Boolean := False) is
        Where : Coordinate := This.Units (Team, Unit).Position (T_WHITE);
    begin
        Apply_Direction (Where, To_Team (Way, Team));
        if Cut then
            if This.Terrain (Where.X, Where.Y) = TT_WIRE
            then
                This.Terrain (Where.X, Where.Y) := TT_OPEN;
            end if;
        else
            if This.Terrain (Where.X, Where.Y) = TT_OPEN
            then
                This.Terrain (Where.X, Where.Y) := TT_WIRE;
            end if;
        end if;
    end Plant_Wire;

    procedure Plant_Cover (
        This : in out Board;
        Team : in Player_ID;
        Unit : in Unit_Type;
        Way : in Coordinates.Direction;
        Cut : in Boolean := False) is
        Where : Coordinate := This.Units (Team, Unit).Position (T_WHITE);
    begin
        Apply_Direction (Where, To_Team (Way, Team));
        if Cut then
            if This.Terrain (Where.X, Where.Y) = TT_SAND
            then
                This.Terrain (Where.X, Where.Y) := TT_OPEN;
            end if;
        else
            if This.Terrain (Where.X, Where.Y) = TT_OPEN
            then
                This.Terrain (Where.X, Where.Y) := TT_SAND;
            end if;
        end if;
    end Plant_Cover;

    procedure Increment_Points (
        This : in out Board;
        Team : in Team_ID := T_NONE; -- NONE is both in this case
        How_Many : in Resource_Points := 1) is
    begin
        if Team = T_NONE or Team = T_WHITE then
            This.Points (T_WHITE) := This.Points (T_WHITE) + How_Many;
        end if;

        if Team = T_NONE or Team = T_BLACK then
            This.Points (T_BLACK) := This.Points (T_BLACK) + How_Many;
        end if;
    end Increment_Points;

    procedure Kill_Unit (
        This : in out Board;
        Team : in Player_ID;
        Unit : in Unit_Type) is
    begin
        This.Units (Team, Unit).Alive := False;
    end Kill_Unit;

    procedure Win_Game (
        This : in out Board;
        Winner : in Player_ID) is
    begin
        This.Winner := Winner;
    end Win_Game;

    procedure Set_Unit_Visibility (
        This : in out Board;
        Team : in Player_ID;
        Unit : in Unit_Type;
        Visible : in Boolean) is
    begin
        This.Units (Team, Unit).Hidden := not Visible;
    end Set_Unit_Visibility;

    procedure Set_Unit_Summon (
        This : in out Board;
        Team : in Player_ID;
        Unit : in Unit_Type;
        Summon : in Boolean) is
    begin
        This.Units (Team, Unit).Summoned := Summon;
    end Set_Unit_Summon;

    procedure Set_Unit_Retreat (
        This : in out Board;
        Team : in Player_ID;
        Unit : in Unit_Type;
        Retreat : in Boolean) is
    begin
        This.Units (Team, Unit).Retreating := Retreat;
    end Set_Unit_Retreat;

    function Try_Upgrade_Cache_Size (
        This : in out Board;
        Team : in Player_ID;
        Unit : in Unit_Type;
        Down : in Boolean := False) return Boolean is
        Cost : Resource_Points := Cache_Size_Cost (This.Units (Team,
            Unit).Cache_Space);
    begin
        if Down then
            if This.Units (Team, Unit).Cache_Space = CS_NONE then
                return False;
            else
                This.Points (Team) := This.Points (Team) + (Cost / 2);
                This.Units (Team, Unit).Cache_Space :=
                    Cache_Size'Pred (This.Units (Team, Unit).Cache_Space);
                return True;
            end if;
        else
            if This.Points (Team) < Cost then
                return False;
            else
                This.Points (Team) := This.Points (Team) - Cost;
                This.Units (Team, Unit).Cache_Space :=
                    Cache_Size'Succ (This.Units (Team, Unit).Cache_Space);
                return True;
            end if;
        end if;
    end Try_Upgrade_Cache_Size;

    function Try_Upgrade_Cache_Type (
        This : in out Board;
        Team : in Player_ID;
        Unit : in Unit_Type;
        Down : in Boolean := False) return Boolean is
        Cost : Resource_Points := Cache_Type_Cost (This.Units (Team,
            Unit).Cache_Kind);
    begin
        if Down then
            if This.Units (Team, Unit).Cache_Kind = CT_NONE then
                return False;
            else
                This.Points (Team) := This.Points (Team) + (Cost / 2);
                This.Units (Team, Unit).Cache_Kind :=
                    Cache_Type'Pred (This.Units (Team, Unit).Cache_Kind);
                return True;
            end if;
        else
            if This.Points (Team) < Cost then
                return False;
            else
                This.Points (Team) := This.Points (Team) - Cost;
                This.Units (Team, Unit).Cache_Kind :=
                    Cache_Type'Succ (This.Units (Team, Unit).Cache_Kind);
                return True;
            end if;
        end if;
    end Try_Upgrade_Cache_Type;

    function Try_Upgrade_Branch_Type (
        This : in out Board;
        Team : in Player_ID;
        Unit : in Unit_Type;
        Down : in Boolean := False) return Boolean is
        Cost : Resource_Points := Branch_Type_Cost (This.Units (Team,
            Unit).Branch_Predictor);
    begin
        if Down then
            if This.Units (Team, Unit).Branch_Predictor = BT_NONE then
                return False;
            else
                This.Points (Team) := This.Points (Team) + (Cost / 2);
                This.Units (Team, Unit).Branch_Predictor :=
                    Branch_Type'Pred (This.Units (Team, Unit).Branch_Predictor);
                return True;
            end if;
        else
            if This.Points (Team) < Cost then
                return False;
            else
                This.Points (Team) := This.Points (Team) - Cost;
                This.Units (Team, Unit).Branch_Predictor :=
                    Branch_Type'Succ (This.Units (Team, Unit).Branch_Predictor);
                return True;
            end if;
        end if;
    end Try_Upgrade_Branch_Type;

    function Try_Upgrade_CPU_Speed (
        This : in out Board;
        Team : in Player_ID;
        Unit : in Unit_Type;
        Down : in Boolean := False) return Boolean is
        Cost : Resource_Points := Speed_Cost (This.Units (Team,
            Unit).Speed);
    begin
        if Down then
            if This.Units (Team, Unit).Speed = CPUS_EIGHT_FRAMES then
                return False;
            else
                This.Points (Team) := This.Points (Team) + (Cost / 2);
                This.Units (Team, Unit).Speed :=
                    CPU_Speed'Pred (This.Units (Team, Unit).Speed);
                return True;
            end if;
        else
            if This.Points (Team) < Cost then
                return False;
            else
                This.Points (Team) := This.Points (Team) - Cost;
                This.Units (Team, Unit).Speed :=
                    CPU_Speed'Succ (This.Units (Team, Unit).Speed);
                return True;
            end if;
        end if;
    end Try_Upgrade_CPU_Speed;

    function Try_Max_Cache_Size (
        This : in out Board;
        Team : in Player_ID;
        Unit : in Unit_Type;
        Down : in Boolean := False) return Boolean is
    begin
        while Try_Upgrade_Cache_Size (This, Team, Unit, Down) loop
            null;
        end loop;

        if Down then
            return This.Units (Team, Unit).Cache_Space = Cache_Size'First;
        else
            return This.Units (Team, Unit).Cache_Space = Cache_Size'Last;
        end if;
    end Try_Max_Cache_Size;

    function Try_Max_Cache_Type (
        This : in out Board;
        Team : in Player_ID;
        Unit : in Unit_Type;
        Down : in Boolean := False) return Boolean is
    begin
        while Try_Upgrade_Cache_Type (This, Team, Unit, Down) loop
            null;
        end loop;

        if Down then
            return This.Units (Team, Unit).Cache_Kind = Cache_Type'First;
        else
            return This.Units (Team, Unit).Cache_Kind = Cache_Type'Last;
        end if;
    end Try_Max_Cache_Type;

    function Try_Max_Branch_Type (
        This : in out Board;
        Team : in Player_ID;
        Unit : in Unit_Type;
        Down : in Boolean := False) return Boolean is
    begin
        while Try_Upgrade_Branch_Type (This, Team, Unit, Down) loop
            null;
        end loop;

        if Down then
            return This.Units (Team, Unit).Branch_Predictor = Branch_Type'First;
        else
            return This.Units (Team, Unit).Branch_Predictor = Branch_Type'Last;
        end if;
    end Try_Max_Branch_Type;

    function Try_Max_CPU_Speed (
        This : in out Board;
        Team : in Player_ID;
        Unit : in Unit_Type;
        Down : in Boolean := False) return Boolean is
    begin
        while Try_Upgrade_CPU_Speed (This, Team, Unit, Down) loop
            null;
        end loop;

        if Down then
            return This.Units (Team, Unit).Speed = CPU_Speed'First;
        else
            return This.Units (Team, Unit).Speed = CPU_Speed'Last;
        end if;
    end Try_Max_CPU_Speed;

    procedure Bomb_Water (This : in out Board; Team : in Player_ID) is begin
        if This.Points (Team) >= 128 then
            This.Points (Team) := This.Points (Team) - 128;

            for P in This.Units'Range (1) loop
                for U in This.Units'Range (2) loop
                    if This.Terrain (
                        This.Units (P, U).Position (T_WHITE).X,
                        This.Units (P, U).Position (T_WHITE).Y) = TT_WATER
                    then
                        This.Units (P, U).Alive := False;
                    end if;
                end loop;
            end loop;
        end if;
    end Bomb_Water;

    procedure Bomb_Beach (This : in out Board; Team : in Player_ID) is begin
        if This.Points (Team) >= 64 then
            This.Points (Team) := This.Points (Team) - 64;

            for Index in Beach_Coords'Range (2) loop
                for P in This.Units'Range (1) loop
                    for U in This.Units'Range (2) loop
                        if This.Units (P, U).Position (T_WHITE) =
                            Beach_Coords (Team, Index)
                        then
                            This.Units (P, U).Alive := False;
                        end if;
                    end loop;
                end loop;
            end loop;
        end if;
    end Bomb_Beach;

    function Winner (This : in Board) return Team_ID is
    begin
        return This.Winner;
    end Winner;
end Boards;
