with Coordinates;

-- TODO: Improve 'Image for enums
package Boards is
    type Cache_Size is (CS_NONE, CS_64, CS_1024, CS_4096, CS_65536);
    type Cache_Type is (
        CT_NONE, CT_TWO_WAY, CT_FOUR_WAY, CT_EIGHT_WAY, CT_FULLY);
    type Branch_Type is (BT_NONE, BT_ONE_BIT, BT_TWO_BIT, BT_TWO_LEVEL_TWO_BIT,
        BT_PERFECT);
    type CPU_Speed is (CPUS_EIGHT_FRAMES, CPUS_SIX_FRAMES, CPUS_FOUR_FRAMES,
        CPUS_TWO_FRAMES, CPUS_EVERY_FRAME);
    type Unit_Type is (UT_CAPTAIN, UT_MORTAR, UT_SNIPER, UT_ENGINEER_SS,
        UT_ENGINEER_FS, UT_MACHINEGUNNER_SS, UT_MACHINEGUNNER_FS, UT_SCOUT_SS,
        UT_SCOUT_FS, UT_RIFLEMAN_SS, UT_RIFLEMAN_FS);
    type Board_Side is (SNIPER_SIDE, FAR_SIDE);
    type Terrain_Type is (TT_OPEN, TT_WIRE, TT_SAND, TT_BEACH, TT_WATER,
        TT_BASE);
    type Team_ID is (T_WHITE, T_BLACK, T_NONE);
    subtype Player_ID is Team_ID range T_WHITE .. T_BLACK;
    type Resource_Points is range 0 .. (2 ** 31 - 1);
    type Board is private;
    type Location is array (Player_ID) of Coordinates.Coordinate;

    Home_Base : constant Coordinates.Coordinate := (0, 7);

    type Unit_State is record
        Cache_Space : Cache_Size;
        Cache_Kind : Cache_Type;
        Branch_Predictor : Branch_Type;
        Speed : CPU_Speed;
        Position, Destination : Location;
        Hidden, Alive, Summoned, Retreating, Setup, Moving, Shooting, Prone :
            Boolean;
    end record;

    Enemy_Of : constant array (Player_ID) of Player_ID := (T_BLACK, T_WHITE);
    Engineers : constant array (Board_Side) of Unit_Type := (
        UT_ENGINEER_SS, UT_ENGINEER_FS);
    Machinegunners : constant array (Board_Side) of Unit_Type := (
        UT_MACHINEGUNNER_SS, UT_MACHINEGUNNER_FS);
    Scouts : constant array (Board_Side) of Unit_Type := (
        UT_SCOUT_SS, UT_SCOUT_FS);
    Riflemen : constant array (Board_Side) of Unit_Type := (
        UT_RIFLEMAN_SS, UT_RIFLEMAN_FS);
    UID_OF : constant array (Unit_Type) of Integer := (
        1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11);
    Cache_Size_Cost : constant array (Cache_Size) of Resource_Points := (
        0, 32, 64, 256, 1024);
    Cache_Type_Cost : constant array (Cache_Type) of Resource_Points := (
        0, 32, 64, 256, 1024);
    Branch_Type_Cost : constant array (Branch_Type) of Resource_Points := (
        0, 32, 64, 256, 1024);
    Speed_Cost : constant array (CPU_Speed) of Resource_Points := (
        0, 64, 128, 512, 2048);

    procedure Initialize (This : out Board);

    function To_Location (
        Where : in Coordinates.Coordinate;
        Team : in Player_ID)
        return Location;

    -- Query Operations
    function Get_Unit (
        This : in Board;
        Unit : in Unit_Type;
        Team : in Player_ID) return Unit_State;

    -- Returns captain if no appropriate ally
    function Get_Nearest_Ally (
        This : in Board;
        Where : in Location;
        Team : in Player_ID;
        Skip : in Natural := 0) return Unit_State;

    function Count_Nearby_Enemies (
        This : in Board;
        Where : in Location;
        Team : in Player_ID;
        Distance : in Positive) return Natural;

    function Get_Nearest_Terrain (
        This : in Board;
        Where : in Location;
        Team : in Player_ID;
        Terrain : in Terrain_Type) return Coordinates.Coordinate;

    function Distance_To_Base (
        This : in Board;
        Where : in Location;
        Team : in Player_ID) return Natural;

    function Compute_Fire_Time (
        State : in Board;
        Unit : in Unit_Type;
        Source : in Location;
        Destination : in Location) return Natural;

    function Is_On_Target (
        State : in Board;
        Team : in Player_ID;
        Unit : in Unit_Type) return Boolean;

    function Team_Of (
        This : in Board;
        Where : in Location) return Team_ID;

    function Unit_Of (
        This : in Board;
        Where : in Location) return Unit_Type;

    function Terrain_Of (
        This : in Board;
        Where : in Location) return Terrain_Type;

    function Cover_Between (
        This : in Board;
        Team : in Player_ID;
        Unit : in Unit_Type;
        Destination : in Location) return Integer;

    function Find_Unit (
        This : in out Board;
        Team : in Player_ID;
        Unit : in Unit_Type;
        Which_Way : in Coordinates.Direction;
        Result : out Coordinates.Coordinate) return Boolean;

    function Get_Points (This : in Board; Team : in Player_ID)
        return Resource_Points;

    function Is_Targeted (
        This : in Board;
        Where : in Location)
        return Boolean;

    function Targeting_Team (
        This : in Board;
        Where : in Location)
        return Team_ID;

    function Targeting_Unit (
        This : in Board;
        Where : in Location)
        return Unit_Type;

    function Target_Of (
        This : in Board;
        Team : in Player_ID;
        Unit : in Unit_Type;
        Valid : out Boolean) return Location;

    -- Preparation functions
    procedure Prepare_Move (
        This : in out Board;
        Team : in Player_ID;
        Unit : in Unit_Type;
        Which_Way : in Coordinates.Direction);

    procedure Prepare_Shoot (
        This : in out Board;
        Team : in Player_ID;
        Unit : in Unit_Type;
        Target : in Coordinates.Coordinate);

    -- Execution functions
    procedure Do_Move (
        This : in out Board;
        Team : in Player_ID;
        Unit : in Unit_Type;
        Which_Way : in Coordinates.Direction);

    procedure Do_Crawl (
        This : in out Board;
        Team : in Player_ID;
        Unit : in Unit_Type;
        Which_Way : in Coordinates.Direction);

    procedure Do_Swim (
        This : in out Board;
        Team : in Player_ID;
        Unit : in Unit_Type;
        Which_Way : in Coordinates.Direction);

    -- NOTE: does not shoot if the team lacks the resources
    function Do_Shoot (
        This : in out Board;
        Team : in Player_ID;
        Unit : in Unit_Type;
        Target : in Location) return Boolean;

    procedure Do_Melee (
        This : in out Board;
        Team : in Player_ID;
        Unit : in Unit_Type);

    procedure Do_Hit (
        This : in out Board;
        Team : in Player_ID;
        Unit : in Unit_Type;
        Which_Way : in Coordinates.Direction);

    procedure Set_Setup (
        This : in out Board;
        Team : in Player_ID;
        Unit : in Unit_Type;
        Setup : in Boolean);

    procedure Set_Prone (
        This : in out Board;
        Team : in Player_ID;
        Unit : in Unit_Type;
        Prone : in Boolean);

    procedure Plant_Wire (
        This : in out Board;
        Team : in Player_ID;
        Unit : in Unit_Type;
        Way : in Coordinates.Direction;
        Cut : in Boolean := False);

    procedure Plant_Cover (
        This : in out Board;
        Team : in Player_ID;
        Unit : in Unit_Type;
        Way : in Coordinates.Direction;
        Cut : in Boolean := False);

    procedure Increment_Points (
        This : in out Board;
        Team : in Team_ID := T_NONE; -- NONE is both in this case
        How_Many : in Resource_Points := 1);

    procedure Kill_Unit (
        This : in out Board;
        Team : in Player_ID;
        Unit : in Unit_Type);

    procedure Win_Game (
        This : in out Board;
        Winner : in Player_ID);

    function Winner (This : in Board) return Team_ID;

    procedure Set_Unit_Visibility (
        This : in out Board;
        Team : in Player_ID;
        Unit : in Unit_Type;
        Visible : in Boolean);

    procedure Set_Unit_Summon (
        This : in out Board;
        Team : in Player_ID;
        Unit : in Unit_Type;
        Summon : in Boolean);

    procedure Set_Unit_Retreat (
        This : in out Board;
        Team : in Player_ID;
        Unit : in Unit_Type;
        Retreat : in Boolean);

    function Try_Upgrade_Cache_Size (
        This : in out Board;
        Team : in Player_ID;
        Unit : in Unit_Type;
        Down : in Boolean := False) return Boolean;

    function Try_Upgrade_Cache_Type (
        This : in out Board;
        Team : in Player_ID;
        Unit : in Unit_Type;
        Down : in Boolean := False) return Boolean;

    function Try_Upgrade_Branch_Type (
        This : in out Board;
        Team : in Player_ID;
        Unit : in Unit_Type;
        Down : in Boolean := False) return Boolean;

    function Try_Upgrade_CPU_Speed (
        This : in out Board;
        Team : in Player_ID;
        Unit : in Unit_Type;
        Down : in Boolean := False) return Boolean;

    function Try_Max_Cache_Size (
        This : in out Board;
        Team : in Player_ID;
        Unit : in Unit_Type;
        Down : in Boolean := False) return Boolean;

    function Try_Max_Cache_Type (
        This : in out Board;
        Team : in Player_ID;
        Unit : in Unit_Type;
        Down : in Boolean := False) return Boolean;

    function Try_Max_Branch_Type (
        This : in out Board;
        Team : in Player_ID;
        Unit : in Unit_Type;
        Down : in Boolean := False) return Boolean;

    function Try_Max_CPU_Speed (
        This : in out Board;
        Team : in Player_ID;
        Unit : in Unit_Type;
        Down : in Boolean := False) return Boolean;

    procedure Bomb_Water (This : in out Board; Team : in Player_ID);

    procedure Bomb_Beach (This : in out Board; Team : in Player_ID);

private

    type Team_States is array (Player_ID, Unit_Type) of Unit_State;
    type Terrain_State is array (
        Coordinates.X_Coordinate, Coordinates.Y_Coordinate) of Terrain_Type;
    type Team_Points is array (Player_ID) of Resource_Points;
    type Support_Requests is array (Player_ID) of Coordinates.Coordinate;

    type Board is record
        Units : Team_States;
        Terrain : Terrain_State;
        Points : Team_Points;
        Winner : Team_ID;
    end record;
end Boards;
