with Coordinates;

-- TODO: Improve 'Image for enums
package Boards is
    type Upgrade_Type is (Cache_Size, Cache_Type, Branch_Type, CPU_Speed);
    type Upgrade_Level is range 0 .. 4;

    -- Cache Size definitions
    CS_NONE : constant Upgrade_Level := 0;
    CS_64 : constant Upgrade_Level := 1;
    CS_1024 : constant Upgrade_Level := 2;
    CS_4096 : constant Upgrade_Level := 3;
    CS_65536 : constant Upgrade_Level := 4;

    -- Cache Type definitions
    CT_NONE : constant Upgrade_Level := 0;
    CT_TWO_WAY : constant Upgrade_Level := 1;
    CT_FOUR_WAY : constant Upgrade_Level := 2;
    CT_EIGHT_WAY : constant Upgrade_Level := 3;
    CT_FULLY : constant Upgrade_Level := 4;

    -- Branch Type definitions
    BT_NONE : constant Upgrade_Level := 0;
    BT_ONE_BIT : constant Upgrade_Level := 1;
    BT_TWO_BIT : constant Upgrade_Level := 2;
    BT_TWO_LEVEL_TWO_BIT : constant Upgrade_Level := 3;
    BT_PERFECT : constant Upgrade_Level := 4;

    -- CPU Speed definitions
    CPUS_EIGHT_FRAMES : constant Upgrade_Level := 0;
    CPUS_SIX_FRAMES : constant Upgrade_Level := 1;
    CPUS_FOUR_FRAMES : constant Upgrade_Level := 2;
    CPUS_TWO_FRAMES : constant Upgrade_Level := 3;
    CPUS_EVERY_FRAME : constant Upgrade_Level := 4;

    type Upgrade_Array is array (Upgrade_Type) of Upgrade_Level;
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
        Upgrades : Upgrade_Array;
        Position, Destination : Location;
        Hidden, Alive, Summoned, Retreating, Setup, Moving, Shooting, Prone :
            Boolean;
    end record;

    Enemy_Of : constant array (Player_ID) of Player_ID := (T_BLACK, T_WHITE);
    Engineer_IDs : constant array (Board_Side) of Unit_Type := (
        UT_ENGINEER_SS, UT_ENGINEER_FS);
    Machinegunner_IDs : constant array (Board_Side) of Unit_Type := (
        UT_MACHINEGUNNER_SS, UT_MACHINEGUNNER_FS);
    Scout_IDs : constant array (Board_Side) of Unit_Type := (
        UT_SCOUT_SS, UT_SCOUT_FS);
    Rifleman_IDs : constant array (Board_Side) of Unit_Type := (
        UT_RIFLEMAN_SS, UT_RIFLEMAN_FS);
    UID_OF : constant array (Unit_Type) of Integer := (
        1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11);
    Upgrade_Cost : constant array (Upgrade_Type, Upgrade_Level)
        of Resource_Points := (
            Cache_Size | Cache_Type | Branch_Type => (0, 32, 64, 256, 1024),
            CPU_Speed => (0, 64, 128, 512, 2048));

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

    function Count_Wire (This : in Board) return Natural;

    function Count_Cover (This : in Board) return Natural;

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

    function Try_Upgrade (
        This : in out Board;
        Team : in Player_ID;
        Unit : in Unit_Type;
        Which : in Upgrade_Type;
        Down : in Boolean := False)
        return Boolean;

    function Try_Max (
        This : in out Board;
        Team : in Player_ID;
        Unit : in Unit_Type;
        Which : in Upgrade_Type;
        Down : in Boolean := False)
        return Boolean;

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
