with Boards;
with Coordinates;
with Ada.Numerics.Discrete_Random;
limited with Games;

-- TODO: Better 'Image for Register_Index/Instruction_ID
package Processors is
    type Register_Index is range 0 .. 31;
    type Register_Type is range -(2 ** 31) .. (2 ** 31 - 1);
    type Register_Array is array (Register_Index) of Register_Type;

    type Address_Type is range 0 .. (2 ** 20 - 1);
    type Small_Immediate_Type is range -(2 ** 9) .. (2 ** 9 - 1);
    type Instruction_ID is range 0 .. 2 ** 7;
    type Memory_Array is array (Address_Type range <>) of Register_Type;

    type Communications is array (
        Boards.Player_ID, Boards.Unit_Type, Address_Type) of Register_Type;
    type Shared_Memory is array (Boards.Player_ID, Address_Type) of
        Register_Type;
    type Shared_Grid is array (Boards.Player_ID, Coordinates.X_Coordinate,
        Coordinates.Y_Coordinate) of Register_Type;

    type Unit_Processor is private;
    type Processor_Array is array (Boards.Player_ID, Boards.Unit_Type) of
        Unit_Processor;

    -- NOTE: Passing these as value is fine, it should be reference under the
    -- hood.
    procedure Initialize (This : out Unit_Processor);
    procedure Load_Code (This : in out Unit_Processor; Code : in Memory_Array);
    procedure Set_Registers (
        Machines : in out Processor_Array;
        State : in Boards.Board);
    procedure Step_Processor (
        Which : in out Games.Game;
        Unit : in Boards.Unit_Type;
        Team : in Boards.Player_ID);
private
    package Random_Registers is new
        Ada.Numerics.Discrete_Random (Register_Type);
    Register_Generator : Random_Registers.Generator;

    type Cache_Entry is record
        Address : Address_Type;
        Value : Register_Type;
        Is_Loaded : Boolean;
        Age : Natural;
    end record;
    type Cache_Array is array (Address_Type range <>) of Cache_Entry;
    type Cache_Contents is record
        Data : Cache_Array (0 .. 65535);
        Length : Address_Type;
        Size : Boards.Cache_Size;
    end record;

    type Branch_Direction is (DIR_TAKEN, DIR_NOT_TAKEN);
    type Branch_Strength is (STRONGLY, WEAKLY);
    type Two_State_Branch is record
        Direction : Branch_Direction;
        Strength : Branch_Strength;
    end record;
    type Two_Level_Branch_States is array (Branch_Direction, Branch_Direction)
        of Two_State_Branch;
    type Branch_State is record
        Last_Direction : Branch_Direction;
        Even_Earlier_Direction : Branch_Direction;
        Prediction : Two_State_Branch;
        Prediction_History : Two_Level_Branch_States;
    end record;
    type Branch_State_Array is array (Address_Type) of Branch_State;

    type Unit_Processor is record
        Memory : Memory_Array (Address_Type'Range);
        Registers : Register_Array;
        Cache : Cache_Contents;
        Predictor : Branch_State_Array;
        ICounter : Natural;
        CCounter : Natural;
        Clock : Natural;
        Advanced : Boolean;
        Behind : Boolean;
    end record;
end Processors;
