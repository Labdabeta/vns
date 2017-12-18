with Processors;
with Boards;
with Games;

package Logger is
    -- Used by 'user'
    type Logger_Verbosity is (LOG_QUIET, LOG_NORMAL, LOG_VERBOSE);

    procedure Set_Verbosity (As : in Logger_Verbosity);

    -- Used by 'model'
    type Log_State is record
        Registers : Processors.Register_Array;
        State : Boards.Unit_State;
    end record;

    type Log_Entry is record
        Unit : Boards.Unit_Type;
        Team : Boards.Player_ID;
        Pre, Post : Log_State;
        Operation : Processors.Instruction_ID;
        A, B, C : Processors.Register_Index;
        Small : Processors.Small_Immediate_Type;
        Immediate : Processors.Address_Type;
    end record;

    procedure Log (What : in Log_Entry);
    procedure Log (A, B, C : in Processors.Register_Type);
    procedure Log_Prep (
        Unit : in Boards.Unit_Type;
        Team : in Boards.Player_ID;
        What : in Processors.Instruction_ID);
    procedure Log_CWait (
        Unit : in Boards.Unit_Type;
        Team : in Boards.Player_ID;
        Wait : in Natural);
    procedure Log_IWait (
        Unit : in Boards.Unit_Type;
        Team : in Boards.Player_ID;
        Wait : in Natural);
    procedure Log_Error (
        Unit : in Boards.Unit_Type;
        Team : in Boards.Player_ID;
        Text : in String);
    procedure Log_UT (
        Tick : in Natural;
        White : in Boards.Resource_Points;
        Black : in Boards.Resource_Points);
    procedure Toggle_Logging (
        Unit : in Boards.Unit_Type;
        Team : in Boards.Player_ID);
private
    Verbosity : Logger_Verbosity := LOG_NORMAL;
end Logger;
