with Ada.Text_IO; use Ada.Text_IO;
with Processors; use Processors;
with Boards; use Boards;
with Coordinates; use Coordinates;

package body Logger is
    procedure Log (What : in Log_Entry) is
    begin
        Put_Line (
            Boards.Player_ID'Image (What.Team) & " " &
            Boards.Unit_Type'Image (What.Unit));
        Put_Line ("Pre-Registers:" &
            " 0|" & Register_Type'Image (What.Pre.Registers (0)) &
            " 1|" & Register_Type'Image (What.Pre.Registers (1)) &
            " 2|" & Register_Type'Image (What.Pre.Registers (2)) &
            " 3|" & Register_Type'Image (What.Pre.Registers (3)) &
            " 4|" & Register_Type'Image (What.Pre.Registers (4)) &
            " 5|" & Register_Type'Image (What.Pre.Registers (5)) &
            " 6|" & Register_Type'Image (What.Pre.Registers (6)) &
            " 7|" & Register_Type'Image (What.Pre.Registers (7)) &
            " 8|" & Register_Type'Image (What.Pre.Registers (8)) &
            " 9|" & Register_Type'Image (What.Pre.Registers (9)) &
            " 10|" & Register_Type'Image (What.Pre.Registers (10)) &
            " 11|" & Register_Type'Image (What.Pre.Registers (11)) &
            " 12|" & Register_Type'Image (What.Pre.Registers (12)) &
            " 13|" & Register_Type'Image (What.Pre.Registers (13)) &
            " 14|" & Register_Type'Image (What.Pre.Registers (14)) &
            " 15|" & Register_Type'Image (What.Pre.Registers (15)) &
            " 16|" & Register_Type'Image (What.Pre.Registers (16)) &
            " 17|" & Register_Type'Image (What.Pre.Registers (17)) &
            " 18|" & Register_Type'Image (What.Pre.Registers (18)) &
            " 19|" & Register_Type'Image (What.Pre.Registers (19)) &
            " 20|" & Register_Type'Image (What.Pre.Registers (20)) &
            " 21|" & Register_Type'Image (What.Pre.Registers (21)) &
            " 22|" & Register_Type'Image (What.Pre.Registers (22)) &
            " 23|" & Register_Type'Image (What.Pre.Registers (23)) &
            " 24|" & Register_Type'Image (What.Pre.Registers (24)) &
            " 25|" & Register_Type'Image (What.Pre.Registers (25)) &
            " 26|" & Register_Type'Image (What.Pre.Registers (26)) &
            " 27|" & Register_Type'Image (What.Pre.Registers (27)) &
            " 28|" & Register_Type'Image (What.Pre.Registers (28)) &
            " 29|" & Register_Type'Image (What.Pre.Registers (29)) &
            " 30|" & Register_Type'Image (What.Pre.Registers (30)) &
            " 31|" & Register_Type'Image (What.Pre.Registers (31)));
        Put_Line ("Pre-State: " &
            Cache_Size'Image (What.Pre.State.Cache_Space) & " " &
            Cache_Type'Image (What.Pre.State.Cache_Kind) & " " &
            Branch_Type'Image (What.Pre.State.Branch_Predictor) & " " &
            CPU_Speed'Image (What.Pre.State.Speed) &
            " X|" & X_Coordinate'Image (What.Pre.State.Position (What.Team).X) &
            " Y|" & Y_Coordinate'Image (What.Pre.State.Position (What.Team).Y) &
            " Hidden|" & Boolean'Image (What.Pre.State.Hidden) &
            " Summoned|" & Boolean'Image (What.Pre.State.Summoned) &
            " Retreating|" & Boolean'Image (What.Pre.State.Retreating) &
            " Setup/Camo|" & Boolean'Image (What.Pre.State.Setup) &
            " Moving|" & Boolean'Image (What.Pre.State.Moving) &
            " Shooting|" & Boolean'Image (What.Pre.State.Shooting) &
            " Prone|" & Boolean'Image (What.Pre.State.Prone));
        Put_Line (
            "Operation: " & Instruction_ID'Image (What.Operation) &
            " A: " & Register_Index'Image (What.A) &
            " B: " & Register_Index'Image (What.B) &
            " C: " & Register_Index'Image (What.C) &
            " Small: " & Small_Immediate_Type'Image (What.Small) &
            " Immediate: " & Address_Type'Image (What.Immediate));
        Put_Line ("Post-Registers:" &
            " 0|" & Register_Type'Image (What.Post.Registers (0)) &
            " 1|" & Register_Type'Image (What.Post.Registers (1)) &
            " 2|" & Register_Type'Image (What.Post.Registers (2)) &
            " 3|" & Register_Type'Image (What.Post.Registers (3)) &
            " 4|" & Register_Type'Image (What.Post.Registers (4)) &
            " 5|" & Register_Type'Image (What.Post.Registers (5)) &
            " 6|" & Register_Type'Image (What.Post.Registers (6)) &
            " 7|" & Register_Type'Image (What.Post.Registers (7)) &
            " 8|" & Register_Type'Image (What.Post.Registers (8)) &
            " 9|" & Register_Type'Image (What.Post.Registers (9)) &
            " 10|" & Register_Type'Image (What.Post.Registers (10)) &
            " 11|" & Register_Type'Image (What.Post.Registers (11)) &
            " 12|" & Register_Type'Image (What.Post.Registers (12)) &
            " 13|" & Register_Type'Image (What.Post.Registers (13)) &
            " 14|" & Register_Type'Image (What.Post.Registers (14)) &
            " 15|" & Register_Type'Image (What.Post.Registers (15)) &
            " 16|" & Register_Type'Image (What.Post.Registers (16)) &
            " 17|" & Register_Type'Image (What.Post.Registers (17)) &
            " 18|" & Register_Type'Image (What.Post.Registers (18)) &
            " 19|" & Register_Type'Image (What.Post.Registers (19)) &
            " 20|" & Register_Type'Image (What.Post.Registers (20)) &
            " 21|" & Register_Type'Image (What.Post.Registers (21)) &
            " 22|" & Register_Type'Image (What.Post.Registers (22)) &
            " 23|" & Register_Type'Image (What.Post.Registers (23)) &
            " 24|" & Register_Type'Image (What.Post.Registers (24)) &
            " 25|" & Register_Type'Image (What.Post.Registers (25)) &
            " 26|" & Register_Type'Image (What.Post.Registers (26)) &
            " 27|" & Register_Type'Image (What.Post.Registers (27)) &
            " 28|" & Register_Type'Image (What.Post.Registers (28)) &
            " 29|" & Register_Type'Image (What.Post.Registers (29)) &
            " 30|" & Register_Type'Image (What.Post.Registers (30)) &
            " 31|" & Register_Type'Image (What.Post.Registers (31)));
        Put_Line ("Post-State: " &
            Cache_Size'Image (What.Post.State.Cache_Space) & " " &
            Cache_Type'Image (What.Post.State.Cache_Kind) & " " &
            Branch_Type'Image (What.Post.State.Branch_Predictor) & " " &
            CPU_Speed'Image (What.Post.State.Speed) &
            " X|" & X_Coordinate'Image (What.Post.State.Position (What.Team).X)
            & " Y|" & Y_Coordinate'Image (
                What.Post.State.Position (What.Team).Y) &
            " Hidden|" & Boolean'Image (What.Post.State.Hidden) &
            " Summoned|" & Boolean'Image (What.Post.State.Summoned) &
            " Retreating|" & Boolean'Image (What.Post.State.Retreating) &
            " Setup/Camo|" & Boolean'Image (What.Post.State.Setup) &
            " Moving|" & Boolean'Image (What.Post.State.Moving) &
            " Shooting|" & Boolean'Image (What.Post.State.Shooting) &
            " Prone|" & Boolean'Image (What.Post.State.Prone));
    end Log;

    procedure Log_Prep (
        Unit : in Boards.Unit_Type;
        Team : in Boards.Player_ID;
        What : in Processors.Instruction_ID) is
    begin
        Put_Line (
            Boards.Player_ID'Image (Team) & " " &
            Boards.Unit_Type'Image (Unit) & ": Working on " &
            Instruction_ID'Image (What) & ".");
    end Log_Prep;

    procedure Log_CWait (
        Unit : in Boards.Unit_Type;
        Team : in Boards.Player_ID;
        Wait : in Natural) is
    begin
        Put_Line (
            Boards.Player_ID'Image (Team) & " " &
            Boards.Unit_Type'Image (Unit) & " CPU ready in " &
            Natural'Image (Wait) & "UT.");
    end Log_CWait;

    procedure Log_IWait (
        Unit : in Boards.Unit_Type;
        Team : in Boards.Player_ID;
        Wait : in Natural) is
    begin
        Put_Line (
            Boards.Player_ID'Image (Team) & " " &
            Boards.Unit_Type'Image (Unit) & " Operation ready in " &
            Natural'Image (Wait) & "RT.");
    end Log_IWait;

    procedure Log_UT (
        Tick : in Natural;
        White : in Boards.Resource_Points;
        Black : in Boards.Resource_Points) is
    begin
        Put_Line ("Universal Tick " & Natural'Image (Tick) &
            " W|" & Resource_Points'Image (White) &
            " B|" & Resource_Points'Image (Black));
    end Log_UT;
end Logger;
