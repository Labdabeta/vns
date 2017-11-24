with Boards; use Boards;
with Coordinates; use Coordinates;

with Processors.Registers; use Processors.Registers;
with Processors.Asks; use Processors.Asks;
with Processors.Floats; use Processors.Floats;

package body Processors.Snipers is
    -- Instruction names
    SNIPER_CAM : constant Instruction_ID := 96;
    SNIPER_UNC : constant Instruction_ID := 97;
    SNIPER_HIP : constant Instruction_ID := 98;
    SNIPER_NMR : constant Instruction_ID := 99;
    SNIPER_IST : constant Instruction_ID := 100;
    SNIPER_TGT : constant Instruction_ID := 101;
    SNIPER_ITF : constant Instruction_ID := 102;
    SNIPER_FAD : constant Instruction_ID := 103;
    SNIPER_FSU : constant Instruction_ID := 104;
    SNIPER_FMU : constant Instruction_ID := 105;
    SNIPER_FDV : constant Instruction_ID := 106;
    SNIPER_CEL : constant Instruction_ID := 107;
    SNIPER_FLR : constant Instruction_ID := 108;
    SNIPER_SIN : constant Instruction_ID := 109;
    SNIPER_COS : constant Instruction_ID := 110;
    SNIPER_TAN : constant Instruction_ID := 111;
    SNIPER_POW : constant Instruction_ID := 112;
    SNIPER_ASN : constant Instruction_ID := 113;
    SNIPER_ACS : constant Instruction_ID := 114;
    SNIPER_ATN : constant Instruction_ID := 115;
    SNIPER_LOG : constant Instruction_ID := 116;
    SNIPER_FCP : constant Instruction_ID := 117;
    SNIPER_LIE : constant Instruction_ID := 118;
    SNIPER_GUP : constant Instruction_ID := 119;
    SNIPER_CSS : constant Instruction_ID := 120;
    SNIPER_CFS : constant Instruction_ID := 121;
    SNIPER_WSS : constant Instruction_ID := 122;
    SNIPER_WFS : constant Instruction_ID := 123;
    SNIPER_BOM : constant Instruction_ID := 124;
    SNIPER_AIR : constant Instruction_ID := 125;
    SNIPER_MOR : constant Instruction_ID := 126;
    SNIPER_SUP : constant Instruction_ID := 127;

    procedure Set_Sniper_Registers (
        Machines : in out Processor_Array;
        State : in Boards.Board) is
        procedure Set_Team_Sniper_Registers (Team : in Player_ID) is
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
        end Set_Team_Sniper_Registers;
    begin
        for T in Player_ID'Range loop
            Set_Team_Sniper_Registers (T);
        end loop;
    end Set_Sniper_Registers;

    function Sniper_Time (
        Op : in Instruction_ID;
        B : in Register_Type;
        C : in Register_Type;
        Position : in Location;
        Team : in Boards.Player_ID;
        State : in out Boards.Board)
        return Natural is
    begin
        case Op is
            when SNIPER_UNC | SNIPER_NMR =>
                return 1;
            when SNIPER_IST | SNIPER_ITF | SNIPER_FAD | SNIPER_FSU |
                SNIPER_CEL | SNIPER_FLR | SNIPER_FCP | SNIPER_CSS |
                SNIPER_CFS | SNIPER_WSS | SNIPER_WFS | SNIPER_BOM |
                SNIPER_AIR | SNIPER_MOR | SNIPER_SUP =>
                return 8;
            when SNIPER_TGT =>
                return 16;
            when SNIPER_FMU =>
                return 32;
            when SNIPER_CAM | SNIPER_FDV | SNIPER_SIN | SNIPER_COS |
                SNIPER_TAN | SNIPER_POW | SNIPER_ASN | SNIPER_ACS |
                SNIPER_ATN | SNIPER_LOG | SNIPER_LIE | SNIPER_GUP =>
                return 64;
            when SNIPER_HIP =>
                Prepare_Shoot (State, Team, UT_SNIPER,
                    (X_Coordinate (B), Y_Coordinate (C)));
                return Compute_Fire_Time (State, UT_RIFLEMAN_SS, Position,
                    To_Location ((X_Coordinate (B), Y_Coordinate (C)), Team));
            when others =>
                return 0;
        end case;
    end Sniper_Time;

    procedure Sniper_Instruction (
        Op : in Instruction_ID;
        Team : in Boards.Player_ID;
        Immediate : in Address_Type;
        State : in out Boards.Board;
        A : in out Register_Type;
        B : in out Register_Type;
        C : in out Register_Type;
        Machines : in out Processor_Array) is
    begin
        case Op is
            when SNIPER_CAM => Set_Setup (State, Team, UT_SNIPER, True);
            when SNIPER_UNC => Set_Setup (State, Team, UT_SNIPER, False);
            when SNIPER_HIP => A := From_Boolean (
                    Do_Shoot (State, Team, UT_SNIPER,
                        To_Location (To_Coordinate (B, C), Team)));
            when SNIPER_NMR =>
                declare
                    My_Valid, You_Valid : Boolean;
                    My_Where, You_Where : Location;
                    My_Time, You_Time : Natural;
                    My_ICount, You_ICount : Natural;
                    My_CCount, You_CCount : Natural;
                begin
                    My_Where := Target_Of (State, Team, UT_MORTAR, My_Valid);
                    if My_Valid then
                        My_ICount := Machines (Team, UT_MORTAR).ICounter;
                        My_CCount := Machines (Team, UT_MORTAR).CCounter;
                        case Get_Unit (State, UT_MORTAR, Team).Speed is
                            when CPUS_EIGHT_FRAMES =>
                                My_Time := My_ICount * 8 - (8 - My_CCount);
                            when CPUS_SIX_FRAMES =>
                                My_Time := My_ICount * 6 - (6 - My_CCount);
                            when CPUS_FOUR_FRAMES =>
                                My_Time := My_ICount * 4 - (4 - My_CCount);
                            when CPUS_TWO_FRAMES =>
                                My_Time := My_ICount * 2 - (2 - My_CCount);
                            when CPUS_EVERY_FRAME =>
                                My_Time := My_ICount;
                        end case;
                    end if;
                    You_Where := Target_Of (State, Team, UT_MORTAR, You_Valid);
                    if You_Valid then
                        You_ICount := Machines (Team, UT_MORTAR).ICounter;
                        You_CCount := Machines (Team, UT_MORTAR).CCounter;
                        case Get_Unit (State, UT_MORTAR, Team).Speed is
                            when CPUS_EIGHT_FRAMES =>
                                You_Time := You_ICount * 8 - (8 - You_CCount);
                            when CPUS_SIX_FRAMES =>
                                You_Time := You_ICount * 6 - (6 - You_CCount);
                            when CPUS_FOUR_FRAMES =>
                                You_Time := You_ICount * 4 - (4 - You_CCount);
                            when CPUS_TWO_FRAMES =>
                                You_Time := You_ICount * 2 - (2 - You_CCount);
                            when CPUS_EVERY_FRAME =>
                                You_Time := You_ICount;
                        end case;
                    end if;

                    if My_Valid then
                        if You_Valid then
                            if My_Time < You_Time then
                                A := Register_Type (My_Time);
                                B := Register_Type (My_Where (Team).X);
                                C := Register_Type (My_Where (Team).Y);
                            else
                                A := Register_Type (You_Time);
                                B := Register_Type (You_Where (Team).X);
                                C := Register_Type (You_Where (Team).Y);
                            end if;
                        else
                            A := Register_Type (My_Time);
                            B := Register_Type (My_Where (Team).X);
                            C := Register_Type (My_Where (Team).Y);
                        end if;
                    else
                        if You_Valid then
                            A := Register_Type (You_Time);
                            B := Register_Type (You_Where (Team).X);
                            C := Register_Type (You_Where (Team).Y);
                        else
                            A := -1;
                            B := -1;
                            C := -1;
                        end if;
                    end if;
                end;
            when SNIPER_IST =>
                declare
                    BC : Location := To_Location (To_Coordinate (B, C), Team);
                begin
                    if Is_Targeted (State, BC) then
                        if Targeting_Team (State, BC) = Team then
                            A := From_Unit (Targeting_Unit (State, BC));
                        else
                            A := -From_Unit (Targeting_Unit (State, BC));
                        end if;
                    end if;
                end;
            when SNIPER_TGT =>
                declare
                    Pos : Location;
                    Valid : Boolean;
                begin
                    if A < 0 then
                        Pos := Target_Of (
                            State, Enemy_Of (Team), To_Unit (-A), Valid);
                    else
                        Pos := Target_Of (State, Team, To_Unit (A), Valid);
                    end if;

                    if Valid then
                        B := Register_Type (Pos (Team).X);
                        C := Register_Type (Pos (Team).Y);
                    else
                        B := -1;
                        C := -1;
                    end if;
                end;
            when SNIPER_LIE => Set_Prone (State, Team, UT_SNIPER, True);
            when SNIPER_GUP => Set_Prone (State, Team, UT_SNIPER, False);
            when SNIPER_ITF | SNIPER_FAD | SNIPER_FSU | SNIPER_FMU |
                SNIPER_FDV | SNIPER_CEL | SNIPER_FLR | SNIPER_SIN |
                SNIPER_COS | SNIPER_TAN | SNIPER_POW | SNIPER_ASN |
                SNIPER_ACS | SNIPER_ATN | SNIPER_LOG | SNIPER_FCP =>
                Float_Instruction (Op, B, C, Immediate, A);
            when SNIPER_CSS | SNIPER_CFS | SNIPER_WSS | SNIPER_WFS |
                SNIPER_BOM | SNIPER_AIR | SNIPER_MOR | SNIPER_SUP =>
                Ask_Instruction (Op, Team, B, C, Immediate, State, A, Machines);
            when others => null;
        end case;
    end Sniper_Instruction;
end Processors.Snipers;
