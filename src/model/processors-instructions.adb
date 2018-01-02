with Games; use Games;
with Boards; use Boards;
with Coordinates; use Coordinates;
with Interfaces; use Interfaces;
with Logger;

with Processors.Registers; use Processors.Registers;
with Processors.Branches; use Processors.Branches;
with Processors.Caches; use Processors.Caches;
with Processors.Captains; use Processors.Captains;
with Processors.Mortars; use Processors.Mortars;
with Processors.Snipers; use Processors.Snipers;
with Processors.Engineers; use Processors.Engineers;
with Processors.Machinegunners; use Processors.Machinegunners;
with Processors.Scouts; use Processors.Scouts;
with Processors.Riflemen; use Processors.Riflemen;

with Ada.Exceptions;

package body Processors.Instructions is
    -- Instruction Names
    COMMON_ADD : constant Instruction_ID := 0;
    COMMON_SUB : constant Instruction_ID := 1;
    COMMON_MUL : constant Instruction_ID := 2;
    COMMON_DIV : constant Instruction_ID := 3;
    COMMON_AND : constant Instruction_ID := 4;
    COMMON_ORR : constant Instruction_ID := 5;
    COMMON_XOR : constant Instruction_ID := 6;
    COMMON_NAN : constant Instruction_ID := 7;
    COMMON_CLZ : constant Instruction_ID := 8;
    COMMON_CNT : constant Instruction_ID := 9;
    COMMON_LSR : constant Instruction_ID := 10;
    COMMON_LSL : constant Instruction_ID := 11;
    COMMON_ABS : constant Instruction_ID := 12;
    COMMON_RND : constant Instruction_ID := 13;
    COMMON_CMP : constant Instruction_ID := 14;
    COMMON_JIZ : constant Instruction_ID := 15;
    COMMON_JNZ : constant Instruction_ID := 16;
    COMMON_JGZ : constant Instruction_ID := 17;
    COMMON_JLZ : constant Instruction_ID := 18;
    COMMON_JGE : constant Instruction_ID := 19;
    COMMON_JLE : constant Instruction_ID := 20;
    COMMON_BIZ : constant Instruction_ID := 21;
    COMMON_BNZ : constant Instruction_ID := 22;
    COMMON_BGZ : constant Instruction_ID := 23;
    COMMON_BLZ : constant Instruction_ID := 24;
    COMMON_BGE : constant Instruction_ID := 25;
    COMMON_BLE : constant Instruction_ID := 26;
    COMMON_BLX : constant Instruction_ID := 27;
    COMMON_LDR : constant Instruction_ID := 28;
    COMMON_STR : constant Instruction_ID := 29;
    COMMON_POP : constant Instruction_ID := 30;
    COMMON_PSH : constant Instruction_ID := 31;
    COMMON_WHO : constant Instruction_ID := 32;
    COMMON_WHT : constant Instruction_ID := 33;
    COMMON_QCS : constant Instruction_ID := 34;
    COMMON_QCT : constant Instruction_ID := 35;
    COMMON_QBP : constant Instruction_ID := 36;
    COMMON_QCK : constant Instruction_ID := 37;
    COMMON_GND : constant Instruction_ID := 38;
    COMMON_WHR : constant Instruction_ID := 39;
    COMMON_DST : constant Instruction_ID := 40;
    COMMON_CVR : constant Instruction_ID := 41;
    COMMON_DED : constant Instruction_ID := 42;
    COMMON_SHT : constant Instruction_ID := 43;
    COMMON_DIR : constant Instruction_ID := 44;
    COMMON_WLK : constant Instruction_ID := 45;
    COMMON_CRL : constant Instruction_ID := 46;
    COMMON_SWM : constant Instruction_ID := 47;
    COMMON_CAP : constant Instruction_ID := 48;
    COMMON_RTT : constant Instruction_ID := 49;
    COMMON_HID : constant Instruction_ID := 50;
    COMMON_SAY : constant Instruction_ID := 51;
    COMMON_RAD : constant Instruction_ID := 52;
    COMMON_YEL : constant Instruction_ID := 53;
    COMMON_EAR : constant Instruction_ID := 54;
    COMMON_DIE : constant Instruction_ID := 55;
    COMMON_NRT : constant Instruction_ID := 56;
    COMMON_NRE : constant Instruction_ID := 57;
    COMMON_EST : constant Instruction_ID := 58;
    COMMON_SOE : constant Instruction_ID := 59;
    COMMON_SOT : constant Instruction_ID := 60;
    COMMON_SOW : constant Instruction_ID := 61;
    COMMON_WST : constant Instruction_ID := 62;
    COMMON_NRW : constant Instruction_ID := 63;
    COMMON_WCS : constant Instruction_ID := 64;
    COMMON_WCT : constant Instruction_ID := 65;
    COMMON_WBP : constant Instruction_ID := 66;
    COMMON_WCL : constant Instruction_ID := 67;
    COMMON_TCS : constant Instruction_ID := 68;
    COMMON_TCT : constant Instruction_ID := 69;
    COMMON_TBP : constant Instruction_ID := 70;
    COMMON_TCL : constant Instruction_ID := 71;
    COMMON_PNT : constant Instruction_ID := 72;
    COMMON_CCS : constant Instruction_ID := 73;
    COMMON_CCT : constant Instruction_ID := 74;
    COMMON_CBP : constant Instruction_ID := 75;
    COMMON_CCL : constant Instruction_ID := 76;
    COMMON_UCS : constant Instruction_ID := 77;
    COMMON_UCT : constant Instruction_ID := 78;
    COMMON_UBP : constant Instruction_ID := 79;
    COMMON_UCL : constant Instruction_ID := 80;
    COMMON_DCS : constant Instruction_ID := 81;
    COMMON_DCT : constant Instruction_ID := 82;
    COMMON_DBP : constant Instruction_ID := 83;
    COMMON_DCL : constant Instruction_ID := 84;
    COMMON_MCS : constant Instruction_ID := 85;
    COMMON_MCT : constant Instruction_ID := 86;
    COMMON_MBP : constant Instruction_ID := 87;
    COMMON_MCL : constant Instruction_ID := 88;
    COMMON_RCS : constant Instruction_ID := 89;
    COMMON_RCT : constant Instruction_ID := 90;
    COMMON_RBP : constant Instruction_ID := 91;
    COMMON_RCL : constant Instruction_ID := 92;
    COMMON_TIM : constant Instruction_ID := 93;
    COMMON_DLY : constant Instruction_ID := 94;
    COMMON_ADV : constant Instruction_ID := 95;

    function Is_Taken (
        Which : in Instruction_ID;
        Value : in Register_Type) return Boolean is
    begin
        case Which is
            when COMMON_JIZ | COMMON_BIZ => return Value = 0;
            when COMMON_JNZ | COMMON_BNZ => return Value /= 0;
            when COMMON_JGZ | COMMON_BGZ => return Value > 0;
            when COMMON_JLZ | COMMON_BLZ => return Value < 0;
            when COMMON_JGE | COMMON_BGE => return Value >= 0;
            when COMMON_JLE | COMMON_BLE => return Value <= 0;
            when others => return False;
        end case;
    end Is_Taken;

    function Compute_Time (
        Which : in out Game;
        Unit : in Unit_Type;
        Team : in Player_ID) return Natural is
        Us : Unit_State := Get_Unit (Which.State, Unit, Team);
        Me : Unit_Processor renames Which.Machines (Team, Unit);
        PC : Register_Type renames Me.Registers (15);
    begin
        Logger.Log_Prep (Unit, Team, Me.Op);

        case Me.Op is
            when COMMON_RTT | COMMON_DIE =>
                return 0;
            when COMMON_ADD | COMMON_SUB | COMMON_AND | COMMON_ORR |
                COMMON_XOR | COMMON_NAN | COMMON_CLZ | COMMON_CNT |
                COMMON_LSR | COMMON_LSL | COMMON_ABS | COMMON_RND |
                COMMON_CMP | COMMON_BLX | COMMON_WHO | COMMON_WHT |
                COMMON_GND | COMMON_WHR | COMMON_DED | COMMON_DIR |
                COMMON_WCS | COMMON_WCT | COMMON_WBP | COMMON_WCL |
                COMMON_TCS | COMMON_TCT | COMMON_TBP | COMMON_TCL |
                COMMON_PNT | COMMON_CCS | COMMON_CCT | COMMON_CBP |
                COMMON_CCL | COMMON_UCS | COMMON_UCT | COMMON_UBP |
                COMMON_UCL | COMMON_DCS | COMMON_DCT | COMMON_DBP |
                COMMON_DCL | COMMON_MCS | COMMON_MCT | COMMON_MBP |
                COMMON_MCL | COMMON_RCS | COMMON_RCT | COMMON_RBP |
                COMMON_RCL | COMMON_TIM | COMMON_DLY | COMMON_QCS |
                COMMON_QCT | COMMON_QBP | COMMON_QCK =>
                return 1;
            when COMMON_MUL | COMMON_DST | COMMON_NRT | COMMON_NRE |
                COMMON_EST | COMMON_SOE | COMMON_SOT | COMMON_SOW |
                COMMON_WST | COMMON_NRW | COMMON_ADV =>
                return 4;
            when COMMON_DIV | COMMON_CVR | COMMON_SAY | COMMON_RAD =>
                return 8;
            when COMMON_YEL | COMMON_EAR =>
                return 16;
            when COMMON_JIZ | COMMON_JNZ | COMMON_JGZ | COMMON_JLZ |
                COMMON_JGE | COMMON_JLE =>
                if Us.Upgrades (Branch_Type) = BT_PERFECT then
                    return 1;
                end if;

                if Is_Taken (Me.Op, Me.A) then
                    return 1;
                else
                    return 8;
                end if;
            when COMMON_BIZ | COMMON_BNZ | COMMON_BGZ | COMMON_BLZ |
                COMMON_BGE | COMMON_BLE =>
                case Us.Upgrades (Branch_Type) is
                    when BT_PERFECT =>
                        return 1;
                    when BT_NONE =>
                        return 8;
                    when others =>
                        if Is_Taken (Me.Op, Me.A) =
                            Predict_Branch (Us.Upgrades (Branch_Type),
                                Me.Predictor (Address_Type (PC)))
                        then
                            return 1;
                        else
                            return 8;
                        end if;
                end case;
            when COMMON_LDR | COMMON_STR =>
                if Check_Cache (Me.Cache, Me.Immediate) then
                    return 1;
                else
                    return 8;
                end if;
            when COMMON_POP | COMMON_PSH =>
                if Check_Cache (
                    Me.Cache, Address_Type (Me.Registers (13)) - Me.Immediate)
                then
                    return 1;
                else
                    return 8;
                end if;
            when COMMON_SHT =>
                Prepare_Shoot (Which.State, Team, Unit,
                    To_Coordinate (Me.B, Me.C));
                return Compute_Fire_Time (Which.State, Unit, Us.Position,
                    To_Location (To_Coordinate (Me.B, Me.C), Team));
            when COMMON_WLK =>
                if Us.Setup and Unit /= UT_SNIPER then
                    return 0;
                end if;
                Prepare_Move (Which.State, Team, Unit, To_Direction (Me.A));
                if Us.Setup then -- Sniper CAMO
                    if Us.Prone then
                        return 32;
                    end if;
                    return 16;
                end if;
                if Us.Prone then
                    return 16;
                end if;
                return 8;
            when COMMON_CRL | COMMON_SWM =>
                Prepare_Move (Which.State, Team, Unit, To_Direction (Me.A));
                return 64;
            when COMMON_CAP =>
                if Is_On_Target (Which.State, Team, Unit) then
                    return 256;
                else
                    return 0;
                end if;
            when COMMON_HID =>
                Set_Unit_Visibility (Which.State, Team, Unit, False);
                return Natural (Me.A) + Natural (Me.Immediate);
            when others =>
                case Unit is
                    when UT_CAPTAIN =>
                        return Captain_Time (Me.Op, Me.A, Team, Which.State);
                    when UT_MORTAR =>
                        return Mortar_Time (Me.Op);
                    when UT_SNIPER =>
                        return Sniper_Time (
                            Me.Op, Me.B, Me.C, Us.Position, Team, Which.State);
                    when UT_ENGINEER_SS | UT_ENGINEER_FS =>
                        return Engineer_Time (Me.Op);
                    when UT_MACHINEGUNNER_SS | UT_MACHINEGUNNER_FS =>
                        return Machinegunner_Time (Me.Op);
                    when UT_SCOUT_SS | UT_SCOUT_FS =>
                        return Scout_Time (
                            Me.Op, Which.State, Team, Unit, Me.A);
                    when UT_RIFLEMAN_SS | UT_RIFLEMAN_FS =>
                        return Rifleman_Time (Me.Op);
                end case;
        end case;
    exception
        when E : others =>
            Logger.Log_Error (Unit, Team,
                Ada.Exceptions.Exception_Information (E));
            return Natural'Last;
    end Compute_Time;

    procedure Do_Instruction (
        Which : in out Games.Game;
        Unit : in Boards.Unit_Type;
        Team : in Boards.Player_ID) is
        State : Board renames Which.State;
        Machines : Processor_Array renames Which.Machines;
        Radios : Communications renames Which.Radios;
        Shared : Shared_Memory renames Which.Shared;
        Clock : Natural renames Which.Clock;
        Us : Unit_State := Get_Unit (State, Unit, Team);
        Me : Unit_Processor renames Which.Machines (Team, Unit);
        PC : Register_Type renames Me.Registers (15);
        A : Register_Type renames Me.Registers (Me.RA);
        B : Register_Type renames Me.Registers (Me.RB);
        C : Register_Type renames Me.Registers (Me.RC);
        Position : Coordinate renames Us.Position (Team);
        New_Log : Logger.Log_Entry;
        Enemy : Player_ID := Enemy_Of (Team);

        function BC return Location is begin
            return To_Location (To_Coordinate (Me.B, Me.C), Team);
        end BC;

    begin
        if Us.Summoned then
            Do_Move (State, Team, Unit,
                Get_Direction_Towards (
                    Position,
                    Get_Unit (State, UT_CAPTAIN, Team).Position (Team)));
            if Adjacent (
                Position,
                Get_Unit (State, UT_CAPTAIN, Team).Position (Team))
            then
                Set_Unit_Summon (State, Team, Unit, False);
            end if;
            return;
        end if;

        if Us.Retreating then
            Do_Move (State, Team, Unit,
                Get_Direction_Towards (Position, Home_Base));
            if Adjacent (Us.Position (Team), Home_Base) then
                Set_Unit_Retreat (State, Team, Unit, False);
            end if;
            return;
        end if;

        New_Log.Pre.Registers := Me.Registers;
        New_Log.Pre.State := Get_Unit (State, Unit, Team);
        New_Log.Unit := Unit;
        New_Log.Team := Team;
        New_Log.Operation := Me.Op;
        New_Log.A := Me.RA;
        New_Log.B := Me.RB;
        New_Log.C := Me.RC;
        New_Log.Small := 0;
        New_Log.Immediate := Me.Immediate;

        case Me.Op is
            when COMMON_ADD => A := Me.B + Me.C;
            when COMMON_SUB => A := Me.B - Me.C;
            when COMMON_MUL => A := Me.B * Me.C;
            when COMMON_DIV => A := Me.B / Me.C;
            when COMMON_AND =>
                A := From_U32 (To_U32 (Me.B) and To_U32 (Me.C));
            when COMMON_ORR =>
                A := From_U32 (To_U32 (Me.B) or To_U32 (Me.C));
            when COMMON_XOR =>
                A := From_U32 (To_U32 (Me.B) xor To_U32 (Me.C));
            when COMMON_NAN =>
                A := From_U32 (not (To_U32 (Me.B) and To_U32 (Me.C)));
            when COMMON_CLZ =>
                B := Count_Leading_Zeroes (Me.A);
                C := Count_Leading_Ones (Me.A);
            when COMMON_CNT =>
                B := Count_Zeroes (Me.A);
                C := Count_Ones (Me.A);
            when COMMON_LSR => A := Shift_Right (Me.B, Me.C);
            when COMMON_LSL => A := Shift_Left (Me.B, Me.C);
            when COMMON_ABS => B := abs Me.A; C := -abs Me.A;
            when COMMON_RND =>
                A := Random_Registers.Random (Register_Generator);
                B := Random_Registers.Random (Register_Generator);
                C := Random_Registers.Random (Register_Generator);
            when COMMON_CMP =>
                if Me.B > Me.C then
                    A := 1;
                elsif Me.B < Me.C then
                    A := -1;
                else
                    A := 0;
                end if;
            when COMMON_JIZ | COMMON_BIZ =>
                if Me.A = 0 then
                    PC := Register_Type (Me.Immediate);
                end if;
            when COMMON_JNZ | COMMON_BNZ =>
                if Me.A /= 0 then
                    PC := Register_Type (Me.Immediate);
                end if;
            when COMMON_JGZ | COMMON_BGZ =>
                if Me.A > 0 then
                    PC := Register_Type (Me.Immediate);
                end if;
            when COMMON_JLZ | COMMON_BLZ =>
                if Me.A < 0 then
                    PC := Register_Type (Me.Immediate);
                end if;
            when COMMON_JGE | COMMON_BGE =>
                if Me.A >= 0 then
                    PC := Register_Type (Me.Immediate);
                end if;
            when COMMON_JLE | COMMON_BLE =>
                if Me.A <= 0 then
                    PC := Register_Type (Me.Immediate);
                end if;
            when COMMON_BLX =>
                Me.Registers (14) := Me.Registers (15);
                PC := Me.A + Register_Type (Me.Immediate);
            when COMMON_LDR => A := Me.Memory (Address_Type (Me.B + Me.C));
            when COMMON_STR => Me.Memory (Address_Type (Me.B + Me.C)) := Me.A;
            when COMMON_POP =>
                Me.Registers (13) := Me.Registers (13) + 1;
                A := Me.Memory (
                    Address_Type (Me.Registers (13)) + Me.Immediate);
            when COMMON_PSH =>
                Me.Memory (Address_Type (Me.Registers (13)) + Me.Immediate) :=
                    Me.A;
                Me.Registers (13) := Me.Registers (13) - 1;
            when COMMON_WHO =>
                if Team_Of (State, BC) = Team then
                    A := 1;
                elsif Team_Of (State, BC) = Enemy then
                    A := -1;
                else
                    A := 0;
                end if;
            when COMMON_WHT =>
                if Team_Of (State, BC) = Team then
                    A := From_Unit (Unit_Of (State, BC));
                elsif Team_Of (State, BC) = Enemy then
                    A := -From_Unit (Unit_Of (State, BC));
                else
                    A := 0;
                end if;
            when COMMON_QCS =>
                if Team_Of (State, BC) = T_NONE then
                    A := -1;
                else
                    A := Register_Type (Get_Unit (
                        State, Unit_Of (State, BC),
                        Team_Of (State, BC)).Upgrades (Cache_Size));
                end if;
            when COMMON_QCT =>
                if Team_Of (State, BC) = T_NONE then
                    A := -1;
                else
                    A := Register_Type (Get_Unit (
                        State, Unit_Of (State, BC),
                        Team_Of (State, BC)).Upgrades (Cache_Type));
                end if;
            when COMMON_QBP =>
                if Team_Of (State, BC) = T_NONE then
                    A := -1;
                else
                    A := Register_Type (Get_Unit (
                        State, Unit_Of (State, BC),
                        Team_Of (State, BC)).Upgrades (Branch_Type));
                end if;
            when COMMON_QCK =>
                if Team_Of (State, BC) = T_NONE then
                    A := -1;
                else
                    A := Register_Type (Get_Unit (
                        State, Unit_Of (State, BC),
                        Team_Of (State, BC)).Upgrades (CPU_Speed));
                end if;
            when COMMON_GND =>
                A := Terrain_Type'Pos (Terrain_Of (State, BC)) + 1;
            when COMMON_WHR =>
                if Me.A > 0 then
                    if Get_Unit (State, To_Unit (Me.A), Team).Alive then
                        B := Register_Type (Get_Unit (
                            State, To_Unit (Me.A), Team).Position (Team).X);
                        C := Register_Type (Get_Unit (
                            State, To_Unit (Me.A), Team).Position (Team).Y);
                    else
                        B := -1;
                        C := -1;
                    end if;
                else
                    if Get_Unit (State, To_Unit (-Me.A), Enemy).Alive and
                        not Get_Unit (State, To_Unit (-Me.A), Enemy).Hidden and
                        not (To_Unit (-Me.A) = UT_SNIPER and
                            Get_Unit (State, To_Unit (-Me.A), Enemy).Setup)
                    then
                        B := Register_Type (Get_Unit (
                            State, To_Unit (-Me.A), Enemy).Position (Team).X);
                        C := Register_Type (Get_Unit (
                            State, To_Unit (-Me.A), Enemy).Position (Team).Y);
                    else
                        B := -1;
                        C := -1;
                    end if;
                end if;
            when COMMON_DST =>
                A :=
                    Get_Path_To (Position, To_Coordinate (Me.B, Me.C))'Length;
            when COMMON_CVR =>
                A := Register_Type (Cover_Between (State, Team, Unit, BC));
            when COMMON_DED =>
                if Get_Unit (State, To_Unit (Me.A), Team).Alive then
                    B := 0;
                else
                    B := 1;
                end if;

                if Get_Unit (State, To_Unit (Me.A), Enemy).Alive then
                    C := 0;
                else
                    C := 1;
                end if;
            when COMMON_SHT =>
                A := From_Boolean (Do_Shoot (State, Team, Unit, BC));
            when COMMON_DIR =>
                A := Direction'Pos (Get_Direction_Towards (
                    Position, To_Coordinate (Me.B, Me.C))) + 1;
            when COMMON_WLK =>
                Do_Move (State, Team, Unit, To_Direction (Me.A));
                B := Register_Type (Position.X);
                C := Register_Type (Position.Y);
            when COMMON_CRL =>
                Do_Crawl (State, Team, Unit, To_Direction (Me.A));
                B := Register_Type (Position.X);
                C := Register_Type (Position.Y);
            when COMMON_SWM =>
                Do_Swim (State, Team, Unit, To_Direction (Me.A));
                B := Register_Type (Position.X);
                C := Register_Type (Position.Y);
            when COMMON_CAP =>
                if Is_On_Target (State, Team, Unit) then
                    Win_Game (State, Team);
                end if;
            when COMMON_RTT => Set_Unit_Retreat (State, Team, Unit, True);
            when COMMON_HID => Set_Unit_Visibility (State, Team, Unit, True);
            when COMMON_SAY => Radios (Team, Unit, Me.Immediate) := Me.A;
            when COMMON_RAD => A := Radios (Team, Unit, Me.Immediate);
            when COMMON_YEL => Shared (Team, Me.Immediate) := Me.A;
            when COMMON_EAR => A := Shared (Team, Me.Immediate);
            when COMMON_DIE =>
                if Debug_Mode then
                    Logger.Log (Me.A, Me.B, Me.C);
                else
                    Kill_Unit (State, Team, Unit);
                end if;
            when COMMON_NRT | COMMON_NRE | COMMON_EST | COMMON_SOE |
                COMMON_SOT | COMMON_SOW | COMMON_WST | COMMON_NRW =>
                declare
                    Dest : Coordinate;
                begin
                    if Find_Unit (State, Team, Unit,
                        Direction'Val (Me.Op - COMMON_NRT), Dest)
                    then
                        B := Register_Type (Dest.X);
                        C := Register_Type (Dest.Y);
                        if Team_Of (State, BC) = Team then
                            A := From_Unit (Unit_Of (State, BC));
                        elsif Team_Of (State, BC) = Enemy then
                            A := -From_Unit (Unit_Of (State, BC));
                        else
                            A := 0;
                        end if;
                    end if;
                end;
            when COMMON_WCS => A := Register_Type (Us.Upgrades (Cache_Size));
            when COMMON_WCT => A := Register_Type (Us.Upgrades (Cache_Type));
            when COMMON_WBP => A :=
                Register_Type (Us.Upgrades (Branch_Type));
            when COMMON_WCL => A := Register_Type (Us.Upgrades (CPU_Speed));
            when COMMON_TCS =>
                A := Register_Type (Get_Unit (
                    State, To_Unit (Me.A), Team).Upgrades (Cache_Size));
            when COMMON_TCT =>
                A := Register_Type (Get_Unit (
                    State, To_Unit (Me.A), Team).Upgrades (Cache_Type));
            when COMMON_TBP =>
                A := Register_Type (Get_Unit (
                    State, To_Unit (Me.A), Team).Upgrades (Branch_Type));
            when COMMON_TCL =>
                A := Register_Type (Get_Unit (
                    State, To_Unit (Me.A), Team).Upgrades (CPU_Speed));
            when COMMON_PNT => A := Register_Type (Get_Points (State, Team));
            when COMMON_CCS =>
                if Us.Upgrades (Cache_Size) < Upgrade_Level'Last then
                    A := Register_Type (Upgrade_Cost (Cache_Size,
                        Us.Upgrades (Cache_Size) + 1));
                else
                    A := 0;
                end if;
            when COMMON_CCT =>
                if Us.Upgrades (Cache_Type) < Upgrade_Level'Last then
                    A := Register_Type (Upgrade_Cost (Cache_Type,
                        Us.Upgrades (Cache_Type) + 1));
                else
                    A := 0;
                end if;
            when COMMON_CBP =>
                if Us.Upgrades (Branch_Type) < Upgrade_Level'Last then
                    A := Register_Type (Upgrade_Cost (Branch_Type,
                        Us.Upgrades (Branch_Type) + 1));
                else
                    A := 0;
                end if;
            when COMMON_CCL =>
                if Us.Upgrades (CPU_Speed) < Upgrade_Level'Last then
                    A := Register_Type (Upgrade_Cost (CPU_Speed,
                        Us.Upgrades (CPU_Speed) + 1));
                else
                    A := 0;
                end if;
            when COMMON_UCS =>
                A :=
                    From_Boolean (Try_Upgrade (State, Team, Unit, Cache_Size));
            when COMMON_UCT =>
                A :=
                    From_Boolean (Try_Upgrade (State, Team, Unit, Cache_Type));
            when COMMON_UBP =>
                A :=
                    From_Boolean (Try_Upgrade (State, Team, Unit, Branch_Type));
            when COMMON_UCL =>
                A :=
                    From_Boolean (Try_Upgrade (State, Team, Unit, CPU_Speed));
            when COMMON_DCS =>
                A := From_Boolean (
                    Try_Upgrade (State, Team, Unit, Cache_Size, True));
            when COMMON_DCT =>
                A := From_Boolean (
                    Try_Upgrade (State, Team, Unit, Cache_Type, True));
            when COMMON_DBP =>
                A := From_Boolean (
                    Try_Upgrade (State, Team, Unit, Branch_Type, True));
            when COMMON_DCL =>
                A := From_Boolean (
                    Try_Upgrade (State, Team, Unit, CPU_Speed, True));
            when COMMON_MCS =>
                A := From_Boolean (
                    Try_Max (State, Team, Unit, Cache_Size));
            when COMMON_MCT =>
                A := From_Boolean (
                    Try_Max (State, Team, Unit, Cache_Type));
            when COMMON_MBP =>
                A := From_Boolean (
                    Try_Max (State, Team, Unit, Branch_Type));
            when COMMON_MCL =>
                A := From_Boolean (
                    Try_Max (State, Team, Unit, CPU_Speed));
            when COMMON_RCS =>
                A := From_Boolean (
                    Try_Max (State, Team, Unit, Cache_Size, True));
            when COMMON_RCT =>
                A := From_Boolean (
                    Try_Max (State, Team, Unit, Cache_Type, True));
            when COMMON_RBP =>
                A := From_Boolean (
                    Try_Max (State, Team, Unit, Branch_Type, True));
            when COMMON_RCL =>
                A := From_Boolean (
                    Try_Max (State, Team, Unit, CPU_Speed, True));
            when COMMON_TIM =>
                A := Register_Type (Clock);
                B := Register_Type (Me.Clock);
                C := 0;
                for Index in Unit_Type'Range loop
                    C := Me.C + Register_Type (Machines (Team, Index).Clock);
                end loop;
            when COMMON_DLY =>
                Me.Behind := True;
            when COMMON_ADV =>
                Me.Advanced := True;
            when others =>
                case Unit is
                    when UT_CAPTAIN =>
                        Captain_Instruction (Team, Me, State, Shared, Radios);
                    when UT_MORTAR =>
                        Mortar_Instruction (Team, State, Machines);
                    when UT_SNIPER =>
                        Sniper_Instruction (Team, State, Machines);
                    when UT_ENGINEER_SS | UT_ENGINEER_FS =>
                        Engineer_Instruction (Team, Unit, State, Machines);
                    when UT_MACHINEGUNNER_SS | UT_MACHINEGUNNER_FS =>
                        Machinegunner_Instruction (Team, Unit, State, Machines);
                    when UT_SCOUT_SS | UT_SCOUT_FS =>
                        Scout_Instruction (Team, Unit, State, Machines);
                    when UT_RIFLEMAN_SS | UT_RIFLEMAN_FS =>
                        Rifleman_Instruction (Team, Unit, State, Machines);
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
end Processors.Instructions;
