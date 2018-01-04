with Games; use Games;
with Boards; use Boards;
with Coordinates; use Coordinates;
with Interfaces; use Interfaces;
with Memory;

with Processors.Registers; use Processors.Registers;
with Processors.Instructions; use Processors.Instructions;
with Processors.Caches; use Processors.Caches;
with Processors.Branches; use Processors.Branches;
with Processors.Asks; use Processors.Asks;
with Processors.Floats; use Processors.Floats;
with Processors.Captains; use Processors.Captains;
with Processors.Mortars; use Processors.Mortars;
with Processors.Snipers; use Processors.Snipers;
with Processors.Engineers; use Processors.Engineers;
with Processors.Machinegunners; use Processors.Machinegunners;
with Processors.Scouts; use Processors.Scouts;
with Processors.Riflemen; use Processors.Riflemen;

with Ada.Numerics.Generic_Elementary_Functions;
with Ada.Unchecked_Conversion;
with Ada.Exceptions;
with Ada.Strings;
with Ada.Strings.Fixed;

with Logger;

package body Processors is
    procedure Set_Seed (Seed : in Integer) is begin
        Random_Registers.Reset (Register_Generator, Seed);
    end Set_Seed;

    procedure Initialize_Package is begin
        Random_Registers.Reset (Register_Generator);
    end Initialize_Package;

    procedure Initialize (This : out Unit_Processor) is begin
        This.Memory := (others => 0);
        This.Registers := (others => 0);
        This.Registers (13) := Register_Type (Address_Type'Last);
        This.A := 0;
        This.B := 0;
        This.C := 0;
        This.Op := 0;
        This.Immediate := 0;
        This.ICounter := 0; -- Set to first instruction runtime if clock is 0
        This.CCounter := 0;
        This.Clock := 0;
        This.Advanced := False;
    end Initialize;

    procedure Load_Code (This : in out Unit_Processor; Code : in Memory_Array)
    is begin
        This.Memory (Code'Range) := Code;
    end Load_Code;

    procedure Set_Debug_Mode (Debug : in Boolean) is
    begin
        Debug_Mode := Debug;
    end Set_Debug_Mode;

    procedure Set_Registers (
        Machines : in out Processor_Array;
        State : in Board) is
    begin
        Set_Captain_Registers (Machines, State);
        Set_Mortar_Registers (Machines, State);
        Set_Sniper_Registers (Machines, State);
        Set_Engineer_Registers (Machines, State);
        Set_Machinegunner_Registers (Machines, State);
        Set_Scout_Registers (Machines, State);
        Set_Rifleman_Registers (Machines, State);

        -- Zero-off all the r0s
        for T in Player_ID'Range loop
            Machines (T, UT_CAPTAIN).Registers (0) := 0;
            Machines (T, UT_MORTAR).Registers (0) := 0;
            Machines (T, UT_SNIPER).Registers (0) := 0;
            for S in Board_Side'Range loop
                Machines (T, Engineer_IDs (S)).Registers (0) := 0;
                Machines (T, Machinegunner_IDs (S)).Registers (0) := 0;
                Machines (T, Scout_IDs (S)).Registers (0) := 0;
                Machines (T, Rifleman_IDs (S)).Registers (0) := 0;
            end loop;
        end loop;
    end Set_Registers;

    procedure Step_Processor (
        Which : in out Game;
        Unit : in Unit_Type;
        Team : in Player_ID) is
        use Interfaces;
        State : Board renames Which.State;
        Me : Unit_Processor renames Which.Machines (Team, Unit);
        Us : Unit_State := Get_Unit (Which.State, Unit, Team);
        Reset_Counter : array (Upgrade_Level) of Natural := (7, 5, 3, 1, 0);

        procedure Execute_Step is
            PC : Register_Type renames Me.Registers (15);
            PCVal : Unsigned_32 := To_U32 (Me.Memory (Address_Type (PC)));

            function Extract_Immediate_Register return Register_Type is
                Result : Register_Type := 0;
            begin
                if (PCVal and 2#1000000000#) /= 0 then
                    Result := Register_Type (Small_Immediate_Type'First);
                end if;

                Result := Result + Register_Type (PCVal and 2#111111111#);
                return Result;
            end Extract_Immediate_Register;

            procedure Fetch_Instruction is
            begin
                PCVal := To_U32 (Me.Memory (Address_Type (PC)));
                Me.RA := Register_Index (Shift_Right (PCVal, 20) and 2#11111#);
                Me.RB := Register_Index (Shift_Right (PCVal, 15) and 2#11111#);
                Me.RC := Register_Index (Shift_Right (PCVal, 10) and 2#11111#);
                Me.Registers (12) := Register_Type (Extract_Immediate_Register);
                Me.A := Me.Registers (Me.RA);
                Me.B := Me.Registers (Me.RB);
                Me.C := Me.Registers (Me.RC);
                Me.Op := Instruction_ID (Shift_Right (PCVal, 25));
                Me.Immediate :=
                    Address_Type (PCVal and 2#11111111111111111111#);
                PC := PC + 1;
                Set_Registers (Which.Machines, Which.State);
                Me.ICounter := Compute_Time (Which, Unit, Team);
            end Fetch_Instruction;
        begin
            if Me.Clock = 0 then
                Fetch_Instruction;
            end if;

            if Us.Summoned then
                Prepare_Move (
                    Which.State,
                    Team, Unit,
                    Get_Direction_Towards (
                        Us.Position (Team),
                        Get_Unit (Which.State, UT_CAPTAIN, Team).Position
                            (Team)));
                Me.ICounter := 1;
                return;
            end if;

            if Us.Retreating then
                Prepare_Move (
                    Which.State,
                    Team, Unit,
                    Get_Direction_Towards (Us.Position (Team), Home_Base));
                Me.ICounter := 1;
                return;
            end if;

            Me.Clock := Me.Clock + 1;

            if Me.ICounter = 0 then
                while Me.ICounter = 0 loop
                    Do_Instruction (Which, Unit, Team);

                    Set_Registers (Which.Machines, Which.State);

                    Fetch_Instruction;
                end loop;
                Me.ICounter := Me.ICounter - 1;
            else
                Me.ICounter := Me.ICounter - 1;
                Logger.Log_IWait (Unit, Team, Me.ICounter);
            end if;
        end Execute_Step;
    begin
        if Us.Alive then
            if Me.CCounter = 0 then
                Execute_Step;
                Me.CCounter := Reset_Counter (
                    Get_Unit (State, Unit, Team).Upgrades (CPU_Speed));
                if Me.Advanced then
                    if Me.CCounter > 0 then
                        Me.CCounter := Me.CCounter - 1;
                    end if;
                    Me.Advanced := False;
                end if;
                if Me.Behind then
                    Me.CCounter := Me.CCounter + 1;
                    Me.Behind := False;
                end if;
            else
                Me.CCounter := Me.CCounter - 1;
                Logger.Log_CWait (Unit, Team, Me.CCounter);
            end if;
        end if;
    end Step_Processor;

    function Get_Representation (
        Which : in Unit_Processor;
        Unit : in Boards.Unit_Type)
        return Processor_Representation is
        Register_Names : constant array (Register_Index) of String (1 .. 2) := (
            "R0", "R1", "R2", "R3", "R4", "R5", "R6", "R7", "R8", "R9", "RA",
            "RB", "IM", "SP", "LR", "PC", "S0", "S1", "S2", "S3", "S4", "S5",
            "S6", "S7", "S8", "S9", "SA", "SB", "SC", "SD", "SE", "SF");

        Result : Processor_Representation := (others => ' ');
    begin
        Ada.Strings.Fixed.Move (
            Memory.To_String (Which.Op, Unit) & " " & -- 4 +
            Register_Type'Image (Which.A) & ":" &     -- 11 +
            Register_Names (Which.RA) & ", " &         -- 4 +
            Register_Type'Image (Which.B) & ":" &     -- 11 +
            Register_Names (Which.RB) & ", " &         -- 4 +
            Register_Type'Image (Which.C) & ":" &     -- 11 +
            Register_Names (Which.RC) & " ",          -- 3 = 48
            Result (1 .. 49), Ada.Strings.Right, Ada.Strings.Left);
        Ada.Strings.Fixed.Move (
            Natural'Image (Which.ICounter),
            Result (50 .. 52), Ada.Strings.Left, Ada.Strings.Right);
        Result (53) := Character'Val (Character'Pos ('0') + Which.CCounter);
        return Result;
    end Get_Representation;
end Processors;
