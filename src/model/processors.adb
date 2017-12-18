with Games; use Games;
with Boards; use Boards;
with Coordinates; use Coordinates;
with Interfaces;

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

with Logger;

package body Processors is
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
        Machines : Processor_Array renames Which.Machines;
        Us : Unit_State := Get_Unit (Which.State, Unit, Team);
        Reset_Counter : array (CPU_Speed) of Natural := (
            CPUS_EIGHT_FRAMES => 7,
            CPUS_SIX_FRAMES => 5,
            CPUS_FOUR_FRAMES => 3,
            CPUS_TWO_FRAMES => 1,
            CPUS_EVERY_FRAME => 0);

        procedure Execute_Step is begin
            if Machines (Team, Unit).Clock = 0 then
                Machines (Team, Unit).ICounter :=
                    Compute_Time (Which, Unit, Team);
            end if;

            Machines (Team, Unit).Clock := Machines (Team, Unit).Clock + 1;

            if Machines (Team, Unit).ICounter = 0 then
                while Machines (Team, Unit).ICounter = 0 loop
                    Do_Instruction (Which, Unit, Team);
                    Machines (Team, Unit).ICounter :=
                        Compute_Time (Which, Unit, Team);
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
