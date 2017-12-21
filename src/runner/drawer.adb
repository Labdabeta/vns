with SDL; use SDL;
with Grid; use Grid;
with Boards; use Boards;
with Games;
with Sprite_Sheet;
with Font_Sheet;
with Clips; use Clips;
with Coordinates; use Coordinates;
with Processors;

package body Drawer is
    Sprite, Font : Image;
    Active_Team : Player_ID;
    Frame_Rate : Natural;

    Unit_Y : constant array (Unit_Type) of Grid_Y := (
        UT_CAPTAIN => 18,
        UT_SNIPER => 19,
        UT_MORTAR => 20,
        UT_ENGINEER_SS => 21,
        UT_ENGINEER_FS => 22,
        UT_MACHINEGUNNER_SS => 23,
        UT_MACHINEGUNNER_FS => 24,
        UT_SCOUT_SS => 25,
        UT_SCOUT_FS => 26,
        UT_RIFLEMAN_SS => 27,
        UT_RIFLEMAN_FS => 28);

    Team_Colour : constant array (Player_ID) of Colour := (
        T_WHITE => (255, 255, 255, 255),
        T_BLACK => (255, 0, 0, 0));

    To_X : constant array (X_Coordinate) of Grid_X := (
        1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16,
        17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32);
    To_Y : constant array (Y_Coordinate) of Grid_Y := (
        1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16);

    function To_Str7 (Val : in Natural) return String is
        Result : String (1 .. 7) := (others => ' ');
        Copy : Natural := Val;
    begin
        for I in reverse Result'Range loop
            Result (I) := Character'Val (Character'Pos ('0') + (Copy mod 10));
            Copy := Copy / 10;
        end loop;

        if Copy > 0 then
            Result (1) := '>';
        end if;

        for I in Result'Range loop
            exit when I = Result'Last;
            exit when Result (I) /= '0';
            Result (I) := ' ';
        end loop;

        return Result;
    end To_Str7;

    procedure Initialize (UTPS : in Natural) is
    begin
        Sprite := Create_Image (Sprite_Sheet.Raw_Data (1)'Access, 448, 384);
        Font := Create_Image (Font_Sheet.Raw_Data (1)'Access, 192, 224);
        Active_Team := T_WHITE;
        Frame_Rate := UTPS;
    end Initialize;

    procedure Finalize is
    begin
        Free_Image (Sprite);
        Free_Image (Font);
    end Finalize;

    procedure Toggle_Team is
    begin
        Active_Team := Enemy_Of (Active_Team);
    end Toggle_Team;

    procedure Set_Frame_Rate (UTPS : in Natural) is
    begin
        Frame_Rate := UTPS;
    end Set_Frame_Rate;

    procedure Draw_Game (Which : in Games.Game_Access) is
        procedure Draw_Unit (
            State : Unit_State;
            Unit : Unit_Type;
            Player : Player_ID) is
            Flip : Boolean := False; -- Unsetup guns are flipped
            Rotate : Angle := 0.0; -- Camouflaged snipers are rotated
            Fade : Colour_Component := 255;
            Where : Rectangle;
        begin
            if not State.Alive then
                return;
            end if;

            -- Draw the player itself
            if (Unit = UT_MACHINEGUNNER_SS or
                Unit = UT_MACHINEGUNNER_FS or
                Unit = UT_MORTAR) and not State.Setup
            then
                Flip := True;
            end if;

            if State.Prone then
                Where := Small_Rect (
                    To_X (State.Position (Active_Team).X),
                    To_Y (State.Position (Active_Team).Y));
            else
                Where := Rect (
                    To_X (State.Position (Active_Team).X),
                    To_Y (State.Position (Active_Team).Y));
            end if;

            if State.Hidden then
                Fade := 128;
            end if;

            if Unit = UT_SNIPER and State.Setup then
                Rotate := 0.785398;
            end if;

            Draw_Image (
                Sprite, Where, Unit_Clip (Unit, Player), Rotate,
                (Where.Width / 2, Where.Height / 2),
                Flip, Blend => (Fade, 255, 255, 255));

            -- Draw actions
            if State.Shooting then
                declare
                    Path : Coordinate_Path := Get_Path_To (
                        State.Position (Active_Team),
                        State.Destination (Active_Team));
                begin
                    for I in Path'Range loop
                        Draw_Image (
                            Sprite, Rect (
                                To_X (Path (I).X), To_Y (Path (I).Y)),
                            Sprite_Clips (ATTACK));
                    end loop;

                    Draw_Line (
                        Using => (255, 255, 0, 0),
                        From => Center (
                            To_X (State.Position (Active_Team).X),
                            To_Y (State.Position (Active_Team).Y)),
                        To => Center (
                            To_X (Path (Path'First).X),
                            To_Y (Path (Path'First).Y)));
                    Draw_Line (
                        Using => (255, 0, 0, 0),
                        From => Center (
                            To_X (Path (Path'First).X),
                            To_Y (Path (Path'First).Y)),
                        To => Center (
                            To_X (Path (Path'Last).X),
                            To_Y (Path (Path'Last).Y)));
                end;
            end if;

            if State.Moving then
                Draw_Image (Sprite, Rect (
                    To_X (State.Position (Active_Team).X),
                    To_Y (State.Position (Active_Team).Y)),
                    Sprite_Clips (MOVE));
                Draw_Image (Sprite, Rect (
                    To_X (State.Destination (Active_Team).X),
                    To_Y (State.Destination (Active_Team).Y)),
                    Sprite_Clips (MOVE));
            end if;

            -- Draw upgrade status
            if Player = Active_Team then
                Draw_Image (
                    Sprite, Rect (2, Unit_Y (Unit)),
                    CS_Clip (State.Cache_Space),
                    Blend => Team_Colour (Active_Team));
                Draw_Image (
                    Sprite, Rect (3, Unit_Y (Unit)),
                    CT_Clip (State.Cache_Kind),
                    Blend => Team_Colour (Active_Team));
                Draw_Image (
                    Sprite, Rect (4, Unit_Y (Unit)),
                    BP_Clip (State.Branch_Predictor),
                    Blend => Team_Colour (Active_Team));
                Draw_Image (
                    Sprite, Rect (5, Unit_Y (Unit)),
                    CPU_Clip (State.Speed),
                    Blend => Team_Colour (Active_Team));
            end if;
        end Draw_Unit;

        procedure Draw_State (Unit : Unit_Type) is
            Repr : Processors.Processor_Representation :=
                Processors.Get_Representation (
                    Which.Machines (Active_Team, Unit), Unit);
        begin
            -- Draw the unit image
            Draw_Image (Sprite, Rect (1, Unit_Y (Unit)),
                Unit_Clip (Unit, Active_Team),
                Blend => Team_Colour (Active_Team));

            -- Draw the command representation
            for I in Positive range 1 .. 25 loop
                Draw_Image (Font, Rect (Grid_X (I + 6), Unit_Y (Unit)),
                    Font_Clip (Repr (I)), Blend => Team_Colour (Active_Team));
            end loop;
            Draw_Image (Font, Small_Rect (Grid_X (32), Unit_Y (Unit)),
                Font_Clip (Repr (26)), Blend => Team_Colour (Active_Team));
        end Draw_State;
    begin
        -- Draw terrain
        for Y in Y_Coordinate'Range loop
            for X in X_Coordinate'Range loop
                Draw_Image (Sprite, Rect (To_X (X), To_Y (Y)), Terrain_Clip (
                    Terrain_Of (Which.State, To_Location ((X, Y), Active_Team))
                    ));
            end loop;
        end loop;

        -- Draw each unit
        for P in Player_ID'Range loop
            for U in Unit_Type'Range loop
                Draw_Unit (Get_Unit (Which.State, U, P), U, P);
            end loop;
        end loop;

        -- Draw CPU state
        for U in Unit_Type'Range loop
            Draw_State (U);
        end loop;

        -- Draw Time/Resources
        declare
            UT_Repr : String (1 .. 7) := To_Str7 (Which.Clock);
            WRP_Repr : String (1 .. 7) := To_Str7 (Natural (
                Get_Points (Which.State, T_WHITE)));
            BRP_Repr : String (1 .. 7) := To_Str7 (Natural (
                Get_Points (Which.State, T_BLACK)));
        begin
            for I in UT_Repr'Range loop
                Draw_Image (Font, Rect (Grid_X (I), 29),
                    Font_Clip (UT_Repr (I)), Blend => (255, 128, 255, 0));
            end loop;

            Draw_Image (Font, Rect (9, 29),
                Font_Clip ('U'), Blend => (255, 128, 255, 0));
            Draw_Image (Font, Rect (10, 29),
                Font_Clip ('T'), Blend => (255, 128, 255, 0));

            for I in WRP_Repr'Range loop
                Draw_Image (Font, Rect (Grid_X (I) + 11, 29),
                    Font_Clip (WRP_Repr (I)), Blend => Team_Colour (T_WHITE));
            end loop;

            Draw_Image (Font, Rect (20, 29),
                Font_Clip ('R'), Blend => Team_Colour (T_WHITE));
            Draw_Image (Font, Rect (21, 29),
                Font_Clip ('P'), Blend => Team_Colour (T_WHITE));

            for I in BRP_Repr'Range loop
                Draw_Image (Font, Rect (Grid_X (I) + 22, 29),
                    Font_Clip (BRP_Repr (I)), Blend => Team_Colour (T_BLACK));
            end loop;

            Draw_Image (Font, Rect (31, 29),
                Font_Clip ('R'), Blend => Team_Colour (T_BLACK));
            Draw_Image (Font, Rect (32, 29),
                Font_Clip ('P'), Blend => Team_Colour (T_BLACK));
        end;

        -- Draw Status Bar
        declare
            Rate_String : String (1 .. 12) := "-- PAUSED --";
        begin
            if Frame_Rate > 0 then
                if Frame_Rate = Natural'Last then
                    Rate_String := "-- INFFPS --";
                else
                    Rate_String (4) := Character'Val (
                        Character'Pos ('0') + ((Frame_Rate / 10) mod 10));
                    Rate_String (5) := Character'Val (
                        Character'Pos ('0') + (Frame_Rate mod 10));
                    Rate_String (6 .. 9) := " FPS";
                end if;
            end if;

            for I in Rate_String'Range loop
                Draw_Image (Font, Rect (Grid_X (I), 17),
                    Font_Clip (Rate_String (I)),
                    Blend => Team_Colour (Active_Team));
            end loop;

            Draw_Image (Sprite, Rect (13, 17), CS_Clip (CS_NONE),
                Blend => Team_Colour (Active_Team));
            Draw_Image (Sprite, Rect (14, 17), CS_Clip (CS_64),
                Blend => Team_Colour (Active_Team));
            Draw_Image (Sprite, Rect (15, 17), CS_Clip (CS_1024),
                Blend => Team_Colour (Active_Team));
            Draw_Image (Sprite, Rect (16, 17), CS_Clip (CS_4096),
                Blend => Team_Colour (Active_Team));
            Draw_Image (Sprite, Rect (17, 17), CS_Clip (CS_65536),
                Blend => Team_Colour (Active_Team));
            Draw_Image (Sprite, Rect (18, 17), CT_Clip (CT_NONE),
                Blend => Team_Colour (Active_Team));
            Draw_Image (Sprite, Rect (19, 17), CT_Clip (CT_TWO_WAY),
                Blend => Team_Colour (Active_Team));
            Draw_Image (Sprite, Rect (20, 17), CT_Clip (CT_FOUR_WAY),
                Blend => Team_Colour (Active_Team));
            Draw_Image (Sprite, Rect (21, 17), CT_Clip (CT_EIGHT_WAY),
                Blend => Team_Colour (Active_Team));
            Draw_Image (Sprite, Rect (22, 17), CT_Clip (CT_FULLY),
                Blend => Team_Colour (Active_Team));
            Draw_Image (Sprite, Rect (23, 17), BP_Clip (BT_NONE),
                Blend => Team_Colour (Active_Team));
            Draw_Image (Sprite, Rect (24, 17), BP_Clip (BT_ONE_BIT),
                Blend => Team_Colour (Active_Team));
            Draw_Image (Sprite, Rect (25, 17), BP_Clip (BT_TWO_BIT),
                Blend => Team_Colour (Active_Team));
            Draw_Image (Sprite, Rect (26, 17), BP_Clip (BT_TWO_LEVEL_TWO_BIT),
                Blend => Team_Colour (Active_Team));
            Draw_Image (Sprite, Rect (27, 17), BP_Clip (BT_PERFECT),
                Blend => Team_Colour (Active_Team));
            Draw_Image (Sprite, Rect (28, 17), CPU_Clip (CPUS_EIGHT_FRAMES),
                Blend => Team_Colour (Active_Team));
            Draw_Image (Sprite, Rect (29, 17), CPU_Clip (CPUS_SIX_FRAMES),
                Blend => Team_Colour (Active_Team));
            Draw_Image (Sprite, Rect (30, 17), CPU_Clip (CPUS_FOUR_FRAMES),
                Blend => Team_Colour (Active_Team));
            Draw_Image (Sprite, Rect (31, 17), CPU_Clip (CPUS_TWO_FRAMES),
                Blend => Team_Colour (Active_Team));
            Draw_Image (Sprite, Rect (32, 17), CPU_Clip (CPUS_EVERY_FRAME),
                Blend => Team_Colour (Active_Team));
        end;
    end Draw_Game;
end Drawer;
