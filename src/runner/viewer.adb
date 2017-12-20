with SDL; use SDL;
with Games;
with Logger;
with Coordinates; use Coordinates;
with Boards; use Boards;
with Sprite_Sheet;
with Ada.Real_Time; use Ada.Real_Time;

package body Viewer is
    type CLIP_ID is (
        BLACK_CAPTAIN, WHITE_CAPTAIN, CPU_0, CPU_1, CPU_2, CPU_3, CPU_4,
        BLACK_MORTAR, WHITE_MORTAR, BP_0, BP_1, BP_2, BP_3, BP_4,
        BLACK_SNIPER, WHITE_SNIPER, CT_0, CT_1, CT_2, CT_3, CT_4,
        BLACK_ENGINEER, WHITE_ENGINEER, CS_0, CS_1, CS_2, CS_3, CS_4,
        BLACK_MG, WHITE_MG, WHITE_RIFLE, WATER, GRASS, WIRE, QUERY,
        BLACK_SCOUT, WHITE_SCOUT, BLACK_RIFLE, BEACH, COVER, MOVE, ATTACK);
    Clips : array (CLIP_ID) of SDL.Rectangle := (
        BLACK_CAPTAIN => (0, 0, 64, 64),
        WHITE_CAPTAIN => (64, 0, 64, 64),
        CPU_0 => (128, 0, 64, 64),
        CPU_1 => (192, 0, 64, 64),
        CPU_2 => (256, 0, 64, 64),
        CPU_3 => (320, 0, 64, 64),
        CPU_4 => (384, 0, 64, 64),
        BLACK_MORTAR => (0, 64, 64, 64),
        WHITE_MORTAR => (64, 64, 64, 64),
        BP_0 => (128, 64, 64, 64),
        BP_1 => (192, 64, 64, 64),
        BP_2 => (256, 64, 64, 64),
        BP_3 => (320, 64, 64, 64),
        BP_4 => (384, 64, 64, 64),
        BLACK_SNIPER => (0, 128, 64, 64),
        WHITE_SNIPER => (64, 128, 64, 64),
        CT_0 => (128, 128, 64, 64),
        CT_1 => (192, 128, 64, 64),
        CT_2 => (256, 128, 64, 64),
        CT_3 => (320, 128, 64, 64),
        CT_4 => (384, 128, 64, 64),
        BLACK_ENGINEER => (0, 192, 64, 64),
        WHITE_ENGINEER => (64, 192, 64, 64),
        CS_0 => (128, 192, 64, 64),
        CS_1 => (192, 192, 64, 64),
        CS_2 => (256, 192, 64, 64),
        CS_3 => (320, 192, 64, 64),
        CS_4 => (384, 192, 64, 64),
        BLACK_MG => (0, 256, 64, 64),
        WHITE_MG => (64, 256, 64, 64),
        WHITE_RIFLE => (128, 256, 64, 64),
        WATER => (192, 256, 64, 64),
        GRASS => (256, 256, 64, 64),
        WIRE => (320, 256, 64, 64),
        QUERY => (384, 256, 64, 64),
        BLACK_SCOUT => (0, 320, 64, 64),
        WHITE_SCOUT => (64, 320, 64, 64),
        BLACK_RIFLE => (128, 320, 64, 64),
        BEACH => (192, 320, 64, 64),
        COVER => (256, 320, 64, 64),
        MOVE => (320, 320, 64, 64),
        ATTACK => (384, 320, 64, 64));
    Unit_Clip : array (Boards.Unit_Type, Boards.Player_ID) of SDL.Rectangle := (
        UT_CAPTAIN => (Clips (WHITE_CAPTAIN), Clips (BLACK_CAPTAIN)),
        UT_MORTAR => (Clips (WHITE_MORTAR), Clips (BLACK_MORTAR)),
        UT_SNIPER => (Clips (WHITE_SNIPER), Clips (BLACK_SNIPER)),
        UT_ENGINEER_SS | UT_ENGINEER_FS => (
            Clips (WHITE_ENGINEER), Clips (BLACK_ENGINEER)),
        UT_MACHINEGUNNER_SS | UT_MACHINEGUNNER_FS => (
            Clips (WHITE_MG), Clips (BLACK_MG)),
        UT_SCOUT_SS | UT_SCOUT_FS => (Clips (WHITE_SCOUT), Clips (BLACK_SCOUT)),
        UT_RIFLEMAN_SS | UT_RIFLEMAN_FS => (
            Clips (WHITE_RIFLE), Clips (BLACK_RIFLE)));
    Terrain_Clip : array (Boards.Terrain_Type) of SDL.Rectangle := (
        TT_OPEN => Clips (GRASS),
        TT_WIRE => Clips (WIRE),
        TT_SAND => Clips (COVER),
        TT_BEACH => Clips (BEACH),
        TT_WATER => Clips (WATER),
        TT_BASE => Clips (QUERY));
    CS_Clip : array (Boards.Cache_Size) of SDL.Rectangle := (
        CS_NONE => Clips (CS_0),
        CS_64 => Clips (CS_1),
        CS_1024 => Clips (CS_2),
        CS_4096 => Clips (CS_3),
        CS_65536 => Clips (CS_4));
    CT_Clip : array (Boards.Cache_Type) of SDL.Rectangle := (
        CT_NONE => Clips (CT_0),
        CT_TWO_WAY => Clips (CT_1),
        CT_FOUR_WAY => Clips (CT_2),
        CT_EIGHT_WAY => Clips (CT_3),
        CT_FULLY => Clips (CT_4));
    BP_Clip : array (Boards.Branch_Type) of SDL.Rectangle := (
        BT_NONE => Clips (BP_0),
        BT_ONE_BIT => Clips (BP_1),
        BT_TWO_BIT => Clips (BP_2),
        BT_TWO_LEVEL_TWO_BIT => Clips (BP_3),
        BT_PERFECT => Clips (BP_4));
    CPU_Clip : array (Boards.CPU_Speed) of SDL.Rectangle := (
        CPUS_EIGHT_FRAMES => Clips (CPU_0),
        CPUS_SIX_FRAMES => Clips (CPU_1),
        CPUS_FOUR_FRAMES => Clips (CPU_2),
        CPUS_TWO_FRAMES => Clips (CPU_3),
        CPUS_EVERY_FRAME => Clips (CPU_4));
    Icon_Placement : array (Boards.Unit_Type, Boards.Player_ID)
        of SDL.Rectangle := (
        UT_CAPTAIN => ((0, 512, 32, 32), (992, 512, 32, 32)),
        UT_MORTAR => ((160, 512, 32, 32), (832, 512, 32, 32)),
        UT_SNIPER => ((320, 512, 32, 32), (672, 512, 32, 32)),
        UT_ENGINEER_SS => ((320, 544, 32, 32), (672, 544, 32, 32)),
        UT_ENGINEER_FS => ((160, 544, 32, 32), (832, 544, 32, 32)),
        UT_RIFLEMAN_SS => ((0, 544, 32, 32), (992, 544, 32, 32)),
        UT_MACHINEGUNNER_SS => ((320, 576, 32, 32), (672, 576, 32, 32)),
        UT_MACHINEGUNNER_FS => ((160, 576, 32, 32), (832, 576, 32, 32)),
        UT_RIFLEMAN_FS => ((0, 576, 32, 32), (992, 576, 32, 32)),
        UT_SCOUT_SS => ((320, 608, 32, 32), (672, 608, 32, 32)),
        UT_SCOUT_FS => ((160, 608, 32, 32), (832, 608, 32, 32)));

    Sprite : SDL.Image;

    -- 0 = paused, Natural'Last = as fast as possible
    Frame_Rate : Natural := 0;
    Old_Rate : Natural := 5;
    Next : Time;

    procedure Initialize is
    begin
        Sprite := SDL.Create_Image (Sprite_Sheet.Raw_Data (1)'Access, 448, 384);

        -- This is a colour test
        SDL.Begin_Draw ((255, 127, 0, 255));
        SDL.Draw_Image (Sprite, (0, 0, 1024, 640));
        SDL.End_Draw;
    end Initialize;

    procedure Finalize is
    begin
        null;
    end Finalize;

    procedure Draw_Game (Which : in Games.Game_Access) is
        B : Boards.Board := Which.State;

        function Get_Render_Rect (
            Where : in Coordinates.Coordinate) return SDL.Rectangle is
            Result : SDL.Rectangle;
        begin
            Result.Left := Integer (Where.X) * 32;
            Result.Top := Integer (Where.Y) * 32;
            Result.Width := 32;
            Result.Height := 32;
            return Result;
        end Get_Render_Rect;

        function Minify_Rect (Rect : in SDL.Rectangle) return SDL.Rectangle is
            Result : SDL.Rectangle;
        begin
            Result.Left := Rect.Left + 4;
            Result.Top := Rect.Top + 4;
            Result.Width := 24;
            Result.Height := 24;
            return Result;
        end Minify_Rect;

        procedure Draw_Unit (
            State : Boards.Unit_State;
            Unit : Boards.Unit_Type;
            Player : Boards.Player_ID) is
            Flip : Boolean := False; -- Unsetup guns are flipped
            Rotate : SDL.Angle := 0.0; -- Hidden snipers are rotated
            Rect : SDL.Rectangle;
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
            Rect := Get_Render_Rect (State.Position (T_WHITE));
            if State.Prone or State.Hidden then
                Rect := Minify_Rect (Rect);
            end if;
            if Unit = UT_SNIPER and State.Setup then
                Rotate := 0.785398;
            end if;
            Draw_Image (
                Sprite, Rect, Unit_Clip (Unit, Player), Rotate,
                (Rect.Width / 2, Rect.Height / 2),
                -- (Rect.Left + Rect.Width / 2, Rect.Top + Rect.Height / 2),
                Flip);

            -- Draw actions
            if State.Shooting then
                declare
                    Path : Coordinate_Path := Get_Path_To (
                    State.Position (T_WHITE), State.Destination (T_WHITE));
                begin
                    for I in Path'Range loop
                        Draw_Image (
                            Sprite, Get_Render_Rect (Path (I)), Clips (ATTACK));
                    end loop;
                    Draw_Line (
                        Using => (255, 0, 0, 0),
                        From => (
                            X => 32 * Integer (Path (Path'First).X) + 16,
                            Y => 32 * Integer (Path (Path'First).Y) + 16
                            ),
                        To => (
                            X => 32 * Integer (Path (Path'Last).X) + 16,
                            Y => 32 * Integer (Path (Path'Last).Y) + 16)
                        );
                end;
            end if;

            if State.Moving then
                Draw_Image (Sprite, Get_Render_Rect (
                    State.Position (T_WHITE)), Clips (MOVE));
                Draw_Image (Sprite, Get_Render_Rect (
                    State.Destination (T_WHITE)), Clips (MOVE));
            end if;

            -- Draw upgrade status
            declare
                Target : SDL.Rectangle := Icon_Placement (Unit, Player);
                Delta_X : Integer;
            begin
                if Player = T_WHITE then
                    Delta_X := 32;
                else
                    Delta_X := -32;
                end if;
                Draw_Image (Sprite, Target, Unit_Clip (Unit, Player));
                Target.Left := Target.Left + Delta_X;
                Draw_Image (Sprite, Target, CS_Clip (State.Cache_Space));
                Target.Left := Target.Left + Delta_X;
                Draw_Image (Sprite, Target, CT_Clip (State.Cache_Kind));
                Target.Left := Target.Left + Delta_X;
                Draw_Image (Sprite, Target, BP_Clip (State.Branch_Predictor));
                Target.Left := Target.Left + Delta_X;
                Draw_Image (Sprite, Target, CPU_Clip (State.Speed));
            end;
        end Draw_Unit;
    begin
        -- First draw terrain
        for Y in Y_Coordinate'Range loop
            for X in X_Coordinate'Range loop
                SDL.Draw_Image (Sprite, Get_Render_Rect ((X, Y)),
                    Terrain_Clip (Boards.Terrain_Of (
                        B, To_Location ((X, Y), Boards.T_WHITE))));
            end loop;
        end loop;

        -- Draw each unit
        for P in Boards.Player_ID'Range loop
            for U in Boards.Unit_Type'Range loop
                Draw_Unit (Boards.Get_Unit (B, U, P), U, P);
            end loop;
        end loop;
    end Draw_Game;

    procedure Step_Game (
        Which : in out Games.Game_Access;
        What : in SDL.Event) is
        procedure Set_Next is
        begin
            if Frame_Rate > 0 then
                Next := Clock + Milliseconds (1000 / Frame_Rate);
            else
                Next := Clock;
            end if;
        end Set_Next;
    begin
        if Frame_Rate = 0 then
            case What.Kind is
                when SDL.KEY_DOWN_EVENT =>
                    case What.Key is
                        when SDL.KEY_DOT =>
                            Games.Step_Game (Which.all);
                        when SDL.KEY_SPACE =>
                            Frame_Rate := Old_Rate;
                            Set_Next;
                        when SDL.KEY_UP =>
                            Frame_Rate := 1;
                            Set_Next;
                        when others => null;
                    end case;
                when SDL.MOUSE_DOWN_EVENT =>
                    for U in Boards.Unit_Type'Range loop
                        for P in Boards.Player_ID'Range loop
                            if SDL.Within (
                                Icon_Placement (U, P),
                                State.Mouse.Where)
                            then
                                Logger.Toggle_Logging (U, P);
                            end if;
                        end loop;
                    end loop;
                when others => null;
            end case;
        else
            case What.Kind is
                when SDL.NO_EVENT =>
                    -- Force the maxed frame rate
                    delay until Next;
                    Games.Step_Game (Which.all);
                    Set_Next;
                when SDL.KEY_DOWN_EVENT =>
                    case What.Key is
                        when SDL.KEY_DOT =>
                            if Frame_Rate /= Natural'Last then
                                Old_Rate := Frame_Rate;
                                Frame_Rate := Natural'Last;
                                Set_Next;
                            end if;
                        when SDL.KEY_UP =>
                            Frame_Rate := Frame_Rate + 1;
                            Set_Next;
                        when SDL.KEY_DOWN =>
                            Frame_Rate := Frame_Rate - 1;
                            Set_Next;
                        when SDL.KEY_SPACE =>
                            Old_Rate := Frame_Rate;
                            Frame_Rate := 0;
                            Set_Next;
                        when others => null;
                    end case;
                when SDL.KEY_UP_EVENT =>
                    if What.Key = SDL.KEY_DOT then
                        Frame_Rate := Old_Rate;
                        Set_Next;
                    end if;
                when others => null;
            end case;
        end if;
    end Step_Game;
end Viewer;
