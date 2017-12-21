with Interfaces; use Interfaces;
with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with System; use System;
with Ada.Text_IO;
with SDL_Event;
with Ada.Numerics; use Ada.Numerics;

package body SDL is
--------------------------------------------------------------------------------
    --  Interface Types
--------------------------------------------------------------------------------
    type SDL_Window_ptr is new System.Address;
    Null_SDL_Window_ptr : constant SDL_Window_ptr :=
        SDL_Window_ptr (System.Null_Address);

    type SDL_Renderer_ptr is new System.Address;
    Null_SDL_Renderer_ptr : constant SDL_Renderer_ptr :=
        SDL_Renderer_ptr (System.Null_Address);

    type SDL_Rect is
        record
            x, y, w, h : int;
        end record;
    pragma Convention (C_Pass_By_Copy, SDL_Rect);

    type SDL_Point is
        record
            x, y : int;
        end record;
    pragma Convention (C_Pass_By_Copy, SDL_Point);

    type SDL_Color is
        record
            r : Unsigned_8;
            g : Unsigned_8;
            b : Unsigned_8;
            a : Unsigned_8;
        end record;
    pragma Convention (C_Pass_By_Copy, SDL_Color);

--------------------------------------------------------------------------------
    --  Interface Functions
--------------------------------------------------------------------------------
    function C_SDL_Init (
        Flags : in Unsigned_32)
        return int;
    pragma Import (C, C_SDL_Init, "SDL_Init");

    procedure C_SDL_Quit;
    pragma Import (C, C_SDL_Quit, "SDL_Quit");

    function C_SDL_GetError return chars_ptr;
    pragma Import (C, C_SDL_GetError, "SDL_GetError");

    function C_SDL_ShowSimpleMessageBox (
        Flags : in Unsigned_32;
        Title : in chars_ptr;
        Message : in chars_ptr;
        Window : SDL_Window_ptr) return int;
    pragma Import (C, C_SDL_ShowSimpleMessageBox, "SDL_ShowSimpleMessageBox");

    function C_SDL_CreateWindow (
        Title : in chars_ptr;
        x : in int;
        y : in int;
        w : in int;
        h : in int;
        Flags : in Unsigned_32)
        return SDL_Window_ptr;
    pragma Import (C, C_SDL_CreateWindow, "SDL_CreateWindow");

    procedure C_SDL_DestroyWindow (Window : in SDL_Window_ptr);
    pragma Import (C, C_SDL_DestroyWindow, "SDL_DestroyWindow");

    function C_SDL_CreateRenderer (
        Window : in SDL_Window_ptr;
        Index : in int;
        Flags : in Unsigned_32) return SDL_Renderer_ptr;
    pragma Import (C, C_SDL_CreateRenderer, "SDL_CreateRenderer");

    procedure C_SDL_DestroyRenderer (Renderer : in SDL_Renderer_ptr);
    pragma Import (C, C_SDL_DestroyRenderer, "SDL_DestroyRenderer");

    procedure C_SDL_DestroyTexture (Texture : in System.Address);
    pragma Import (C, C_SDL_DestroyTexture, "SDL_DestroyTexture");

    function C_SDL_SetTextureAlphaMod (
        Texture : in System.Address;
        Alpha : in Unsigned_8)
        return int;
    pragma Import (C, C_SDL_SetTextureAlphaMod, "SDL_SetTextureAlphaMod");

    function C_SDL_SetTextureColorMod (
        Texture : in System.Address;
        R : in Unsigned_8;
        G : in Unsigned_8;
        B : in Unsigned_8)
        return int;
    pragma Import (C, C_SDL_SetTextureColorMod, "SDL_SetTextureColorMod");

    function C_SDL_PollEvent (Event : access SDL_Event.SDL_Event) return int;
    pragma Import (C, C_SDL_PollEvent, "SDL_PollEvent");

    function C_SDL_SetRenderDrawColor (
        Renderer : in SDL_Renderer_ptr;
        R : in Unsigned_8;
        G : in Unsigned_8;
        B : in Unsigned_8;
        A : in Unsigned_8) return int;
    pragma Import (C, C_SDL_SetRenderDrawColor, "SDL_SetRenderDrawColor");

    function C_SDL_RenderClear (Renderer : in SDL_Renderer_ptr) return int;
    pragma Import (C, C_SDL_RenderClear, "SDL_RenderClear");

    procedure C_SDL_RenderPresent (Renderer : in SDL_Renderer_ptr);
    pragma Import (C, C_SDL_RenderPresent, "SDL_RenderPresent");

    function C_SDL_RenderCopyEx (
        Renderer : SDL_Renderer_ptr;
        Texture : System.Address;
        Source : access SDL_Rect;
        Destination : access SDL_Rect;
        Angle : double;
        Center : access SDL_Point;
        Flip : int) return int;
    pragma Import (C, C_SDL_RenderCopyEx, "SDL_RenderCopyEx");

    function C_SDL_CreateTextureFromSurface (
        Renderer : in SDL_Renderer_ptr;
        Surface : in System.Address) return System.Address;
    pragma Import (C, C_SDL_CreateTextureFromSurface,
        "SDL_CreateTextureFromSurface");

    procedure C_SDL_FreeSurface (Surface : System.Address);
    pragma Import (C, C_SDL_FreeSurface, "SDL_FreeSurface");

    function C_SDL_QueryTexture (
        Texture : System.Address;
        Format : access Unsigned_32;
        Access_Type : access int;
        w : access int;
        h : access int) return int;
    pragma Import (C, C_SDL_QueryTexture, "SDL_QueryTexture");

    function C_SDL_CreateRGBSurfaceFrom (
        Pixels : Raw_Image;
        Width : int;
        Height : int;
        Depth : int;
        Pitch : int;
        Rmask : Unsigned_32;
        Gmask : Unsigned_32;
        Bmask : Unsigned_32;
        Amask : Unsigned_32)
        return System.Address;
    pragma Import (C, C_SDL_CreateRGBSurfaceFrom, "SDL_CreateRGBSurfaceFrom");

    function C_SDL_RenderDrawLine (
        Renderer : SDL_Renderer_ptr;
        X1 : int;
        Y1 : int;
        X2 : int;
        Y2 : int) return int;
    pragma Import (C, C_SDL_RenderDrawLine, "SDL_RenderDrawLine");

--------------------------------------------------------------------------------
    --  SDL Constants
--------------------------------------------------------------------------------
    SDL_INIT_EVERYTHING : constant := 29233;
    SDL_WINDOWPOS_UNDEFINED : constant := 536805376;
    SDL_MESSAGEBOX_ERROR : constant := 16;

--------------------------------------------------------------------------------
    --  SDL Variables
--------------------------------------------------------------------------------
    The_Window : SDL_Window_ptr;
    The_Renderer : SDL_Renderer_ptr;

--------------------------------------------------------------------------------
    --  Implementations
--------------------------------------------------------------------------------
    procedure Begin_Draw (On : Colour := (255, 0, 0, 0)) is begin
        if C_SDL_SetRenderDrawColor (
            The_Renderer, Unsigned_8 (On.R),
            Unsigned_8 (On.G), Unsigned_8 (On.B), Unsigned_8 (On.A)) /= 0
        then
            Ada.Text_IO.Put_Line ("SDL_SetRenderDrawColor: " &
                Value (C_SDL_GetError));
        end if;

        if C_SDL_RenderClear (The_Renderer) /= 0 then
            Ada.Text_IO.Put_Line ("SDL_RenderClear: " & Value (C_SDL_GetError));
        end if;
    end Begin_Draw;

    function Create_Image (
        Data : in Raw_Image;
        Width : in Positive;
        Height : in Positive)
        return Image is
        Result : Image;
        Surface : System.Address;
    begin
        Surface := C_SDL_CreateRGBSurfaceFrom (
            Data, int (Width), int (Height), 32, 4 * int (Width),
            16#00FF0000#,
            16#0000FF00#,
            16#000000FF#,
            16#FF000000#);
        Result.Width := Width;
        Result.Height := Height;
        Result.Texture := C_SDL_CreateTextureFromSurface (
            The_Renderer, Surface);
        C_SDL_FreeSurface (Surface);
        return Result;
    end Create_Image;

    procedure Free_Image (Which : in out Image) is
    begin
        C_SDL_DestroyTexture (Which.Texture);
    end Free_Image;

    procedure Display_Error is begin
        if C_SDL_ShowSimpleMessageBox (
            SDL_MESSAGEBOX_ERROR,
            New_String ("SDL Error"),
            C_SDL_GetError,
            Null_SDL_Window_ptr) /= 0
        then
            Ada.Text_IO.Put_Line ("Error: " & Value (C_SDL_GetError));
        end if;
    end Display_Error;

    procedure Draw_Image (
        Which : in Image;
        Destination : in Rectangle;
        Source : in Rectangle := (0, 0, 0, 0);
        Rotation : in Angle := 0.0;
        Center : in Coordinate := (0, 0);
        VFlip : in Boolean := False;
        HFlip : in Boolean := False;
        Blend : in Colour := (255, 255, 255, 255)) is
        C_Source : aliased SDL_Rect := (
            x => int (Source.Left),
            y => int (Source.Top),
            w => int (Source.Width),
            h => int (Source.Height));
        C_Destination : aliased SDL_Rect := (
            x => int (Destination.Left),
            y => int (Destination.Top),
            w => int (Destination.Width),
            h => int (Destination.Height));
        C_Center : aliased SDL_Point := (
            x => int (Center.X),
            y => int (Center.Y));
        C_Flip : int := 0;

        function Rad_To_Deg (X : in Angle) return double is begin
            return (double (X) * 180.0) / Pi;
        end Rad_To_Deg;
    begin
        if Which.Texture = System.Null_Address then
            Ada.Text_IO.Put_Line ("Can't render a null texture!");
            return;
        end if;

        -- First set alpha and colour mods
        if C_SDL_SetTextureAlphaMod (Which.Texture, Unsigned_8 (Blend.A)) /= 0
        then
            Ada.Text_IO.Put_Line ("Warning: Alpha blending not supported.");
        end if;

        if C_SDL_SetTextureColorMod (
            Which.Texture,
            Unsigned_8 (Blend.R),
            Unsigned_8 (Blend.G),
            Unsigned_8 (Blend.B)) /= 0
        then
            Ada.Text_IO.Put_Line ("Warning: Colour blending not supported.");
        end if;

        if HFlip then
            C_Flip := C_Flip + 1;
        end if;
        if VFlip then
            C_Flip := C_Flip + 2;
        end if;
        if Source.Top = 0 and Source.Left = 0 and
            Source.Width = 0 and Source.Height = 0
        then
            if C_SDL_RenderCopyEx (
                The_Renderer,
                Which.Texture,
                null,
                C_Destination'Access,
                Rad_To_Deg (Rotation),
                C_Center'Access,
                C_Flip) /= 0
            then
                Ada.Text_IO.Put_Line (
                    "SDL_RenderCopyEx: " & Value (C_SDL_GetError));
            end if;
        else
            if C_SDL_RenderCopyEx (
                The_Renderer,
                Which.Texture,
                C_Source'Access,
                C_Destination'Access,
                Rad_To_Deg (Rotation),
                C_Center'Access,
                C_Flip) /= 0
            then
                Ada.Text_IO.Put_Line (
                    "SDL_RenderCopyEx: " & Value (C_SDL_GetError));
            end if;
        end if;
    end Draw_Image;

    procedure Draw_Image_Centered (
        Which : in Image;
        Destination : in Rectangle;
        Source : in Rectangle := (0, 0, 0, 0);
        Rotation : in Angle := 0.0;
        Center : in Coordinate := (0, 0);
        VFlip : in Boolean := False;
        HFlip : in Boolean := False) is
        True_Area : SDL.Rectangle;
        True_Source : SDL.Rectangle := Source;
    begin
        if Source = (0, 0, 0, 0) then
            True_Source := (0, 0, Which.Width, Which.Height);
        end if;

        --  Check if the image will even fit
        if Destination.Width < True_Source.Width then
            True_Area.Left := Destination.Left;
        else
            True_Area.Left := Destination.Left +
                (Destination.Width / 2) -
                (True_Source.Width / 2);
        end if;

        if Destination.Height < True_Source.Height then
            True_Area.Top := Destination.Top;
        else
            True_Area.Top := Destination.Top +
                (Destination.Height / 2) -
                (True_Source.Height / 2);
        end if;

        True_Area.Width := True_Source.Width;
        if True_Source.Width > Destination.Width then
            True_Area.Width := True_Source.Width;
        end if;

        True_Area.Height := True_Source.Height;
        if True_Source.Height > Destination.Height then
            True_Area.Height := True_Source.Height;
        end if;

        Draw_Image (Which, True_Area, Source, Rotation, Center, VFlip, HFlip);
    end Draw_Image_Centered;

    procedure End_Draw is begin
        C_SDL_RenderPresent (The_Renderer);
    end End_Draw;

    procedure Draw_Line (
        Using : in Colour;
        From : in Coordinate;
        To : in Coordinate) is
    begin
        if C_SDL_SetRenderDrawColor (
            The_Renderer,
            Unsigned_8 (Using.R),
            Unsigned_8 (Using.G),
            Unsigned_8 (Using.B),
            Unsigned_8 (Using.A)) /= 0
        then
            Ada.Text_IO.Put_Line ("SetRenderDrawColor: " &
                Value (C_SDL_GetError));
        end if;

        if C_SDL_RenderDrawLine (
            The_Renderer,
            int (From.X),
            int (From.Y),
            int (To.X),
            int (To.Y)) /= 0
        then
            Ada.Text_IO.Put_Line ("RenderDrawLine: " & Value (C_SDL_GetError));
        end if;
    end Draw_Line;

    procedure Finalize is begin
        C_SDL_DestroyRenderer (The_Renderer);
        C_SDL_DestroyWindow (The_Window);
        C_SDL_Quit;
    end Finalize;

    function Initialize (
       Title : in String;
       Width : in Positive;
       Height : in Positive) return Boolean is
    begin
        if C_SDL_Init (SDL_INIT_EVERYTHING) /= 0 then
            Display_Error;
            return False;
        end if;

        The_Window := C_SDL_CreateWindow (
            New_String (Title),
            SDL_WINDOWPOS_UNDEFINED,
            SDL_WINDOWPOS_UNDEFINED,
            int (Width),
            int (Height),
            32); -- SDL_WINDOW_RESIZABLE

        if The_Window = Null_SDL_Window_ptr then
            Display_Error;
            C_SDL_Quit;
            return False;
        end if;

        The_Renderer := C_SDL_CreateRenderer (The_Window, -1, 0);

        if The_Renderer = Null_SDL_Renderer_ptr then
            Display_Error;
            C_SDL_DestroyWindow (The_Window);
            C_SDL_Quit;
            return False;
        end if;

        State.Window.Width := Width;
        State.Window.Height := Height;
        return True;
    end Initialize;

    function Is_Null (Which : in Image) return Boolean is
    begin
        return Which.Texture = System.Null_Address;
    end Is_Null;

    function Step return Event is
        E : aliased SDL_Event.SDL_Event;

        function Sym_To_Key (Sym : Integer_32) return Key_Type is begin
            case Sym is
                when 8 => return KEY_BACKSPACE;
                when 9 => return KEY_TAB;
                when 13 => return KEY_ENTER;
                when 27 => return KEY_ESCAPE;
                when 32 => return KEY_SPACE;
                when 39 => return KEY_APOSTROPHE;
                when 44 => return KEY_COMMA;
                when 45 => return KEY_MINUS;
                when 46 => return KEY_DOT;
                when 47 => return KEY_SLASH;
                when 48 => return KEY_0;
                when 49 => return KEY_1;
                when 50 => return KEY_2;
                when 51 => return KEY_3;
                when 52 => return KEY_4;
                when 53 => return KEY_5;
                when 54 => return KEY_6;
                when 55 => return KEY_7;
                when 56 => return KEY_8;
                when 57 => return KEY_9;
                when 59 => return KEY_SEMICOLON;
                when 61 => return KEY_EQUALS;
                when 91 => return KEY_LEFT_BRACKET;
                when 93 => return KEY_RIGHT_BRACKET;
                when 97 => return KEY_A;
                when 98 => return KEY_B;
                when 99 => return KEY_C;
                when 100 => return KEY_D;
                when 101 => return KEY_E;
                when 102 => return KEY_F;
                when 103 => return KEY_G;
                when 104 => return KEY_H;
                when 105 => return KEY_I;
                when 106 => return KEY_J;
                when 107 => return KEY_K;
                when 108 => return KEY_L;
                when 109 => return KEY_M;
                when 110 => return KEY_N;
                when 111 => return KEY_O;
                when 112 => return KEY_P;
                when 113 => return KEY_Q;
                when 114 => return KEY_R;
                when 115 => return KEY_S;
                when 116 => return KEY_T;
                when 117 => return KEY_U;
                when 118 => return KEY_V;
                when 119 => return KEY_W;
                when 120 => return KEY_X;
                when 121 => return KEY_Y;
                when 122 => return KEY_Z;
                when 127 => return KEY_DELETE;
                when 1073741896 => return KEY_PAUSE;
                when 1073741898 => return KEY_HOME;
                when 1073741899 => return KEY_PAGE_UP;
                when 1073741901 => return KEY_END;
                when 1073741902 => return KEY_PAGE_DOWN;
                when 1073741903 => return KEY_RIGHT;
                when 1073741904 => return KEY_LEFT;
                when 1073741905 => return KEY_DOWN;
                when 1073741906 => return KEY_UP;
                when 1073741913 => return KEY_PAD1;
                when 1073741914 => return KEY_PAD2;
                when 1073741915 => return KEY_PAD3;
                when 1073741916 => return KEY_PAD4;
                when 1073741917 => return KEY_PAD5;
                when 1073741918 => return KEY_PAD6;
                when 1073741919 => return KEY_PAD7;
                when 1073741920 => return KEY_PAD8;
                when 1073741921 => return KEY_PAD9;
                when 1073741922 => return KEY_PAD0;
                when 1073741923 => return KEY_PAD_DOT;
                when 1073742048 => return KEY_LEFT_CTRL;
                when 1073742049 => return KEY_LEFT_SHIFT;
                when 1073742050 => return KEY_LEFT_ALT;
                when 1073742052 => return KEY_RIGHT_CTRL;
                when 1073742053 => return KEY_RIGHT_SHIFT;
                when 1073742054 => return KEY_RIGHT_ALT;
                when others => return KEY_UNKNOWN;
            end case;
        end Sym_To_Key;
    begin
        if C_SDL_PollEvent (E'Access) = 0 then
            return (Kind => NO_EVENT);
        end if;

        case E.c_type is
            when SDL_Event.SDL_QUIT =>
                return (Kind => QUIT_EVENT);
            when SDL_Event.SDL_WINDOWEVENT_E =>
                if E.window.event = SDL_Event.SDL_WINDOWEVENT_RESIZED or
                    E.window.event = SDL_Event.SDL_WINDOWEVENT_SIZE_CHANGED
                then
                    State.Window.Width := Natural (E.window.data1);
                    State.Window.Height := Natural (E.window.data2);
                end if;
                return (Kind => IRRELEVANT_EVENT);
            when SDL_Event.SDL_KEYDOWN =>
                State.Keyboard (Sym_To_Key (E.key.keysym.sym)) := True;
                return (
                    Kind => KEY_DOWN_EVENT,
                    Key => Sym_To_Key (E.key.keysym.sym));
            when SDL_Event.SDL_KEYUP =>
                State.Keyboard (Sym_To_Key (E.key.keysym.sym)) := False;
                return (
                    Kind => KEY_UP_EVENT,
                    Key => Sym_To_Key (E.key.keysym.sym));
            when SDL_Event.SDL_MOUSEMOTION =>
                State.Mouse.Where.X := Integer (E.motion.x);
                State.Mouse.Where.Y := Integer (E.motion.y);
                return (
                    Kind => MOUSE_MOTION_EVENT,
                    DX => Integer (E.motion.xrel),
                    DY => Integer (E.motion.yrel));
            when SDL_Event.SDL_MOUSEBUTTONDOWN =>
                State.Mouse.Where.X := Integer (E.button.x);
                State.Mouse.Where.Y := Integer (E.button.y);
                State.Mouse.Buttons (Button_Type'Val (E.button.button - 1)) :=
                    True;
                return (
                    Kind => MOUSE_DOWN_EVENT,
                    Button => Button_Type'Val (E.button.button - 1));
            when SDL_Event.SDL_MOUSEBUTTONUP =>
                State.Mouse.Where.X := Integer (E.button.x);
                State.Mouse.Where.Y := Integer (E.button.y);
                State.Mouse.Buttons (Button_Type'Val (E.button.button - 1)) :=
                    False;
                return (
                    Kind => MOUSE_UP_EVENT,
                    Button => Button_Type'Val (E.button.button - 1));
            when SDL_Event.SDL_MOUSEWHEEL =>
                return (
                    Kind => MOUSE_WHEEL_EVENT,
                    WX => Integer (E.wheel.x),
                    WY => Integer (E.wheel.y));
            when others =>
                return (Kind => IRRELEVANT_EVENT);
        end case;
    end Step;

    function Within (
        Box : in Rectangle;
        Point : in Coordinate) return Boolean is
        Relative_X : Integer := Point.X - Box.Left;
        Relative_Y : Integer := Point.Y - Box.Top;
    begin
        if
            Relative_X >= 0 and Relative_X <= Box.Width and
            Relative_Y >= 0 and Relative_Y <= Box.Height
        then
            return True;
        end if;

        return False;
    end Within;
end SDL;
