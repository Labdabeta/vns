with System;
with Ada.Numerics; use Ada.Numerics;
with Interfaces.C;
with Interfaces.C.Pointers;

package SDL is
--------------------------------------------------------------------------------
    --  Enumerations
--------------------------------------------------------------------------------
    type Key_Type is (
        KEY_A, KEY_B, KEY_C, KEY_D, KEY_E, KEY_F, KEY_G, KEY_H, KEY_I, KEY_J,
        KEY_K, KEY_L, KEY_M, KEY_N, KEY_O, KEY_P, KEY_Q, KEY_R, KEY_S, KEY_T,
        KEY_U, KEY_V, KEY_W, KEY_X, KEY_Y, KEY_Z, KEY_0, KEY_1, KEY_2, KEY_3,
        KEY_4, KEY_5, KEY_6, KEY_7, KEY_8, KEY_9, KEY_APOSTROPHE, KEY_BACKSPACE,
        KEY_COMMA, KEY_DELETE, KEY_UP, KEY_DOWN, KEY_LEFT, KEY_RIGHT, KEY_END,
        KEY_EQUALS, KEY_ESCAPE, KEY_HOME, KEY_PAD0, KEY_PAD1, KEY_PAD2,
        KEY_PAD3, KEY_PAD4, KEY_PAD5, KEY_PAD6, KEY_PAD7, KEY_PAD8, KEY_PAD9,
        KEY_PAD_DOT, KEY_LEFT_ALT, KEY_RIGHT_ALT, KEY_LEFT_CTRL, KEY_RIGHT_CTRL,
        KEY_LEFT_BRACKET, KEY_RIGHT_BRACKET, KEY_LEFT_SHIFT, KEY_RIGHT_SHIFT,
        KEY_MINUS, KEY_PAGE_UP, KEY_PAGE_DOWN, KEY_PAUSE, KEY_DOT, KEY_ENTER,
        KEY_SEMICOLON, KEY_SLASH, KEY_SPACE, KEY_TAB, KEY_UNKNOWN);
    type Button_Type is (LEFT, MIDDLE, RIGHT);

    type Event_Type is (
        NO_EVENT, IRRELEVANT_EVENT, QUIT_EVENT,
        KEY_DOWN_EVENT, KEY_UP_EVENT,
        MOUSE_MOTION_EVENT,
        MOUSE_DOWN_EVENT, MOUSE_UP_EVENT,
        MOUSE_WHEEL_EVENT);

--------------------------------------------------------------------------------
    --  Records
--------------------------------------------------------------------------------
    type Event (Kind : Event_Type) is
        record
            case Kind is
                when NO_EVENT | IRRELEVANT_EVENT | QUIT_EVENT => null;
                when KEY_DOWN_EVENT | KEY_UP_EVENT => Key : Key_Type;
                when MOUSE_MOTION_EVENT => DX, DY : Integer;
                when MOUSE_DOWN_EVENT | MOUSE_UP_EVENT => Button : Button_Type;
                when MOUSE_WHEEL_EVENT => WX, WY : Integer;
            end case;
        end record;

    type Coordinate is
        record
            X, Y : Integer;
        end record;

    type Mouse_State_Array is array (Button_Type) of Boolean;
    type Mouse_Status is
        record
            Where : Coordinate;
            Buttons : Mouse_State_Array;
        end record;

    type Keyboard_State_Array is array (Key_Type) of Boolean;

    type Window_Status is
        record
            Width, Height : Natural;
        end record;

    type Status is
        record
            Mouse : Mouse_Status;
            Keyboard : Keyboard_State_Array;
            Window : Window_Status;
        end record;

    type Image is
        record
            Width, Height : Positive;
            Texture : System.Address;
        end record;

    Null_Image : constant Image := (1, 1, System.Null_Address);

    type Rectangle is
        record
            Left, Top, Width, Height : Natural;
        end record;

    type Angle is delta Pi / 3600000000 range -Pi .. Pi;
    type Colour_Component is mod 256;
    pragma Convention (C, Colour_Component);

    type Colour is
        record
            A, R, G, B : Colour_Component;
        end record;
    pragma Convention (C_Pass_By_Copy, Colour);

    type Raw_Image_Data is array (Positive range <>) of aliased Colour;
    pragma Convention (C, Raw_Image_Data);

    package C_Image_Data_Pointers is new Interfaces.C.Pointers (
        Index => Positive,
        Element => Colour,
        Element_Array => Raw_Image_Data,
        Default_Terminator => (0, 0, 0, 0));

    type Raw_Image is new C_Image_Data_Pointers.Pointer;
--------------------------------------------------------------------------------
    --  Variables
--------------------------------------------------------------------------------
    State : Status;

--------------------------------------------------------------------------------
    --  Basic Functions
--------------------------------------------------------------------------------
    function Initialize (
       Title : in String;
       Width : in Positive;
       Height : in Positive) return Boolean;

    function Step return Event;

    procedure Finalize;

    --  Actually render clear
    procedure Begin_Draw (On : Colour := (255, 0, 0, 0));
    procedure End_Draw; --  Actually render present

    function Within (
        Box : in Rectangle;
        Point : in Coordinate) return Boolean;

--------------------------------------------------------------------------------
    --  Image Functions
--------------------------------------------------------------------------------
    function Create_Image (
        Data : in Raw_Image;
        Width : in Positive;
        Height : in Positive)
        return Image;

    procedure Draw_Image (
        Which : in Image;
        Destination : in Rectangle;
        Source : in Rectangle := (0, 0, 0, 0);
        Rotation : in Angle := 0.0;
        Center : in Coordinate := (0, 0);
        VFlip : in Boolean := False;
        HFlip : in Boolean := False);

    procedure Draw_Image_Centered (
        Which : in Image;
        Destination : in Rectangle;
        Source : in Rectangle := (0, 0, 0, 0);
        Rotation : in Angle := 0.0;
        Center : in Coordinate := (0, 0);
        VFlip : in Boolean := False;
        HFlip : in Boolean := False);

    function Is_Null (Which : in Image) return Boolean;
end SDL;
