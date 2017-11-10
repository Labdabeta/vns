with Interfaces; use Interfaces;
with Interfaces.C;
with Interfaces.C.Strings;
with SDL_Keyboard;
with System;

package SDL_Event is
    --  Event types
    SDL_FIRSTEVENT : constant := 0;
    SDL_QUIT : constant := 256;
    SDL_APP_TERMINATING : constant := 257;
    SDL_APP_LOWMEMORY : constant := 258;
    SDL_APP_WILLENTERBACKGROUND : constant := 259;
    SDL_APP_DIDENTERBACKGROUND : constant := 260;
    SDL_APP_WILLENTERFOREGROUND : constant := 261;
    SDL_APP_DIDENTERFOREGROUND : constant := 262;
    SDL_WINDOWEVENT_E : constant := 512;
    SDL_SYSWMEVENT_E : constant := 513;
    SDL_KEYDOWN : constant := 768;
    SDL_KEYUP : constant := 769;
    SDL_TEXTEDITING : constant := 770;
    SDL_TEXTINPUT : constant := 771;
    SDL_KEYMAPCHANGED : constant := 772;
    SDL_MOUSEMOTION : constant := 1024;
    SDL_MOUSEBUTTONDOWN : constant := 1025;
    SDL_MOUSEBUTTONUP : constant := 1026;
    SDL_MOUSEWHEEL : constant := 1027;
    SDL_JOYAXISMOTION : constant := 1536;
    SDL_JOYBALLMOTION : constant := 1537;
    SDL_JOYHATMOTION : constant := 1538;
    SDL_JOYBUTTONDOWN : constant := 1539;
    SDL_JOYBUTTONUP : constant := 1540;
    SDL_JOYDEVICEADDED : constant := 1541;
    SDL_JOYDEVICEREMOVED : constant := 1542;
    SDL_CONTROLLERAXISMOTION : constant := 1616;
    SDL_CONTROLLERBUTTONDOWN : constant := 1617;
    SDL_CONTROLLERBUTTONUP : constant := 1618;
    SDL_CONTROLLERDEVICEADDED : constant := 1619;
    SDL_CONTROLLERDEVICEREMOVED : constant := 1620;
    SDL_CONTROLLERDEVICEREMAPPED : constant := 1621;
    SDL_FINGERDOWN : constant := 1792;
    SDL_FINGERUP : constant := 1793;
    SDL_FINGERMOTION : constant := 1794;
    SDL_DOLLARGESTURE : constant := 2048;
    SDL_DOLLARRECORD : constant := 2049;
    SDL_MULTIGESTURE : constant := 2050;
    SDL_CLIPBOARDUPDATE : constant := 2304;
    SDL_DROPFILE : constant := 4096;
    SDL_AUDIODEVICEADDED : constant := 4352;
    SDL_AUDIODEVICEREMOVED : constant := 4353;
    SDL_RENDER_TARGETS_RESET : constant := 8192;
    SDL_RENDER_DEVICE_RESET : constant := 8193;
    SDL_USEREVENT_E : constant := 32768;
    SDL_LASTEVENT : constant := 65535;

    SDL_WINDOWEVENT_RESIZED : constant := 5;
    SDL_WINDOWEVENT_SIZE_CHANGED : constant := 6;

    type SDL_CommonEvent is record
        c_type : aliased Unsigned_32;
        timestamp : aliased Unsigned_32;
    end record;
    pragma Convention (C_Pass_By_Copy, SDL_CommonEvent);

    type SDL_WindowEvent is record
        c_type : aliased Unsigned_32;
        timestamp : aliased Unsigned_32;
        windowId : aliased Unsigned_32;
        event : aliased Unsigned_8;
        padding1 : aliased Unsigned_8;
        padding2 : aliased Unsigned_8;
        padding3 : aliased Unsigned_8;
        data1 : aliased Integer_32;
        data2 : aliased Integer_32;
    end record;
    pragma Convention (C_Pass_By_Copy, SDL_WindowEvent);

    type SDL_KeyboardEvent is record
        c_type : aliased Unsigned_32;
        timestamp : aliased Unsigned_32;
        windowId : aliased Unsigned_32;
        state : aliased Unsigned_8;
        repeat : aliased Unsigned_8;
        padding2 : aliased Unsigned_8;
        padding3 : aliased Unsigned_8;
        keysym : aliased SDL_Keyboard.SDL_Keysym;
    end record;
    pragma Convention (C_Pass_By_Copy, SDL_KeyboardEvent);

    subtype Short_Text_Array is Interfaces.C.char_array (0 .. 31);
    type SDL_TextEditingEvent is record
        c_type : aliased Unsigned_32;
        timestamp : aliased Unsigned_32;
        windowId : aliased Unsigned_32;
        text : aliased Short_Text_Array;
        start : aliased Integer_32;
        length : aliased Integer_32;
    end record;
    pragma Convention (C_Pass_By_Copy, SDL_TextEditingEvent);

    type SDL_TextInputEvent is record
        c_type : aliased Unsigned_32;
        timestamp : aliased Unsigned_32;
        windowId : aliased Unsigned_32;
        text : aliased Short_Text_Array;
    end record;
    pragma Convention (C_Pass_By_Copy, SDL_TextInputEvent);

    type SDL_MouseMotionEvent is record
        c_type : aliased Unsigned_32;
        timestamp : aliased Unsigned_32;
        windowId : aliased Unsigned_32;
        which : aliased Unsigned_32;
        state : aliased Unsigned_32;
        x : aliased Integer_32;
        y : aliased Integer_32;
        xrel : aliased Integer_32;
        yrel : aliased Integer_32;
    end record;
    pragma Convention (C_Pass_By_Copy, SDL_MouseMotionEvent);

    type SDL_MouseButtonEvent is record
        c_type : aliased Unsigned_32;
        timestamp : aliased Unsigned_32;
        windowId : aliased Unsigned_32;
        which : aliased Unsigned_32;
        button : aliased Unsigned_8;
        state : aliased Unsigned_8;
        clicks : aliased Unsigned_8;
        padding1 : aliased Unsigned_8;
        x : aliased Integer_32;
        y : aliased Integer_32;
    end record;
    pragma Convention (C_Pass_By_Copy, SDL_MouseButtonEvent);

    type SDL_MouseWheelEvent is record
        c_type : aliased Unsigned_32;
        timestamp : aliased Unsigned_32;
        windowId : aliased Unsigned_32;
        which : aliased Unsigned_32;
        x : aliased Integer_32;
        y : aliased Integer_32;
        direction : aliased Unsigned_32;
    end record;
    pragma Convention (C_Pass_By_Copy, SDL_MouseWheelEvent);

    type SDL_JoyAxisEvent is record
        c_type : aliased Unsigned_32;
        timestamp : aliased Unsigned_32;
        which : aliased Integer_32;
        axis : aliased Unsigned_8;
        padding1 : aliased Unsigned_8;
        padding2 : aliased Unsigned_8;
        padding3 : aliased Unsigned_8;
        value : aliased Integer_16;
        padding4 : aliased Unsigned_16;
    end record;
    pragma Convention (C_Pass_By_Copy, SDL_JoyAxisEvent);

    type SDL_JoyBallEvent is record
        c_type : aliased Unsigned_32;
        timestamp : aliased Unsigned_32;
        which : aliased Integer_32;
        ball : aliased Unsigned_8;
        padding1 : aliased Unsigned_8;
        padding2 : aliased Unsigned_8;
        padding3 : aliased Unsigned_8;
        xrel : aliased Integer_16;
        yrel : aliased Integer_16;
    end record;
    pragma Convention (C_Pass_By_Copy, SDL_JoyBallEvent);

    type SDL_JoyHatEvent is record
        c_type : aliased Unsigned_32;
        timestamp : aliased Unsigned_32;
        which : aliased Integer_32;
        hat : aliased Unsigned_8;
        value : aliased Unsigned_8;
        padding1 : aliased Unsigned_8;
        padding2 : aliased Unsigned_8;
    end record;
    pragma Convention (C_Pass_By_Copy, SDL_JoyHatEvent);

    type SDL_JoyButtonEvent is record
        c_type : aliased Unsigned_32;
        timestamp : aliased Unsigned_32;
        which : aliased Integer_32;
        button : aliased Unsigned_8;
        state : aliased Unsigned_8;
        padding1 : aliased Unsigned_8;
        padding2 : aliased Unsigned_8;
    end record;
    pragma Convention (C_Pass_By_Copy, SDL_JoyButtonEvent);

    type SDL_JoyDeviceEvent is record
        c_type : aliased Unsigned_32;
        timestamp : aliased Unsigned_32;
        which : aliased Integer_32;
    end record;
    pragma Convention (C_Pass_By_Copy, SDL_JoyDeviceEvent);

    type SDL_ControllerAxisEvent is record
        c_type : aliased Unsigned_32;
        timestamp : aliased Unsigned_32;
        which : aliased Integer_32;
        axis : aliased Unsigned_8;
        padding1 : aliased Unsigned_8;
        padding2 : aliased Unsigned_8;
        padding3 : aliased Unsigned_8;
        value : aliased Integer_16;
        padding4 : aliased Unsigned_16;
    end record;
    pragma Convention (C_Pass_By_Copy, SDL_ControllerAxisEvent);

    type SDL_ControllerButtonEvent is record
        c_type : aliased Unsigned_32;
        timestamp : aliased Unsigned_32;
        which : aliased Integer_32;
        button : aliased Unsigned_8;
        state : aliased Unsigned_8;
        padding1 : aliased Unsigned_8;
        padding2 : aliased Unsigned_8;
    end record;
    pragma Convention (C_Pass_By_Copy, SDL_ControllerButtonEvent);

    type SDL_ControllerDeviceEvent is record
        c_type : aliased Unsigned_32;
        timestamp : aliased Unsigned_32;
        which : aliased Integer_32;
    end record;
    pragma Convention (C_Pass_By_Copy, SDL_ControllerDeviceEvent);

    type SDL_AudioDeviceEvent is record
        c_type : aliased Unsigned_32;
        timestamp : aliased Unsigned_32;
        which : aliased Unsigned_32;
        iscapture : aliased Unsigned_8;
        padding1 : aliased Unsigned_8;
        padding2 : aliased Unsigned_8;
        padding3 : aliased Unsigned_8;
    end record;
    pragma Convention (C_Pass_By_Copy, SDL_AudioDeviceEvent);

    type SDL_TouchFingerEvent is record
        c_type : aliased Unsigned_32;
        timestamp : aliased Unsigned_32;
        touchId : aliased Integer_64;
        fingerId : aliased Integer_64;
        x : aliased Interfaces.C.C_float;
        y : aliased Interfaces.C.C_float;
        dx : aliased Interfaces.C.C_float;
        dy : aliased Interfaces.C.C_float;
        pressure : aliased Interfaces.C.C_float;
    end record;
    pragma Convention (C_Pass_By_Copy, SDL_TouchFingerEvent);

    type SDL_MultiGestureEvent is record
        c_type : aliased Unsigned_32;
        timestamp : aliased Unsigned_32;
        touchId : aliased Integer_64;
        dTheta : aliased Interfaces.C.C_float;
        dDist : aliased Interfaces.C.C_float;
        x : aliased Interfaces.C.C_float;
        y : aliased Interfaces.C.C_float;
        numFingers : aliased Unsigned_16;
        padding : aliased Unsigned_16;
    end record;
    pragma Convention (C_Pass_By_Copy, SDL_MultiGestureEvent);

    type SDL_DollarGestureEvent is record
        c_type : aliased Unsigned_32;
        timestamp : aliased Unsigned_32;
        touchId : aliased Integer_64;
        gestureId : aliased Integer_64;
        numFingers : aliased Unsigned_32;
        error : aliased Interfaces.C.C_float;
        x : aliased Interfaces.C.C_float;
        y : aliased Interfaces.C.C_float;
    end record;
    pragma Convention (C_Pass_By_Copy, SDL_DollarGestureEvent);

    type SDL_DropEvent is record
        c_type : aliased Unsigned_32;
        timestamp : aliased Unsigned_32;
        file : Interfaces.C.Strings.chars_ptr;
    end record;
    pragma Convention (C_Pass_By_Copy, SDL_DropEvent);

    type SDL_QuitEvent is record
        c_type : aliased Unsigned_32;
        timestamp : aliased Unsigned_32;
    end record;
    pragma Convention (C_Pass_By_Copy, SDL_QuitEvent);

    type SDL_OSEvent is record
        c_type : aliased Unsigned_32;
        timestamp : aliased Unsigned_32;
    end record;
    pragma Convention (C_Pass_By_Copy, SDL_OSEvent);

    type SDL_UserEvent is record
        c_type : aliased Unsigned_32;
        timestamp : aliased Unsigned_32;
        windowID : aliased Unsigned_32;
        code : aliased Integer_32;
        data1 : System.Address;
        data2 : System.Address;
    end record;
    pragma Convention (C_Pass_By_Copy, SDL_UserEvent);

    type SDL_SysWMEvent is record
        c_type : aliased Unsigned_32;
        timestamp : aliased Unsigned_32;
        msg : System.Address;
    end record;
    pragma Convention (C_Pass_By_Copy, SDL_SysWMEvent);

    type SDL_Event_Padding_Array is array (0 .. 55) of aliased Unsigned_8;
    type SDL_Event (Option : Integer := 0) is record
        case Option is
            when 0 => c_type : aliased Unsigned_32;
            when 1 => common : aliased SDL_CommonEvent;
            when 2 => window : aliased SDL_WindowEvent;
            when 3 => key : aliased SDL_KeyboardEvent;
            when 4 => edit : aliased SDL_TextEditingEvent;
            when 5 => text : aliased SDL_TextInputEvent;
            when 6 => motion : aliased SDL_MouseMotionEvent;
            when 7 => button : aliased SDL_MouseButtonEvent;
            when 8 => wheel : aliased SDL_MouseWheelEvent;
            when 9 => jaxis : aliased SDL_JoyAxisEvent;
            when 10 => jball : aliased SDL_JoyBallEvent;
            when 11 => jhat : aliased SDL_JoyHatEvent;
            when 12 => jbutton : aliased SDL_JoyButtonEvent;
            when 13 => jdevice : aliased SDL_JoyDeviceEvent;
            when 14 => caxis : aliased SDL_ControllerAxisEvent;
            when 15 => cbutton : aliased SDL_ControllerButtonEvent;
            when 16 => cdevice : aliased SDL_ControllerDeviceEvent;
            when 17 => adevice : aliased SDL_AudioDeviceEvent;
            when 18 => quit : aliased SDL_QuitEvent;
            when 19 => user : aliased SDL_UserEvent;
            when 20 => syswm : aliased SDL_SysWMEvent;
            when 21 => tfinger : aliased SDL_TouchFingerEvent;
            when 22 => mgesture : aliased SDL_MultiGestureEvent;
            when 23 => dgesture : aliased SDL_DollarGestureEvent;
            when 24 => drop : aliased SDL_DropEvent;
            when others => padding : aliased SDL_Event_Padding_Array;
        end case;
    end record;
    pragma Convention (C_Pass_By_Copy, SDL_Event);
    pragma Unchecked_Union (SDL_Event);

    type SDL_Event_ptr is access all SDL_Event;
    pragma Convention (C, SDL_Event_ptr);
end SDL_Event;
