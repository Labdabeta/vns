with Interfaces; use Interfaces;
with Interfaces.C;

package SDL_Keyboard is
    type SDL_Keysym is record
        scancode : aliased Interfaces.C.unsigned;
        sym : aliased Integer_32;
        c_mod : aliased Unsigned_16;
        unused : aliased Unsigned_32;
    end record;
    pragma Convention (C_Pass_By_Copy, SDL_Keysym);
end SDL_Keyboard;
