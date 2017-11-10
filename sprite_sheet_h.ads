pragma Ada_2005;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings;

package sprite_sheet_h is

   --  arg-macro: procedure HEADER_PIXEL (data, pixel)
   --    {pixel(0) := (((data(0) - 33) << 2) or ((data(1) - 33) >> 4)); pixel(1) := ((((data(1) - 33) and 16#F#) << 4) or ((data(2) - 33) >> 2)); pixel(2) := ((((data(2) - 33) and 16#3#) << 6) or ((data(3) - 33))); data += 4; }
   width : aliased unsigned;  -- res/sprite_sheet.h:3
   pragma Import (C, width, "width");

   height : aliased unsigned;  -- res/sprite_sheet.h:4
   pragma Import (C, height, "height");

   header_data : Interfaces.C.Strings.chars_ptr;  -- res/sprite_sheet.h:14
   pragma Import (C, header_data, "header_data");

end sprite_sheet_h;
