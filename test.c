#include <stdio.h>

#include "res/sprite_sheet.c"

int main()
{
    int x, y, i, ll;

    i = 0;
    ll = 0;

    printf("package Sprite_Sheet is\n"
           "    type Byte is mod 2**8;\n"
           "    pragma Convention (C, Byte);\n"
           "    Raw_Sprite_Sheet_Data : constant array (1 .. %d) of aliased Byte := (\n        ",
           gimp_image.height*gimp_image.width*4);
    for (y = 0; y < gimp_image.height; ++y) {
        for (x = 0; x < gimp_image.width; ++x) {
            ll += printf("%d, ", gimp_image.pixel_data[i++]);
            ll += printf("%d, ", gimp_image.pixel_data[i++]);
            ll += printf("%d, ", gimp_image.pixel_data[i++]);
            ll += printf("%d, ", gimp_image.pixel_data[i++]);
            if (ll > 60) {
                ll = 0;
                printf("\n        ");
            }
        }
    }
    printf("\n    pragma Convention (C, Raw_Sprite_Sheet_Data);\nend Sprite_Sheet;");

    return 0;
}
