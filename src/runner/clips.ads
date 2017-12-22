with SDL;
with Boards; use Boards;

package Clips is
    type Sprite_Clip_ID is (
        BLACK_CAPTAIN, WHITE_CAPTAIN, CPU_0, CPU_1, CPU_2, CPU_3, CPU_4,
        BLACK_MORTAR, WHITE_MORTAR, BP_0, BP_1, BP_2, BP_3, BP_4,
        BLACK_SNIPER, WHITE_SNIPER, CT_0, CT_1, CT_2, CT_3, CT_4,
        BLACK_ENGINEER, WHITE_ENGINEER, CS_0, CS_1, CS_2, CS_3, CS_4,
        BLACK_MG, WHITE_MG, WHITE_RIFLE, WATER, GRASS, WIRE, QUERY,
        BLACK_SCOUT, WHITE_SCOUT, BLACK_RIFLE, BEACH, COVER, MOVE, ATTACK);
    Sprite_Clips : constant array (Sprite_Clip_ID) of SDL.Rectangle := (
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
        UT_CAPTAIN => (
            Sprite_Clips (WHITE_CAPTAIN), Sprite_Clips (BLACK_CAPTAIN)),
        UT_MORTAR => (Sprite_Clips (WHITE_MORTAR), Sprite_Clips (BLACK_MORTAR)),
        UT_SNIPER => (Sprite_Clips (WHITE_SNIPER), Sprite_Clips (BLACK_SNIPER)),
        UT_ENGINEER_SS | UT_ENGINEER_FS => (
            Sprite_Clips (WHITE_ENGINEER), Sprite_Clips (BLACK_ENGINEER)),
        UT_MACHINEGUNNER_SS | UT_MACHINEGUNNER_FS => (
            Sprite_Clips (WHITE_MG), Sprite_Clips (BLACK_MG)),
        UT_SCOUT_SS | UT_SCOUT_FS => (
            Sprite_Clips (WHITE_SCOUT), Sprite_Clips (BLACK_SCOUT)),
        UT_RIFLEMAN_SS | UT_RIFLEMAN_FS => (
            Sprite_Clips (WHITE_RIFLE), Sprite_Clips (BLACK_RIFLE)));
    Terrain_Clip : array (Boards.Terrain_Type) of SDL.Rectangle := (
        Sprite_Clips (GRASS),
        Sprite_Clips (WIRE),
        Sprite_Clips (COVER),
        Sprite_Clips (BEACH),
        Sprite_Clips (WATER),
        Sprite_Clips (QUERY));
    CS_Clip : array (Boards.Upgrade_Level) of SDL.Rectangle := (
        Sprite_Clips (CS_0),
        Sprite_Clips (CS_1),
        Sprite_Clips (CS_2),
        Sprite_Clips (CS_3),
        Sprite_Clips (CS_4));
    CT_Clip : array (Boards.Upgrade_Level) of SDL.Rectangle := (
        Sprite_Clips (CT_0),
        Sprite_Clips (CT_1),
        Sprite_Clips (CT_2),
        Sprite_Clips (CT_3),
        Sprite_Clips (CT_4));
    BP_Clip : array (Boards.Upgrade_Level) of SDL.Rectangle := (
        Sprite_Clips (BP_0),
        Sprite_Clips (BP_1),
        Sprite_Clips (BP_2),
        Sprite_Clips (BP_3),
        Sprite_Clips (BP_4));
    CPU_Clip : array (Boards.Upgrade_Level) of SDL.Rectangle := (
        Sprite_Clips (CPU_0),
        Sprite_Clips (CPU_1),
        Sprite_Clips (CPU_2),
        Sprite_Clips (CPU_3),
        Sprite_Clips (CPU_4));
    function Font_Clip (C : Character) return SDL.Rectangle;
end Clips;
