with Interfaces; use Interfaces;

package body Processors.Registers is

    function From_Boolean (Val : in Boolean) return Register_Type is
    begin
        if Val then
            return 1;
        else
            return 0;
        end if;
    end From_Boolean;

    function To_Direction (X : in Register_Type) return Coordinates.Direction is
    begin
        return Coordinates.Direction'Val (X - 1);
    end To_Direction;
    function To_Coordinate (
        X : in Register_Type;
        Y : in Register_Type)
        return Coordinates.Coordinate is
    begin
        return (Coordinates.X_Coordinate (X), Coordinates.Y_Coordinate (Y));
    end To_Coordinate;

    function To_Unit (X : in Register_Type) return Boards.Unit_Type is
    begin
        return Boards.Unit_Type'Val (X - 1);
    end To_Unit;

    function From_Unit (X : in Boards.Unit_Type) return Register_Type is
        UID_OF : constant array (Boards.Unit_Type) of Register_Type := (
            1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11);
    begin
        return UID_OF (X);
    end From_Unit;

    function Count_Leading_Zeroes (X : in Register_Type) return Register_Type is
        LogTable256 : constant array (Register_Type range 0 .. 255)
            of Register_Type := (
            0 => -1,
            1 => 0,
            2 .. 3 => 1,
            4 .. 7 => 2,
            8 .. 15 => 3,
            16 .. 31 => 4,
            32 .. 64 => 5,
            65 .. 127 => 6,
            128 .. 255 => 7);
        TT : Register_Type := Shift_Right (X, 16);
        T : Register_Type;
    begin
        if TT = 0 then
            T := Shift_Right (X, 8);
            if T = 0 then
                return LogTable256 (X);
            else
                return LogTable256 (T) + 8;
            end if;
        else
            T := Shift_Right (TT, 8);
            if T = 0 then
                return LogTable256 (TT) + 16;
            else
                return LogTable256 (T) + 24;
            end if;
        end if;
    end Count_Leading_Zeroes;

    function Count_Leading_Ones (X : in Register_Type) return Register_Type is
    begin
        return Count_Leading_Zeroes (From_U32 (not To_U32 (X)));
    end Count_Leading_Ones;

    function Count_Ones (X : in Register_Type) return Register_Type is
        BitSetTable256 : constant array (Unsigned_32 range 0 .. 255)
            of Register_Type := (
            0 => 0,
            1 | 2 | 4 | 8 | 16 | 32 | 64 | 128 => 1,
            3 | 5 | 6 | 9 | 10 | 12 | 17 | 18 | 20 | 24 | 33 | 34 | 36 | 40 |
            48 | 65 | 66 | 68 | 72 | 80 | 96 | 129 | 130 | 132 | 136 | 144 |
            160 | 192 => 2,
            7 | 11 | 13 | 14 | 19 | 21 | 22 | 25 | 26 | 28 | 35 | 37 | 38 |
            41 | 42 | 44 | 49 | 50 | 52 | 56 | 67 | 69 | 70 | 73 | 74 | 76 |
            81 | 82 | 84 | 88 | 97 | 98 | 100 | 104 | 112 | 131 | 133 | 134 |
            137 | 138 | 140 | 145 | 146 | 148 | 152 | 161 | 162 | 164 | 168 |
            176 | 193 | 194 | 196 | 200 | 208 | 224 => 3,
            15 | 23 | 27 | 29 | 30 | 39 | 43 | 45 | 46 | 51 | 53 | 54 | 57 | 58
            | 60 | 71 | 75 | 77 | 78 | 83 | 85 | 86 | 89 | 90 | 92 | 99 | 101 |
            102 | 105 | 106 | 108 | 113 | 114 | 116 | 120 | 135 | 139 | 141 |
            142 | 147 | 149 | 150 | 153 | 154 | 156 | 163 | 165 | 166 | 169 |
            170 | 172 | 177 | 178 | 180 | 184 | 195 | 197 | 198 | 201 | 202 |
            204 | 209 | 210 | 212 | 216 | 225 | 226 | 228 | 232 | 240 => 4,
            31 | 47 | 55 | 59 | 61 | 62 | 79 | 87 | 91 | 93 | 94 | 103 | 107 |
            109 | 110 | 115 | 117 | 118 | 121 | 122 | 124 | 143 | 151 | 155 |
            157 | 158 | 167 | 171 | 173 | 174 | 179 | 181 | 182 | 185 | 186 |
            188 | 199 | 203 | 205 | 206 | 211 | 213 | 214 | 217 | 218 | 220 |
            227 | 229 | 230 | 233 | 234 | 236 | 241 | 242 | 244 | 248 => 5,
            63 | 95 | 111 | 119 | 123 | 125 | 126 | 159 | 175 | 183 | 187 |
            189 | 190 | 207 | 215 | 219 | 221 | 222 | 231 | 235 | 237 | 238 |
            243 | 245 | 246 | 249 | 250 | 252 => 6,
            127 | 191 | 223 | 239 | 247 | 251 | 253 | 254 => 7,
            255 => 8);
        V : Unsigned_32 := To_U32 (X);
    begin
        return BitSetTable256 (V and 2#11111111#) +
            BitSetTable256 (Shift_Right (V, 8) and 2#11111111#) +
            BitSetTable256 (Shift_Right (V, 16) and 2#11111111#) +
            BitSetTable256 (Shift_Right (V, 24) and 2#11111111#);
    end Count_Ones;

    function Count_Zeroes (X : in Register_Type) return Register_Type is
    begin
        return 32 - Count_Ones (X);
    end Count_Zeroes;

    function Shift_Right (X, By : in Register_Type) return Register_Type is
    begin
        return From_U32 (Shift_Right (To_U32 (X), Natural (By)));
    end Shift_Right;

    function Shift_Left (X, By : in Register_Type) return Register_Type is
    begin
        return From_U32 (Shift_Left (To_U32 (X), Natural (By)));
    end Shift_Left;
end Processors.Registers;
