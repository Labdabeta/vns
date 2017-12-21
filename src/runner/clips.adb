package body Clips is
    function Font_Clip (C : Character) return SDL.Rectangle is
    begin
        case C is
            when '0' => return (0, 0, 32, 32);
            when '1' => return (32, 0, 32, 32);
            when '2' => return (64, 0, 32, 32);
            when '3' => return (96, 0, 32, 32);
            when '4' => return (128, 0, 32, 32);
            when '5' => return (160, 0, 32, 32);
            when '6' => return (0, 32, 32, 32);
            when '7' => return (32, 32, 32, 32);
            when '8' => return (64, 32, 32, 32);
            when '9' => return (96, 32, 32, 32);
            when 'A' | 'a' => return (128, 32, 32, 32);
            when 'B' | 'b' => return (160, 32, 32, 32);
            when 'C' | 'c' => return (0, 64, 32, 32);
            when 'D' | 'd' => return (32, 64, 32, 32);
            when 'E' | 'e' => return (64, 64, 32, 32);
            when 'F' | 'f' => return (96, 64, 32, 32);
            when 'G' | 'g' => return (128, 64, 32, 32);
            when 'H' | 'h' => return (160, 64, 32, 32);
            when 'I' | 'i' => return (0, 96, 32, 32);
            when 'J' | 'j' => return (32, 96, 32, 32);
            when 'K' | 'k' => return (64, 96, 32, 32);
            when 'L' | 'l' => return (96, 96, 32, 32);
            when 'M' | 'm' => return (128, 96, 32, 32);
            when 'N' | 'n' => return (160, 96, 32, 32);
            when 'O' | 'o' => return (0, 128, 32, 32);
            when 'P' | 'p' => return (32, 128, 32, 32);
            when 'Q' | 'q' => return (64, 128, 32, 32);
            when 'R' | 'r' => return (96, 128, 32, 32);
            when 'S' | 's' => return (128, 128, 32, 32);
            when 'T' | 't' => return (160, 128, 32, 32);
            when 'U' | 'u' => return (0, 160, 32, 32);
            when 'V' | 'v' => return (32, 160, 32, 32);
            when 'W' | 'w' => return (64, 160, 32, 32);
            when 'X' | 'x' => return (96, 160, 32, 32);
            when 'Y' | 'y' => return (128, 160, 32, 32);
            when 'Z' | 'z' => return (160, 160, 32, 32);
            when '?' => return (0, 192, 32, 32);
            when '+' => return (32, 192, 32, 32);
            when '-' => return (64, 192, 32, 32);
            when ' ' => return (160, 192, 32, 32);
            when others => return (0, 192, 32, 32);
        end case;
    end Font_Clip;
end Clips;
