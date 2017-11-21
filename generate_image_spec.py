#!/usr/bin/env python3
import sys
from PIL import Image

# Data is a giant array
def parse_data_to_ranges(data):
    ranges = {}
    current_start = 0
    last_elem = data[0]
    for i in range(len(data)):
        if data[i] != last_elem:
            if not last_elem in ranges.keys():
                ranges[last_elem] = []
            ranges[last_elem] += [(current_start, i-1)]
            current_start = i
            last_elem = data[i]
    if not last_elem in ranges.keys():
        ranges[last_elem] = []
    ranges[last_elem] += [(current_start, i)]
    return ranges

def image_to_array(path):
    return list(Image.open(path).getdata())

def main():
    rs = parse_data_to_ranges(image_to_array(sys.argv[1]))
    print("with SDL;")
    print("pragma Style_Checks (Off);")
    print("package " + sys.argv[2] + " is");
    print("    Raw_Data : aliased SDL.Raw_Image_Data := (", end="");
    commaed = False
    for k in rs:
        if commaed:
            print(",")
        else:
            commaed = True
            print("")
        needspipe = False
        for x in rs[k]:
            if needspipe:
                print("|")
            else:
                needspipe = True
            (s,e) = x
            print(str(s+1) + ".." + str(e+1), end="")
        # (r,g,b,a) = k
        # (g,r,b,a) = k
        # (b,r,g,a) = k
        # (r,b,g,a) = k
        # (g,b,r,a) = k
        # (b,g,r,a) = k
        # (b,g,a,r) = k
        # (g,b,a,r) = k
        # (a,b,g,r) = k
        # (b,a,g,r) = k
        # (g,a,b,r) = k
        # (a,g,b,r) = k
        # (a,r,b,g) = k
        # (r,a,b,g) = k
        # (b,a,r,g) = k
        # (a,b,r,g) = k
        # (r,b,a,g) = k
        # (b,r,a,g) = k
        (g,r,a,b) = k # Why this is correct will always be a mystery to me!
        # (r,g,a,b) = k
        # (a,g,r,b) = k
        # (g,a,r,b) = k
        # (r,a,g,b) = k
        # (a,r,g,b) = k
        print("=>(" + str(a) + ", " + str(r) + ", " + str(g) + ", " + str(b) + ")", end="")
    print(");")
    print("end " + sys.argv[2] + ";")

if __name__ == "__main__":
    main()
