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
        print("    ", end="")
        for x in rs[k]:
            if needspipe:
                print("|")
            else:
                needspipe = True
            (s,e) = x
            print(str(s+1) + ".." + str(e+1), end="")
        print("=>" + str(k), end="")
    print(");")
    print("end " + sys.argv[2] + ";")

if __name__ == "__main__":
    main()
