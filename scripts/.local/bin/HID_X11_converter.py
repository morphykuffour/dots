#!/usr/bin/env python3
"""
Usage:
$ python3 HID_X11_converter.py --type X11 <keycode number as shown in `xmodma -pke`>
$ python3 HID_X11_converter.py --type HID <keycode number used in QMK>

HID keycodes as used in QMK can be found in https://docs.google.com/spreadsheets/d/1TAK6df8bObeswn8CnxIGgZJB_NYON_p8GReMfZ2O-sE/edit#gid=0. 
Look for the "Basic Keycodes" sheet and don't forget to convert from hexadecimal to decimal notation when using this tool.

An enum starts on 0 unless specified otherwise and the integer affected to every item in the enum increases one by one as you go down the enum, unless a specific integer is assigned to an enum item.
"""
import argparse
import sys

parser = argparse.ArgumentParser(description='Convert between X11 and HID keycodes')
parser.add_argument('--type',  
        type=str, 
        required=True, 
        choices=["HID", "h", "X", "x", "X11"],
        help="The type of provided keycode value.")
parser.add_argument('keycode',
        type=int,
        nargs="?",
        default=sys.stdin,
        help="The numeric keycode value in decimal to convert. (Default: stdin)")
args = parser.parse_args()

# Source of the below lookup table:
# https://elixir.bootlin.com/linux/latest/source/drivers/hid/hid-input.c#L27
hid_keyboard = [ 0,  0,  0,  0, 30, 48, 46, 32, 18, 33, 34, 35, 23, 36, 37, 38,
        50, 49, 24, 25, 16, 19, 31, 20, 22, 47, 17, 45, 21, 44,  2,  3,
        4,  5,  6,  7,  8,  9, 10, 11, 28,  1, 14, 15, 57, 12, 13, 26,
        27, 43, 43, 39, 40, 41, 51, 52, 53, 58, 59, 60, 61, 62, 63, 64,
        65, 66, 67, 68, 87, 88, 99, 70,119,110,102,104,111,107,109,106,
        105,108,103, 69, 98, 55, 74, 78, 96, 79, 80, 81, 75, 76, 77, 71,
        72, 73, 82, 83, 86,127,116,117,183,184,185,186,187,188,189,190,
        191,192,193,194,134,138,130,132,128,129,131,137,133,135,136,113,
        115,114,None,None,None,121,None, 89, 93,124, 92, 94, 95,None,None,None,
        122,123, 90, 91, 85,None,None,None,None,None,None,None,111,None,None,None,
        None,None,None,None,None,None,None,None,None,None,None,None,None,None,None,None,
        None,None,None,None,None,None,179,180,None,None,None,None,None,None,None,None,
        None,None,None,None,None,None,None,None,None,None,None,None,None,None,None,None,
        None,None,None,None,None,None,None,None,111,None,None,None,None,None,None,None,
        29, 42, 56,125, 97, 54,100,126,164,166,165,163,161,115,114,113,
        150,158,159,128,136,177,178,176,142,152,173,140,None,None,None,None
        ];

# The evdev driver in X11 remaps 
# Linux input keycodes by adding 8 to them; 
# libinput does the same thing.
def HID_to_Xorg(keycode):
    assert type(keycode) == int, f"keycode is of type {type(keycode)}"
    return hid_keyboard[keycode] + 8

def Xorg_to_HID(keycode):
    assert type(keycode) == int, f"keycode is of type {type(keycode)}"
    return hid_keyboard.index(keycode - 8)


if type(args.keycode) == int:
    keycode = args.keycode
else: # read stdint
    keycode = int(args.keycode.read())

if args.type in ("HID", "h"):
    print(HID_to_Xorg(keycode))
else:
    print(Xorg_to_HID(keycode))