#!/bin/bash

LNX=$HOME/Documents/linux-5.16.2
cd / 	
find  $LNX                                                                \
	-path "$LNX/arch/*" ! -path "$LNX/arch/i386*" -prune -o               \
	-path "$LNX/include/asm-*" ! -path "$LNX/include/asm-i386*" -prune -o \
	-path "$LNX/tmp*" -prune -o                                           \
	-path "$LNX/Documentation*" -prune -o                                 \
	-path "$LNX/scripts*" -prune -o                                       \
	-path "$LNX/drivers*" -prune -o                                       \
        -name "*.[chxsS]" -print >/home/morp/Documents/linux-5.16.2/cscope/cscope.files
