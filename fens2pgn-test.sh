#!/bin/bash
# fens2pgn-test - tests fens2pgn by comparing obtained and expected output
# Copyright (C) 2015 Paweł Zacharek
# 
# -----------------------------------------------------------------------
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License along
# with this program; if not, write to the Free Software Foundation, Inc.,
# 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
# -----------------------------------------------------------------------
# 
# date: 2015-09-18

NAME='fens2pgn.c'
TESTS='tests'
ANSWERS='answers'
OUTDIR='out'

if [ ! -d "$TESTS" ]; then
	echo 'No tests were found.'
	exit 0
elif [ ! -d "$ANSWERS" ]; then
	echo 'No answers were found.'
	exit 0
fi

gcc -std=gnu11 -o a.out "$NAME" &> /dev/null
if [ "$?" -ne 0 ]; then
	echo 'Cannot compile source file.'
	exit 0
fi

if [ ! -d "$OUTDIR" ]; then
	mkdir "$OUTDIR"
fi

cd "$TESTS"
for I in *.in; do
	I="${I%.*}"
	../a.out -f -o "../$OUTDIR/${I}.out" "${I}.in" &> /dev/null
	cmp "../$OUTDIR/${I}.out" "../$ANSWERS/${I}.pgn" &> /dev/null
	if [ "$?" -ne 0 ]; then
		echo -ne "DIFFERS: \e[31m${I}.out"
	else
		echo -ne "OK: \e[32m${I}.out"
	fi
	echo -e "\e[0m"
done

rm ../a.out
exit 0
