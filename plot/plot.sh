#!/bin/bash

gawk -F\; -f vmatr.gawk "$1" >"${1%.*}.dat"

cat >"${1%.*}.plt" <<EOF
set xlabel "Cols"
set ylabel "Rows"

set pm3d;
splot "${1%.*}.dat" with lines lt rgb "black";

pause -1
EOF

gnuplot "${1%.*}.plt"

