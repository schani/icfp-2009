#!/bin/bash

# $1 filename

if [ "$1" = "" ]; then
	echo $0 "<filename>";
	exit 1;
fi

# get cookie
curl -c cookies -d "login=fikdm09@tilab.tuwien.ac.at&pwd=Mensch&Login" http://www.icfpcontest.org/login.php

#eat cookie

curl -b cookies -F userfile=@"$1"   http://www.icfpcontest.org/upload.php

# scores

curl -b cookies http://www.icfpcontest.org/teampage.php >tp.html
lynx -dump tp.html  | awk 'BEGIN {p=0}{ if (match(substr($0,1,18),"   Scenario number")) p=1; if (p==1) print $0;if (match(substr($0,1,13),"     ________")) p=0;}'


