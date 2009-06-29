#!/bin/bash 
#set -o nounset
#set -o errexit


#by now this shellscript is small, but it will get much bigger in future - trust me

SCHANIDIR="disassembler-from-joe"

GENERALSCHANIEXE="./runner"


#here comes the temporary files
TMPDIR="/tmp/hoscherei"
#here comes the final files
OUTPUTBASEDIR="/homes/icfp/ierehcsoh_is_hoscherei_spelled_backwards/batteries_not_included"

#compares two float values
#returns 1 (first arg bigger) / 0 (equal) / -1 (second val bigger)
SCORECOMPARE="perl floatcompare.pl"

# Command line handling:

SCENARG=
DIROFFSET="/$1"
FROM=
#while getopts d:s:f: opt
#do
#    case "$opt" in
#      d)  DIROFFSET="$OPTARG";;
#      s)  SCENARG="$OPTARG";;      
#      f)  FROM="$OPTARG";;
#      \?)		# unknown flag
#      	  echo >&2 \
#	  "Usage: $0 [-d directoryoffset] [-s filename] [-f (schani|baerli)]]"
#	  exit 1;;
#    esac
#done
#shift `expr $OPTIND - 1`
#echo "OUTPUT PARAMETERS: $FROM $SCENARG"

OUTPUTDIR=$OUTPUTBASEDIR/$DIROFFSET
BINTRACEOUTPUTDIR=$OUTPUTDIR/bintraces
EMPTRACEOUTPUTDIR=$OUTPUTDIR/emptraces

echo "removing old output directory..."
rm -rf $OUTPUTDIR


echo "generating directories..."
#generate hoschereiverzeichnisse für outputsi
mkdir -p $BINTRACEOUTPUTDIR $EMPTRACEOUTPUTDIR $TMPDIR


echo "rebuilding executables..."
#rebuild executables
make --directory $SCHANIDIR all > /dev/null


# if [ -n "$FROM" ] && [ -n "$SCENARG" ]; then
for problem in 1 2 3 4; do
  for scenario in 1 2 3 4; do

    echo "running scenario ${problem}00${scenario}"


    PROBLEMSCHANIEXE=$GENERALSCHANIEXE$problem


    echo "evaluating schani..."
    cd $SCHANIDIR &> /dev/null
    SCHANIPOINTS=`$PROBLEMSCHANIEXE -s ${problem}00${scenario} -d $TMPDIR/${problem}00${scenario}.schani.emp -t $TMPDIR/${problem}00${scenario}.schani.osf    | awk '/score/ {print $3}' | sort -n | tail -1`
    cd - &> /dev/null
    echo "schani scored $SCHANIPOINTS points."


      #schani war besser
      echo "therefore, using schani's solution for ${problem}00${scenario}. (sorry baerli ;( )"
      tar -C $TMPDIR -jcf $EMPTRACEOUTPUTDIR/${problem}00${scenario}.$SCHANIPOINTS.schani.emp.tar.bz2 ${problem}00${scenario}.schani.emp &> /dev/null
      mv $TMPDIR/${problem}00${scenario}.schani.osf $BINTRACEOUTPUTDIR/${problem}00${scenario}.$SCHANIPOINTS.schani.osf 
    # clean up tmpdir
    rm $TMPDIR/* &> /dev/null

  done
done

#else
#    
#    # evaluating only schani
#    if [ "$FROM" = "schani" ] ; then
#     	echo "Evaluating schani for $SCENARG"
#
#        PROBLEMSCHANIEXE=$GENERALSCHANIEXE$problem
#        cd $SCHANIDIR &> /dev/null
#        SCHANIPOINTS=`$PROBLEMSCHANIEXE -s ${SCENARG} -d $TMPDIR/${SCENARG}.schani.emp -t $TMPDIR/${SCENARG}.schani.osf | grep score | awk '{print $3}'`
#        cd - &> /dev/null
#        echo "schani scored $SCHANIPOINTS points."
#
#        #schani war besser
#        tar -C $TMPDIR -jcf $EMPTRACEOUTPUTDIR/${SCENARG}.$SCHANIPOINTS.schani.emp.tar.bz2 ${SCENARG}.schani.emp &> /dev/null
#        mv $TMPDIR/${SCENARG}.schani.osf $BINTRACEOUTPUTDIR/${SCENARG}.$SCHANIPOINTS.schani.osf 
#
#    # evaluating only baerli
#    elif [ "$FROM" = "baerli" ]; then
#     	echo "Evaluating baerli for $SCENARG"
#        cd $BAERLIDIR &> /dev/null
#        BAERLIPOINTS=`$GENERALBAERLIEXE -s ${SCENARG} | grep "final points" | awk '{print $3}'`
#        mv ${SCENARG}.emp $TMPDIR/${SCENARG}.baerli.emp
#        mv ${SCENARG}.osf $TMPDIR/${SCENARG}.baerli.osf
#        cd - &> /dev/null
#        echo "baerli scored $BAERLIPOINTS points."
#
#        #baerli war besser
#        tar -C $TMPDIR -jcf $EMPTRACEOUTPUTDIR/${SCENARG}.$BAERLIPOINTS.baerli.emp.tar.bz2 ${SCENARG}.baerli.emp &> /dev/null
#        mv $TMPDIR/${SCENARG}.baerli.osf $BINTRACEOUTPUTDIR/${SCENARG}.$BAERLIPOINTS.baerli.osf
#
#    # evaluating given scenario for both:
#    else
#     	echo "Evaluating schani for $SCENARG"
#
#        PROBLEMSCHANIEXE=$GENERALSCHANIEXE$problem
#        cd $SCHANIDIR &> /dev/null
#        SCHANIPOINTS=`$PROBLEMSCHANIEXE -s ${SCENARG} -d $TMPDIR/${SCENARG}.schani.emp -t $TMPDIR/${SCENARG}.schani.osf | grep score | awk '{print $3}'`
#        cd - &> /dev/null
#        echo "schani scored $SCHANIPOINTS points."
#
#        tar -C $TMPDIR -jcf $EMPTRACEOUTPUTDIR/${SCENARG}.$SCHANIPOINTS.schani.emp.tar.bz2 ${SCENARG}.schani.emp &> /dev/null
#        mv $TMPDIR/${SCENARG}.schani.osf $BINTRACEOUTPUTDIR/${SCENARG}.$SCHANIPOINTS.schani.osf 
#
#     	echo "Evaluating baerli for $SCENARG"
#        cd $BAERLIDIR &> /dev/null
#        BAERLIPOINTS=`$GENERALBAERLIEXE -s ${SCENARG} | grep "final points" | awk '{print $3}'`
#        mv ${SCENARG}.emp $TMPDIR/${SCENARG}.baerli.emp
#        mv ${SCENARG}.osf $TMPDIR/${SCENARG}.baerli.osf
#        cd - &> /dev/null
#        echo "baerli scored $BAERLIPOINTS points."
#
#        tar -C $TMPDIR -jcf $EMPTRACEOUTPUTDIR/${SCENARG}.$BAERLIPOINTS.baerli.emp.tar.bz2 ${SCENARG}.baerli.emp &> /dev/null
#        mv $TMPDIR/${SCENARG}.baerli.osf $BINTRACEOUTPUTDIR/${SCENARG}.$BAERLIPOINTS.baerli.osf
#	
#    fi
#    rm $TMPDIR/* &> /dev/null
#fi






