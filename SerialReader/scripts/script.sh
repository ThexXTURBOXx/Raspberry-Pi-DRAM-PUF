#!/bin/bash

#Number of dumps generated for each measurement configuration
RUNS=10

#Check number of arguments and whether dir already exists
if [ $# != 1 ]; then
	echo "Please specify only the directory name"
exit
fi
if [ -e $1* ]; then
        echo "$1 already exists. Please use a different name for directory"
exit
fi

mkdir $1
pushd $1

#Needed when we want to start climate chamber simultaneously with this script
#echo "Sleeping for 1 hour"
#sleep 1h
echo "Starting with measurements"

#Filenames of created .bin
FILEPREFIX=run_
echo "Generated .bin will have the scheme run_TIME_ATTEMPT.bin"

#function for running 10 measurements for a given decay time
function measure {
  #create directory
  mkdir $1
  pushd $1

  #set dump file name
  NAME=$FILEPREFIX$1_
  echo "Directory name: $1 ; Measurement in seconds: $2"
  #run cmd for PUF
  echo "Collecting $RUNS file(s) for $2 sec decay time"
  ########
  ~/SerialReader -p 0 -p 0 -p 0 -p $3 -p $4 -p $5 -p 1 -p 1 -p $2 -o $NAME -m $RUNS
  ########
  echo "Run for $2 seconds decay time completed."
  popd
}

function area {
  mkdir $1$2
  pushd $1$2

  echo "Measuring in area [$1-$2] with $3 as challenge"
  measure 10s 10 $1 $2 $3

  measure 30s 30 $1 $2 $3

  measure 1min 60 $1 $2 $3

  measure 2min 120 $1 $2 $3

  measure 3min 180 $1 $2 $3

  measure 4min 240 $1 $2 $3

  measure 5min 300 $1 $2 $3

  measure 6min 360 $1 $2 $3

  measure 7min 420 $1 $2 $3

  measure 8min 480 $1 $2 $3

  measure 9min 540 $1 $2 $3

  measure 10min 600 $1 $2 $3

  measure 15min 600 $1 $2 $3

  measure 20min 600 $1 $2 $3

  measure 25min 600 $1 $2 $3

  measure 30min 600 $1 $2 $3

  measure 40min 600 $1 $2 $3

  measure 50min 600 $1 $2 $3

  measure 60min 600 $1 $2 $3

  echo "$1 and $2 done"
  popd
}


area c3 c38 000000000

area c38 c4 fffffffff

area c4 c48 fffffffff

area c48 c5 000000000

area c5 c58 fffffffff

area c58 c6 000000000

area c6 c68 000000000

area c68 c7 fffffffff


echo "Completed all runs."
popd
