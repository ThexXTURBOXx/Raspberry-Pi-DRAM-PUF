#!/bin/bash

make clean
cd arm_chainloader
make clean
cd ..

set -e

echo "-----------------------------------------"
echo "Building chainloader ..."
echo "-----------------------------------------"
cd arm_chainloader
make -j4
echo "-----------------------------------------"
echo "Building firmware ..."
echo "-----------------------------------------"
cd ..
make -j4

# stage through WSL
if [ "$1" = "sw" ]; then
	tools/wslstage.py
fi