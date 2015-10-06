#!/bin/bash 

case $1 in 
    "init") 
	echo "Initialising sandbox" 
	echo "sandbox.sh: Only need to init sandbox once"
	cabal sandbox init 
	;;
    "obsidian") 
	echo "Install Obsidian into sandbox" 
	cabal install ./Obsidian/.
	;;
    *) 
    echo "provide argument init or obsidian"
    ;;
esac 
