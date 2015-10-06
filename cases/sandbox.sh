#!/bin/bash 

case $1 in 
    "init") 
	echo "Initialising sandbox" 
	echo "sandbox.sh: Only need to init sandbox once"
	cabal sandbox init 
	;;
    "obsidian") 
	echo "Installing Obsidian into sandbox" 
	cabal install ./Obsidian/.
	;;
    "sobol") 
	echo "Installing Sobol into sandbox" 
	cabal install ./ObsidianSobol/.
	;;
    *) 
    echo "provide argument init or obsidian"
    ;;
esac 
