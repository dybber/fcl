Functional Computing Language
=============================
A low-level functional GPU language compiling to OpenCL.

Language.CGen
--------------
This module and submodules contain a library for generating OpenCL and
CUDA code. Currently only generating device code.

Language.FCL
----------------------
This aims to be a reimplementation of Obsidian as an external language
(not embedded), using CGen for code-generation.

cases
-----
This directory contains different use-cases which we want to
support. Read the README in that directory for more info.

microcl
-------
A small OpenCL wrapper library, making it a lot easier to write
host-code :-)


OpenCL installation
-------------------
Recommendation: Use AMD drivers for development, even on Intel CPUs,
as their compiler is more polished and gives better error messages.

    sudo ./AMD-APP-SDK-v3.0.130.136-GA-linux64.sh # install in /opt
    sudo ln -s /opt/AMDAPPSDK-3.0/lib/x86_64/sdk/* /usr/lib/
    sudo ldconfig


More info: http://wiki.tiker.net/OpenCLHowTo