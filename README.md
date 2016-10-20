Functional Computing Language
=============================
A low-level functional GPU language compiling to OpenCL.

cases
-----
The directory 'cases' contains different use-cases which we want to
support. Read the README in that directory for more info.

Dependencies
------------
 * [microcl](http://github.com/dybber/microcl) - A small OpenCL wrapper library in C, making it a lot easier to write host-code.
 * [cgen](http://github.com/dybber/cgen) - A small library for generating C/OpenCL/CUDA

*microcl* is included as a submodule. Initialise it with:

    git submodule init
    git submodule update

OpenCL installation
-------------------
Recommendation: Use AMD drivers for development, even on Intel CPUs,
as their compiler is more polished and gives better error messages.

    sudo ./AMD-APP-SDK-v3.0.130.136-GA-linux64.sh # install in /opt
    sudo ln -s /opt/AMDAPPSDK-3.0/lib/x86_64/sdk/* /usr/lib/
    sudo ldconfig

More info: http://wiki.tiker.net/OpenCLHowTo
