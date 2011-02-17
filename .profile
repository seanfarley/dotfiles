export EDITOR=vi
export PROJECTS=$HOME/projects
export FACETS=$PROJECTS/facets
export RESEARCH=$PROJECTS/research
export PYTHONPATH=$HOME/local/lib/python2.6/site-packages:$HOME/local/lib/python2.6

##
# PETSc commands
##

alias pd="echo \$PETSC_DIR"
alias pdset="export PETSC_DIR=\$PWD"
alias pa="echo \$PETSC_ARCH"
alias pa-sieve="export PETSC_ARCH=darwin10.6.0-sieve-debug"
alias pa-cxx="export PETSC_ARCH=darwin10.6.0-cxx-debug"

export PETSC_DIR=$PROJECTS/petsc/petsc-3.1
export PETSC_ARCH=darwin10.6.0-cxx-debug

##
# Directory shortcuts
##

alias semester="cd ~/Documents/Class/2009\ Spring"
alias projects="cd \$PROJECTS"
alias sb="cd \$PROJECTS/sandbox"
alias sp="cd \$PROJECTS/scienceports"
alias fb="cd \$PROJECTS/aperture/fb-aperture-dev"
alias resume="cd \$PROJECTS/resume"
alias talks="cd \$PROJECTS/talks"
alias bout="cd \$PROJECTS/bout/bout++-dev/bout++"
alias gonzo="cd \$PROJECTS/gonzo"
alias proposals="cd \$PROJECTS/proposals"
alias hpc="cd /opt/hpc"
alias petsc="cd \$PROJECTS/petsc"
alias petsc-3.1="export PETSC_DIR=\$PROJECTS/petsc/petsc-3.1;export PETSC_ARCH=darwin10.6.0-cxx-debug"
alias petsc-dev="export PETSC_DIR=\$PROJECTS/petsc/petsc-dev;export PETSC_ARCH=darwin10.6.0-cxx-debug"
alias petsc-port="export PETSC_DIR=/opt/local;export PETSC_ARCH="
alias facets="cd \$FACETS"
alias research="cd \$RESEARCH"
alias facets-env="export PETSC_DIR=; export PETSC_ARCH=; source \$PROJECTS/facets/contrib/facetspkgs.sh; source \$PROJECTS/facets/internal/facetsall.sh"
alias facets-activate="sudo port activate gfortran @4.5.0_0; sudo port activate mpich2 @1.3.1_0+hydra; sudo port deactivate py26-netcdf netcdf hdf5-18 py26-matplotlib py26-tables py26-scientific py26-scipy py26-numpy fftw fftw-3"
alias facets-deactivate="sudo port activate hdf5-18 @1.8.5-patch1_0+cxx+mpich2; sudo port activate netcdf @4.1.1_0+mpich2+netcdf4; sudo port activate fftw @2.1.5_2+gfortran; sudo port activate fftw-3 @3.2.2_0+i386; sudo port activate py26-numpy @1.5.1_1; sudo port activate py26-scipy @0.8.0_0+no_atlas; sudo port activate py26-scientific @2.8_2; sudo port activate py26-matplotlib @1.0.0_0+gtk2+qt4; sudo port activate py26-tables @2.2.1_0; sudo port activate py26-netcdf @0.9.3_0"

##
# SSH tunnels
##

alias ssh-anl="ssh -q -C -N -D 9999 login.mcs.anl.gov -L10722:petsc.mcs.anl.gov:22 -L3389:kronosts.it.anl.gov:3389" 
alias ssh-lsu="ssh -q -C -N -D 9999 julia.math.lsu.edu"
alias ssh-iit="ssh -q -C -N -D 9999 laplace.math.iit.edu -L10723:karlin.math.iit.edu:22"
alias ssh-petsc="ssh -p 10722 petsc-local"
alias ssh-karlin="ssh -p 10723 karlin-local"
alias ssh-ubuntu="ssh -p 10724 ubuntu-local"
alias ssh-snowleopard="ssh -p 10725 snowleopard-local"


##
# TAU commands
##
#export TAU_MAKEFILE=$PROJECTS/sandbox/tau-bin/apple/lib/Makefile.tau-mpi-pdt
#export PATH=$PROJECTS/sandbox/pdt-bin/apple/bin:$PROJECTS/sandbox/tau-bin/apple/bin:$PATH

# MacPorts Installer addition on 2009-09-12_at_17:33:52: adding an appropriate PATH variable for use with MacPorts.
export PATH=$HOME/local/bin:/opt/local/bin:/opt/local/sbin:$PATH

if [ -f /opt/local/etc/bash_completion ]; then
        . /opt/local/etc/bash_completion
fi
if [ -f $HOME/local/etc/bash_completion ]; then
        . $HOME/local/etc/bash_completion
fi
