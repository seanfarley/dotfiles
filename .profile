export EDITOR=vi
export PROJECTS=$HOME/projects
export FACETS=$PROJECTS/facets
export RESEARCH=$PROJECTS/research
export PYTHONPATH=$HOME/local/lib/python2.6/site-packages:$HOME/local/lib/python2.6
export HGRCPATH=$HOME/.hgrc

##
# Helpful shortcuts
##

shopt -s globstar
alias gda="gdb --args"
alias make_db="make RM=echo"
alias make_petsc="make -j4 -C \$PETSC_DIR/\$PETSC_ARCH"
alias g="grep"
alias e="echo"

##
# PETSc commands
##

alias pd="echo \$PETSC_DIR"
alias pdset="export PETSC_DIR=\$PWD"
alias pa="echo \$PETSC_ARCH"
alias pa-sieve="export PETSC_ARCH=arch-sieve"
alias pa-cxx="export PETSC_ARCH=arch-cxx"
alias pa-c="export PETSC_ARCH=arch-c"
alias pa-complex="export PETSC_ARCH=arch-c-complex"
alias pa-fast-c="export PETSC_ARCH=arch-fast-c"
alias pa-matlab="export PETSC_ARCH=arch-matlab"
alias pa-cuda="export PETSC_ARCH=arch-cuda"
alias ts="cd \$PETSC_DIR/src/ts/examples/tutorials"
alias snes="cd \$PETSC_DIR/src/snes/examples/tutorials"
alias ksp="cd \$PETSC_DIR/src/ksp/ksp/examples/tutorials"
alias vec="cd \$PETSC_DIR/src/vec/vec/examples/tutorials"

# Needed for arch-matlab
export DYLD_FALLBACK_LIBRARY_PATH=/Applications/MATLAB_R2011a.app/bin/maci64:/Applications/MATLAB_R2011a.app/sys/os/maci64

export PETSC_DIR=$PROJECTS/petsc/petsc-dev
export PETSC_ARCH=arch-c

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
alias bout="cd \$PROJECTS/bout/bout-dev/"
alias gonzo="cd \$PROJECTS/gonzo"
alias proposals="cd \$PROJECTS/proposals"
alias hpc="cd /opt/hpc"
alias petsc="cd \$PETSC_DIR"
alias petsc-3.1="export PETSC_DIR=\$PROJECTS/petsc/petsc-3.1;export PETSC_ARCH=darwin10.7.0-cxx-debug"
alias petsc-dev="export PETSC_DIR=\$PROJECTS/petsc/petsc-dev;export PETSC_ARCH=darwin10.7.0-cxx-debug"
alias petsc-port="export PETSC_DIR=/opt/local;export PETSC_ARCH="
alias facets="cd \$FACETS"
alias research="cd \$RESEARCH"
alias facets-env="export PETSC_DIR=; export PETSC_ARCH=; source \$FACETS/software/facetsall.sh"
alias facets-activate="sudo port activate gfortran @4.5.0_0; sudo port activate mpich2 @1.3.2p1_0+hydra; sudo port deactivate netcdf hdf5-18 fftw fftw-3"
alias facets-deactivate="sudo port activate hdf5-18 @1.8.6_0+cxx+mpich2; sudo port activate netcdf @4.1.1_0+mpich2+netcdf4; sudo port activate fftw @2.1.5_2+gfortran; sudo port activate fftw-3 @3.2.2_0+i386"

##
# SSH tunnels
##

alias ssh-anl="ssh -q -C -N -D 9999 login.mcs.anl.gov -L10722:petsc.mcs.anl.gov:22 -L3389:kronosts.it.anl.gov:3389" 
alias ssh-iit="ssh -q -C -N -D 9999 laplace.math.iit.edu -L10723:karlin.math.iit.edu:22"
alias ipython="ipython-2.7 -noconfirm_exit -nobanner"


##
# TAU commands
##
#export TAU_MAKEFILE=$PROJECTS/sandbox/tau-bin/apple/lib/Makefile.tau-mpi-pdt
#export PATH=$PROJECTS/sandbox/pdt-bin/apple/bin:$PROJECTS/sandbox/tau-bin/apple/bin:$PATH

export PATH=$HOME/local/bin:/opt/local/bin:/opt/local/sbin:$PATH

if [ -f /opt/local/etc/bash_completion ]; then
        . /opt/local/etc/bash_completion
fi
if [ -f $HOME/local/etc/bash_completion ]; then
        . $HOME/local/etc/bash_completion
fi
if [ -d /usr/local/cuda ]; then
        export PATH=/usr/local/cuda/bin:$PATH
fi
