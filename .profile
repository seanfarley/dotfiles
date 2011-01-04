export EDITOR=vi
export PROJECTS=$HOME/projects
export FACETS=$PROJECTS/facets
export RESEARCH=$PROJECTS/research
export PYTHONPATH=$HOME/local/lib/python2.6

##
# PETSc commands
##

alias pd="echo \$PETSC_DIR"
alias pdset="export PETSC_DIR=\$PWD"
alias pa="echo \$PETSC_ARCH"

export PETSC_DIR=$PROJECTS/petsc/petsc-3.1
export PETSC_ARCH=darwin10.5.0-cxx-debug

##
# Directory shortcuts
##

alias semester="cd ~/Documents/Class/2009\ Spring"
alias projects="cd \$PROJECTS"
alias sb="cd \$PROJECTS/sandbox"
alias sp="cd \$PROJECTS/scienceports"
alias fb="cd \$PROJECTS/aperture/fb-aperture-dev"
alias ge="cd \$PROJECTS/general-exam"
alias resume="cd \$PROJECTS/resume"
alias talks="cd \$PROJECTS/talks"
alias bout="cd \$PROJECTS/bout/bout++-dev/bout++"
alias gonzo="cd \$PROJECTS/gonzo"
alias proposals="cd \$PROJECTS/proposals"
alias hpc="cd /opt/hpc"
alias petsc="cd \$PROJECTS/petsc"
alias petsc-3.1="export PETSC_DIR=\$PROJECTS/petsc/petsc-3.1;export PETSC_ARCH=$PETSC_ARCH"
alias petsc-dev="export PETSC_DIR=\$PROJECTS/petsc/petsc-dev;export PETSC_ARCH=$PETSC_ARCH"
alias petsc-port="export PETSC_DIR=/opt/local;export PETSC_ARCH="
alias facets="cd \$FACETS"
alias research="cd \$RESEARCH"
alias mp="cd /opt/local/var/macports-local"

##
# SSH tunnels
##

alias ssh-anl="ssh -q -C -N -D 9999 login.mcs.anl.gov -L10722:petsc.mcs.anl.gov:22 -L3389:kronosts.it.anl.gov:3389" 
alias ssh-lsu="ssh -q -C -N -D 9999 julia.math.lsu.edu"
alias ssh-iit="ssh -q -C -N -D 9999 laplace.math.iit.edu -L10723:karlin.math.iit.edu:22"
alias ssh-petsc="ssh -p 10722 petsc-local"
alias ssh-karlin="ssh -p 10723 karlin-local"
alias ssh-siam="ssh -p 10721 siam-local"


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
