from mercurial import templatefilters

# for now this is just for phases
def extsetup(ui):
    templatefilters.filters["phasecolor"] = lambda x: (['0;34m','0;32m','1;31m'][x])
    templatefilters.filters["fill180"] = lambda x: templatefilters.fill(x,180)

