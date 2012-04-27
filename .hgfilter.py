from mercurial import templatefilters

# for now this is just for phases
def extsetup(ui):
    templatefilters.filters["phasecolor"] = lambda x: (['1;31m','0;32m','0;34m'][x])
    templatefilters.filters["fill180"] = lambda x: templatefilters.fill(x,180)

