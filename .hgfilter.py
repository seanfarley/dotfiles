from mercurial import templatefilters

# for now this is just for phases
def extsetup(ui):
    templatefilters.filters["permute"] = lambda x: ([3,2,1][x])

