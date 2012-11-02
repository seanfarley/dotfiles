#!/usr/bin/env python
"""
Uses direct SQLite to compute the recursive dependecies of a port,
plus the variants containing a compiler
Usage:
port-rdependents.py mpich2 gcc47
"""

import sys

# import matplotlib.pyplot as plt
import networkx as nx
import sqlite3 as lite

def variants(cur,name):
    cur.execute("""
        SELECT version,revision,variants,negated_variants FROM ports
        WHERE state='installed' AND name='%s'""" % name
    )

    r = cur.fetchone()
    if r:
        return "@%s_%s %s%s" % r
    return ''

def deps(cur,name,variant=''):
    d = []
    cur.execute("""
        SELECT ports.id, ports.name, ports.variants FROM dependencies
        INNER JOIN ports ON ports.name = dependencies.name
        WHERE dependencies.id IN (
            SELECT ports.id FROM ports WHERE ports.name='%s' AND ports.state='installed'
        ) AND ports.state='installed' AND ports.variants LIKE '%%%s%%'""" % (name,variant)
    )

    for r in cur.fetchall():
        d.append(r)

    return d

def dependents(cur,name):
    d = []
    cur.execute("""
        SELECT dependencies.id,ports.name,ports.version,ports.revision,ports.variants
        FROM dependencies INNER JOIN ports ON dependencies.id = ports.id
        WHERE dependencies.name='%s' AND ports.state='installed'""" % name
    )

    for r in cur.fetchall():
        d.append(r)

    return d

def rdependents(cur,g,name,comp=""):
    g.add_node(name, variants=variants(cur,name))

    # This following loop is an approximation. It will search the parents of all
    # dependents (and then their children). Ideally, we'd search for all port with the
    # +compiler variants
    if comp:
        d1 = deps(cur,name,comp)
        for r in d1:
            if not g.has_node(r[1]):
                rdependents(cur,g,r[1],comp)
                g.add_edge(name,r[1])

    d2 = dependents(cur,name)
    for r in d2:
        if not g.has_node(r[1]):
            rdependents(cur,g,r[1],comp)
        g.add_edge(r[1],name)

con = lite.connect('/opt/local/var/macports/registry/registry.db')

with con:

    cur = con.cursor()

    # expected to pass argument of the port name
    if len(sys.argv) < 2:
        sys.exit(1)

    port = sys.argv[1]
    comp = ''
    if len(sys.argv) > 2:
        comp = sys.argv[2]

    g = nx.DiGraph()
    rdependents(cur,g,port,comp)

    if g.nodes():

        for i in nx.topological_sort(g):
            print i,g.node[i]['variants']

        # nx.draw_graphviz(g) # only available if pydot is installed
        # plt.show()
