#!/usr/bin/env python
"""
Uses direct SQLite to compute the recursive dependecies of a port
Usage:
port-rdependents.py mpich2
"""

import sys

# import matplotlib.pyplot as plt
import networkx as nx
import sqlite3 as lite

def rdependents(cur,g,name):
    g.add_node(name)
    cur.execute("""
        SELECT dependencies.id,ports.name,ports.version,ports.revision,ports.variants
        FROM dependencies INNER JOIN ports ON dependencies.id = ports.id
        WHERE dependencies.name='%s' AND ports.state='installed'""" % name
    )
    for r in cur.fetchall():
        # print "PORT: %s @%s_%s%s" % (r[1],r[2],r[3],r[4])
        rdependents(cur,g,r[1])
        g.add_edge(name,r[1])

g=nx.DiGraph()

con = lite.connect('/opt/local/var/macports/registry/registry.db')

with con:

    cur = con.cursor()

    # expected to pass argument of the port name
    if len(sys.argv) < 2:
        sys.exit(1)

    port = sys.argv[1]
    rdependents(cur,g,port)

    d=nx.dfs_tree(g,port)

    if d.nodes():
        for i in nx.dfs_postorder_nodes(d,port):
            print i

    # nx.draw_graphviz(d) # only available if pydot is installed
    # plt.show()
