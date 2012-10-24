#!/usr/bin/env python
"""
Uses direct SQLite to compute the recursive dependecies of a port
"""

# import matplotlib.pyplot as plt
import networkx as nx
import sqlite3 as lite

def rdependents(cur,g,name):
    g.add_node(name)
    cur.execute("""
        SELECT dependencies.id,ports.name FROM dependencies
        INNER JOIN ports ON dependencies.id = ports.id
        WHERE dependencies.name='%s' AND ports.state='installed'""" % name
    )
    for r in cur.fetchall():
        rdependents(cur,g,r[1])
        g.add_edge(name,r[1])

g=nx.DiGraph()

con = lite.connect('/opt/local/var/macports/registry/registry.db')

with con:

    cur = con.cursor()

    rdependents(cur,g,'mpich2')

    d=nx.dfs_tree(g,'mpich2')

    for i in nx.dfs_postorder_nodes(d,'mpich2'):
        print i

    # nx.draw_graphviz(d) # only available if pydot is installed
    # plt.show()
