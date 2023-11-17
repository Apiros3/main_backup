

import networkx as nx
import numpy as np
import matplotlib.pyplot as plt
import netgraph

# G = nx.Graph()
# G.add_edges_from(
#     [('A', 'B'), ('A', 'C'), ('D', 'B'), ('E', 'C'), ('E', 'F'),
#      ('B', 'H'), ('B', 'G'), ('B', 'F'), ('C', 'G')])

# nx.draw(G, cmap = plt.get_cmap('jet'))
# plt.show()


G = nx.DiGraph()

G.add_edge('1','2')
G.add_edge('1','3')
G.add_edge('3','2')
G.add_edge('3','4')
G.add_edge('4','3')

nx.draw(G, node_color='w', edgecolors='k', width=2.0, with_labels=True)
plt.show()