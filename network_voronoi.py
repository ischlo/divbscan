import osmnx as ox
import geopandas as gpd
import pandas as pd
import networkx as nx
import random as rd
####
# 
# sf_nodes = gpd.read_file('data/sf_all_'+city+'.gpkg',layer='nodes').set_index('osmid')
# sf_edges = gpd.read_file('data/sf_all_'+city+'.gpkg',layer='edges').set_index(['u', 'v', 'key'])
# 
# sf_all = ox.utils_graph.graph_from_gdfs(gdf_nodes=sf_nodes,gdf_edges=sf_edges)

nx_df=r.nx_graph

node_id = r.py_node_id

sf_all = nx.from_pandas_edgelist(nx_df,source='from',target='to',edge_attr='dist')

# nx.get_edge_attributes(sf_all,name='dist')

if sf_all.is_directed():
  sf_all = sf_all.to_undirected()

# r.sf_all
# edges_df = pd.DataFrame(r.sf_all['data'])
# sf_all=nx.from_pandas_edgelist(edges_df,source='from',target='to',edge_attr='dist')

# type(rd.choice([x for x in sf_all.nodes]))

# local_max_nodes = [str(x) for x in r.local_max_nodes]

net_vor=nx.voronoi_cells(G=sf_all,center_nodes=node_id,weight='dist')

max([len(x) for x in net_vor.values()])

net_vor_dict = {str(x):list(int(n) for n in net_vor[x]) for x in net_vor }
# net_vor_dict
