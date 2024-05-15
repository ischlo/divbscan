import osmnx as ox
import geopandas as pd
from os.path import exists
import pyrosm as pyr
ox.settings.timeout=9000

#####
#  SF bbox
#   north =37.8152
#   ,south= 37.6803
#   ,east = -122.3527
#   ,west = -122.5171

city = r.city

bbox_of_interest= r.bbox_of_interest

output_path = r.output_path

# NY bbox
# west= -74.25884
# east= -73.70023
# south= 40.47658  
# north= 40.91763

# Paris
# west= 1.9569
# east= 2.8413
# south= 48.5684  
# north= 49.0784

bbox_of_interest[city]

net_filepath = "data/sf_all_"+city+".gpkg"

if not exists(net_filepath):
  if bbox_of_interest[city]['src']=='':
    sf_net = ox.graph_from_bbox(
      west= bbox_of_interest[city]['bbox'][1]
      ,east= bbox_of_interest[city]['bbox'][2]
      ,south= bbox_of_interest[city]['bbox'][3]
      ,north= bbox_of_interest[city]['bbox'][4]
      ,network_type='all'
      ,simplify=True
      ,retain_all=False
      ,truncate_by_edge=True
      ,clean_periphery=True
      ,custom_filter=None
    )
    ox.save_graph_geopackage(sf_net,net_filepath)
  elif bbox_of_interest[city]['src']!='': # experimental : from a local pbf file construct the network
    
    print('Extracting the network from the bbox in file '+bbox_of_interest[city]['src'])
    # Initialize the reader
    osm = pyr.OSM(output_path)
    # Get all walkable roads and the nodes 
    nodes,edges = osm.get_network(network_type="all",nodes=True)
    
    # nodes.set_index('id',inplace=True)
    # edges.set_index(['u','v','key'],inplace=True)
    # 
    # nodes.columns
    # edges.columns
    nodes.to_file(net_filepath, layer="nodes", driver="GPKG")
    edges.to_file(net_filepath, layer="edges", driver="GPKG")

    # # Create NetworkX graph
    # sf_net = osm.to_graph(nodes,edges,graph_type="networkx",network_type='all',osmnx_compatible=True)
    # 
    # # sf_net=ox.graph_from_gdfs(gdf_nodes=nodes,gdf_edges=edges)
    # 
    # # sf_net = ox.project_graph(sf_net)
    # # 
    # # sf_net = ox.consolidate_intersections(sf_net, rebuild_graph=True, tolerance=5,reconnect_edges=True, dead_ends=True)
    # # 
    # # sf_net = ox.project_graph(sf_net, to_crs=4326)
    # 
    # for node, data in sf_net.nodes(data=True):
    #   if 'osmid' in data:
    #     data['osmid_original']=data.pop('osmid')
    #   else:
    #     print('no osmid here')
    # 
    # ox.save_graph_geopackage(G=sf_net,filepath=net_filepath)
    
else :
  print('Raw network file exists')

print('finished!')
