import osmnx as ox
from os.path import exists

ox.settings.timeout=9000

#####
#  SF bbox
#   north =37.8152
#   ,south= 37.6803
#   ,east = -122.3527
#   ,west = -122.5171

city = r.city

bbox_of_interest = {
  'ny': { 'west':-74.25884,'east':-73.70023,'south':40.47658,'north': 40.91763 }
  ,'sf': { 'west':-122.5171,'east':-122.3527,'south':37.6803,'north':37.8152 }
  ,'paris': { 'west':1.9569,'east':2.8413,'south':48.5684  ,'north': 49.0784 }
  ,'copenhagen':{ 'west':12.4842,'east':12.6580,'south':55.6313  ,'north': 55.7258 }
}

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
  sf_net = ox.graph_from_bbox(
    west= bbox_of_interest[city]['west']
    ,east= bbox_of_interest[city]['east']
    ,south= bbox_of_interest[city]['south']
    ,north= bbox_of_interest[city]['north']
    ,network_type='all'
    ,simplify=True
    ,retain_all=False
    ,truncate_by_edge=True
    ,clean_periphery=True
    ,custom_filter=None
  )
  
  ox.save_graph_geopackage(sf_net,net_filepath)
else :
  print('Raw network file exists')

print('finished!')
