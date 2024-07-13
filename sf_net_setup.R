# Setting up the network
library(sf)
library(cppRouting)
library(data.table)
library(Btoolkit)

source('cppr_network_setup.R')

####

# city <- py$city

if(!file.exists(paste0('data/networks/',city,'_all.rds'))) {

  network_filepath <- paste0('data/sf_all_',city,'.gpkg')
  
  sf_nodes <- sf::st_read(network_filepath,layer='nodes') |> as.data.table()
  
  sf_edges <- sf::st_read(network_filepath,layer = 'edges') |> as.data.table()
  
  ####
  
  sf_edges[,unique(highway)]
  
  links_to_exclude <- c('motorway',"motorway_link")
  
  sf_edges <- sf_edges[!(highway %in% links_to_exclude),]
  
  ### make sure a from and to column is present
  if(all(c('u','v') %in% colnames(sf_edges))) {
    sf_edges[,c('from','to'):=list(as.character(u),as.character(v))]
  }
  
  if(!('osmid'%in%colnames(nodes))){
    sf_nodes[,'osmid':=id]
  }
  
  if(!all(c('x','y')%in% colnames(sf_nodes))){
    sf_nodes[,c('x','y'):=as.data.frame(sf::st_coordinates(geom))]
  }

  sf_all <- make_cppr_net(edges=sf_edges
                          ,nodes = sf_nodes)

  sf_all_ch <- sf_all |> cppRouting::cpp_contract()
  
  # nodes <- sf_nodes[,.(osmid,geom)]
  
  if(!dir.exists("data/networks")) dir.create("data/networks")
  
  list('graph'=sf_all
       ,'contracted' = sf_all_ch) |> rlist::list.save(paste0('data/networks/',city,'_all.rds'))
  
  
} else {
  print('File already present.\n Delete to recreate')
}
