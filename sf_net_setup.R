# Setting up the network
library(sf)
library(cppRouting)
library(data.table)
library(Btoolkit)

source('cppr_network_setup.R')

####

# city <- py$city
# 

network_filepath <- paste0('data/sf_all_',city,'.gpkg')

if(file.exists(paste0('data/networks/',city,'_all.rds'))) {
  
  # print('File already present.\n Delete to recreate')
  cli::cli_alert_info('Loading existing network data.')
  network_ <- rlist::list.load(paste0('data/networks/',city,'_all.rds'))
  sf_all <- network_$graph
  sf_all_ch <- network_$contracted
  cli::cli_alert_success("network loaded")
  
} else if(all(sapply(c(paste0(network_filename,"road_segments.csv")
                       ,paste0(network_filename,"nodes.csv")),FUN = file.exists))){
  
  edges <- data.table::fread(paste0(network_filename,"road_segments.csv"))
  nodes <- data.table::fread(paste0(network_filename,"nodes.csv"))
  
  sf_all <-  make_cppr_net(edges = edges
                           ,nodes = nodes[!duplicated(id),])
  
  sf_all_ch <- sf_all |> cppRouting::cpp_contract()
  
  # nodes <- sf_nodes[,.(osmid,geom)]
  
  if(!dir.exists("data/networks")) dir.create("data/networks")
  
  list('graph'=sf_all
       ,'contracted' = sf_all_ch) |> rlist::list.save(paste0('data/networks/',city,'_all.rds'))
  
} else if (file.exists(network_filepath)) {
  
  sf_nodes <- sf::st_read(network_filepath,layer='nodes') |> as.data.table()
  
  sf_edges <- sf::st_read(network_filepath,layer = 'edges') |> as.data.table()
  
  ####
  
  sf_edges[,unique(highway)]
  
  links_to_exclude <- c('motorway',"motorway_link")
  
  sf_edges <- sf_edges[!(highway %in% links_to_exclude),]
  
  ### make sure a from and to column is present
  if(all(c('u','v') %in% colnames(sf_edges))) {
    sf_edges[,c('from','to') := list(as.character(u),as.character(v))]
  }
  
  if(!('osmid' %in% colnames(sf_nodes))){
    sf_nodes[,'osmid' := id]
  }
  
  if(!all(c('x','y') %in% colnames(sf_nodes))){
    sf_nodes[,c('x','y') := as.data.frame(sf::st_coordinates(geom))]
  }

  sf_all <- make_cppr_net(edges = sf_edges
                          ,nodes = sf_nodes)

  sf_all_ch <- sf_all |> cppRouting::cpp_contract()
  
  # nodes <- sf_nodes[,.(osmid,geom)]
  
  if(!dir.exists("data/networks")) dir.create("data/networks")
  
  list('graph'=sf_all
       ,'contracted' = sf_all_ch) |> rlist::list.save(paste0('data/networks/',city,'_all.rds'))
  
} else {
  cli::cli_abort("Cannot create network.")
}
