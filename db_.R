# postgis_thorough_r

library(DBI)
library(RPostgreSQL)

####

# check :https://r4ds.hadley.nz/databases for ways to do this in a temporary directory with duckdb
# duckDb does not have a nearest feature operator, but has a h3 extension
# , consider rewriting cppr$nearest neighbor with that in mind
# alternatively, explore setting up a temporary sqlite db with spatialite extension and nearest neighbour search there

conn <- DBI::dbConnect(RPostgreSQL::PostgreSQL())
# 
# res <- DBI::dbSendQuery(conn = conn,
#                         statement = 'CREATE EXTENSION postgis;')

if(DBI::dbExistsTable(conn = conn,'road_nodes')) DBI::dbRemoveTable(conn = conn,'road_nodes')

DBI::dbCreateTable(conn,
                   name = 'road_nodes'
                   # ,field = c('osmid'='varchar(20)'
                   #            ,'x'='real'
                   #            ,'y'='real')
                   ,as.data.frame(sf_all$coords))

nodes_ <- as.data.frame(sf_all$coords) |> sf::st_as_sf(coords=c(2,3),crs=4326)

sf::st_write(nodes_,dsn = conn,layer = 'road_nodes',layer_options = c("OVERWRITE=yes", "LAUNDER=true"))

res <- DBI::dbSendQuery(conn = conn
                        ,statement = 'CREATE index nodes_simp_idx ON road_nodes USING GIST(geometry);')

hex <- hexagons |> 
  select(h3_index,centroid) |> 
  # mutate(centroid=sf::st_as_text(centroid))
  sf::st_as_sf()

if(DBI::dbExistsTable(conn = conn,'hexagons')) DBI::dbRemoveTable(conn = conn,'hexagons')

sf::st_write(hex,dsn = conn,layer = 'hexagons',layer_options = c("OVERWRITE=yes", "LAUNDER=true"))

nn_search <- DBI::dbGetQuery(conn = conn
                             ,statement = 'SELECT road_nodes.osmid AS node, hexagons.h3_index AS h3_index FROM hexagons CROSS JOIN LATERAL (SELECT road_nodes.osmid, road_nodes.geometry <-> hexagons.centroid AS dist FROM road_nodes ORDER BY dist LIMIT 1 ) road_nodes;')

