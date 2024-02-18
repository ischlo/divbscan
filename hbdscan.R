library(data.table)
library(sf)
library(dbscan)
library(dplyr)
library(tmap)

####

manh <- osmdata::getbb('Manhattan,New York')

manh_sf <- Btoolkit::make_poly(manh)

# manh_sf |> tmap::qtm()
amenities <- sf::st_read('data/amenities_clean.geojson')
####
amenities_manh <- amenities |> filter(as.vector(sf::st_intersects(geometry,manh_sf,sparse=FALSE))) |> 
  st_transform(32118)

amenities_coord <- amenities_manh |> sf::st_coordinates()

####

distances <- 1:10*10

hdb <- lapply(distances,FUN = \(d) dbscan::dbscan(amenities_coord,eps = d,minPts = 5))

object.size(hdb)

sapply(hdb,FUN = \(db) db$cluster |> unique() |> length())


space_clusterings <- mapply(hdb,distances,SIMPLIFY = FALSE,FUN = \(db,dis) {
  
  amenities_manh |> 
    mutate(clus=db$cluster
           # ,distance=rep_len(dis,length.out=length(db$cluster))
           ) |> 
    group_by(clus) |> 
    summarise(geometry=sf::st_combine(geometry)) |> 
    sf::st_convex_hull()

})

space_clusterings

# amen_neighb[290,] |> 
#   sf::st_convex_hull() |>
#   tmap::qtm()
# 
# res <- list()
# for(i in 1:nrow(amen_neighb)){
#   print(i)
#   res <- append(res,st_concave_hull(amen_neighb[i,],ratio = .2,allow_holes = FALSE))
# }

tmap::tmap_mode('view')
tmap::qtm(space_clusterings[[10]]
          ,fill = 'MAP_COLORS'
          ,fill.alpha=.6)

