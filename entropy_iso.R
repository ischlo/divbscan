entropy_iso <- function(d
                        ,iso
                        ,by_='amenity'
                        ,cor_num = 1){
  
  # make sure that the isochrones data is perfectly alligned by row with the data.
  int <- sf::st_intersects(iso, d)
  # once we have made the intersection, we need to check that they all intersect at least with their own amenity
  # this is not the case every time because some amenities are in locations with no roads around
  # while the isochrones uses the underlyinig road network to build the areas.
  checks <- map(int,length) |> unlist()
  # when the intersection is zero, we impose that there is just the amenity for which the isochrone is computed
  bad_values <- which(checks == 0)
  # put the index of the amenity itself in the intersection
  int[bad_values] <- -1 #bad_values
  
  d <- data.table::as.data.table(d)
  
  entropy <- parallel::mclapply(int, mc.cores = cor_num, FUN = \(i) {
    if(all(i!=-1)){
      counted <- d[i,.N,by=by_]
      p <- counted$N/sum(counted$N)
      ubiq <- nrow(counted)
      e <- c('entropy'=-sum(p*log(p))
             ,'size'=sum(counted$N)
             ,'ubiq'=ubiq
      )
      return(e)
    } else {
      return(c('entropy'=NA,'size'=0
               ,'ubiq'=0
      ))
    }
    
  })
  
  data.frame(matrix(entropy |> unlist(), ncol = 3, byrow = TRUE)) |> `colnames<-`(c('entropy','size'
                                                                                    ,'ubiquity'
  ))
}

#####