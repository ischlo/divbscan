library(plot.matrix)
library(plot3D)
library(RColorBrewer)

####
entropy <- \(vec) {
  
  # print(vec)
  
  vals <- unique(vec)
  # print(vals)
  
  probs <- sapply(vals,FUN = \(x) sum(vec==x))/length(vec)
  
  return(-sum(probs*log(probs)))
}

# entropy(vec=c(1,1,1,1,1,1))

# entropy(vec = c(1,2,3,4,5,6))==log(6)

entropy_grid <- \(pp,mesh_vec,d_lim){
  
  mesh_grid <- mesh(mesh_vec,mesh_vec,z=NA)
  mesh_grid$n <- mesh_grid$z
  # mesh_grid |> str()
  for (i in 1:length(mesh_vec)){
    for(j in 1:length(mesh_vec)){
      ps <- which(terra::distance(matrix(c(i,j),ncol = 2,byrow = TRUE),pp[,c(1,2)],lonlat=FALSE)<d_lim)
      mesh_grid$z[i,j,1] <- entropy(pp[ps,3])
      mesh_grid$n[i,j,1] <- length(pp[ps,3])
    }
  }
  mesh_grid$z <- mesh_grid$z/log(unique(length(pp[,3])))
  
  return(mesh_grid)
}

# this script looks at a simple model of categorical nodes palced on a grid and their diversity being measured.

grid_size <- 300

n_sample <- 1500

n_categories <- 90

mesh_size <- 10

mesh_coords <- 0:trunc(grid_size/mesh_size)*mesh_size

# initializing grid position of n_sample

set.seed(3)

x <- runif(n_sample,0,1)*grid_size |> round()

y <- runif(n_sample,0,1)*grid_size |> round()

categories <- (runif(n_sample,0,1)*(n_categories-1)) |> round() |> as.factor()

unique(categories)

entropy(categories)/log(n_sample)

hist(as.numeric(categories),breaks=n_categories+1)

#####

pal_fact <- leaflet::colorFactor('Set2',domain = unique(categories))

plot(x,y,col=pal_fact(categories),pch=20)

####

pal_entropy <- leaflet::colorNumeric(palette = 'viridis',domain = c(0,1))

p_pattern <- cbind(x,y,categories)

res <- entropy_grid(pp=p_pattern,mesh_vec = mesh_coords,d_lim =15)

hist(res$z,breaks=100)
hist(res$n,breaks=100)
# 
# plot(res$x[,,1] |> as.vector()
#      ,res$y[,,1]|> as.vector()
#      ,col=pal_entropy(res$z[,,1])
#      ,pch=20
#      ,cex=3.5
#      ,xlab='x'
#      ,ylab='y'
#      )
# points(x,y,col=pal_fact(categories),pch=19)

xlim_2 <- max(res$n)

plot(1:xlim_2,log(1:xlim_2)/log(xlim_2),col='darkred',type='l',xlim=c(1,xlim_2))

points(res$n |> as.vector()
     ,res$z |> as.vector()
     ,col='black')

max(res$z |> as.vector())

