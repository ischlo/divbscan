# store functions that are reused across scripts

no_na <- function(x) {
  return(x[!is.na(x)])
}

nn_hex <- \(neighbors, k = 1) {
  # this function in the future could be replace by network voronoi style one
  if(k%in% c(0,1)){ return(neighbors) }
  for(i in 2:k){
    neighbors <- lapply(neighbors,FUN = \(x) touching_filt[x] |> unlist() |> unique() |> no_na())
  }
  return(neighbors)
}

#####
mutual_information <- function(cont_tab,ind_prob) {
  
  ind_prob[which(cont_tab==0,arr.ind = TRUE)] <- 1
  
  mat_1 <- cont_tab
  
  mat_1[which(cont_tab==0,arr.ind = TRUE)] <- 1
  
  sum(c(cont_tab*log(mat_1/ind_prob)))
}
