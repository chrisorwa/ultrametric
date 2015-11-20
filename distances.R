
#load library
library(stringdist)

#settings
MAX = 1000000

#calculate ultrametric distance
ultrametric <-function(a,b,c){

  #calculate ultrametric distance
  x = 2^-(stringdist(a,b,method = 'osa'))
  y = 2^-(stringdist(a,c,method = 'osa'))
  z = 2^-(stringdist(b,c,method = 'osa'))
  
  compute = list(round(x*MAX,4),round(y*MAX,4),round(z*MAX,4))
  return(list(compute))
}

#modified function for calculating ultrametric distance
mod_ultrametric <-function(a,b){
  #split sentence to words
  a = strsplit(a,split = "")[[1]]
  b = strsplit(b,split = "")[[1]]
  
  #calculate ultrametric distance
  x = 2^-(length(intersect(a,b))+1)
  compute = round(x*MAX,2)
  return(compute)
}
