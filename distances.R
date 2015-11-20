
#load library
library(stringdist)

#create function for calculating ultrametric distance
ultrametric <-function(a,b,c){

  #calculate ultrametric distance
  x = 2^-(stringdist(a,b,method = 'osa'))
  y = 2^-(stringdist(a,c,method = 'osa'))
  z = 2^-(stringdist(b,c,method = 'osa'))
  
  compute = list(round(x*MAX,4),round(y*MAX,4),round(z*MAX,4))
  return(list(compute))
}
