
#load pre-defined libraries
source('Text_Processing.R')
source('distances.R')

#load data
tweets <-read.csv(file = file.choose(),stringsAsFactors = FALSE,nrows = 20)

#==================== Create Ultrametric Matrix ===================================================

m = matrix(data = 0,nrow = nrow(tweets),ncol = nrow(tweets))

for (i in 1:nrow(tweets)){
  for (j in 1:nrow(tweets)){
    if(i-j < 0){
      m[i,j] <- mod_ultrametric(tweets$text[i],tweets$text[j])
    }
  }
}

colnames(m) <-c(1:nrow(tweets))

df = t(as.data.frame(sort(m[1,1:ncol(m)])))
#==================================================================================================|
#                                SOLUTION 
#==================================================================================================|

avalanche <-function(tweets){
  
  #create empty data frane
  frame = data.frame()
  
  #calculate ultrametric distance
  for(i in 1: nrow(tweets)){
    f <- mod_ultrametric(tweets$text[1],tweets$text[i])
    frame <- rbind(frame,f)
    rm(f)
  }
  
  #rename columns
  #colnames(m) <-c(1:nrow(tweets))
  
  #sort row in decreasing mode
  df <-t(as.data.frame(sort(m[1,1:ncol(m)])))
  #colnames(df) <-c("leaf")
  
  #return sorted column
  return(frame)
}

#==================================================================================================|
#                             VISUALISATION
#==================================================================================================|
library(igraph)

graph <-graph.adjacency(m,mode = 'undirected')
plot(graph)

#==================================================================================================|
#                            RECURSION
#==================================================================================================|

#get subset
s <-tweets[c(4,11),]

mat = matrix(data = 0,nrow = nrow(s),ncol = nrow(s))

for (i in 1:nrow(s)){
  for (j in 1:nrow(s)){
    if(i-j < 0){
      mat[i,j] <- mod_ultrametric(s$text[i],s$text[j])
    }
  }
}

colnames(mat) <-c(4,11)

df = t(as.data.frame(sort(mat[1,1:ncol(mat)])))

#=================================================================================================|
#                                    Data Table
#=================================================================================================|
library(data.table)

dt = data.table()
dt[list(m)]

