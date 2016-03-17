

#This is the main file


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

