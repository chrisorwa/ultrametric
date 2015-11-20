
#load required libraries
library(tm)

#function to remove blank strings
remove_blanks <-function(a){
  ind = c()
  for (j in 1:length(a)){
    if(nchar(a[j]) == 0){
      ind = c(ind,j)
    }
  }
  
  if(is.null(ind) == TRUE){
    return(a)
  } else{
    return(a[-c(ind)])
  }
}

#function for processing text
process_text <-function(text){
  text = tolower(text)
  text = gsub('[[:punct:]]',"",text)
  text = gsub("http.*", "", text)
  text = stripWhitespace(text)
  text = strsplit(removeWords(as.character(text),stopwords('SMART')),split =" ")
  text = as.character(unlist(text))
  #text = remove_blanks(text)
  return(as.vector(text))   
}
