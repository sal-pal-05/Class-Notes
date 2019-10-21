##10/21/19

library(tidyverse)
library(plyr)


data_set=list.files("Datasets/", full=T)

data1=adply(data_set, .margins = 1, function(file){
  
  ##read the data
  d= read_csv(file)
  
  return(d)
}, .inform = T)


save(data1, file="Datasets For Class")










