library(tidyverse)
load("fish_data.Rdata")

library(plyr)

##ddply
##adding column denoting depths denoting "shallow" and "deep"
##test suset to make sure all functions inside the"ddply" umbrella function work
##Sandwich function =lines 11-29
d=fish[fish$depth_fac=="Deep",] ##subsetting to get only ones in depth fac that are "deep"
##using, fish, make a subset "x" that is the different levels in "depth_fac"
nd=ddply(.data=fish, .variables="depth_fac",function(x){
  
  
 z=unique(x$depth_fac) 

 depth_condition=function(y){
   if(y=="Deep")
     q=50
   else if (y=="Mid")
   q=25
   else 
    q=15
 }
 
 x$depth_z=depth_condition(y = z)
 
  return(x)
 
}, .inform = T, .progress="text")



x$depth=depth_condition (y="Mid")
test
head(d) ##subsetter "d" section of data

head(nd)
unique(nd$depth_z)  ## should get 3 values based on the if else conditions it was given in the "sandwich function"

##adply----

#list all the physical data files in a given directory
batch_data=list.files("batch_data/", full=TRUE, pattern="ISIIS")
batch_data

##Umbrella Function with "adply" = lines 49-69
##phy will have all the months data collapsed into a SINGLE dataframe
phy=adply(batch_data, 1, function(file){
  
  
  #read the data
  d=read.table(batch_data[1], sep="\t", skip=10, header=TRUE, fileEncoding = "ISO-8859-1", stringsAsFactors = FALSE,
               quote="\"",check.names = FALSE, encoding = "UTF-8", 
               na.strings ="9999.99")
  
  #clean names
  head=names(d)
  head=str_replace(head, "\\(.*\\)", "")
  head=str_trim(head)
  head=make.names(head)
  head=tolower(head)
  head=str_replace(head,fixed(".."), ".")
  
  ##assign names
  names(d)=head
  
    #create a proper date+time format
  )
  
  
  
  
  
  
  
  
  
}, .inform=T, .progress="text")




