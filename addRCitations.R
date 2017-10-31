# adding all R packages citations

library(Ref)
listOfPackages <- c("dplyr","ggplot2","base","ggthemes","sp1","sp2","pandoc","citr","rstudio")

write.bib(listOfPackages, file='rReferences')
