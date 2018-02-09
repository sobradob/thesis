# adding all R packages citations

library(bibtex)
listOfPackages <- c("dplyr","ggplot2","base",
                    "ggthemes","sp","pandoc","citr",
                    "rstudio","tidyr","leaflet","tibbletime",
                    "padr","bibtex","gridExtra","scales","papaja","knitr","kableExtra")

write.bib(listOfPackages, file='../rReferences')
