# results df

resultsDf <- data.frame( mean5min = c(82,43,269,426),
                         median5min = c(4,0,0,0),
                         meanexpDist5min = c(NA,NA,414,NA),
                         medianexpDist5min = c(NA,NA,0,NA),
                         mean1hr = c(345,497,908,1502),
                         median1hr = c(6,4,0,0),
                         meanexpDist1hour = c(NA,NA,916, NA),
                         medianexpDis1hour = c(NA,NA,0, NA),
                         mean1day = c(9273,NA,5757,14266),
                         median1day = c(12,NA,0,1288),
                         meanexpDist1Day = c(NA,NA,5757, NA),
                         medianexpDis1Day = c(NA,NA,0, NA))

rownames(resultsDf) <- c("Barnett & Onella", "Palmius", "Sobrado","Naive")
colnames(resultsDf) <- c("Mean","Median","Expected Mean","Expected Median",
                         "Mean","Median","Expected Mean","Expected Median",
                         "Mean","Median","Expected Mean","Expected Median")

# median 12 meters mean 9273
library(knitr)
library(kableExtra)
options(knitr.kable.NA = '-')
kable(resultsDf, "html") %>%
  kable_styling("striped") %>%
  add_header_above(c(" " = 1, "Five Minutes" = 4, "One Hour" = 4,"One Day" = 4))

