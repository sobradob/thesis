# results df

resultsDf <- data.frame( mean5min = c(82,43,269,426),
                         median5min = c(4,0,0,0),
                         mean1hr = c(345,497,908,1502),
                         median1hr = c(6,4,0,0),
                         mean1day = c(9273,NA,5757,14266),
                         median1day = c(12,NA,0,1288)
                         )

rownames(resultsDf) <- c("Barnett & Onella", "Palmius", "Sobrado","Naive Baseline")
colnames(resultsDf) <- c("Mean","Median",
                         "Mean","Median",
                         "Mean","Median")

# median 7 meters mean 60 not imputed 282 NA's Ian all data

library(knitr)
library(kableExtra)
options(knitr.kable.NA = '-')
kable(resultsDf, "html") %>%
  kable_styling("striped") %>%
  add_header_above(c(" " = 1, "Five Minutes" = 2, "One Hour" = 2,"One Day" = 2))

