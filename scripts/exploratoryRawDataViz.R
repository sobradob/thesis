
Data_Start<- as.POSIXct("2015-09-07 01:55:00")
Data_End  <- as.POSIXct("2015-09-10 01:59:00")

exploreDates <- function(Data_Start= "2017-02-15", Data_End = "2017-02-17"){
  
  # helper function
  create_time_formula <- function(lhs, rhs) {
    
    if(!inherits(lhs, c("character", "Date", "POSIXct"))) {
      stop("LHS must be a character or date")
    }
    if(!inherits(rhs, c("character", "Date", "POSIXct"))) {
      stop("RHS must be a character or date")
    }
    
    if(inherits(lhs, "Date")) {
      lhs <- as.character(lhs)
    } else if (inherits(lhs, "POSIXct")) {
      lhs <- gsub(" ", " + ", lhs)
    }
    
    if(inherits(rhs, "Date")) {
      rhs <- as.character(rhs)
    } else if (inherits(rhs, "POSIXct")) {
      rhs <- gsub(" ", " + ", rhs)
    }
    
    rlang::new_formula(lhs, rhs)
  }
  
  time_formula <- create_time_formula(Data_Start, Data_End)
  # new scatterplot
  all %>%
    select(time,lat,lon,accuracy) %>%
    as_tbl_time(index = time) %>%
    time_filter(time_formula = time_formula) %>%
    filter(accuracy > 1000) %>% 
    leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
    addTiles() %>%
    addCircles(lng = ~lon, lat = ~lat,
               radius = ~accuracy, fillOpacity = 0.02,color = "#DF2935")%>%
    addProviderTiles(providers$CartoDB.Positron)
  
}

exploreDates("2014-10-27","2014-10-27")

