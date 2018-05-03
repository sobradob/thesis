
getRemoveIndex <- function(fin,thickenParam="5 min",seed=1234){
  set.seed(seed)
  removed<- fin%>%
    as_tbl_time(time) %>% 
    filter_time(~"2017-03") %>% 
    thicken(thickenParam,colname = "removed") %>%
    distinct(removed) %>% 
    sample_frac(.25)  %>% 
    pull(removed) %>%
    as.character()
  
  index <- fin%>%    as_tbl_time(time) %>% 
    thicken(thickenParam,colname = "removed")%>% pull(removed) %>% as.character()

  remove_ind <- which(index %in% removed)
  cat(head(as.character(removed)))
  
return(list(remove_ind,removed))
  
}

getRemoveIndexDay <- function(fin,thickenParam="day",seed=1234){
  set.seed(seed)
  removed<- fin%>%
    as_tbl_time(time) %>% 
    filter_time(~"2017-03") %>% 
    thicken(thickenParam,colname = "removed") %>%
    distinct(removed) %>% 
    sample_frac(.25)  %>% 
    pull(removed) %>%
    as.Date() %>% as.character()
  
  index <- fin%>%    as_tbl_time(time) %>% 
    thicken(thickenParam,colname = "removed")%>% pull(removed) %>% as.Date()%>% as.character()
  
  remove_ind <- which(index %in% removed)
  cat(head(as.character(removed)))
  
  return(list(remove_ind,removed))
  
}