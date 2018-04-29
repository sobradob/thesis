# train line sampling frequency

library(readr)
objLog<- read_csv2("objectiveLog.csv")

objLog <- objLog %>%
  mutate( time = as.POSIXct(paste(Date, Time), format="%d-%m-%Y %H:%M:%S")) %>% 
  as_tbl_time(time)

objLog %>% filter(Location %in% c("Utrecht Centraal", "Amsterdam Centraal"))

ts<- objLog %>% arrange(time) %>% 
  mutate( nextStop = lead(Location),
          nextType = lead(Type),
          nextTime = lead(time)) %>% 
  filter( (Location == "Utrecht Centraal" & nextStop == "Amsterdam Centraal")|
          (Location == "Amsterdam Centraal" & nextStop == "Utrecht Centraal")  
           ) %>% 
  mutate(duration = nextTime - time) %>% 
  filter(Type == "Check-in") %>% 
  select(time,nextTime)

timeInput <- ts[1,1][[1]]
nextTime <- ts[1,2][[1]]

getSampFreq <- function(timeInput,nextTime){
  duration <- as.numeric(nextTime)-as.numeric(timeInput)
  fin %>% 
    filter(timestampMs >= as.numeric(timeInput) & timestampMs <= as.numeric(nextTime)) %>% 
    summarise(count = n(),
              hz = count/duration) %>% pull(hz)->sampFreq
  return(sampFreq)
}

hzs<- vector()
for( i in 1:nrow(ts)){
  hzs[i]<- getSampFreq(ts[i,1][[1]],ts[i,2][[1]])
}

mean(hzs)

r_refs(file = "../manuscript/r-references.bib")
my_citations <- cite_r(file = "r-references.bib")

,"r-references.bib"
