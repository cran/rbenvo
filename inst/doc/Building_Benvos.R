## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup,results='hide',message=FALSE,warning=FALSE-------------------------
library(rbenvo)

## ----LA_data------------------------------------------------------------------
LA_schools

## -----------------------------------------------------------------------------
LA_FF

## ----convert_demo-------------------------------------------------------------
LA_schools <- sf::st_as_sf(LA_schools,coords=c("Longitude","Latitude"),crs=4326)

## ----convert_hide,echo=F------------------------------------------------------
FFR <- sf::st_as_sf(LA_FF,coords=c("Latitude","Longitude"),crs=4326)

## -----------------------------------------------------------------------------
benvo(LA_schools,by='cdscode')

## -----------------------------------------------------------------------------
benvo(LA_schools,by='cdscode') %>% 
  add_BEF(FFR,bef_id = 'osm_id') %>% 
  activate(FFR) -> bdf
bdf

## ----dplyrverbs---------------------------------------------------------------
bdf <- bdf %>% 
  mutate(Distance = as.numeric(Distance/1E3)) %>% 
  filter(Distance<=10) %>% 
  distinct(cdscode,osm_id,Distance)
bdf

## -----------------------------------------------------------------------------
plot(bdf,'map') + ggplot2::theme_bw() + ggplot2::theme_void() 

## ----setup dates--------------------------------------------------------------
set.seed(342431)
LA_schools_five  <-  LA_schools %>% filter(Grade==5)
origin <- lubridate::as_date("2018-08-01")
school_start <- origin+sample(1:120,size = nrow(LA_schools_five),replace = TRUE)
measure_date <- lubridate::as_date("2019-05-01")
close_date <- measure_date
business_start_date <- origin + sample(-786:786,size = nrow(LA_FF),replace = TRUE)
business_close_date <- business_start_date + sapply((lubridate::today()-business_start_date),function(x) sample(1:x,1))
FFR <- LA_FF %>% 
  mutate(open=business_start_date,close=business_close_date)
schools <- LA_schools_five %>% mutate(open=school_start,measure=measure_date,close=close_date)

## ----create_benvo-------------------------------------------------------------
bdf <- benvo(schools,
      start_date_col="open",
      measurement_date="measure",
      stop_date_col="close",by='cdscode') %>% 
  add_BEF(FFR,bef_id = 'osm_id')

## -----------------------------------------------------------------------------
bdf %>% activate(FFR) %>% 
  select(-open,-close) ## for better display

## ----timeline-----------------------------------------------------------------
plot(bdf,'time')

