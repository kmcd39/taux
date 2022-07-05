devtools::document()
devtools::load_all()
library(tidyverse)
library(sf)


states <- c(9 , 44)
yrs <- c(2012, 2019)
geog <- 'tract'


ts <- tidycensus2recoded.tblList(states, yrs
                                 ,geo = geog)

ts

ms <- pull.tidycensus.median.tables(states, yrs
                                    ,geo = 'tract')

tots <- gett.census.totals(states, yrs,
                   geo = geog)
tots %>% count(yr)



tmp %>% names()


# county subarea ----------------------------------------------------------

twns <- mass.acs.pull.wrapper(states, yrs,
                      geo = 'county subdivision')
meta <- acs.tbl.index()
meta
twns$B25058
