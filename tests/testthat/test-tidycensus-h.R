library(tidyverse)
rm(list = ls())
devtools::document()
devtools::load_all()


# meta --------------------------------------------------------------------

meta <- pull.acs.metadata(year = 2019)


# pull --------------------------------------------------------------------


vacancyts <- multiyr.acs.wrapper('B25034'
                               ,state = 6
                               , geo = 'tract'
                               ,metadata = meta
                               )

vacancyts %>%
  count(label, yr)



# recode ------------------------------------------------------------------

# ?acs.bldg.age.recode

vacancyts <- vacancyts %>%
  acs.bldg.age.recode()

tmp <- vacancyts %>%
  group_by(geoid, yr, recode) %>%
  summarise(n = sum(estimate))
# dope!
