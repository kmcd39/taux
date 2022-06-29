
# pulling data wrappers -----------------------------------------------------



#' multiyr.acs.wrapper
#'
#' Wraps `tidycensus::get_acs` for multiple years, tables, and states.
#' Structures long by geography, table, year. Maps through tables/years.
#' Tidycensus vectorized over states.
#'
#' I'll need to figure out and maybe refine this function to work with
#' reasonable combinations of states/geographies.
#'
#' @param table table to get from ACS; i.e., 'B01001.' See
#'   `tidycensus::load_variables`
#' @param states,geo,years passed to `tidycensus::get_acs`. There is one get_acs
#'   call for every combination of table and years.
#' @param metadata To use for labels. Result of `pull.acs.metadata` or
#'   `tidycensus::load_variables`
#'
#' @export multiyr.acs.wrapper
multiyr.acs.wrapper <- function( tables
                                 , states
                                 , geo
                                 , years = c(2010, 2015, 2019)
                                 , metadata = NULL
                                 ,cache = T) {


  if( is.null(metadata))
    metadata <- pull.acs.metadata(year = tail(years, 1))

  #browser()
  params <- expand.grid(tables, years)

  x <- map2_dfr( params[[1]], params[[2]]
                 , ~{tidycensus::get_acs(
                   geography = geo
                   ,table = .x
                   ,year = .y
                   ,state = states
                   ,survey = "acs5"
                   ,cache_table = cache
                 ) %>%
                     mutate(tabl = .x
                            ,yr = .y
                     ) %>%
                     rename_with( tolower )
                 })

  x <- x %>% select(-name)


  # add labels
  x <- x %>%
    left_join(metadata[c('name', 'label')]
              , by = c('variable' = 'name')) %>%
    mutate(var =
             as.numeric(
               str_extract(variable, '[0-9]{3}$')))
  return(x)
}


#' pull.acs.metadata
#'
#' Pulls ACS metadata and cleans labels.
#'
#'@export pull.acs.metadata
pull.acs.metadata <- function(year
                              ,dataset = 'acs5'
                              ,cache = T) {

  meta <- tidycensus::load_variables(year = year
                             ,dataset = dataset
                             ,cache = cache)

  # clean
  meta <- meta %>%
    mutate(label = gsub('!!', ' ', label)) %>%
    mutate(label = gsub('Estimate ', '', label)) %>%
    mutate(label = gsub(':$', '', label))

  return(meta)
}


# recoding fcns -----------------------------------------------------------

#' acs.demographic.recode
#'
#' Recodes demographic info, as from table B03002. Was developed for 2019 ACS;
#' could break if they change encodings.
#'
#' Encodings this was developed for:
#' B03002_001	Total
#' B03002_002	Total: Not Hispanic or Latino
#' B03002_003	Total: Not Hispanic or Latino: White alone
#' B03002_004	Total: Not Hispanic or Latino: Black or African American alone
#' B03002_005	Total: Not Hispanic or Latino: American Indian and Alaska Native alone
#' B03002_006	Total: Not Hispanic or Latino: Asian alone
#' B03002_007	Total: Not Hispanic or Latino: Native Hawaiian and Other Pacific Islander alone
#' B03002_008	Total: Not Hispanic or Latino: Some other race alone
#' B03002_009	Total: Not Hispanic or Latino: Two or more races
#' B03002_010	Total: Not Hispanic or Latino: Two or more races: Two races including Some other race
#' B03002_011	Total: Not Hispanic or Latino: Two or more races: Two races excluding Some other race, and three or more races
#' B03002_012	Total: Hispanic or Latino
#' B03002_013	Total: Hispanic or Latino: White alone
#' B03002_014	Total: Hispanic or Latino: Black or African American alone
#' B03002_015	Total: Hispanic or Latino: American Indian and Alaska Native alone
#' B03002_016	Total: Hispanic or Latino: Asian alone
#' B03002_017	Total: Hispanic or Latino: Native Hawaiian and Other Pacific Islander alone
#' B03002_018	Total: Hispanic or Latino: Some other race alone
#' B03002_019	Total: Hispanic or Latino: Two or more races
#' B03002_020	Total: Hispanic or Latino: Two or more races: Two races including Some other race
#' B03002_021	Total: Hispanic or Latino: Two or more races: Two races excluding Some other race, and three or more races
#'
#'
#' @param demos a demographics table, as from `multiyr.acs.wrapper(B03002, ...)`.
#' @param other.vars Vars (number following underscore) for groups to put into an "Other group" category.
#' @param filter.aggregates If true, filter totals and subtotals.
#'
#' @export acs.demographic.recode
acs.demographic.recode <- function(demos
                                   ,other.vars = c(5, 7:11)
                                   ,filter.aggregates = T) {

  if(filter.aggregates)
    demos <- demos %>%
      filter( ! var %in% c(1, 2, 13:21) )

  demos <- demos %>%
    mutate( label = gsub('Total: Not Hispanic or Latino: ', '', label)) %>%
    mutate( label = gsub('Total: ', '', label)) %>%

    mutate(recode = case_when(
      var %in% other.vars ~ 'Other group'
      ,TRUE ~ label)
    )

  return(demos)
}



#'acs.vacancy.recode
#'
#' Recodes demographic info, as from table B25004 Was developed for 2019 ACS;
#' could break if they change encodings.
#'
#' B25004_001	Total
#' B25004_002	Total: For rent
#' B25004_003	Total: Rented, not occupied
#' B25004_004	Total: For sale only
#' B25004_005	Total: Sold, not occupied
#' B25004_006	Total: For seasonal, recreational, or occasional use
#' B25004_007	Total: For migrant workers
#'
#' @param vacancy a vacancy table, as from `multiyr.acs.wrapper(B25004, ...)`.
#' @inheritParams acs.demographic.recode
#'
#' @export acs.vacancy.recode
acs.vacancy.recode <- function(vacancy
                               ,filter.aggregates = T
                               ) {

  if(filter.aggregates)
    vacancy <- vacancy %>%
      filter( ! var %in% c(1) )

  vacancy <- vacancy %>%
    mutate( label = gsub('Total: ', '', label)) %>%
    mutate(recode = case_when(
      var %in% 1:5 ~ 'For rent/sale, or just rented/sold'
      ,TRUE ~ label)
    )
  return(vacancy)
}




#' acs.bldg.age.recode
#'
#' Recodes demographic info, as from table B25034 Was developed for 2019 ACS;
#' could break if they change encodings. Note also table B25035_001 is just
#' median year structure built.
#'
#' B25034_001	Total
#' B25034_002	Total: Built 2014 or later
#' B25034_003	Total: Built 2010 to 2013
#' B25034_004	Total: Built 2000 to 2009
#' B25034_005	Total: Built 1990 to 1999
#' B25034_006	Total: Built 1980 to 1989
#' B25034_007	Total: Built 1970 to 1979
#' B25034_008	Total: Built 1960 to 1969
#' B25034_009	Total: Built 1950 to 1959
#' B25034_010	Total: Built 1940 to 1949
#' B25034_011	Total: Built 1939 or earlier
#'
#'
#' @param vacancy a building-age table, as from `multiyr.acs.wrapper(B25034, ...)`.
#' @inheritParams acs.demographic.recode
#'
#'
#' @export acs.bldg.age.recode
acs.bldg.age.recode <- function(bldgs
                                ,filter.aggregates = T) {

  if(filter.aggregates)
    bldgs <- bldgs %>%
      filter( ! var %in% c(1) )

  bldgs <- bldgs %>%
    mutate( label = gsub('Total: ', '', label)) %>%
    mutate(recode = case_when(
      var %in% c(2:4) ~ 'Built since 2000'
      ,var %in% c(5:7) ~ 'Built from 1970-1999'
      ,var %in% c(8:11) ~ 'Built before 1970'
      ,TRUE ~ label
    ))
  return(bldgs)
}

# scratch -----------------------------------------------------------------
