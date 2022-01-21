library(tidyverse)

devtools::load_all()
# colm in tbllist --------------------------------------------------------------

tbList <- list( x = tibble(AA = 1:3)
               ,y = tibble(BB = 4:6)
               ,z= tibble(AA = 1:8)
               ,aa = tibble(CC = 1:5
                            ,AA = 1:5))

tbList %>%
  colm.in.tblList('AA')


test_that("colm.in.tblList AA", {
  testthat::expect_equal(
    {tbList %>%
      colm.in.tblList('AA',F)}
    ,{c('x', 'z', 'aa')}
  )
})

test_that("colm.in.tblList BB", {
  testthat::expect_equal(
    {tbList %>%
        colm.in.tblList('BB',F)}
    ,{c('y')}
  )
})

test_that("colm.in.tblList BB", {
  testthat::expect_equal(
    {tbList %>%
        unname() %>%
        colm.in.tblList('BB',F)}
    ,{c(F, T, F, F)}
  )
})


