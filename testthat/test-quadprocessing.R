library(testthat)
library(dplyr)
context("tests quadrat quality control functions")


testfile = '../../quadrat_test_data.csv'
testdat = read.csv(testfile, stringsAsFactors = F)

# supplementary files
splist = read.csv('../../test_JRN_plant_species.csv', stringsAsFactors = F)
quaddates = read.csv('../../test_quadrat_dates.csv', stringsAsFactors = F)
quadlist = unique(quaddates$quadrat)

test_that("check_quadrat_names catches invalid quadrat names", {
  expect_equal(check_quadrat_names(testdat,column_name='quadrat',quadratlist=quadlist),
               cbind(testdat[c(23,34),], error_type=rep('quadratname',2),error_value=c('','b1'), stringsAsFactors=F))
})

test_that("check_species_codes catches invalid species codes", {
  expect_equal(check_species_codes(testdat,column_name='USDA_code',specieslist=splist$USDA_code),
               cbind(testdat[c(5,14,20,29,40),], error_type=rep('speciescode',5),
                     error_value=c('pecti','2FORB','2PLANT','2PLANT',''),stringsAsFactors=F))
})

test_that("find_nonnumeric_values catches nonnumeric values", {
  expect_equal(find_nonnumeric_values(testdat, column_name = 'density'),
               cbind(testdat[c(5,34),], error_type=rep('nonnumeric',2),
                     error_value=c('>10',''), stringsAsFactors=F))
})

test_that("check_numeric_range catches invalid number", {
  expect_equivalent(check_numeric_range(testdat,column_name='density',min_val=0,allow_min=F),
                    cbind(testdat[16,],error_type='invalid density', error_value = '0', stringsAsFactors=F))
})

test_that("find_duplicate_rows catches duplicated data", {
  expect_equivalent(find_duplicate_rows(testdat, columns = c('quadrat','year','month','USDA_code')),
                    cbind(testdat[c(41,42),], error_type=rep('duplicate_row',2),
                          error_value = rep(NA,2), stringsAsFactors=F))
})
