# use a set of test data to ensure that layer_functions are catching errors and summarizing data as expected

context("test functions in layer_functions.R")

# # test data (contains built-in errors)
# pol_test = read.csv('pol_test_data.csv',stringsAsFactors = F)
# # prep test data
# pol_test = pol_test[pol_test$Plant != 'Plot boundary',]
# pol_quadinfo = list('B1','2016','pol')
# test_sp_list = data.frame(Genus_USDA=c('Bouteloua','Sporobolus'),
#                           Species_USDA=c('eripoda','airoides'),
#                           USDA_code=c('BOER4','SPAI'),
#                           Habit=c('P','P'),
#                           Form=c('GRASS','GRASS'))
# test_dates = data.frame(quadrat='B1',
#                         year=2016,
#                         month=10,
#                         day=14)

# test database
db_test = 'N5_test.gdb'
sp_list = read.csv('test_JRN_plant_species.csv', stringsAsFactors = F)
quaddates = read.csv('test_quadrat_dates.csv', stringsAsFactors = F)
pol_test = rgdal::readOGR(dsn = db_test,
                          layer = 'N5_2016_pol',
                          verbose = F,
                          stringsAsFactors = F)@data
pnt_test = rgdal::readOGR(dsn = db_test,
                          layer = 'S5_2016_pnt',
                          verbose = F,
                          stringsAsFactors = F)@data


test_that("error_check_polygon finds errors", {
  expect_equal(error_check_polygon(pol_test, sp_list),
               cbind(pol_test[c(2,5,6,7),], error_type=rep('speciescode', 4), error_value=rep('SPORO',4),
                     stringsAsFactors=F))
})

test_that("error_check_point finds errors", {
  expect_equivalent(error_check_point(pnt_test, sp_list),
               data.frame(Plant='OPEN', error_type='speciescode', error_value='OPEN', stringsAsFactors=F))
})

test_that("add_species_info returns data frame with correct columns", {

})

test_that("add_date_info returns data frame with correct columns", {

})

test_that("create_cover_df returns expected information", {

})

test_that("create_count_df returns expected information", {

})
