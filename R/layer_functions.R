# Functions that operate on layers from gdb -- error checking and extracting information

#' @title add species info to data frame
#'
#' @param df data frame of quadrat data containing a column for USDA species code
#' @param column_name name of column from df containing USDA species code
#' @param species_df data frame of species information to be merged with df, containing a column USDA_code
#'
add_species_info = function(df, column_name, species_df) {
  mergedframe = merge(df, species_df, by.x = column_name, by.y = 'USDA_code', all.x = T)
  # make columns for export
  mergedframe$scientific_name = paste(mergedframe$Genus_USDA, mergedframe$Species_USDA)
  mergedframe$duration = mergedframe$Habit
  mergedframe$form = tolower(mergedframe$Form)
  return(dplyr::select(mergedframe,c(names(df),'scientific_name','duration','form')))
}


#' @title add date info to data frame
#'
#' @param df data frame of quadrat data. Must contain columns "quadrat" and "year"
#' @param date_df data frame of date information to be merged with df, containing columns "quadrat" and "year"
#'
add_date_info = function(df, date_df) {
  mergedframe = merge(date_df, df, by = c('quadrat','year'))
  return(mergedframe)
}

#' @title error check polygon data frame
#' @description checks polygon layer of quadrat data from a gdb file for errors
#'
#' @param df data frame of polygon data created by readOGR
#' @param sp_list data frame of species information
#'
error_check_polygon = function(df, sp_list) {
  # remove 'plot boundary' row
  plot_boundary = df[df$Plant == 'Plot boundary',]
  dat = df[df$Plant != 'Plot boundary',]
  # check species names valid
  badcodes = check_species_codes(dat, column_name = 'Plant', specieslist = sp_list$USDA_code)

  # check polygon shape info is numeric
  numeric1 = find_nonnumeric_values(dat, 'SHAPE_Length')
  numeric2 = find_nonnumeric_values(dat, 'SHAPE_Area')

  # check area is within (0,plot_boundary] (cannot be equal to 0 but can be equal to plot_boundary)
  areacheck = check_numeric_range(dat, column_name = 'SHAPE_Area', min_val = 0, max_val = plot_boundary$SHAPE_Area,
                                  allow_min=F, allow_max=T)

  # check perimeter is >0 (cannot be equal to 0)
  perimetercheck = check_numeric_range(dat, column_name = 'SHAPE_Length', min_val = 0, allow_min=F)

  # compile any problematic data rows returned by checks
  errors = rbind(badcodes, numeric1, numeric2, areacheck, perimetercheck)
  return(errors)
}

#' @title create cover data frame
#' @description creates data frame of cover data from polygon data layer from gdb
#'
#' @param df data frame of polygon data created by readOGR
#' @param quad_info information about quadrat name and year gleaned from layer file name
#' @param sp_list data frame of species information
#' @param dates_df data frame of quadrat sampling dates
#'
create_cover_df = function(df, quad_info, sp_list, dates_df) {
  quad = quad_info[1]
  year = quad_info[2]

  # rename columns
  df = dplyr::rename(df, area = SHAPE_Area, perimeter = SHAPE_Length, USDA_code = Plant)

  # construct rest of data frame
  layer_df = df
  layer_df$count = rep(1)
  layer_df$quadrat = rep(quad)
  layer_df$year = rep(year)

  # merge with species info
  layer_df_sp = add_species_info(layer_df, column_name = 'USDA_code', sp_list)

  # merge with date info (add month column)
  layer_df_sp_date = add_date_info(layer_df_sp, dates_df)

  cover_df = dplyr::select(layer_df_sp_date, quadrat, year, month, USDA_code, scientific_name, duration, form, area, perimeter)

  return(cover_df)
}


#' @title error check point data frame
#' @description checks point layer of quadrat data from a gdb file for errors
#'
#' @param df data frame of point data created by readOGR
#' @param sp_list data frame of species information
#'
error_check_point = function(df, sp_list) {
  # check species names valid (and finds NA/missing values)
  badcodes = check_species_codes(df, column_name = 'Plant', specieslist = sp_list$USDA_code)

  # compile any problematic data rows returned by checks
  errors = badcodes
  return(errors)
}

#' @title create count data frame
#' @description creates data frame of count data from point data layer from gdb
#'
#' @param df data frame of point data created by readOGR
#' @param quad_info information about quadrat name and year gleaned from layer file name
#' @param sp_list data frame of species information
#' @param dates_df data frame of quadrat sampling dates
#'
create_count_df = function(df, quad_info, sp_list, dates_df) {
  quad = quad_info[1]
  year = quad_info[2]

  # aggregate by species
  df$count = rep(1)
  df_agg = aggregate(df$count, by=list(Plant = df$Plant), FUN=sum)
  df_agg = dplyr::rename(df_agg, count = x)

  # construct rest of data frame
  layer_df = df_agg
  layer_df$quadrat = rep(quad)
  layer_df$year = rep(year)

  # merge with species info
  layer_df_sp = add_species_info(layer_df, column_name = 'Plant', sp_list)

  # merge with date info (add month column)
  layer_df_sp_date = add_date_info(layer_df_sp, dates_df)

  # fix species column name
  layer_df_sp_date = dplyr::rename(layer_df_sp_date, USDA_code = Plant)

  # if bare ground is recorded ('2BARE') make count = 0
  layer_df_sp_date$count[layer_df_sp_date$USDA_code == '2BARE'] <- 0

  count_df = dplyr::select(layer_df_sp_date, quadrat, year, month, USDA_code, scientific_name, duration, form, count)

  return(count_df)
}