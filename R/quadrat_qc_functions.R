# functions for QA/QC on quadrat data

#library(dplyr)

#' @title Check quadrat names
#' @description Check a dataframe of quadrat data for valid quadrat names. Will flag quadrat names that are the incorrect case (upper/lower), or NA and missing values
#'
#' @param df Data frame of quadrat data to validate
#' @param column_name Name of column in df containing quadrat names
#' @param quadratlist Vector of valid quadrat names
#'
#' @return Rows from df containing invalid quadrat names, plus additional columns indicating error_type and error_value.
#'        If there are no errors, this will return an empty data frame.
#' @export
#'
#' @examples check_quadrat_names(df = data, column_name = 'quadrat', quadratlist = dates$quadrat)
#'
check_quadrat_names = function(df, column_name, quadratlist) {
  badquad <- df[!(df$quadrat %in% quadratlist),]
  try(badquad$error_type <- rep('quadratname'), silent = T)
  try(badquad$error_value <- df[row.names(badquad),column_name], silent=T)
  return(badquad)
}

#' @title Check species codes
#' @description Check a dataframe of quadrat data for valid plant species codes. Will flag species codes that are the incorrect case (upper/lower), or NA and missing values
#'
#' @param df Data frame of quadrat data to validate
#' @param column_name Name of column in df containing species codes
#' @param specieslist Vector of valid species codes
#'
#' @return Rows from df containing invalid species codes, plus additional columns indicating error_type and error_value.
#'        If there are no errors, this will return an empty data frame.
#' @export
#' @examples check_species_codes(dataframe, column_name = 'Species', specieslist = sp_list$USDA_codes)
#'
check_species_codes = function(df, column_name, specieslist) {
  codes = dplyr::select(df,column_name)[[1]]
  badcodes = codes[which(!codes %in% specieslist)] %>% unique()
  badrows = df[which(df[,column_name] %in% c(badcodes,NA)),,drop=F]
  # add error_type column [will fail if there are no badrows, hence the 'try']
  try(badrows$error_type <- rep('speciescode'), silent=T)
  try(badrows$error_value <- df[row.names(badrows),column_name], silent=T)
  return(badrows)
}


#' @title Find non-numeric data in data column
#' @description Check a data frame of quadrat data for valid count/area data. Indicate name of column that should contain only numeric data (e.g. counts, area, or perimeter). Returns rows that are non-numeric or NA and missing values
#'
#' @param df Data frame of quadrta data to validate
#' @param column_name Name of column in df containing numeric data (e.g. density/area/perimeter)
#'
#' @return Rows from df containing non-numeric density/area values, plus additional columns indicating error_type and error_value.
#'         If there are no errors, this will return an empty data frame.
#' @export
#' @examples find_nonnumeric_values(df, column_name = 'density')
#'
find_nonnumeric_values = function(df, column_name) {
  makenumeric = df
  makenumeric[,column_name] = suppressWarnings(as.numeric(makenumeric[,column_name]))
  badnumeric = df[is.na(makenumeric[,column_name]),]
  # add error_type column [will fail if there are no badrows, hence the 'try']
  try(badnumeric$error_type <- rep('nonnumeric'), silent=T)
  try(badnumeric$error_value <- df[row.names(badnumeric),column_name], silent=T)
  return(badnumeric)
}

#' @title Check numeric range
#'
#' @description Check a data frame of quadrat data for valid numeric data: number must be between given min and max values
#'
#' @param df Data frame of quadrat data to validate
#' @param column_name Name of column in df to check
#' @param min_val Minimum allowed value; default = -Inf
#' @param max_val Maximum allowed value; default = Inf
#' @param allow_min T/F: T = indicated minimum value is an allowed value; F = minimum value is not an allowed value. default = T
#' @param allow_max T/F: T = indicated maximum value is an allowed value; F = maximum value is not an allowed value. default = T
#'
#' @return Rows from df containing values outside valid range, plus additional columns indicating error_type and error_value.
#'         If there are no errors, this will return an empty data frame.
#' @export
#' @examples check_numeric_range(df, column_name = 'area', min_val = 0, max_val = 1, allow_min=F, allow_max=T)
#'
check_numeric_range = function(df, column_name, min_val = -Inf, max_val = Inf, allow_min = T, allow_max = T) {
  df2 = df
  # convert column_name to numeric and remove any rows with NA (non-numeric values are looked for in a different function)
  df2[,column_name] = suppressWarnings(as.numeric(df2[,column_name]))
  df3 = df2[!is.na(df2[,column_name]),]
  if (allow_min == T) {
    toolow = df3[df3[,column_name]<min_val,]
  } else if (allow_min == F) {
    toolow = df3[df3[,column_name]<=min_val,]
  }
  if (allow_max == T) {
    toohigh = df3[df3[,column_name]>max_val,]
  } else if (allow_max == F) {
    toohigh = df3[df3[,column_name]>=max_val,]
  }
  invalidnum = rbind(toolow,toohigh)
  # return column_name to original data class (necessary for testing)
  class(invalidnum[,column_name]) <- class(df[,column_name])
  try(invalidnum$error_type <- paste('invalid',column_name), silent=T)
  try(invalidnum$error_value <- df[row.names(invalidnum),column_name], silent=T)
  return(invalidnum)
}

#' @title Find duplicate rows
#' @description Check a data frame of quadrat data for duplicate data (possibly data entry error). User supplies list of column names that should define a unique entry (e.g. quadrat, date, species should be associated with a single value). Ignores rows with NA or missing values in the key columns
#'
#' @param df Data frame of quadrat data to validate
#' @param columns Vector of column names that determine unique key
#'
#' @return Rows from df that are duplicates according to the given key, plus an additional column indicating error_type.
#'         If there are no errors, this will return an empty data frame.
#' @export
#' @examples find_duplicate_rows(df, columns = c('quadrat','year','month','USDA_code'))
#'
find_duplicate_rows = function(df, columns) {
  df_selected = df[,columns]
  duplicates = df_selected[duplicated(df_selected),]
  duplicated_rows = merge(duplicates,df,by=columns)
  try(duplicated_rows$error_type <- rep('duplicate_row'), silent=T)
  try(duplicated_rows$error_value <- rep(NA), silent=T)
  return(duplicated_rows)
}
