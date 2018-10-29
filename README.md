# quadprocessing

This R package contains functions for doing error checking on the Jornada permanent quadrat data.

## Installation
You can install this package from GitHub with:
```r
install.packages('devtools')
devtools::install_github('emchristensen/quadprocessing')
```

## Examples
This package is a work-in-progress. There are some .csv files of example data containing built-in errors for testing and demonstration.

#### Load example data
```r
testdat = read.csv('quadrata_test_data.csv', stringsAsFactors = F)
splist = read.csv('test_JRN_plant_species.csv', stringsAsFactors = F)
quaddates = read.csv('test_quadrat_dates.csv', stringsAsFactors = F)
```

### So far this package contains functions to:

Check that quadrat names are valid
```r
# get list of valid quadrat names from 'quaddates'
quadlist = unique(quaddates$quadrat)
badquads = check_quadrat_names(testdat, column_name = 'quadrat', quadratlist = quadlist)
```

Check for valid plant species codes
```r
badspcodes = check_species_codes(testdat, column_name = 'USDA_code', specieslist = splist$USDA_code)
```

Check data in a given column is in numeric format (i.e. finds 'NA' or non-numeric entries)
```r
badnumeric = find_nonnumeric_values(testdat, column_name = 'density')
```

Check for duplicated rows (i.e. there should be a single value associated with a unique quadrat, date, and species)
```r
duplicaterows = find_duplicate_rows(testdat, columns = c('quadrat','year','month','USDA_code'))
```
