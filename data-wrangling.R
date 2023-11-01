#Initial wrangling by Evan MacArthur-Waltz, Emily Byers, Andrew Leung, rewrangled by Evan MacArthur-Waltz

# load packages
library(mosaic)
library(car)
library(rjson)
library(jsonlite)
library(httr)
library(tidyverse)
library(glue)
library(purrr)
library(janitor)
library(rvest)
library(robotstxt)
library(hablar)
library(utils)
library(dplyr)
library(states)
library(tigris)
library(GGally)
library(readr)

# Wrangle data =================================================================

# General County Data ====
# scrape county data from Wikipedia
# assign url as county_wiki
county_wiki <- "https://en.wikipedia.org/wiki/List_of_United_States_counties_and_county_equivalents"

# check if scraping is allowed
paths_allowed(county_wiki)

# get table with county names and population
county_list <- county_wiki %>% 
  read_html() %>% 
  html_elements("table") %>% 
  pluck(1) %>% 
  html_table() %>% 
  clean_names() %>%
  # get rid of wiki citation brackets 
  separate(county_or_equivalent, 
           c("county",
             "bracket"),
           "\\[") %>% 
  dplyr::select(-c(bracket)) %>% 
  # rename columns to simpler names
  rename(population = population_2020_census,
         state = state_or_equivalent) %>% 
  mutate(new_state = gsub("ʻ", "", state)) %>% 
  dplyr::select(-c(state)) %>% 
  rename(state = new_state) %>% 
  # turn population into a numeric value
  mutate(new_pop = parse_number(population)) %>% 
  dplyr::select(-population) %>% 
  rename(population = new_pop)

# filter out US territories, as this will not be part of our data analysis
territories <- c("American Samoa",
                 "Guam",
                 "Northern Mariana Islands",
                 "Puerto Rico",
                 "U.S. Minor Outlying Islands",
                 "Virgin Islands (U.S.)")

county_list <- county_list %>% 
  filter(!state %in% territories)

# get rid of all text after commas, since only county name is needed
county_list <- county_list %>% 
  separate(county,
           c("county",
             "bracket"),
           ",.*") %>% 
  dplyr::select(-bracket) %>%  
  separate(county,
           c("county",
             "bracket"),
           " Parish") %>% 
  dplyr::select(-bracket) %>% 
  separate(county,
           c("county",
             "bracket"),
           " Borough") %>% 
  dplyr::select(-bracket) %>% 
  separate(county,
           c("county",
             "bracket"),
           " Census Area") %>% 
  dplyr::select(-bracket)

# now, county codes need to be scraped
# assign url as county_code_wiki
county_code_wiki <- ("https://en.wikipedia.org/wiki/List_of_United_States_FIPS_codes_by_county")

# check if paths are allowed
paths_allowed(county_code_wiki)

# get county code table + tidy names + filter out territories
county_code_list <- county_code_wiki %>% 
  read_html() %>% 
  html_elements("table") %>% 
  pluck(2) %>% 
  html_table() %>% 
  clean_names() %>% 
  # rename columns
  rename(state = state_or_equivalent,
         county = county_or_equivalent) %>% 
  filter(!state %in% territories) %>% 
  # isolate each name to not have "county" after it
  separate(county,
           c("county",
             "bracket"),
           " County") %>% 
  dplyr::select(-bracket) %>% 
  # get rid of all text after comma in county
  separate(county,
           c("county",
             "bracket"),
           ",.*") %>% 
  dplyr::select(-bracket) %>% 
  separate(county,
           c("county",
             "bracket"),
           "\\[") %>% 
  dplyr::select(-bracket) %>% 
  # convert FIPS code to character so I can use substr()
  clean_names() %>% 
  convert(chr(fips))

# shorten 5 digit county code to 3 digits
county_code_list <- county_code_list %>% 
  mutate(county_code = ifelse(nchar(county_code_list$fips) == 4,
                              str_sub(county_code_list$fips, 2, 4),
                              str_sub(county_code_list$fips, 3, 5))) %>% 
  separate(county,
           c("county",
             "bracket"),
           " Parish") %>% 
  dplyr::select(-c(bracket)) %>% 
  mutate(new_state = gsub("ʻ", "", state)) %>% 
  dplyr::select(-c(state)) %>% 
  rename(state = new_state) %>% 
  separate(county,
           c("county",
             "bracket"),
           " Borough") %>% 
  dplyr::select(-bracket) %>% 
  separate(county,
           c("county",
             "bracket"),
           " Census Area") %>% 
  dplyr::select(-bracket)

# now get land area per county
# create dataframe to join datasets
state_abbr <- tibble(abbr = state.abb,
                     state = state.name)
# import land area excel

land_area <- read_csv("raw-data/census-land-area-2000.csv")

land_area <- land_area %>% 
  filter(str_detect(area_name,
                    "(,)",
                    negate = FALSE)) %>% 
  # take only last 3 digits of county code
  mutate(three_digit_code = substr(county_code, 3, 5)) %>% 
  # convert land area from square miles to acres
  mutate(county_acreage_2000 = land_area_sq_mi_2000 * 640) %>% 
  dplyr::select(-c(county_code,
            land_area_sq_mi_2000)) %>% 
  separate(area_name,
           into = c("county",
                    "abbr"),
           sep = ", ")

land_area <- left_join(land_area, 
                       state_abbr,
                       by = "abbr") 

land_area <- land_area %>% 
  # select relevant columns 
  dplyr::select(c(county,
           abbr,
           state,
           three_digit_code,
           county_acreage_2000)) %>% 
  rename(county_code = three_digit_code)

# join the three county-related datasets
us_counties <- left_join(county_list,
                         county_code_list,
                         by = c("county", "state")) %>% 
  # add in lowercase state column for joining
  mutate(state_lower = tolower(state)) %>% 
  # select relevant columns
  dplyr::select(c(county_code,
           county,
           state,
           population,
           state_lower))

# join us_counties with land_area 
full_county_list <- inner_join(us_counties,
                               land_area,
                               by = c("county_code",
                                      "county",
                                      "state")) %>% 
  # select relevant columns
  dplyr::select(c(county_code,
           county,
           state,
           population,
           county_acreage_2000,
           state_lower,
           abbr))


# Agricultural Census Data ====
# read in ag census data .tsv
agcensus <- read_tsv("raw-data/2017_cdqt_data.tsv")

# remove null county codes
countyag <- agcensus %>% 
  filter(!COUNTY_CODE == "NULL") %>% 
  dplyr::select(!c(CENSUS_CHAPTER, CENSUS_TABLE, CENSUS_ROW, 
            CENSUS_COLUMN, STATE_FIPS_CODE, STATE_ALPHA))


#need to important countyag dataset before you get started here
countyag <- group_by(countyag, SHORT_DESC)

#This makes a dataframe of just the potential categories to choose from, this df is helpful to copy from to avoid typos
categories <- unique(countyag$SHORT_DESC)
categories <- as.data.frame(categories)
View(categories)


#temp df that is just selecting the variables you are interested in, replace everything in quotes with SHORT DESC that you are interested in
projdf <- countyag |> 
  filter(SHORT_DESC %in% c("FARM OPERATIONS - ACRES OPERATED",
                           "FARM OPERATIONS - NUMBER OF OPERATIONS",
                           "AG LAND, CROPLAND - ACRES", 
                           "AG LAND, IRRIGATED - ACRES",
                           "CROP TOTALS - SALES, MEASURED IN $",
                           "INCOME, NET CASH FARM, OF OPERATIONS - NET INCOME, MEASURED IN $ / OPERATION",
                           "FERTILIZER TOTALS, INCL LIME & SOIL CONDITIONERS - EXPENSE, MEASURED IN $",
                           "CHEMICAL TOTALS - EXPENSE, MEASURED IN $",
                           "FUELS, INCL LUBRICANTS - EXPENSE, MEASURED IN $",
                           "LABOR, HIRED - EXPENSE, MEASURED IN $",
                           "LABOR, CONTRACT - EXPENSE, MEASURED IN $",
                           "LABOR, HIRED - NUMBER OF WORKERS",
                           "AG LAND - ACRES",
                           "EMUS - SALES, MEASURED IN HEAD",
                           "HONEY, BEE COLONIES - INVENTORY, MEASURED IN COLONIES",
                           "HONEY - SALES, MEASURED IN $",
                           "MUSHROOM SPAWN - SALES, MEASURED IN $",
                           "PRACTICES, ROTATIONAL OR MGMT INTENSIVE GRAZING - NUMBER OF OPERATIONS",
                           "PRODUCERS - AGE, AVG, MEASURED IN YEARS",
                           "CUT CHRISTMAS TREES - OPERATIONS WITH SALES",
                           "ORNAMENTAL FISH - OPERATIONS WITH SALES & DISTRIBUTION",
                           "BISON - OPERATIONS WITH SALES",
                           "AQUATIC PLANTS - OPERATIONS WITH AREA IN PRODUCTION")) |> 
  #Takes out problematic rows
  filter(is.na(DOMAINCAT_DESC)) |> 
  #takes out duplicate rows that were an issue at one point
  distinct() |> 
  #replaces 0s that didn't read well to actual 0s
  mutate(VALUE = ifelse(VALUE == "(D)", 0, VALUE),
         #makes a new column with the state names in lower case
         state_lower = tolower(STATE_NAME))

#selects only columns that you need, can add more, but really no reason to change this
projdf <- projdf |> 
  dplyr::select(c(VALUE, COUNTY_NAME, COUNTY_CODE, STATE_NAME))

#makes a new df in order to pivot, this is a very important step because it turns the SHORT DESC column with many options for rows
#into multiple columns. the pivot function is super powerful, but can be a bit. 
projdf2 <- projdf |> 
  pivot_wider(names_from = SHORT_DESC, values_from = VALUE)

#this is a nice little function that you downloaded the janitor package for, it makes the column names easier to call by making them all tidy
#this is not as important given we are about to rename them all, but saves a lot of backticks while doing this
df1 <- projdf2 |> 
  clean_names()


#you will need to make a lot of changes here as well, this renames each of variable names to be less bad
df1 <- df1 |> 
  rename("farm_acres" = farm_operations_acres_operated) |> 
  rename("cropland_acres" = ag_land_cropland_acres) |> 
  rename("irrigated_acres" = ag_land_irrigated_acres) |> 
  rename("farm_income_1" = income_net_cash_farm_of_operations_net_income_measured_in_operation) |> 
  rename("xmas_tree_sale" = cut_christmas_trees_operations_with_sales) |> 
  rename("fertilizer_use" = fertilizer_totals_incl_lime_soil_conditioners_expense_measured_in) |> 
  rename("chemical_use" = chemical_totals_expense_measured_in) |> 
  rename("fuel_use_$" = fuels_incl_lubricants_expense_measured_in) |> 
  rename("hired_labor_cost" = labor_hired_expense_measured_in) |> 
  rename("contract_labor_cost" = labor_contract_expense_measured_in) |> 
  rename("worker_number" = labor_hired_number_of_workers) |> 
  rename("emus_sold" = emus_sales_measured_in_head) |> 
  rename("bee_colonies" = honey_bee_colonies_inventory_measured_in_colonies) |> 
  rename("honey_sold" = honey_sales_measured_in) |> 
  rename("ornamental_fish" = ornamental_fish_operations_with_sales_distribution) |> 
  rename("bison_sale" = bison_operations_with_sales) |> 
  rename("aquatic_plants" = aquatic_plants_operations_with_area_in_production) |> 
  rename("mushroom_spawn_sale" = mushroom_spawn_sales_measured_in) |> 
  rename("grazing_rotation" = practices_rotational_or_mgmt_intensive_grazing_number_of_operations) |> 
  rename("producer_age_group" = producers_age_avg_measured_in_years)




# Merge datasets ====


# ok so for the join you need to have columns with the same names that the values match from, so we need state_lower and county code in each df

df1 <- df1 |> 
  mutate(state_lower = tolower(state_name))

us_counties <- us_counties |> 
  mutate(state_lower = tolower(state))

land_area <- land_area |> 
  mutate(state_lower = tolower(state))


#this joins values from land area to counties that exist in the ag census data
df2 <- df1 |> 
  left_join(land_area, by = c("county_code", "state_lower"))

#this does the same thing, but for county population
df2 <- df2 |> 
  left_join(us_counties, by = c("county_code", "state_lower"))

#this takes out some extraction columns
df2 <- df2 |> 
  dplyr::select(!c(state.y, county.y, state_name, state.x))

#ok so there is been a bit of persistent issue with alaska and forget exactly why, I think it had to do with not using counties
df3 <- df2 |> 
  filter(!state_lower == "alaska")

#removes more extra columns columns
df3 <- df3 |> 
  dplyr::select(!c(county_name, abbr))

#renames one last column
df3 <- df3 |> 
  rename("county" = county.x)

#this just removes values that snuck in that don't have a state associated for whatever reason. 
df3 <- df3 |> 
  filter(!is.na(state_lower))

#ok so this should result in a df that has ~20 columns that you picked from the ag census, population and area at a county level
#along with county and state names, and county codes. This should be functional for the project part. 

#not a bad idea to do a write_csv/write.csv for this result so that you only have to run the wrangling once between the 4 of your


write_csv(df3, 'C:\\Users\\emaca\\Desktop\\gooddata.csv')


