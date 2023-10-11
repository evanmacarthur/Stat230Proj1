# ==============================================================================
# Economic food and beverage firm data obtained from 
# The US Annual Establishment Industry Table: https://www.census.gov/data/tables/2019/econ/susb/2019-susb-annual.html 
# 
# Agricultural Census Data obtained from 
# Agricultural Census in 2017 Query Tool: https://www.nass.usda.gov/Publications/AgCensus/2017/ 
# 
# County data obtained from 
# Wikipedia: https://en.wikipedia.org/wiki/List_of_United_States_counties_and_county_equivalents 
#
# Authors: Evan MacArthur-Waltz, Andrew Leung, Emily Byers
# Last updated: 10/16/22
# 
# Data dictionary:
#
# `state`: state name.
# `state_lower`: state name in lowercase format.
# `county-code`: 3 digit county code.
# `county`: county name with first letter capitalized.
# `naics_description`: The North American Industry 
#     Classification System (NAICS) is the standard used by 
#     Federal statistical agencies in classifying business 
#     establishments for the purpose of collecting, analyzing, 
#     and publishing statistical data related to the U.S. 
#     business economy. Source - https://www.census.gov/naics/
# `establishments`: a single physical location where one 
#     predominant activity occurs.
# `food_and_beverage_stores`: according to Bureau of Labor Services, this
#     NAICS category includes usually retail food and beverages merchandise 
#     from fixed point-of-sale locations.
# `food_services_and_drinking_places`: according to Bureau of Labor Services, 
#     this NAICS category includes places that prepare meals, snacks, and 
#     beverages to customer order for immediate on-premises and off-premises 
#     consumption.
# ==============================================================================

# load packages
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
  select(-c(bracket)) %>% 
  # rename columns to simpler names
  rename(population = population_2020_census,
         state = state_or_equivalent) %>% 
  mutate(new_state = gsub("ʻ", "", state)) %>% 
  select(-c(state)) %>% 
  rename(state = new_state) %>% 
  # turn population into a numeric value
  mutate(new_pop = parse_number(population)) %>% 
  select(-population) %>% 
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
  select(-bracket) %>%  
  separate(county,
           c("county",
             "bracket"),
           " Parish") %>% 
  select(-bracket) %>% 
  separate(county,
           c("county",
             "bracket"),
           " Borough") %>% 
  select(-bracket) %>% 
  separate(county,
           c("county",
             "bracket"),
           " Census Area") %>% 
  select(-bracket)

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
  select(-bracket) %>% 
  # get rid of all text after comma in county
  separate(county,
           c("county",
             "bracket"),
           ",.*") %>% 
  select(-bracket) %>% 
  separate(county,
           c("county",
             "bracket"),
           "\\[") %>% 
  select(-bracket) %>% 
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
  select(-c(bracket)) %>% 
  mutate(new_state = gsub("ʻ", "", state)) %>% 
  select(-c(state)) %>% 
  rename(state = new_state) %>% 
  separate(county,
           c("county",
             "bracket"),
           " Borough") %>% 
  select(-bracket) %>% 
  separate(county,
           c("county",
             "bracket"),
           " Census Area") %>% 
  select(-bracket)

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
  select(-c(county_code,
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
  select(c(county,
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
  select(c(county_code,
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
  select(c(county_code,
           county,
           state,
           population,
           county_acreage_2000,
           state_lower,
           abbr))
  

# Economic Census Data ====
# read in economic census data as firms
firms_raw <- read_csv('raw-data/county_3digitnaics_2019.csv') %>% 
  # clean up column names
  clean_names()

# Clean up economic census data
firms <- 
  firms_raw %>% 
  # filter for Food and Beverage Stores and Food Services and Drinking Places
  filter(naics_description %in% c("Food and Beverage Stores", 
                                  "Food Services and Drinking Places"),
         # filter for only total value of firms per category
         enterprise_size == "1: Total") %>% 
  # selecting relevant columns 
  select(state_name, county, county_name, naics_description, establishments) %>% 
  # rename columns for consistency with other data sets
  rename(county_code = county,
         state = state_name,
         county = county_name) %>% 
  # make state names all lowercase to eventually join with other datasets
  mutate(state_lower = tolower(state))

# pivot to wider format
firms_wider <-
  firms %>% 
  pivot_wider(names_from = naics_description,
              values_from = establishments) %>% 
  clean_names()


# Agricultural Census Data ====
# read in ag census data .tsv
agcensus <- read_tsv("raw-data/2017_cdqt_data.tsv")

# remove null county codes
countyag <- agcensus %>% 
  filter(!COUNTY_CODE == "NULL") %>% 
  select(!c(CENSUS_CHAPTER, CENSUS_TABLE, CENSUS_ROW, 
            CENSUS_COLUMN, STATE_FIPS_CODE, STATE_ALPHA))

countyag$VALUE <- as.numeric(gsub(",","", countyag$VALUE))

categories <- unique(countyag$SHORT_DESC)
categories <- as.data.frame(categories)

#picking out a few columns
fertilizer <- countyag |> 
  filter(str_detect(SHORT_DESC, "FERTILIZER")) |> 
  filter(is.na(DOMAINCAT_DESC)) |> 
  distinct() |> 
  mutate(VALUE = ifelse(VALUE == "(D)", 0, VALUE),
         state_lower = tolower(STATE_NAME))

countyag <- group_by(countyag, SHORT_DESC)

save(countyag, file = "countyag.csv")

#START HERE



projdf <- countyag |> 
  filter(SHORT_DESC %in% c("FARM OPERATIONS - ACRES OPERATED", 
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
                           "AQUATIC PLANTS - OPERATIONS WITH AREA IN PRODUCTION"
                           )) |> 
  filter(is.na(DOMAINCAT_DESC)) |> 
  distinct() |> 
  mutate(VALUE = ifelse(VALUE == "(D)", 0, VALUE),
         state_lower = tolower(STATE_NAME))

projdf <- projdf |> 
  select(c(VALUE, COUNTY_NAME, COUNTY_CODE, STATE_NAME))

projdf2 <- projdf |> 
  pivot_wider(names_from = SHORT_DESC, values_from = VALUE)

df1 <- projdf2 |> 
  clean_names()

df1 <- df1 |> 
  rename("farm_acres" = farm_operations_acres_operated) |> 
  rename("cropland_acres" = ag_land_cropland_acres) |> 
  rename("irrigated_acres" = ag_land_irrigated_acres) |> 
  rename("farm_income_1" = income_net_cash_farm_of_operations_net_income_measured_in_operation) |> 
  rename("xmas_tree_sale" = cut_christmas_trees_operations_with_sales) |> 
  rename("fertilizer_use_$" = fertilizer_totals_incl_lime_soil_conditioners_expense_measured_in) |> 
  rename("chemical_use_$" = chemical_totals_expense_measured_in) |> 
  rename("fuel_use_$" = fuels_incl_lubricants_expense_measured_in) |> 
  rename("hired_labor_cost_$" = labor_hired_expense_measured_in) |> 
  rename("contract_labor_cost_$" = labor_contract_expense_measured_in) |> 
  rename("worker_number" = labor_hired_number_of_workers) |> 
  rename("emus_sold" = emus_sales_measured_in_head) |> 
  rename("bee_colonies" = honey_bee_colonies_inventory_measured_in_colonies) |> 
  rename("honey_sold" = honey_sales_measured_in) |> 
  rename("ornamental_fish" = ornamental_fish_operations_with_sales_distribution) |> 
  rename("bison_sale" = bison_operations_with_sales) |> 
  rename("aquatic_plants" = aquatic_plants_operations_with_area_in_production) |> 
  rename("mushroom_spawn_sale_$" = mushroom_spawn_sales_measured_in) |> 
  rename("grazing_rotation" = practices_rotational_or_mgmt_intensive_grazing_number_of_operations) |> 
  rename("producer_age_group" = producers_age_avg_measured_in_years)





# select relevant columns
d_cropland <- countyag %>% 
  filter(COMMODITY_DESC == "AG LAND") %>% 
  # filter for agricultural land rows
  filter(str_detect(SHORT_DESC, c("OPERATIONS", "ACRES"))) %>% 
  # filter for values pertaining to operations and acres
  filter(SHORT_DESC == "AG LAND, CROPLAND, HARVESTED - ACRES") %>% 
  # filter for ag land acres and economic description
  filter(is.na(DOMAINCAT_DESC)) %>% 
  # removes duplicates
  distinct() %>% 
  # making "(D)" into zeroes
  mutate(VALUE = ifelse(VALUE == "(D)", 0, VALUE),
         state_lower = tolower(STATE_NAME))

# Filter for operations data
d_operations <- countyag %>% 
  filter(SHORT_DESC == "FARM OPERATIONS - NUMBER OF OPERATIONS") %>% 
  filter(SECTOR_DESC == "ECONOMICS") %>% 
  distinct() %>% 
  filter(is.na(DOMAINCAT_DESC)) %>% 
  # renaming values as operations
  rename("OPERATIONS" = VALUE) %>% 
  mutate(state_lower = tolower(STATE_NAME))

#Crop sales data wrangling
crop_sales <- countyag %>% 
  filter(str_detect(SECTOR_DESC, c("CROP"))) %>% 
  mutate(VALUE = ifelse(VALUE == "(D)", 0, VALUE)) %>% 
  filter(str_detect(SHORT_DESC, c("CROP TOTALS - SALES"))) %>% 
  distinct() %>% 
  rename("DOLLARS" = VALUE)



# corn sales
df_corn <- countyag %>% 
  filter(str_detect(COMMODITY_DESC, c("CORN"))) %>% 
  filter(str_detect(SHORT_DESC, c("SALES, MEASURED"))) %>% 
  select(c(VALUE, COUNTY_NAME, COUNTY_CODE, STATE_NAME)) %>% 
  rename("Corn" = VALUE)

# wheat sales
df_wheat <- countyag %>% 
  filter(str_detect(COMMODITY_DESC, c("WHEAT"))) %>% 
  filter(str_detect(SHORT_DESC, c("SALES, MEASURED"))) %>% 
  select(c(VALUE, COUNTY_NAME, COUNTY_CODE, STATE_NAME)) %>% 
  rename("Wheat" = VALUE)

# soybean sales
df_soybean <- countyag %>% 
  filter(str_detect(COMMODITY_DESC, c("SOYBEAN"))) %>% 
  filter(str_detect(SHORT_DESC, c("SALES, MEASURED"))) %>% 
  select(c(VALUE, COUNTY_NAME, COUNTY_CODE, STATE_NAME)) %>% 
  rename("Soybeans" = VALUE)

# merging corn, wheat, and soybean sales in single dataframe
threecropsales <- crop_sales %>% 
  full_join(df_corn, by = c("STATE_NAME", "COUNTY_CODE")) %>% 
  full_join(df_wheat, by = c("STATE_NAME", "COUNTY_CODE")) %>% 
  full_join(df_soybean, by = c("STATE_NAME", "COUNTY_CODE")) %>%  
  select(-ends_with(c(".y", "x.x"))) %>% 
  select(!c(SHORT_DESC, SECTOR_DESC,
            COMMODITY_DESC, AGG_LEVEL_DESC, 
            DOMAINCAT_DESC)) %>% 
  mutate(state_lower = tolower(STATE_NAME)) %>% 
  rename(county_code = COUNTY_CODE)

# clean up dataframe
cropland_final <- d_cropland %>% 
  full_join(d_operations,
            by = c("STATE_NAME", "COUNTY_CODE")) %>%
  # select relevant columns
  select(STATE_NAME, COUNTY_CODE, VALUE, OPERATIONS) %>% 
  # add column of state names in lowercase
  mutate(state_lower = tolower(STATE_NAME)) %>% 
  # rename county codes column and value column
  rename(county_code = COUNTY_CODE,
         ag_acreage = VALUE,
         operations = OPERATIONS)

# National County Map Data ====
#generates table of each county and geometry 
tigris_counties <- counties(state = NULL, cb = TRUE) %>% 
  clean_names()

#rename to columns to match tigris columns
us_county_pop_tigris <- full_county_list %>% 
  rename(state_name = state,
         countyfp = county_code,
         name = county)

#adds geometries to complete 
big_map_merged <- full_join(tigris_counties,
                            us_county_pop_tigris,
                            c("countyfp", "name", "state_name")) %>% 
  rename(county_code = countyfp,
         county = name,
         state = state_name)



# Merge datasets ====
df1 <- df1 |> 
  mutate(state = tolower(state_name))

us_counties <- us_counties |> 
  mutate(state_lower = tolower(state))

land_area <- land_area |> 
  mutate(state_lower = tolower(state))

df2 <- df1 |> 
  left_join(land_area, by = c("county_code", "state_lower"))

df2 <- df2 |> 
  left_join(us_counties, by = c("county_code", "state_lower"))

df2 <- df2 |> 
  select(!c(state.y, county.y, state_name, state.x))

df3 <- df2 |> 
  filter(!state_lower == "alaska")

df3 <- df3 |> 
  select(!c(county_name, state_lower, abbr))

df3 <- df3 |> 
  rename("county" = county.x)

df3 <- df3 |> 
  filter(!is.na(state))


food_county <- firms_wider %>% 
  # Merge with county data
  full_join(full_county_list,
             by = c("state_lower", "county_code")) %>% 
  # Merge with ag census data
  full_join(cropland_final,
             by = c("state_lower", "county_code")) %>% 
  full_join(threecropsales,
            by = c("state_lower", "county_code")) %>% 
  # Drop NA counties 
  drop_na(county.y) %>% 
  # Parse number from operations
 # mutate(operations = parse_number(operations)) %>% 
  # Select relevant columns
  select(county_code, 
         # Keep only full county list county and state columns
         county.y, 
         state.y, 
         county_acreage_2000,
         population,
         food_and_beverage_stores, 
         food_services_and_drinking_places,
         ag_acreage,
         operations,
         Corn,
         Wheat,
         Soybeans,
         DOLLARS) %>% 
  # Rename state.x and county.x
  rename(state = state.y,
         county = county.y)

# Create separate dataframe 
food_map_merged <- inner_join(big_map_merged,
                             food_county,
            by = c("state", "county_code")) %>%
  select(c(statefp,
           county_code,
           county.x,
           state,
           state_lower,
           population.x,
           food_and_beverage_stores,
           food_services_and_drinking_places,
           county_acreage_2000.x,
           ag_acreage,
           operations,
           countyns,
           affgeoid,
           geoid,
           namelsad,
           stusps,
           lsad,
           aland,
           awater,
           geometry)) %>%
  rename(state_code = statefp,
         county = county.x,
         population = population.x,
         county_acreage_2000 = county_acreage_2000.x,
         state_abbr = stusps)

# Calculations added to food_map_merged ====
# proportion of stores per 100 people in each county
food_map_merged <- food_map_merged %>% 
  mutate(store_pop_prop = round((food_and_beverage_stores / population)*100, 3),
         ag_land_prop = round((ag_acreage / county_acreage_2000), 3),
         # convert NAs to zeroes
         store_pop_prop = coalesce(store_pop_prop, 0),
         ag_land_prop = coalesce(ag_land_prop, 0))

# Get rid of Inf value
food_map_merged <- food_map_merged %>%
  filter(ag_land_prop != Inf)

# County name frequency ====
wc_df <- food_county %>% 
  select(county) %>% 
  mutate(val = 1) %>% 
  group_by(county) %>% 
  summarize(sum = sum(val)) %>% 
  arrange(desc(sum)) %>% 
  head(50)


# Save datasets ================================================================

# Check if subfolders exist; if not, create them
if (!dir.exists("the-state-of-food/data")) {
  dir.create("the-state-of-food/data")
}

# Save all cleaned data frames in a single .RData file in Shiny app *data* folder
save(food_county,
     food_map_merged,
     wc_df,
     file = "the-state-of-food/data/county-food.RData")



