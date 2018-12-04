#library(tidycensus)
#
#THIS WORKS
v16 <- load_variables(year = 2016,
                      dataset = "acs5",
                      cache = TRUE)

v16

# chooose a variable
library(tidyverse)
## foreign born
B05012 <- filter(v16, str_detect(name, "B05012"))
library(tidyverse)
library(sf)

foreign_born <- get_acs(geography = "county", 
                        variables = "B05012_003", 
                        # make true if you want to include the geo data
                        geometry = TRUE,
                        shift_geo = TRUE,
                        survey = "acs5",
                        # make summary variable the population in that year to divide by later
                        summary_var = "B03002_001")

## number of rows to show up to 6
head(foreign_born)
plot(foreign_born["estimate"])

## But I want the foreing born percentage so divide by the summary estimate
fb_percentage <- foreign_born %>% 
  mutate(pct=100 * (estimate/ summary_est))

## plot this for static map
plot(fb_percentage["pct"])

# make web based interactive map
library(mapview)
mapview(fb_percentage, zcol = "pct", legend = TRUE)

# Make a dot plot map with races
# first choose the race variables
library(tidycensus)
library(tidyverse)
library(sf)

racevars <- c(White = "P005003", 
              Black = "P005004", 
              Asian = "P005006", 
              Hispanic = "P004003")
# then get the data, the geo coordinates and the summary variable - population
# the get the percents
race <- get_decennial(geography = "county", variables = racevars, 
                      state = "NY", 
                      #county = "District of Columbia", 
                      geometry = TRUE,
                      summary_var = "P001001") %>%
  mutate(percent = 100 * (value / summary_value)) #%>%
  #st_transform(26918)


head(race)

dot_plot <- map(c("White", "Black", "Hispanic", "Asian"), function(group) {
  race %>%
    filter(variable == group) %>%
    st_sample(., size = .$value / 100) %>%
    st_sf() %>%
    mutate(group = group) 
}) %>%
  reduce(rbind) %>%
  group_by(group) %>%
  summarize()


## get a bunch of other map features
dc_roads <- roads("DC", "District of Columbia") %>%
  filter(RTTYP %in% c("I", "S", "U"))

# Get an area water dataset for DC
dc_water <- area_water("DC", "District of Columbia")

# Get the boundary of DC
dc_boundary <- counties("DC", cb = TRUE)

## make dot plot in ggplot
ggplot() + 
 # geom_sf(data = dc_boundary, color = NA, fill = "white") + 
  geom_sf(data = dot_plot, aes(color = group, fill = group), size = 0.1) + 
  geom_sf(data = dc_water, color = "lightblue", fill = "lightblue") + 
  geom_sf(data = dc_roads, color = "grey") + 
  coord_sf(crs = 26918, datum = NA) + 
  scale_color_brewer(palette = "Set1", guide = FALSE) +
  scale_fill_brewer(palette = "Set1") +
  labs(title = "The racial geography of Washington, DC", 
       subtitle = "2010 decennial U.S. Census", 
       fill = "", 
       caption = "1 dot = approximately 100 people.\nData acquired with the R tidycensus and tigris packages.")
#make centroids - showing some key values on a map

library(sf)
# Get a dataset of median home values from the 1-year ACS
usa_value <- get_acs(geography = "state", 
                       variables = "B25077_001", 
                       survey = "acs1", 
                       geometry = TRUE, 
                       shift_geo = TRUE)

# Plot the dataset to view the shifted geometry
plot(usa_value["estimate"])

# Generate point centers
centers <- st_centroid(usa_value)

# Set size parameter and the size range
ggplot() + 
  geom_sf(data = usa_value, fill = "white") + 
  geom_sf(data = centers, aes(size = estimate), shape = 21, 
          fill = "lightblue", alpha = 0.7, show.legend = "point") + 
  scale_size_continuous(range = c(1, 20))


# use the tally function
data %>% group_by(state) %>% tally()

### get the maximum of each category
tx_largest <- tx_race %>%
  group_by(GEOID) %>%
  filter(estimate == max(estimate)) %>%
  select(NAME, variable, estimate)

tx_largest

## make your own groups from census data
wa_grouped <- wa_income %>%
  filter(variable != "B19001_001") %>%
  # the case_when function allows you to choose the cases to include in the newly created variable
  mutate(incgroup = case_when(
    variable < "B19001_008" ~ "below35k", 
    variable < "B19001_013" ~ "35kto75k", 
    TRUE ~ "above75k")) %>%
  group_by(NAME, incgroup) %>%
  summarize(group_est = sum(estimate))

wa_grouped
  
  # margin of errors moe is 90% CI

  wyoming_age2 <- wyoming_age %>%
  # # get rid of " County, Wyoming" in the name variable
  mutate(NAME = str_replace(NAME, " County, Wyoming", ""))
ggplot(wyoming_age2, 
       aes(x = estimate, 
           # reorder put the plots in order of their size
           y = reorder(NAME, estimate))) + 
  geom_errorbarh(aes(xmin = estimate - moe, 
                     xmax = estimate + moe)) + 
  geom_point(size = 3, color = "darkgreen") + 
  theme_grey(base_size = 14) + 
  labs(title = "Median age, counties in Wyoming", 
       subtitle = "2012-2016 American Community Survey",
       x = "ACS estimate (bars represent margins of error)", 
       y = "")
summarize(group_est = sum(estimate))

## tigris package
## mapping
library(tigris)
ri_tiger <- counties("RI",options(tigris_use_cache = TRUE))

ri_cb <- counties("RI", cb = TRUE)
par(mfrow = c(1, 2))
plot(ri_tiger, main = "TIGER/Line")
plot(ri_cb, main = "Cartographic boundary")

library(tigris)

missouri <- tracts("MO", cb = TRUE)
kansas <- tracts("KS", cb = TRUE)
attr(missouri, "tigris")
plot(missouri, main = "Cartographic boundary")

attr(kansas, "tigris")
plot(kansas, main = "Cartographic boundary")

kansas_missouri <- rbind_tigris(kansas, missouri)

plot(kansas_missouri$geometry)   


### doesn't standalone
library(tidyverse)

new_england <- c("ME", "NH", "VT", "MA")
ne_tracts <- map(new_england, function(x) {
  tracts(state = x, cb = TRUE)
}) %>%
  rbind_tigris()


new_england
new_england <- c("ME", "NH", "VT", "MA")
library(ggplot2)
# Iterate through the states and request tract data for state
ne_tracts <- map(new_england, function(x) {
  tracts(state = x, cb = TRUE)
}) %>%
  rbind_tigris()

plot(ne_tracts$geometry)


# Get boundaries for Texas and set the house parameter
tx_house <- state_legislative_districts(state = "TX", house = "lower", cb = TRUE)

# Merge data on legislators to their corresponding boundaries
tx_joined <- left_join(tx_house, tx_members, by = c("NAME" = "District"))
library(sf)
head(tx_house)
ggplot(tx_house, aes(fill = Party)) + 
  geom_sf() + 
  scale_fill_manual(values = c("R" = "red", "D" = "blue"))
# us glimpse function to bet basic structure of df
# 








# get percentages using summary variable - divide by the summary variable


library(tigris)

idaho_income <- get_acs(geography = "school district (unified)", 
                        variables = "B19013_001", 
                        state = "ID")

idaho_school <- school_districts(state = "ID", 
                                 type = "unified", 
                                 class = "sf")

id_school_joined <- left_join(idaho_school, 
                              idaho_income, 
                              by = "GEOID")

plot(id_school_joined["estimate"])

state_value <- get_acs(geography = "state", 
                       variables = "B25077_001", 
                       survey = "acs1", 
                       geometry = TRUE,
                       # this moves alaska and hawaii
                       shift_geo = TRUE)



ggplot(cook_value, aes(fill = estimate, color = estimate)) + 
  geom_sf() + 
  scale_fill_viridis_c(labels = scales::dollar) +  
  scale_color_viridis_c(guide = FALSE) + 
  theme_minimal() + 
  coord_sf(crs = 26916, datum = NA) + 
  labs(title = "Median home value by Census tract", 
       subtitle = "Cook County, Illinois", 
       caption = "Data source: 2012-2016 ACS.\nData acquired with the R 
       tidycensus package.", 
       fill = "ACS estimate")

library(mapview)
mapview(cook_value, zcol = "estimate", legend = TRUE)
plot(state_value["estimate"])
#########################
############################# GET data from ACS
all_race <- get_acs(geography = "county", 
                    #state = "CA",
                    variables = race_vars, 
                    #summary_var is the population of the county
                    summary_var = "B03002_001")

### summarize data you want
largest <- all_race %>%
  group_by(GEOID) %>%
  filter(estimate == max(estimate)) %>%
  select(NAME, variable, estimate)

largest

all_dots <- map(c("White", "Black", "Hispanic", "Asian"), function(group) {
  all_race %>%
    filter(variable == group) %>%
    st_sample(., size = .$value / 100) %>%
    st_sf() %>%
    mutate(group = group) 
}) %>%
  reduce(rbind)


dc_dots <- map(c("White", "Black", "Hispanic", "Asian"), function(group) {
  race %>%
    filter(variable == group) %>%
    st_sample(., size = .$value / 100) %>%
    st_sf() %>%
    mutate(group = group) 
}) %>%
  reduce(rbind) %>%
  group_by(group) %>%
  summarize()
options(tigris_class = "sf")

library(tidycensus)
library(tidyverse)
library(sf)




library(tidycensus)
library(tidyverse)
library(sf)

racevars <- c(White = "P005003", 
              Black = "P005004", 
              Asian = "P005006", 
              Hispanic = "P004003")


################
race <- get_decennial(geography = "county", variables = racevars, 
                      #state = "DC", county = "District of Columbia", 
                      geometry = TRUE,
                      summary_var = "P001001") %>%
  mutate(percent = 100 * (value / summary_value)) %>%
  st_transform(26918)


head(race)


# Remove the gridlines and generate faceted maps
ggplot(race, aes(fill = percent, color = percent)) + 
  geom_sf() + 
  coord_sf(datum = NA) + 
  facet_wrap(~variable)


## make an interactive map
##    library(tidycensus)
library(tidyverse)
library(sf)

# Get dataset with geometry set to TRUE
usa_value <- get_acs(geography = "county",
                     variables = "B25077_001", 
                     geometry = TRUE)

# Plot the estimate to view a map of the data
plot(usa_value["estimate"])
m <- mapview(usa_value,
             zcol = "estimate")
m@map




# Set the color guide to FALSE and add a subtitle and caption to your map
# library(ggplot2)

# Create a choropleth map with ggplot
county_value <- get_acs(geography = "county", 
                        variables = "B25077_001", 
                        survey = "acs5", 
                        geometry = TRUE, 
                        shift_geo = TRUE)

# Plot the dataset to view the shifted geometry
plot(county_value["estimate"])


ggplot(marin_value, aes(fill = estimate)) + 
  geom_sf()


ggplot(marin_value, aes(fill = estimate, color = estimate)) + 
  geom_sf() + 
  scale_fill_viridis_c(labels = scales::dollar) +  
  scale_color_viridis_c(guide = FALSE) + 
  theme_minimal() + 
  coord_sf(crs = 26911, datum = NA) + 
  labs(title = "Median owner-occupied housing value by Census tract", 
       subtitle = "Marin County, California", 
       caption = "Data source: 2012-2016 ACS.\nData acquired with the R tidycensus package.", 
       fill = "ACS estimate")


#   Census hierarchy - state, county, Census tract, block group, and block - as well as zip code tabulation areas
#   
# # Get an income dataset for Idaho by school district
idaho_income <- get_acs(geography = "school district (unified)", 
                        variables = "B19013_001", 
                        state = "ID")

# Get a school district dataset for Idaho
idaho_school <- school_districts(state = "ID", type = "unified", class = "sf")

# Join the income dataset to the boundaries dataset
id_school_joined <- left_join(idaho_school, idaho_income, by = "GEOID")

plot(id_school_joined["estimate"])

### dot plot stuff
# Filter the DC roads object for major roads only
roads <- roads() %>%
  filter(RTTYP %in% c("I", "S", "U"))

# Get an area water dataset for DC
dc_water <- area_water("DC", "District of Columbia")

# Get the boundary of DC
dc_boundary <- counties("DC", cb = TRUE)

ggplot() + 
  geom_sf(data = dc_boundary, color = NA, fill = "white") + 
  geom_sf(data = dc_dots, aes(color = group, fill = group), size = 0.1) + 
  geom_sf(data = dc_water, color = "lightblue", fill = "lightblue") + 
  geom_sf(data = dc_roads, color = "grey") + 
  coord_sf(crs = 26918, datum = NA) + 
  scale_color_brewer(palette = "Set1", guide = FALSE) +
  scale_fill_brewer(palette = "Set1") +
  labs(title = "The racial geography of Washington, DC", 
       subtitle = "2010 decennial U.S. Census", 
       fill = "", 
       caption = "1 dot = approximately 100 people.\nData acquired with the R tidycensus and tigris packages.")

#other packages for census data
library(ipumsr)
library(censusapi)