library(tidycensus)
library(tidyverse)
library(sf)
library(mapview)

#### this is my list of variables I am accumulating for the analysis - IGNORE For maps
new_vars <- c(White16 = "B03002_003",
              Black16 = "B03002_004",  
              Asian16 = "B03002_006", Hispanic16 = "B03002_012",
              hh_income16="B19013_001",
              pop16="B01003_001",
              employed16="B23025_001",
              native16= "B05012_002",
              foreign_born16="B05012_003")
             # education="B15003")



#####
# First make the interactive foreign born map
#library(tidycensus)

v16 <- load_variables(year = 2016,
                      dataset = "acs5",
                      cache = TRUE)

v16

# chooose a variable
## foreign born
var <- filter(v16, str_detect(name, "B05012"))


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

mapview(fb_percentage, zcol = "pct", legend = TRUE)
## educational cats
## library(tidycensus)


####### NOW MAKE DOT PlOT MAP
# Download table "B15003"
#   education for inds 25 and over
options(tigris_use_cache = TRUE)
education <- get_acs(geography = "county", 
                     #state = "NY", 
                     table = "B15003",
                     geometry = TRUE,
                     shift_geo = TRUE,
                     survey = "acs5",
                     summary_var = "B15003_001") %>%
  mutate(percent = 100 * (estimate / summary_est)) %>% as.data.frame()

# make some gorups out of the education cats
ed_groups <- education %>%
  filter(variable != "B15003_001") %>%
  # the case_when function allows you to choose the cases to include in the newly created variable
  mutate(edgroup = case_when(
    variable < "B15003_016" ~ 1, 
    variable < "B15003_022" ~ 2, 
    variable < "B15003_023" ~ 3, 
    variable < "B15003_024" ~ 4, 
    variable < "B15003_025" ~ 5,
    TRUE  ~ 6)) %>%
  
  group_by(NAME, edgroup) %>%
  summarize(group_est = sum(estimate)) 

ed_groups

education <- education %>% select("NAME","geometry")
education <- unique(education)

z <- ed_groups %>% left_join(education,by="NAME")
# make z into an 'sf' object
z <- st_sf(z)

dot_plot <- map(c(1,2,3,4,5,6), function(group) {
  z %>%
    filter(edgroup == group) %>%
    st_sample(., size = .$group_est / 500) %>%
    st_sf() %>%
    mutate(group = group) 
}) %>%
  reduce(rbind) %>%
  group_by(group) %>%
  summarize()


## get a bunch of other map features
#dc_roads <- roads("DC", "District of Columbia") %>%
#  filter(RTTYP %in% c("I", "S", "U"))

# Get an area water dataset for DC
#dc_water <- area_water("DC", "District of Columbia")

# Get the boundary of DC
#dc_boundary <- counties("DC", cb = TRUE)

w <- ggplot() + 
  # geom_sf(data = dc_boundary, color = NA, fill = "white") + 
  geom_sf(data = dot_plot, aes(fill=as.factor(group),color = as.factor(group)), size = 0.1)+
  scale_fill_manual("Education level", 
                     breaks = c(1,2,3,4,5,6),
                     values = c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2"),
                     
                     labels = c("did not finish HS", "high school to bachelors",
                                "bachelors", "masters","professional", "doctorate"))+
  scale_color_manual("", 
                    breaks = c(1,2,3,4,5,6),
                    values = c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2"),
                    
                    labels = c("did not finish HS", "high school to bachelors",
                               "bachelors", "masters","professional", "doctorate"),
                    guide=FALSE)
########################################
######################################
#######################################
####LEFT OFF HERE!!!!!!!
########################################  













new_data16 <- get_acs(geography = "county", 
                      #state = "CA",
                      variables = new_vars,
                      survey="acs5",
                      year=2016,
                      cache_table = TRUE,
                      #keep_geo_vars=TRUE,
                      #output="wide",
                      geometry = TRUE,
                      summary_var = "B01003_001") %>%
  mutate(percent = 100 * (estimate / summary_est)) %>% as.data.frame()






education <- filter(v16, str_detect(name, "P005003"))
edvars <- c(White = "P005003", 
              Black = "P005004", 
              Asian = "P005006", 
              Hispanic = "P004003")
# then get the data, the geo coordinates and the summary variable - population
# the get the percents
ed <- get_acs(geography = "county", variables = edvars, 
                      #state = "DC", county = "District of Columbia", 
                      geometry = TRUE,
                      summary_var = "P001001") %>%
  mutate(percent = 100 * (value / summary_value)) #%>%
#st_transform(26918)


head(race)

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



racevars <- c(White = "P005003", 
              Black = "P005004", 
              Asian = "P005006", 
              Hispanic = "P004003")
# then get the data, the geo coordinates and the summary variable - population
# the get the percents
new <- get_acs(geography = "county", variables = "B99151", 
                      state = "NY", 
                      #county = "District of Columbia", 
                      geometry = TRUE,
                      summary_var = "P001001") %>%
  mutate(percent = 100 * (value / summary_value)) #%>%