# load libraries
source("R/project_libraries.R")
source("R/function_as.Date.multicol().R")
source("R/function_levels_check().R")

#### create the sites sheet ----
# first check the structure of the template
sites_data <- 
  read_excel(path = "data/data_form_chickgrowth.xlsx", 
             sheet = "2. sites", skip = 1, 
             col_types = "text", na = "NA")

names(sites_data)

# make the sheet for the Ceuta data
sites_data_CEUT <- 
  data.frame(site_name = "Bahía de Ceuta",
             site_abbreviation = "CEUT",
             country = "Mexico",
             latitude = 23.917,
             longitude = -106.964,
             species = c("SNPL", "WIPL"),
             years_range = c("2006-2013, 2015-2020",
                             "2018-2020"),
             food_data = "NO",
             comments = c("2014 data lost", NA))

# check if the wrangled data frame contains the same structure as the template
names(sites_data) == names(sites_data_CEUT)

# export to Excel
write.xlsx(sites_data_CEUT, "data/data_form_chickgrowth_CEUT.xlsx", 
           showNA = TRUE, sheetName = "2. sites", append = FALSE, row.names = FALSE)

#### create the biometrics sheet ----
# first check the structure of the template
biometric_data <- 
  read_excel(path = "data/data_form_chickgrowth.xlsx", 
             sheet = "4. biometric data", skip = 1, 
             col_types = "text", na = "NA") %>% 
  rename(remarks = ...18) %>% 
  slice(-c(1:11))

names(biometric_data)

# wrangle the chick capture data
chicks_captures <-
  
  # read the Captures table
  dbReadTable(CeutaCLOSED, "Captures") %>% 
  
  # subset to juvenile captures that have a date
  filter(age == "J",
         !is.na(date)) %>% 
  
  # average the left and right tarsus length measurements in to one value
  mutate(tarsus_length_mm = rowMeans(cbind(as.numeric(left_tarsus), 
                                           as.numeric(right_tarsus)), 
                                     na.rm = TRUE),
         wing_length = rowMeans(cbind(as.numeric(left_wing), 
                                         as.numeric(right_wing)), 
                                     na.rm = TRUE)) %>%
  
  # group by bird identity
  group_by(ring) %>% 

  # specify the biological nest ID as the ID of the earliest capture (i.e.,
  # brood mixing can occur and create multiple nest IDs for later captures of
  # chicks)
  mutate(brood_ID = ID[which.min(date)]) %>% 

  # convert to dataframe
  data.frame() %>% 
  
  # convert date columns to the %Y-%m-%d format
  as.Date.multicol() %>% 
  
  # rename columns to fit with the template
  rename(species_abbreviation = species,
         site_abbreviation = population,
         ring_number = ring,
         observer_abbreviation = observer,
         bill_length_mm = bill,
         mass_g = weight,
         remarks = comments) %>% 
  
  # change the site abbreviation to the 4 letter code
  mutate(site_abbreviation = ifelse(site_abbreviation == "Ceuta", "CEUT"),
         
  # make dummy columns for the uncollected P10 data
         P10_length_total_mm = NA, 
         P10_length_exposed_mm = NA)

# wrangle the hatch dates
hatch_dates <- 
  
  # read the Nests table
  dbReadTable(CeutaCLOSED, "Nests") %>% 
  
  # subset the nests that have hatch date information
  filter(fate == "Hatch") %>% 
  
  # define as a dataframe
  data.frame() %>% 
  
  # classify date columns in the appropriate format
  as.Date.multicol() %>%
  
  # subset the result as simply the nest ID and their respective hatch dates
  select(ID, end_date) %>% 
  
  # rename the columns to match the chick captures dataframe
  rename(brood_ID = ID,
         hatchdate_observed = end_date)

# join and wrangle the chick and hatch date data
biometric_data_CEUT <- 
  
  # join the hatch dates to the chick captures by brood ID
  left_join(x = chicks_captures, y = hatch_dates, by = "brood_ID") %>%
  
  # classify variables as factor or numeric and calculate age at capture
  ungroup() %>% 
  mutate(age_observed = as.numeric(date - hatchdate_observed)) %>% 
  
  # remove observations in which the age is less than 2 days before the nest's 
  # hatch date (i.e., hatch dates represent the average hatch date of a brood 
  # and thus the earliest chick to hatch in a nest could be up 2 days earlier 
  # than the nest's average hatch date)
  filter(age_observed >= -2) %>% 
  
  # correct individual hatch dates (i.e., those that had ages < 0)
  mutate(hatchdate_observed = ifelse(age_observed < 0, 
                                     hatchdate_observed + age_observed, 
                                     hatchdate_observed),
         # specify that all ages were calculated from "hatch_date"
         age_method = "hatch_date",
         
         # specify that there were no cases in which the age was estimated
         age_estimated = NA) %>% 

  # consolidate the dataframe to match the template
  select(species_abbreviation, site_abbreviation, ring_number, brood_ID, 
         hatchdate_observed, age_observed, age_estimated, age_method, date, 
         time, observer_abbreviation, wing_length, P10_length_total_mm, 
         P10_length_exposed_mm, tarsus_length_mm, bill_length_mm, mass_g, 
         remarks) %>% 
  
  # classify date columns in the appropriate format
  as.Date.multicol() %>% 
  
  # back-transform the ages to the correct hatch date (i.e., for those that
  # hatched earlier than the nest average. Should only include ages >= 0 now)
  mutate(age_observed = as.numeric(date - hatchdate_observed)) %>% 
  
  # change the date and time to the format in the template
  mutate(date = format(date, "%d/%m/%Y"),
         hatchdate_observed = format(hatchdate_observed, "%d/%m/%Y"),
         time = paste0(substr(time, 1, 2), ":", substr(time, 3, 4))) %>% 
  
  # correct the abbreviations of observers to the 4-letter code
  mutate(observer_abbreviation = ifelse(observer_abbreviation == "CK" | observer_abbreviation == "KC", "CKUE",
                                        ifelse(observer_abbreviation == "MC" | observer_abbreviation == "MCL", "MCLO",
                                               ifelse(observer_abbreviation == "SG" | observer_abbreviation == "SGA", "SGDA",
                                                      ifelse(observer_abbreviation == "LEP", "LJEP",
                                                             ifelse(observer_abbreviation == "LL", "LLAN",
                                                                    ifelse(observer_abbreviation == "LF", "LRFR",
                                                                           ifelse(observer_abbreviation == "DVR", "DVRO",
                                                                                  ifelse(observer_abbreviation == "KAVR", "KAVR",
                                                                                         ifelse(observer_abbreviation == "RB", "RBBA",
                                                                                                ifelse(observer_abbreviation == "RB", "RBBA",
                                                                                                       ifelse(observer_abbreviation == "RB", "RBBA",
                                                                                                              ifelse(observer_abbreviation == "OC", "OCGO",
                                                                                                                     ifelse(observer_abbreviation == "OV", "OSVE",
                                                                                                                            ifelse(observer_abbreviation == "SQ", "RSQF",
                                                                                                                                   ifelse(observer_abbreviation == "AAT", "AATI",
                                                                                                                                          ifelse(observer_abbreviation == "DG", "DGJA",
                                                                                                                                                 ifelse(observer_abbreviation == "KA", "KACA",
                                                                                                                                                        ifelse(observer_abbreviation == "TV", "TVOL",
                                                                                                                                                               ifelse(observer_abbreviation == "WR", "WRAB", "XXXX")))))))))))))))))))) 

# check if the wrangled data frame contains the same structure as the template
names(biometric_data) == names(biometric_data_CEUT)

# export to Excel
write.xlsx(biometric_data_CEUT, "data/data_form_chickgrowth_CEUT.xlsx", 
           showNA = TRUE, sheetName = "4. biometric data", append = TRUE, row.names = FALSE)

#### create the observers sheet ----
# first check the structure of the template
observers_and_authors_data <- 
  read_excel(path = "data/data_form_chickgrowth.xlsx", 
             sheet = "3. observers & authors", skip = 1, 
             col_types = "text", na = "NA")

names(observers_and_authors_data)

# make the sheet for the Ceuta data
observers_and_authors_data_CEUT <- 
  data.frame(first_name = NA,
             all_names = NA,
             last_name = NA,
             abbreviation = levels_check(biometric_data_CEUT, col_name = "observer_abbreviation"),
             sites = "CEUT",
             institution = NA,
             department = NA,
             address = NA,
             ZIP = NA,
             country = NA) %>% 
  mutate(first_name = ifelse(abbreviation == "LJEP", "Luke", NA),
         all_names = ifelse(abbreviation == "LJEP", "L. J.", NA),
         last_name = ifelse(abbreviation == "LJEP", "Eberhart-Hertel", NA),
         institution = ifelse(abbreviation == "LEP", "Max Planck Institute for Ornithology", NA),
         department = ifelse(abbreviation == "LJEP", "Research Group for Behavioural Genetics and Evolutionary Ecology", NA),
         address = ifelse(abbreviation == "LJEP", "Eberhard-Gwinner-Str. 5, Seewiesen", NA),
         ZIP = ifelse(abbreviation == "LJEP", "82319", NA),
         country = ifelse(abbreviation == "LJEP", "Germany", NA))

# check if the wrangled data frame contains the same structure as the template
names(observers_and_authors_data) == names(observers_and_authors_data_CEUT)

# export to Excel
write.xlsx(observers_and_authors_data_CEUT, "data/data_form_chickgrowth_CEUT.xlsx", 
           showNA = TRUE, sheetName = "3. observers & authors", append = TRUE, row.names = FALSE)
