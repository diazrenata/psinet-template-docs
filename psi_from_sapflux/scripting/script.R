library(readxl)
library(tidyr)
library(dplyr)
library(openxlsx)

#### Establish connections to files ####

blank_psinet_template <- lapply(2:11,
                                FUN = function(x) readxl::read_xlsx(here::here("psi_from_sapflux", "scripting", "psinet-blank.xlsx"), sheet = x))

sfn_site_md <- readxl::read_xlsx(here::here("psi_from_sapflux", "scripting", "USA_MOR_SF_metadata.xlsx"), sheet = 1, range = "A2:D22") |>
  select(Variable, Value) |>
  tidyr::pivot_wider(names_from = Variable, values_from = Value)

sfn_stand_md <- readxl::read_xlsx(here::here("psi_from_sapflux", "scripting", "USA_MOR_SF_metadata.xlsx"), sheet = 2, range = "A1:D17") |>
  select(Variable, Value) |>
  tidyr::pivot_wider(names_from = Variable, values_from = Value)

sfn_species_md <- readxl::read_xlsx(here::here("psi_from_sapflux", "scripting", "USA_MOR_SF_metadata.xlsx"), sheet = 3, n_max = 5) |>
  select(-c(Description, Units)) |>
  select(where(fn = function(x) any(!is.na(x)))) |>
  tidyr::pivot_longer(-Variable, names_to = "Individual") |>
  tidyr::pivot_wider(id_cols = Individual, names_from = Variable)

sfn_plant_md <-  readxl::read_xlsx(here::here("psi_from_sapflux", "scripting", "USA_MOR_SF_metadata.xlsx"), sheet = 4, n_max = 25) |>
  select(where(fn = function(x) any(!is.na(x)))) |> 
  select(-c(Description, Units)) |>
  tidyr::pivot_longer(-Variable, names_to = "Individual") |>
  tidyr::pivot_wider(id_cols = Individual, names_from = Variable)

sfn_env_md <-  readxl::read_xlsx(here::here("psi_from_sapflux", "scripting", "USA_MOR_SF_metadata.xlsx"), sheet = 5, range = "A1:D17") |>
  select(Variable, Value) |>
  tidyr::pivot_wider(names_from = Variable, values_from = Value)

sfn_sapflow <- readxl::read_xlsx(here::here("psi_from_sapflux", "scripting", "USA_MOR_SF_metadata.xlsx"), sheet = 6, skip = 4) |> 
  select(where(fn = function(x) any(!is.na(x)))) |>
  mutate(across(-1, as.numeric)) |> 
  tidyr::pivot_longer(-TIMESTAMP, names_to = "pl_code")

sfn_env <-  readxl::read_xlsx(here::here("psi_from_sapflux", "scripting", "USA_MOR_SF_metadata.xlsx"), sheet = 7, skip = 3) |>
  select(where(fn = function(x) any(!is.na(x)))) 

sfn_wp <- readxl::read_xlsx(here::here("psi_from_sapflux", "scripting", "USA_MOR_SF.xlsx"), sheet = 3, na = c("NA")) 

##### New file path

site_name <- sfn_wp$id_sfn[1]

site_path <- here::here("psi_from_sapflux", "scripting", paste0(site_name, "_as_psinet.xlsx"))

orig_psinet_template <- loadWorkbook(here::here("psi_from_sapflux", "scripting", "psinet-blank.xlsx"))
filled_psinet_template <- copyWorkbook(orig_psinet_template)

#### Sheet 1 Site Metadata ####

site_md <- blank_psinet_template[[1]]

site_md$`Submitting author first name`[2] <- sfn_site_md$si_contact_firstname[1]
site_md$`Submitting author last name`[2] <- sfn_site_md$si_contact_lastname[1]
site_md$Institution[2] <- sfn_site_md$si_contact_institution[1]
site_md$Email[2] <- sfn_site_md$si_contact_email[1]
site_md$`Data publication?`[2] <- !is.na(sfn_site_md$si_paper[1])
site_md$`Data publication DOI(s)`[2] <- sfn_site_md$si_paper
site_md$`Study type`[2] <- "Field study"
site_md$`Begin year`[2] <- NA
site_md$`End year`[2] <- NA
site_md$`Latitude (WGS84)`[2] <- sfn_site_md$si_lat
site_md$`Longitude (WGS84)`[2] <- sfn_site_md$si_long
site_md$Remarks[2] <- sfn_site_md$si_remarks

writeData(filled_psinet_template, 2, site_md)


#### Sheet 2 Data Description ####

data_desc <- blank_psinet_template[[2]]

# Pressure chamber
data_desc$`Is it available?`[2] <- any("chamber-bagged" %in% sfn_wp$method,
                                       "chamber-unbagged" %in% sfn_wp$method)
if(data_desc$`Is it available?`[2]) {
  
  data_desc$Methodology[2] <- unique(sfn_wp$method)
  
}

# Psychrometer
data_desc$`Is it available?`[3] <- "psychometer" %in% sfn_wp$method

if(data_desc$`Is it available?`[3]) {
  
  data_desc$Methodology[3] <- unique(sfn_wp$method)
  
}

# Soil water content shallow
data_desc$`Is it available?`[4] <- "swc_shallow" %in% colnames(sfn_env)

if(data_desc$`Is it available?`[4]) {
  
  data_desc$`Soil depth - start (cm)`[4] <- sfn_env_md$env_swc_shallow_depth
  data_desc$`Soil depth - end (cm)`[4] <- sfn_env_md$env_swc_shallow_depth
  data_desc$Units[4] <- "cm3/cm3 (volumetric)"
  
}


# Soil water content deep
data_desc$`Is it available?`[5] <- "swc_deep" %in% colnames(sfn_env)

if(data_desc$`Is it available?`[5]) {
  
  data_desc$`Soil depth - start (cm)`[5] <- sfn_env_md$env_swc_deep_depth
  data_desc$`Soil depth - end (cm)`[5] <- sfn_env_md$env_swc_deep_depth
  data_desc$Units[5] <- "cm3/cm3 (volumetric)"
  
}

# Soil water potential not collected
data_desc$`Is it available?`[6:7] <- FALSE


# Precipitation
data_desc$`Is it available?`[8] <- "precip" %in% colnames(sfn_env)

if(data_desc$`Is it available?`[8]) {
  
  data_desc$`Sensor location`[8] <- sfn_env_md$env_precip[1]
}

# Relative humidity
data_desc$`Is it available?`[9] <- "rh" %in% colnames(sfn_env)

if(data_desc$`Is it available?`[9]) {
  
  data_desc$`Sensor location`[9] <- sfn_env_md$env_rh[1]
}


# Vapor pressure deficit
data_desc$`Is it available?`[10] <- "vpd" %in% colnames(sfn_env)

if(data_desc$`Is it available?`[10]) {
  
  data_desc$`Sensor location`[10] <- sfn_env_md$env_vpd[1]
}

# Air temperature
data_desc$`Is it available?`[11] <- "ta" %in% colnames(sfn_env)

if(data_desc$`Is it available?`[11]) {
  
  data_desc$`Sensor location`[11] <- sfn_env_md$env_ta[1]
}

# PPFD
data_desc$`Is it available?`[12] <- "ppfd_in" %in% colnames(sfn_env)

if(data_desc$`Is it available?`[12]) {
  
  data_desc$`Sensor location`[12] <- sfn_env_md$env_ppfd_in[1]
}


# Incident shortwave radiation
data_desc$`Is it available?`[13] <- "sw_in" %in% colnames(sfn_env)

if(data_desc$`Is it available?`[13]) {
  
  data_desc$`Sensor location`[13] <- sfn_env_md$env_sw_in[1]
}


# Net radiation
data_desc$`Is it available?`[14] <- "netrad" %in% colnames(sfn_env)

if(data_desc$`Is it available?`[14]) {
  
  data_desc$`Sensor location`[14] <- sfn_env_md$env_netrad[1]
}

# Windspeed
data_desc$`Is it available?`[15] <- "ws" %in% colnames(sfn_env)

if(data_desc$`Is it available?`[15]) {
  
  data_desc$`Sensor location`[15] <- sfn_env_md$env_ws[1]
}

writeData(filled_psinet_template, 3, data_desc)

#### Sheet 3 Additional datasets #### 

data_avail <- blank_psinet_template[[3]]

data_avail$Availability[7] <- TRUE
data_avail$Publication[7] <- TRUE
data_avail$Network[7] <- "SAPFLUXNET"
data_avail$`Network or database ID`[7] <- site_name
data_avail$Remarks[7] <- "Imported from SAPFLUXNET"


# For other variables, figure out how to get this information (second hand, ish)

writeData(filled_psinet_template, 4, data_avail)

#### Sheet 4 Treatments ####

treatments <- blank_psinet_template[[4]]

if(any(!is.na(sfn_wp$pl_treatment))) {
  
  treatments$`Treatment ID` <- c(treatments$`Treatment description`[1], paste0("Treatment_", 1:length(unique(sfn_wp$pl_treatment))))
  treatments$`Treatment description` <- c(treatments$`Treatment description`[1], unique(sfn_wp$pl_treatment))
  
} else {
  
  treatments$`Level of treatment`[2] <- "Whole study"
  treatments$`Treatment ID`[2] <- "No treatment"
  
}

writeData(filled_psinet_template, 5, treatments)


#### Sheet 5 Plots ####

plots <- blank_psinet_template[[5]]

plots$`Plot ID`[2] <- "Whole study"
plots$`Vegetation type`[2] <- sfn_site_md$si_igbp[1]
plots$`Growth condition`[2] <- sfn_stand_md$st_growth_condition[1]
plots$Aspect[2] <- sfn_stand_md$st_aspect[1]
plots$Terrain[2] <- sfn_stand_md$st_terrain[1]
plots$`Soil texture`[2] <- tolower(sfn_stand_md$st_soil_texture[1])
plots$`Percent sand`[2] <- sfn_stand_md$st_sand_perc[1]
plots$`Percent silt`[2] <- sfn_stand_md$st_silt_perc[1]
plots$`Percent clay`[2] <- sfn_stand_md$st_clay_perc[1]

writeData(filled_psinet_template, 6, plots)


#### Sheet 6 Plants ####

saveWorkbook(filled_psinet_template, site_path, overwrite = T)
