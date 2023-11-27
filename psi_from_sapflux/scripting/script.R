library(readxl)
library(tidyr)
library(dplyr)

### Establish connections to files

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
