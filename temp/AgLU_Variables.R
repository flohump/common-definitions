library(dplyr)
library(tidyr)

# Load AR6 definitions
readxl::read_excel("temp/IPCC_AR6_WG3_Global_sectoral_Pathways_scenario_template_v2.1.xlsx",
                   sheet = "variable_definitions_Full") %>%
  select(Category, Variable, Unit, Definition, Core, AFOLU) -> AR6_Var


AR6_Var %>%
  # keep only AFOLU variables
  filter(!is.na(AFOLU)) %>%
  # remove emission variables
  filter(Category != "Emissions") ->
  AR6_Var_AFOLU_original

## Adding AFOLU related food variables ----
AR6_Var %>%
  filter(is.na(AFOLU)) %>%
  filter(grepl("Food", Variable)) ->
  AR6_Var_AFOLU_add_food

## Adding AFOLU related water variables ----
AR6_Var %>%
  filter(Category == "Water") %>%
  filter(grepl("Irrigation|Livestock", Variable)) ->
  AR6_Var_AFOLU_add_water

## Adding AFOLU related price variables ----
AR6_Var %>%
  filter(grepl("Price", Variable)) %>%
  # Keep only biomass or ag
  filter(grepl("Primary Energy\\|Biomass|Ag", Variable)) ->
  AR6_Var_AFOLU_add_price

## Adding AFOLU related labor variables ----
AR6_Var %>%
  filter(grepl("Employment", Variable)) %>%
  # Keep only biomass or ag
  filter(grepl("Ag", Variable)) ->
  AR6_Var_AFOLU_add_labor

## Adding forestry variables ----
AR6_Var %>%
  filter(Category == "Forestry") ->
  AR6_Var_AFOLU_add_forestry


# Bind all----
AR6_Var_AFOLU_original %>%
  bind_rows(AR6_Var_AFOLU_add_food) %>%
  bind_rows(AR6_Var_AFOLU_add_water) %>%
  bind_rows(AR6_Var_AFOLU_add_price) %>%
  bind_rows(AR6_Var_AFOLU_add_labor) %>%
  bind_rows(AR6_Var_AFOLU_add_forestry)->
  AR6_Var_AFOLU_all

AR6_Var_AFOLU_all %>% readr::write_csv("temp/AR6_Var_AFOLU_all.csv")



DF_To_yamlList <- function(.df){

  #assert columns in df include key ones
  assertthat::assert_that(is.data.frame(.df))
  assertthat::assert_that(all(c("Variable", "Description", "Unit", "Tier") %in% names(.df)))
  assertthat::assert_that(is.integer(.df$Tier), msg = "Column is not of integer type; please convert it.")

  lapply(1:nrow(.df), function(x){

    .df %>% dplyr::slice(x) %>%
      select(Variable, Description, Unit, Tier) %>%
      group_by(Variable) %>%
      nest() %>% ungroup() %>%
      mutate(data = list(Variable = as.list(data %>% purrr::pluck(1) ) )) -> .df2

    names(.df2$data) <- .df2$Variable

    .df2$data
  }
  )
}

AR6_Var_AFOLU_all %>%
  mutate(Tier = as.integer(Core), Description = Definition) %>%
  DF_To_yamlList %>%
  yaml::write_yaml("temp/test.yaml")

