# --- 1. Dependencies ---------------------------------------------------------

rm(list = ls())
suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(tidyr)
})


# --- 2. Config ---------------------------------------------------------------

# Place the DHIS2 export next to this script, or change this path.
input_path <- "2023 dhis2.xlsx"
out_dir <- "data"

dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# Map long DHIS2 indicator columns -> short analytic names used by the app.
rename_map <- c(
  "105-MA01. Admissions" = "admissions",
  "105-MA04. Total deliveries in the unit" = "deliveries",
  "105-MA05a1. Births in the unit - Live births Total" = "live_births",
  "105-MA05b1. Births in the unit - Fresh Still birth Total" = "fresh_stillbirth",
  "105-MA05c1. Births in the unit - Macerated still birth Total" = "macerated_stillbirth",
  "105-MA09. Live babies at discharge" = "live_at_discharge",
  "105-MA12_2019 Neonatal Death 8-28 days" = "neonatal_deaths_late",
  "105-MA12_2019. Newborn deaths (0-7 days)" = "newborn_deaths",
  "105-MA14a. Mothers who initiated breastfeeding within the 1st hour after delivery - Total" = "breastfeeding",
  "105-MA23. No.of babies with Birth asphyxia" = "birth_asphyxia",
  "105-MA24. No. of Live babies Successfully Resuscitated" = "resuscitated"
)
indicator_cols <- unname(rename_map)


# reading the dataset
raw <- read_excel(input_path, sheet = 1)

# --- 4. Clean ----------------------------------------------------------------

cleaned <- raw %>%
  rename(any_of(setNames(names(rename_map), unname(rename_map)))) %>%
  mutate(
    across(any_of(indicator_cols), ~ suppressWarnings(as.numeric(.x))),
    across(any_of(indicator_cols), ~ tidyr::replace_na(.x, 0)),
    period = as.character(periodcode),
    year = substr(period, 1, 4),
    month_str = substr(period, 5, 6),
    month = as.Date(sprintf("%s-%s-01", year, month_str)),
    region = orgunitlevel2
  ) %>%
  select(month, year, region, any_of(indicator_cols))


# --- 5. Aggregate ------------------------------------------------------------

sum_na <- function(x) sum(x, na.rm = TRUE)

monthly <- cleaned %>%
  group_by(month) %>%
  summarise(across(any_of(indicator_cols), sum_na), .groups = "drop") %>%
  arrange(month)


by_region <- cleaned %>%
  group_by(month, region) %>%
  summarise(across(any_of(indicator_cols), sum_na), .groups = "drop") %>%
  arrange(month, region)


by_year <- cleaned %>%
  group_by(year) %>%
  summarise(across(any_of(indicator_cols), sum_na), .groups = "drop") %>%
  arrange(year)


# --- 6. Save -----------------------------------------------------------------

saveRDS(monthly, file.path(out_dir, "monthly.rds"))
saveRDS(by_region, file.path(out_dir, "by_region.rds"))
saveRDS(by_year, file.path(out_dir, "by_year.rds"))
