# ============================================================
# FINAL COURSE PROJECT
# Analysis of NOAA Storm Database - 2024
# Author: Brian Punch-Eastman
# ============================================================

# SECTION 1: Load required packages

packages <- c("dplyr", "readr", "ggplot2", "tidyr",
              "stringr", "knitr", "rmarkdown", "scales")

for (pkg in packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, repos = "https://cloud.r-project.org")
  }
}

library(dplyr)
library(readr)
library(ggplot2)
library(tidyr)
library(stringr)
library(scales)

# SECTION 2: Define dataset paths

details_file <- file.path(
  "data",
  "StormEvents_details-ftp_v1.0_d2024_c20261119.csv.gz"
)

fatalities_file <- file.path(
  "data",
  "StormEvents_fatalities-ftp_v1.0_d2024_c20261119.csv.gz"
)

locations_file <- file.path(
  "data",
  "StormEvents_locations-ftp_v1.0_d2024_c20261119.csv.gz"
)

# Verify that all files exist

stopifnot(
  file.exists(details_file),
  file.exists(fatalities_file),
  file.exists(locations_file)
)

cat("\nAll three NOAA datasets located successfully.\n")

# SECTION 3: Import raw datasets

details <- read_csv(
  details_file,
  show_col_types = FALSE
)

fatalities <- read_csv(
  fatalities_file,
  show_col_types = FALSE
)

locations <- read_csv(
  locations_file,
  show_col_types = FALSE
)

# SECTION 4: Initial data validation

cat("\n========================================\n")
cat("NOAA STORM DATABASE - INITIAL VALIDATION\n")
cat("========================================\n")

cat("\nDetails dataset dimensions:\n")
print(dim(details))

cat("\nFatalities dataset dimensions:\n")
print(dim(fatalities))

cat("\nLocations dataset dimensions:\n")
print(dim(locations))

cat("\nUnique event IDs:\n")

cat("Details:", n_distinct(details$EVENT_ID), "\n")
cat("Fatalities:", n_distinct(fatalities$EVENT_ID), "\n")
cat("Locations:", n_distinct(locations$EVENT_ID), "\n")

cat("\nRepeated event IDs:\n")

cat("Details:", sum(duplicated(details$EVENT_ID)), "\n")
cat("Fatalities:", sum(duplicated(fatalities$EVENT_ID)), "\n")
cat("Locations:", sum(duplicated(locations$EVENT_ID)), "\n")

cat("\nMissing event IDs:\n")

cat("Details:", sum(is.na(details$EVENT_ID)), "\n")
cat("Fatalities:", sum(is.na(fatalities$EVENT_ID)), "\n")
cat("Locations:", sum(is.na(locations$EVENT_ID)), "\n")

cat("\nUnmatched event IDs:\n")

cat("Fatalities:",
    sum(!fatalities$EVENT_ID %in% details$EVENT_ID), "\n")

cat("Locations:",
    sum(!locations$EVENT_ID %in% details$EVENT_ID), "\n")

cat("\nYears represented:\n")
print(unique(details$YEAR))

cat("\nNumber of geographic areas:\n")
print(n_distinct(details$STATE))

cat("\nNumber of unique event types:\n")
print(n_distinct(details$EVENT_TYPE))

cat("\nEvent types:\n")
print(sort(unique(details$EVENT_TYPE)))

cat("\nDetails column names:\n")
print(names(details))

cat("\nFatalities column names:\n")
print(names(fatalities))

cat("\nLocations column names:\n")
print(names(locations))

cat("\n========================================\n")
cat("INITIAL DATA VALIDATION COMPLETE\n")
cat("========================================\n")

# ============================================================
# SECTION 5: DATA INTEGRATION
# ============================================================

cat("\n========================================\n")
cat("DATA INTEGRATION\n")
cat("========================================\n")

# Summarize fatality records by storm event

fatality_summary <- fatalities %>%
  group_by(EVENT_ID) %>%
  summarise(
    FATALITY_RECORDS = n(),
    .groups = "drop"
  )

# Summarize location records by storm event

location_summary <- locations %>%
  group_by(EVENT_ID) %>%
  summarise(
    LOCATION_RECORDS = n(),
    .groups = "drop"
  )

# Merge summarized datasets with storm details

storm_data <- details %>%
  left_join(fatality_summary, by = "EVENT_ID") %>%
  left_join(location_summary, by = "EVENT_ID")

# Replace unmatched record counts with zero

storm_data <- storm_data %>%
  mutate(
    FATALITY_RECORDS = replace_na(FATALITY_RECORDS, 0),
    LOCATION_RECORDS = replace_na(LOCATION_RECORDS, 0)
  )

# Validate merged dataset

cat("\nOriginal storm event rows:",
    nrow(details), "\n")

cat("Integrated dataset rows:",
    nrow(storm_data), "\n")

cat("Integrated dataset columns:",
    ncol(storm_data), "\n")

cat("Unique event IDs:",
    n_distinct(storm_data$EVENT_ID), "\n")

cat("Duplicate event IDs:",
    sum(duplicated(storm_data$EVENT_ID)), "\n")

cat("Total fatality records:",
    sum(storm_data$FATALITY_RECORDS), "\n")

cat("Total location records:",
    sum(storm_data$LOCATION_RECORDS), "\n")

cat("Events with fatality records:",
    sum(storm_data$FATALITY_RECORDS > 0), "\n")

cat("Events with location records:",
    sum(storm_data$LOCATION_RECORDS > 0), "\n")

# Verify that integration did not inflate records

stopifnot(
  nrow(storm_data) == nrow(details),
  !anyDuplicated(storm_data$EVENT_ID),
  sum(storm_data$FATALITY_RECORDS) == nrow(fatalities),
  sum(storm_data$LOCATION_RECORDS) == nrow(locations)
)

# Save integrated dataset

dir.create("output", showWarnings = FALSE)

write_csv(
  storm_data,
  "output/noaa_storm_integrated_2024.csv"
)

cat("\nIntegrated dataset saved successfully.\n")

cat("\n========================================\n")
cat("DATA INTEGRATION VALIDATION: PASS\n")
cat("========================================\n")

# ============================================================
# SECTION 6: DATA CLEANING AND TRANSFORMATION
# ============================================================

cat("\n========================================\n")
cat("DATA CLEANING AND TRANSFORMATION\n")
cat("========================================\n")

# Function to convert NOAA damage values into dollars

convert_damage <- function(x) {

  x <- toupper(trimws(as.character(x)))
  x <- gsub(",", "", x)

  valid <- !is.na(x) &
    grepl("^[0-9]+(\\.[0-9]+)?[KMB]?$", x)

  result <- rep(NA_real_, length(x))

  values <- as.numeric(
    sub("[KMB]$", "", x[valid])
  )

  multiplier <- case_when(
    grepl("K$", x[valid]) ~ 1000,
    grepl("M$", x[valid]) ~ 1000000,
    grepl("B$", x[valid]) ~ 1000000000,
    TRUE ~ 1
  )

  result[valid] <- values * multiplier

  return(result)
}

# Validate damage conversion function

stopifnot(
  convert_damage("25K") == 25000,
  convert_damage("2.5M") == 2500000,
  convert_damage("1.2B") == 1200000000,
  convert_damage("0.00K") == 0,
  is.na(convert_damage(NA)),
  is.na(convert_damage("UNKNOWN"))
)

cat("\nDamage conversion function: PASS\n")

# Clean and transform integrated dataset

storm_clean <- storm_data %>%
  mutate(

    STATE = trimws(STATE),

    EVENT_TYPE = trimws(EVENT_TYPE),

    # Calculate population health impacts

    TOTAL_DEATHS =
      DEATHS_DIRECT + DEATHS_INDIRECT,

    TOTAL_INJURIES =
      INJURIES_DIRECT + INJURIES_INDIRECT,

    TOTAL_CASUALTIES =
      TOTAL_DEATHS + TOTAL_INJURIES,

    # Convert economic damage to dollars

    PROPERTY_DAMAGE_USD =
      convert_damage(DAMAGE_PROPERTY),

    CROP_DAMAGE_USD =
      convert_damage(DAMAGE_CROPS),

    # Identify incomplete damage information

    DAMAGE_INCOMPLETE =
      is.na(PROPERTY_DAMAGE_USD) |
      is.na(CROP_DAMAGE_USD),

    # Calculate known reported economic damage

    TOTAL_DAMAGE_USD =
      coalesce(PROPERTY_DAMAGE_USD, 0) +
      coalesce(CROP_DAMAGE_USD, 0),

    # Organize months chronologically

    MONTH_NAME = factor(
      MONTH_NAME,
      levels = month.name,
      ordered = TRUE
    ),

    MONTH_NUMBER = as.integer(MONTH_NAME),

    # Assign meteorological seasons

    SEASON = case_when(
      MONTH_NUMBER %in% c(12, 1, 2) ~ "Winter",
      MONTH_NUMBER %in% c(3, 4, 5) ~ "Spring",
      MONTH_NUMBER %in% c(6, 7, 8) ~ "Summer",
      MONTH_NUMBER %in% c(9, 10, 11) ~ "Fall",
      TRUE ~ NA_character_
    ),

    SEASON = factor(
      SEASON,
      levels = c("Winter", "Spring", "Summer", "Fall")
    )
  )

# ============================================================
# SECTION 7: CLEANED DATA VALIDATION
# ============================================================

cat("\n========================================\n")
cat("CLEANED DATASET VALIDATION\n")
cat("========================================\n")

cat("\nCleaned dataset rows:",
    nrow(storm_clean), "\n")

cat("Cleaned dataset columns:",
    ncol(storm_clean), "\n")

cat("Duplicate event IDs:",
    sum(duplicated(storm_clean$EVENT_ID)), "\n")

# Missing values in analytical variables

cat("\nMissing values in analytical variables:\n")

analysis_columns <- c(
  "EVENT_ID",
  "STATE",
  "EVENT_TYPE",
  "TOTAL_DEATHS",
  "TOTAL_INJURIES",
  "TOTAL_CASUALTIES",
  "PROPERTY_DAMAGE_USD",
  "CROP_DAMAGE_USD",
  "TOTAL_DAMAGE_USD",
  "MONTH_NUMBER",
  "SEASON"
)

print(
  colSums(is.na(storm_clean[, analysis_columns]))
)

# Identify unrecognized damage values

cat("\nUnrecognized property damage values:\n")

print(
  unique(
    storm_clean$DAMAGE_PROPERTY[
      is.na(storm_clean$PROPERTY_DAMAGE_USD) &
      !is.na(storm_clean$DAMAGE_PROPERTY)
    ]
  )
)

cat("\nUnrecognized crop damage values:\n")

print(
  unique(
    storm_clean$DAMAGE_CROPS[
      is.na(storm_clean$CROP_DAMAGE_USD) &
      !is.na(storm_clean$DAMAGE_CROPS)
    ]
  )
)

cat("\nEvents with incomplete damage information:",
    sum(storm_clean$DAMAGE_INCOMPLETE), "\n")

# Population health totals

cat("\n========================================\n")
cat("PRELIMINARY POPULATION HEALTH RESULTS\n")
cat("========================================\n")

cat("\nDirect deaths:",
    sum(storm_clean$DEATHS_DIRECT, na.rm = TRUE), "\n")

cat("Indirect deaths:",
    sum(storm_clean$DEATHS_INDIRECT, na.rm = TRUE), "\n")

cat("Total deaths:",
    sum(storm_clean$TOTAL_DEATHS, na.rm = TRUE), "\n")

cat("Direct injuries:",
    sum(storm_clean$INJURIES_DIRECT, na.rm = TRUE), "\n")

cat("Indirect injuries:",
    sum(storm_clean$INJURIES_INDIRECT, na.rm = TRUE), "\n")

cat("Total injuries:",
    sum(storm_clean$TOTAL_INJURIES, na.rm = TRUE), "\n")

cat("Total casualties:",
    sum(storm_clean$TOTAL_CASUALTIES, na.rm = TRUE), "\n")

cat("Fatality detail records:",
    sum(storm_clean$FATALITY_RECORDS), "\n")

# Compare reported deaths with fatality records

cat("Events with differing death and fatality counts:",
    sum(
      storm_clean$TOTAL_DEATHS !=
        storm_clean$FATALITY_RECORDS,
      na.rm = TRUE
    ), "\n")

# Economic damage totals

cat("\n========================================\n")
cat("PRELIMINARY ECONOMIC DAMAGE RESULTS\n")
cat("========================================\n")

cat("\nReported property damage:",
    dollar(sum(
      storm_clean$PROPERTY_DAMAGE_USD,
      na.rm = TRUE
    )), "\n")

cat("Reported crop damage:",
    dollar(sum(
      storm_clean$CROP_DAMAGE_USD,
      na.rm = TRUE
    )), "\n")

cat("Total known reported economic damage:",
    dollar(sum(
      storm_clean$TOTAL_DAMAGE_USD,
      na.rm = TRUE
    )), "\n")

# Monthly distribution

cat("\nStorm events by month:\n")

print(table(storm_clean$MONTH_NAME, useNA = "ifany"))

# Seasonal distribution

cat("\nStorm events by season:\n")

print(table(storm_clean$SEASON, useNA = "ifany"))

# Final integrity checks

stopifnot(
  nrow(storm_clean) == nrow(details),
  !anyDuplicated(storm_clean$EVENT_ID),
  all(storm_clean$YEAR == 2024),
  sum(is.na(storm_clean$MONTH_NUMBER)) == 0,
  sum(is.na(storm_clean$SEASON)) == 0,
  all(storm_clean$TOTAL_DAMAGE_USD >= 0),
  sum(storm_clean$FATALITY_RECORDS) == nrow(fatalities),
  sum(storm_clean$LOCATION_RECORDS) == nrow(locations)
)

# Save cleaned dataset

write_csv(
  storm_clean,
  "output/noaa_storm_clean_2024.csv"
)

cat("\nCleaned dataset saved successfully.\n")

cat("\n========================================\n")
cat("DATA CLEANING VALIDATION: PASS\n")
cat("========================================\n")

# ============================================================
# SECTION 8: RESEARCH QUESTION 1 - POPULATION HEALTH
# ============================================================

cat("\n========================================\n")
cat("RESEARCH QUESTION 1 - POPULATION HEALTH\n")
cat("========================================\n")

dir.create("figures", showWarnings = FALSE)

# Document fatality discrepancies

fatality_discrepancies <- storm_clean %>%
  filter(TOTAL_DEATHS != FATALITY_RECORDS) %>%
  select(
    EVENT_ID,
    STATE,
    EVENT_TYPE,
    MONTH_NAME,
    DEATHS_DIRECT,
    DEATHS_INDIRECT,
    TOTAL_DEATHS,
    FATALITY_RECORDS
  ) %>%
  mutate(
    DIFFERENCE = FATALITY_RECORDS - TOTAL_DEATHS
  )

cat("\nFatality reconciliation:\n")

print(fatality_discrepancies, width = Inf)

write_csv(
  fatality_discrepancies,
  "output/fatality_reconciliation.csv"
)

# Summarize health impacts by event type

health_summary <- storm_clean %>%
  group_by(EVENT_TYPE) %>%
  summarise(
    TOTAL_EVENTS = n(),
    DIRECT_DEATHS = sum(DEATHS_DIRECT),
    INDIRECT_DEATHS = sum(DEATHS_INDIRECT),
    TOTAL_DEATHS = sum(TOTAL_DEATHS),
    DIRECT_INJURIES = sum(INJURIES_DIRECT),
    INDIRECT_INJURIES = sum(INJURIES_INDIRECT),
    TOTAL_INJURIES = sum(TOTAL_INJURIES),
    TOTAL_CASUALTIES = sum(TOTAL_CASUALTIES),
    .groups = "drop"
  ) %>%
  mutate(
    CASUALTIES_PER_100_EVENTS =
      round(TOTAL_CASUALTIES / TOTAL_EVENTS * 100, 2)
  ) %>%
  arrange(desc(TOTAL_CASUALTIES))

# Validate population health summary

stopifnot(
  nrow(health_summary) == 50,
  sum(health_summary$TOTAL_EVENTS) == nrow(storm_clean),
  sum(health_summary$TOTAL_DEATHS) ==
    sum(storm_clean$TOTAL_DEATHS),
  sum(health_summary$TOTAL_INJURIES) ==
    sum(storm_clean$TOTAL_INJURIES),
  sum(health_summary$TOTAL_CASUALTIES) ==
    sum(storm_clean$TOTAL_CASUALTIES)
)

# Top 10 event types by deaths

top_deaths <- health_summary %>%
  arrange(desc(TOTAL_DEATHS), EVENT_TYPE) %>%
  slice_head(n = 10)

cat("\nTOP 10 EVENT TYPES BY DEATHS:\n")

print(
  top_deaths %>%
    select(EVENT_TYPE, TOTAL_EVENTS, TOTAL_DEATHS),
  n = 10
)

# Top 10 event types by injuries

top_injuries <- health_summary %>%
  arrange(desc(TOTAL_INJURIES), EVENT_TYPE) %>%
  slice_head(n = 10)

cat("\nTOP 10 EVENT TYPES BY INJURIES:\n")

print(
  top_injuries %>%
    select(EVENT_TYPE, TOTAL_EVENTS, TOTAL_INJURIES),
  n = 10
)

# Top 10 event types by casualties

top_casualties <- health_summary %>%
  slice_head(n = 10)

cat("\nTOP 10 EVENT TYPES BY TOTAL CASUALTIES:\n")

print(
  top_casualties %>%
    select(
      EVENT_TYPE,
      TOTAL_DEATHS,
      TOTAL_INJURIES,
      TOTAL_CASUALTIES
    ),
  n = 10
)

# Save population health summary

write_csv(
  health_summary,
  "output/population_health_by_event.csv"
)

# ============================================================
# FIGURE 1: TOP 10 EVENT TYPES BY DEATHS
# ============================================================

plot_deaths <- ggplot(
  top_deaths,
  aes(
    x = reorder(EVENT_TYPE, TOTAL_DEATHS),
    y = TOTAL_DEATHS
  )
) +
  geom_col(fill = "firebrick") +
  geom_text(
    aes(label = TOTAL_DEATHS),
    hjust = -0.15,
    size = 3.5
  ) +
  coord_flip() +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.15))
  ) +
  labs(
    title = "Top 10 Weather Event Types by Fatalities",
    subtitle = "NOAA Storm Events Database, 2024",
    x = "Weather Event Type",
    y = "Total Deaths",
    caption = "Source: NOAA Storm Events Database"
  ) +
  theme_minimal(base_size = 12)

ggsave(
  "figures/01_top_deaths.png",
  plot_deaths,
  width = 10,
  height = 6,
  dpi = 300
)

# ============================================================
# FIGURE 2: TOP 10 EVENT TYPES BY INJURIES
# ============================================================

plot_injuries <- ggplot(
  top_injuries,
  aes(
    x = reorder(EVENT_TYPE, TOTAL_INJURIES),
    y = TOTAL_INJURIES
  )
) +
  geom_col(fill = "steelblue") +
  geom_text(
    aes(label = TOTAL_INJURIES),
    hjust = -0.15,
    size = 3.5
  ) +
  coord_flip() +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.15))
  ) +
  labs(
    title = "Top 10 Weather Event Types by Injuries",
    subtitle = "NOAA Storm Events Database, 2024",
    x = "Weather Event Type",
    y = "Total Injuries",
    caption = "Source: NOAA Storm Events Database"
  ) +
  theme_minimal(base_size = 12)

ggsave(
  "figures/02_top_injuries.png",
  plot_injuries,
  width = 10,
  height = 6,
  dpi = 300
)

# ============================================================
# FIGURE 3: COMBINED POPULATION HEALTH IMPACT
# ============================================================

health_long <- top_casualties %>%
  select(
    EVENT_TYPE,
    TOTAL_DEATHS,
    TOTAL_INJURIES,
    TOTAL_CASUALTIES
  ) %>%
  pivot_longer(
    cols = c(TOTAL_DEATHS, TOTAL_INJURIES),
    names_to = "IMPACT_TYPE",
    values_to = "COUNT"
  ) %>%
  mutate(
    IMPACT_TYPE = recode(
      IMPACT_TYPE,
      TOTAL_DEATHS = "Deaths",
      TOTAL_INJURIES = "Injuries"
    )
  )

plot_casualties <- ggplot(
  health_long,
  aes(
    x = reorder(EVENT_TYPE, TOTAL_CASUALTIES),
    y = COUNT,
    fill = IMPACT_TYPE
  )
) +
  geom_col() +
  coord_flip() +
  scale_fill_manual(
    values = c(
      "Deaths" = "firebrick",
      "Injuries" = "steelblue"
    )
  ) +
  labs(
    title = "Weather Events with the Greatest Health Impact",
    subtitle = "Top 10 event types by combined casualties, 2024",
    x = "Weather Event Type",
    y = "Total Casualties",
    fill = "Health Impact",
    caption = "Source: NOAA Storm Events Database"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")

ggsave(
  "figures/03_combined_health_impact.png",
  plot_casualties,
  width = 10,
  height = 6,
  dpi = 300
)

# ============================================================
# FIGURE 4: DIRECT AND INDIRECT DEATHS
# ============================================================

death_comparison <- top_deaths %>%
  select(
    EVENT_TYPE,
    TOTAL_DEATHS,
    DIRECT_DEATHS,
    INDIRECT_DEATHS
  ) %>%
  pivot_longer(
    cols = c(DIRECT_DEATHS, INDIRECT_DEATHS),
    names_to = "DEATH_TYPE",
    values_to = "COUNT"
  ) %>%
  mutate(
    DEATH_TYPE = recode(
      DEATH_TYPE,
      DIRECT_DEATHS = "Direct",
      INDIRECT_DEATHS = "Indirect"
    )
  )

plot_death_comparison <- ggplot(
  death_comparison,
  aes(
    x = reorder(EVENT_TYPE, TOTAL_DEATHS),
    y = COUNT,
    fill = DEATH_TYPE
  )
) +
  geom_col() +
  coord_flip() +
  scale_fill_manual(
    values = c(
      "Direct" = "firebrick",
      "Indirect" = "orange"
    )
  ) +
  labs(
    title = "Direct and Indirect Weather-Related Deaths",
    subtitle = "Top 10 event types by fatalities, 2024",
    x = "Weather Event Type",
    y = "Number of Deaths",
    fill = "Death Type",
    caption = "Source: NOAA Storm Events Database"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")

ggsave(
  "figures/04_direct_indirect_deaths.png",
  plot_death_comparison,
  width = 10,
  height = 6,
  dpi = 300
)

# ============================================================
# RESEARCH QUESTION 1 - FINAL RESULTS
# ============================================================

cat("\n========================================\n")
cat("RESEARCH QUESTION 1 - FINAL RESULTS\n")
cat("========================================\n")

cat("\nLeading cause of weather-related deaths:",
    top_deaths$EVENT_TYPE[1], "\n")

cat("Total deaths:",
    top_deaths$TOTAL_DEATHS[1], "\n")

cat("\nLeading cause of weather-related injuries:",
    top_injuries$EVENT_TYPE[1], "\n")

cat("Total injuries:",
    top_injuries$TOTAL_INJURIES[1], "\n")

cat("\nGreatest combined population health impact:",
    top_casualties$EVENT_TYPE[1], "\n")

cat("Total casualties:",
    top_casualties$TOTAL_CASUALTIES[1], "\n")

cat("\nPercentage of all casualties:",
    round(
      top_casualties$TOTAL_CASUALTIES[1] /
        sum(health_summary$TOTAL_CASUALTIES) * 100,
      2
    ), "%\n")

# Verify output files

health_files <- c(
  "output/fatality_reconciliation.csv",
  "output/population_health_by_event.csv",
  "figures/01_top_deaths.png",
  "figures/02_top_injuries.png",
  "figures/03_combined_health_impact.png",
  "figures/04_direct_indirect_deaths.png"
)

stopifnot(all(file.exists(health_files)))

cat("\nAll six research output files saved successfully.\n")

cat("\n========================================\n")
cat("RESEARCH QUESTION 1 VALIDATION: PASS\n")
cat("========================================\n")


# ============================================================
# SECTION 9: GEOGRAPHIC DISTRIBUTION OF WEATHER EVENTS
# Research Question 2
# ============================================================

cat("\n========================================\n")
cat("GEOGRAPHIC DISTRIBUTION ANALYSIS\n")
cat("========================================\n")

# Summarize weather events by geographic area

state_summary <- storm_clean %>%
  group_by(STATE) %>%
  summarise(
    TOTAL_EVENTS = n(),
    TOTAL_DEATHS = sum(TOTAL_DEATHS, na.rm = TRUE),
    TOTAL_INJURIES = sum(TOTAL_INJURIES, na.rm = TRUE),
    TOTAL_CASUALTIES = sum(TOTAL_CASUALTIES, na.rm = TRUE),
    TOTAL_DAMAGE_USD = sum(TOTAL_DAMAGE_USD, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(TOTAL_EVENTS))

# Identify top 10 geographic areas by event frequency

top_state_events <- state_summary %>%
  slice_max(TOTAL_EVENTS, n = 10, with_ties = FALSE)

# Identify top 10 geographic areas by fatalities

top_state_deaths <- state_summary %>%
  slice_max(TOTAL_DEATHS, n = 10, with_ties = FALSE)

# Identify top 10 geographic areas by injuries

top_state_injuries <- state_summary %>%
  slice_max(TOTAL_INJURIES, n = 10, with_ties = FALSE)

# Identify top 10 geographic areas by combined casualties

top_state_casualties <- state_summary %>%
  slice_max(TOTAL_CASUALTIES, n = 10, with_ties = FALSE)

# Summarize weather event types by geographic area

state_event_types <- storm_clean %>%
  group_by(STATE, EVENT_TYPE) %>%
  summarise(
    TOTAL_EVENTS = n(),
    TOTAL_DEATHS = sum(TOTAL_DEATHS, na.rm = TRUE),
    TOTAL_INJURIES = sum(TOTAL_INJURIES, na.rm = TRUE),
    .groups = "drop"
  )

# Identify most frequent weather event type
# in each geographic area

dominant_state_events <- state_event_types %>%
  arrange(STATE, desc(TOTAL_EVENTS), EVENT_TYPE) %>%
  group_by(STATE) %>%
  slice_head(n = 1) %>%
  ungroup()

# Limit dominant event analysis to top 10 areas

top_dominant_events <- dominant_state_events %>%
  filter(STATE %in% top_state_events$STATE) %>%
  arrange(desc(TOTAL_EVENTS))

# Display analytical results

cat("\nTop 10 geographic areas by weather events:\n")
print(top_state_events, n = 10, width = Inf)

cat("\nTop 10 geographic areas by fatalities:\n")
print(top_state_deaths, n = 10, width = Inf)

cat("\nTop 10 geographic areas by injuries:\n")
print(top_state_injuries, n = 10, width = Inf)

cat("\nTop 10 geographic areas by combined casualties:\n")
print(top_state_casualties, n = 10, width = Inf)

cat("\nMost frequent event types in top 10 areas:\n")
print(top_dominant_events, n = 10, width = Inf)

# Save geographic summary tables

write_csv(
  state_summary,
  "output/geographic_summary.csv"
)

write_csv(
  state_event_types,
  "output/geographic_event_types.csv"
)

write_csv(
  dominant_state_events,
  "output/dominant_geographic_events.csv"
)

write_csv(
  top_state_casualties,
  "output/top_geographic_casualties.csv"
)

# ============================================================
# FIGURE 05: TOP GEOGRAPHIC AREAS BY EVENT FREQUENCY
# ============================================================

plot05 <- ggplot(
  top_state_events,
  aes(
    x = reorder(STATE, TOTAL_EVENTS),
    y = TOTAL_EVENTS
  )
) +
  geom_col(fill = "darkgreen") +
  geom_text(
    aes(label = scales::comma(TOTAL_EVENTS)),
    hjust = -0.1,
    size = 3.5
  ) +
  coord_flip() +
  scale_y_continuous(
    labels = scales::comma,
    expand = expansion(mult = c(0, 0.15))
  ) +
  labs(
    title = "Top 10 Geographic Areas by Weather Events",
    subtitle = "NOAA Storm Events Database, 2024",
    x = "Geographic Area",
    y = "Total Weather Events",
    caption = "Source: NOAA Storm Events Database"
  ) +
  theme_minimal(base_size = 13)

ggsave(
  "figures/05_geographic_event_frequency.png",
  plot = plot05,
  width = 10,
  height = 6,
  dpi = 300
)

cat("\nFigure 05 saved successfully.\n")

# ============================================================
# FIGURE 06: TOP GEOGRAPHIC AREAS BY FATALITIES
# ============================================================

plot06 <- ggplot(
  top_state_deaths,
  aes(
    x = reorder(STATE, TOTAL_DEATHS),
    y = TOTAL_DEATHS
  )
) +
  geom_col(fill = "firebrick") +
  geom_text(
    aes(label = scales::comma(TOTAL_DEATHS)),
    hjust = -0.1,
    size = 3.5
  ) +
  coord_flip() +
  scale_y_continuous(
    labels = scales::comma,
    expand = expansion(mult = c(0, 0.15))
  ) +
  labs(
    title = "Top 10 Geographic Areas by Weather-Related Deaths",
    subtitle = "NOAA Storm Events Database, 2024",
    x = "Geographic Area",
    y = "Total Deaths",
    caption = "Source: NOAA Storm Events Database"
  ) +
  theme_minimal(base_size = 13)

ggsave(
  "figures/06_geographic_fatalities.png",
  plot = plot06,
  width = 10,
  height = 6,
  dpi = 300
)

cat("Figure 06 saved successfully.\n")

# ============================================================
# FIGURE 07: TOP GEOGRAPHIC AREAS BY INJURIES
# ============================================================

plot07 <- ggplot(
  top_state_injuries,
  aes(
    x = reorder(STATE, TOTAL_INJURIES),
    y = TOTAL_INJURIES
  )
) +
  geom_col(fill = "steelblue") +
  geom_text(
    aes(label = scales::comma(TOTAL_INJURIES)),
    hjust = -0.1,
    size = 3.5
  ) +
  coord_flip() +
  scale_y_continuous(
    labels = scales::comma,
    expand = expansion(mult = c(0, 0.15))
  ) +
  labs(
    title = "Top 10 Geographic Areas by Weather-Related Injuries",
    subtitle = "NOAA Storm Events Database, 2024",
    x = "Geographic Area",
    y = "Total Injuries",
    caption = "Source: NOAA Storm Events Database"
  ) +
  theme_minimal(base_size = 13)

ggsave(
  "figures/07_geographic_injuries.png",
  plot = plot07,
  width = 10,
  height = 6,
  dpi = 300
)

cat("Figure 07 saved successfully.\n")

# ============================================================
# FIGURE 08: DOMINANT WEATHER EVENTS BY GEOGRAPHIC AREA
# ============================================================

plot08 <- ggplot(
  top_dominant_events,
  aes(
    x = reorder(STATE, TOTAL_EVENTS),
    y = TOTAL_EVENTS,
    fill = EVENT_TYPE
  )
) +
  geom_col() +
  geom_text(
    aes(label = scales::comma(TOTAL_EVENTS)),
    hjust = -0.1,
    size = 3.5
  ) +
  coord_flip() +
  scale_y_continuous(
    labels = scales::comma,
    expand = expansion(mult = c(0, 0.15))
  ) +
  labs(
    title = "Most Frequent Weather Event by Geographic Area",
    subtitle = "10 geographic areas with the most weather events, 2024",
    x = "Geographic Area",
    y = "Frequency of Dominant Event Type",
    fill = "Weather Event Type",
    caption = "Source: NOAA Storm Events Database"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom") +
  guides(fill = guide_legend(nrow = 2))

ggsave(
  "figures/08_dominant_geographic_events.png",
  plot = plot08,
  width = 11,
  height = 7,
  dpi = 300
)

cat("Figure 08 saved successfully.\n")

# ============================================================
# GEOGRAPHIC ANALYSIS VALIDATION
# ============================================================

cat("\n========================================\n")
cat("GEOGRAPHIC ANALYSIS VALIDATION\n")
cat("========================================\n")

cat("Geographic areas:", nrow(state_summary), "\n")
cat("Total weather events:", sum(state_summary$TOTAL_EVENTS), "\n")
cat("Total deaths:", sum(state_summary$TOTAL_DEATHS), "\n")
cat("Total injuries:", sum(state_summary$TOTAL_INJURIES), "\n")
cat("Total casualties:", sum(state_summary$TOTAL_CASUALTIES), "\n")

stopifnot(
  nrow(state_summary) == 68,
  sum(state_summary$TOTAL_EVENTS) == nrow(storm_clean),
  sum(state_summary$TOTAL_DEATHS) == sum(storm_clean$TOTAL_DEATHS),
  sum(state_summary$TOTAL_INJURIES) == sum(storm_clean$TOTAL_INJURIES),
  sum(state_summary$TOTAL_CASUALTIES) == sum(storm_clean$TOTAL_CASUALTIES),
  sum(state_event_types$TOTAL_EVENTS) == nrow(storm_clean),
  nrow(dominant_state_events) == nrow(state_summary),
  nrow(top_dominant_events) == 10
)

stopifnot(
  all(file.exists(c(
    "figures/05_geographic_event_frequency.png",
    "figures/06_geographic_fatalities.png",
    "figures/07_geographic_injuries.png",
    "figures/08_dominant_geographic_events.png",
    "output/geographic_summary.csv",
    "output/geographic_event_types.csv",
    "output/dominant_geographic_events.csv",
    "output/top_geographic_casualties.csv"
  )))
)

cat("\nAll geographic summary tables saved successfully.\n")
cat("All four geographic visualizations saved successfully.\n")

cat("\n========================================\n")
cat("GEOGRAPHIC ANALYSIS VALIDATION: PASS\n")
cat("========================================\n")


# ============================================================
# STEP 7: SEASONAL PATTERNS ANALYSIS
# ============================================================

cat("\n========================================\n")
cat("SEASONAL PATTERNS ANALYSIS\n")
cat("========================================\n")

library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(readr)

dir.create("figures", showWarnings = FALSE)
dir.create("output", showWarnings = FALSE)

# ------------------------------------------------------------
# 7.1 Load cleaned dataset
# ------------------------------------------------------------

clean_files <- list.files(
  "output",
  pattern = "clean.*\\.csv$",
  full.names = TRUE,
  ignore.case = TRUE
)

if (length(clean_files) != 1) {
  stop("Expected exactly one cleaned CSV in the output folder.")
}

season_data <- read_csv(
  clean_files[1],
  show_col_types = FALSE
)

season_data <- season_data %>%
  mutate(
    MONTH_NAME = factor(
      MONTH_NAME,
      levels = month.name
    ),
    SEASON = factor(
      SEASON,
      levels = c("Winter", "Spring", "Summer", "Fall")
    )
  )

# ------------------------------------------------------------
# 7.2 Monthly summary
# ------------------------------------------------------------

monthly_summary <- season_data %>%
  group_by(MONTH_NAME) %>%
  summarise(
    TOTAL_EVENTS = n(),
    TOTAL_DEATHS = sum(TOTAL_DEATHS, na.rm = TRUE),
    TOTAL_INJURIES = sum(TOTAL_INJURIES, na.rm = TRUE),
    TOTAL_CASUALTIES = sum(TOTAL_CASUALTIES, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(MONTH_NAME)

print(monthly_summary, n = 12)

write_csv(
  monthly_summary,
  "output/monthly_weather_summary.csv"
)

# ------------------------------------------------------------
# 7.3 Seasonal summary
# ------------------------------------------------------------

seasonal_summary <- season_data %>%
  group_by(SEASON) %>%
  summarise(
    TOTAL_EVENTS = n(),
    TOTAL_DEATHS = sum(TOTAL_DEATHS, na.rm = TRUE),
    TOTAL_INJURIES = sum(TOTAL_INJURIES, na.rm = TRUE),
    TOTAL_CASUALTIES = sum(TOTAL_CASUALTIES, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    EVENT_PERCENTAGE = round(
      TOTAL_EVENTS / sum(TOTAL_EVENTS) * 100, 2
    )
  )

print(seasonal_summary)

write_csv(
  seasonal_summary,
  "output/seasonal_weather_summary.csv"
)

# ------------------------------------------------------------
# 7.4 Weather event types by season
# ------------------------------------------------------------

seasonal_event_types <- season_data %>%
  group_by(SEASON, EVENT_TYPE) %>%
  summarise(
    TOTAL_EVENTS = n(),
    TOTAL_DEATHS = sum(TOTAL_DEATHS, na.rm = TRUE),
    TOTAL_INJURIES = sum(TOTAL_INJURIES, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(SEASON, desc(TOTAL_EVENTS))

write_csv(
  seasonal_event_types,
  "output/seasonal_event_types.csv"
)

dominant_seasonal_events <- seasonal_event_types %>%
  group_by(SEASON) %>%
  slice_max(
    order_by = TOTAL_EVENTS,
    n = 1,
    with_ties = FALSE
  ) %>%
  ungroup()

cat("\nMost frequent weather event by season:\n")
print(dominant_seasonal_events)

write_csv(
  dominant_seasonal_events,
  "output/dominant_seasonal_events.csv"
)

# ------------------------------------------------------------
# FIGURE 09: Monthly weather event frequency
# ------------------------------------------------------------

plot09 <- ggplot(
  monthly_summary,
  aes(x = MONTH_NAME, y = TOTAL_EVENTS)
) +
  geom_col(fill = "darkgreen") +
  geom_text(
    aes(label = comma(TOTAL_EVENTS)),
    vjust = -0.4,
    size = 3.5
  ) +
  scale_y_continuous(
    labels = comma,
    expand = expansion(mult = c(0, 0.12))
  ) +
  labs(
    title = "Monthly Weather Event Frequency",
    subtitle = "NOAA Storm Events Database, 2024",
    x = "Month",
    y = "Total Weather Events",
    caption = "Source: NOAA Storm Events Database"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    )
  )

ggsave(
  "figures/09_monthly_event_frequency.png",
  plot09,
  width = 10,
  height = 6,
  dpi = 300
)

# ------------------------------------------------------------
# FIGURE 10: Seasonal weather event frequency
# ------------------------------------------------------------

plot10 <- ggplot(
  seasonal_summary,
  aes(x = SEASON, y = TOTAL_EVENTS, fill = SEASON)
) +
  geom_col(width = 0.65) +
  geom_text(
    aes(
      label = paste0(
        comma(TOTAL_EVENTS),
        "\n(",
        EVENT_PERCENTAGE,
        "%)"
      )
    ),
    vjust = -0.3,
    size = 4
  ) +
  scale_fill_manual(
    values = c(
      "Winter" = "steelblue",
      "Spring" = "forestgreen",
      "Summer" = "orange",
      "Fall" = "sienna"
    )
  ) +
  scale_y_continuous(
    labels = comma,
    expand = expansion(mult = c(0, 0.18))
  ) +
  labs(
    title = "Weather Event Frequency by Season",
    subtitle = "NOAA Storm Events Database, 2024",
    x = "Season",
    y = "Total Weather Events",
    caption = "Source: NOAA Storm Events Database"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none")

ggsave(
  "figures/10_seasonal_event_frequency.png",
  plot10,
  width = 10,
  height = 6,
  dpi = 300
)

# ------------------------------------------------------------
# FIGURE 11: Monthly deaths and injuries
# ------------------------------------------------------------

monthly_health <- monthly_summary %>%
  select(
    MONTH_NAME,
    Deaths = TOTAL_DEATHS,
    Injuries = TOTAL_INJURIES
  ) %>%
  pivot_longer(
    cols = c(Deaths, Injuries),
    names_to = "HEALTH_IMPACT",
    values_to = "TOTAL"
  )

plot11 <- ggplot(
  monthly_health,
  aes(
    x = MONTH_NAME,
    y = TOTAL,
    fill = HEALTH_IMPACT
  )
) +
  geom_col(position = "dodge") +
  scale_fill_manual(
    values = c(
      "Deaths" = "firebrick",
      "Injuries" = "steelblue"
    )
  ) +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Monthly Weather-Related Deaths and Injuries",
    subtitle = "Population health impacts during 2024",
    x = "Month",
    y = "Number of Deaths and Injuries",
    fill = "Health Impact",
    caption = "Source: NOAA Storm Events Database"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    ),
    legend.position = "bottom"
  )

ggsave(
  "figures/11_monthly_health_impact.png",
  plot11,
  width = 10,
  height = 6,
  dpi = 300
)

# ------------------------------------------------------------
# FIGURE 12: Most frequent weather event by season
# ------------------------------------------------------------

plot12 <- ggplot(
  dominant_seasonal_events,
  aes(
    x = SEASON,
    y = TOTAL_EVENTS,
    fill = EVENT_TYPE
  )
) +
  geom_col(width = 0.65) +
  geom_text(
    aes(label = comma(TOTAL_EVENTS)),
    vjust = -0.4,
    size = 4
  ) +
  scale_y_continuous(
    labels = comma,
    expand = expansion(mult = c(0, 0.15))
  ) +
  labs(
    title = "Most Frequent Weather Event Type by Season",
    subtitle = "Dominant weather events during 2024",
    x = "Season",
    y = "Frequency of Dominant Event Type",
    fill = "Weather Event Type",
    caption = "Source: NOAA Storm Events Database"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom")

ggsave(
  "figures/12_dominant_seasonal_events.png",
  plot12,
  width = 10,
  height = 6,
  dpi = 300
)

# ------------------------------------------------------------
# 7.5 Validation
# ------------------------------------------------------------

cat("\n========================================\n")
cat("SEASONAL ANALYSIS VALIDATION\n")
cat("========================================\n")

stopifnot(
  nrow(season_data) == 69802,
  !anyNA(season_data$MONTH_NAME),
  !anyNA(season_data$SEASON),
  nrow(monthly_summary) == 12,
  nrow(seasonal_summary) == 4,
  nrow(dominant_seasonal_events) == 4,
  sum(monthly_summary$TOTAL_EVENTS) == 69802,
  sum(seasonal_summary$TOTAL_EVENTS) == 69802,
  sum(monthly_summary$TOTAL_DEATHS) == 1141,
  sum(monthly_summary$TOTAL_INJURIES) == 1840,
  sum(seasonal_summary$TOTAL_DEATHS) == 1141,
  sum(seasonal_summary$TOTAL_INJURIES) == 1840,
  sum(seasonal_summary$TOTAL_CASUALTIES) == 2981
)

cat("Monthly event totals: PASS\n")
cat("Seasonal event totals: PASS\n")
cat("Death totals: PASS\n")
cat("Injury totals: PASS\n")
cat("Combined casualty totals: PASS\n")

cat("\nMonth with most weather events:\n")
print(slice_max(monthly_summary, TOTAL_EVENTS, n = 1))

cat("\nMonth with most fatalities:\n")
print(slice_max(monthly_summary, TOTAL_DEATHS, n = 1))

cat("\nMonth with most injuries:\n")
print(slice_max(monthly_summary, TOTAL_INJURIES, n = 1))

cat("\nSeason with greatest health impact:\n")
print(slice_max(seasonal_summary, TOTAL_CASUALTIES, n = 1))

expected_figures <- c(
  "figures/09_monthly_event_frequency.png",
  "figures/10_seasonal_event_frequency.png",
  "figures/11_monthly_health_impact.png",
  "figures/12_dominant_seasonal_events.png"
)

stopifnot(all(file.exists(expected_figures)))

cat("\nAll four seasonal visualizations saved successfully.\n")

cat("\n========================================\n")
cat("SEASONAL ANALYSIS VALIDATION: PASS\n")
cat("========================================\n")


# ============================================================
# STEP 8: ECONOMIC DAMAGE ANALYSIS
# ============================================================

cat("\n========================================\n")
cat("ECONOMIC DAMAGE ANALYSIS\n")
cat("========================================\n")

# Locate the cleaned dataset
cleaned_files <- list.files(
  "output",
  pattern = "clean.*\\.csv$",
  full.names = TRUE,
  ignore.case = TRUE
)

stopifnot(length(cleaned_files) == 1)

economic_data <- readr::read_csv(
  cleaned_files[1],
  show_col_types = FALSE
)

stopifnot(nrow(economic_data) == 69802)
stopifnot(anyDuplicated(economic_data$EVENT_ID) == 0)

# Economic damage by weather event type

economic_by_event <- economic_data %>%
  group_by(EVENT_TYPE) %>%
  summarise(
    TOTAL_EVENTS = n(),
    PROPERTY_DAMAGE = sum(
      PROPERTY_DAMAGE_USD, na.rm = TRUE
    ),
    CROP_DAMAGE = sum(
      CROP_DAMAGE_USD, na.rm = TRUE
    ),
    TOTAL_DAMAGE = sum(
      TOTAL_DAMAGE_USD, na.rm = TRUE
    ),
    INCOMPLETE_DAMAGE_RECORDS = sum(
      is.na(PROPERTY_DAMAGE_USD) |
      is.na(CROP_DAMAGE_USD)
    ),
    .groups = "drop"
  ) %>%
  arrange(desc(TOTAL_DAMAGE))

# Top 10 event types by property damage

top_property_damage <- economic_by_event %>%
  slice_max(
    PROPERTY_DAMAGE,
    n = 10,
    with_ties = FALSE
  )

# Top 10 event types by crop damage

top_crop_damage <- economic_by_event %>%
  slice_max(
    CROP_DAMAGE,
    n = 10,
    with_ties = FALSE
  )

# Top 10 event types by combined economic damage

top_total_damage <- economic_by_event %>%
  slice_max(
    TOTAL_DAMAGE,
    n = 10,
    with_ties = FALSE
  )

# Economic damage by geographic area

economic_by_state <- economic_data %>%
  group_by(STATE) %>%
  summarise(
    TOTAL_EVENTS = n(),
    PROPERTY_DAMAGE = sum(
      PROPERTY_DAMAGE_USD, na.rm = TRUE
    ),
    CROP_DAMAGE = sum(
      CROP_DAMAGE_USD, na.rm = TRUE
    ),
    TOTAL_DAMAGE = sum(
      TOTAL_DAMAGE_USD, na.rm = TRUE
    ),
    .groups = "drop"
  ) %>%
  arrange(desc(TOTAL_DAMAGE))

# Economic damage by month

economic_by_month <- economic_data %>%
  group_by(MONTH_NUMBER) %>%
  summarise(
    TOTAL_EVENTS = n(),
    PROPERTY_DAMAGE = sum(
      PROPERTY_DAMAGE_USD, na.rm = TRUE
    ),
    CROP_DAMAGE = sum(
      CROP_DAMAGE_USD, na.rm = TRUE
    ),
    TOTAL_DAMAGE = sum(
      TOTAL_DAMAGE_USD, na.rm = TRUE
    ),
    .groups = "drop"
  ) %>%
  arrange(MONTH_NUMBER) %>%
  mutate(
    MONTH = month.name[MONTH_NUMBER]
  )

# Economic damage by season

economic_by_season <- economic_data %>%
  group_by(SEASON) %>%
  summarise(
    TOTAL_EVENTS = n(),
    PROPERTY_DAMAGE = sum(
      PROPERTY_DAMAGE_USD, na.rm = TRUE
    ),
    CROP_DAMAGE = sum(
      CROP_DAMAGE_USD, na.rm = TRUE
    ),
    TOTAL_DAMAGE = sum(
      TOTAL_DAMAGE_USD, na.rm = TRUE
    ),
    .groups = "drop"
  ) %>%
  mutate(
    SEASON = factor(
      SEASON,
      levels = c(
        "Winter", "Spring", "Summer", "Fall"
      )
    )
  ) %>%
  arrange(SEASON)

# Display analytical results

cat("\nTOP 10 EVENT TYPES BY PROPERTY DAMAGE\n")

print(
  top_property_damage %>%
    select(EVENT_TYPE, PROPERTY_DAMAGE) %>%
    mutate(
      PROPERTY_DAMAGE = scales::dollar(
        PROPERTY_DAMAGE, accuracy = 1
      )
    ),
  n = 10
)

cat("\nTOP 10 EVENT TYPES BY CROP DAMAGE\n")

print(
  top_crop_damage %>%
    select(EVENT_TYPE, CROP_DAMAGE) %>%
    mutate(
      CROP_DAMAGE = scales::dollar(
        CROP_DAMAGE, accuracy = 1
      )
    ),
  n = 10
)

cat("\nTOP 10 EVENT TYPES BY TOTAL ECONOMIC DAMAGE\n")

print(
  top_total_damage %>%
    select(EVENT_TYPE, TOTAL_DAMAGE) %>%
    mutate(
      TOTAL_DAMAGE = scales::dollar(
        TOTAL_DAMAGE, accuracy = 1
      )
    ),
  n = 10
)

cat("\nTOP 10 GEOGRAPHIC AREAS BY ECONOMIC DAMAGE\n")

print(
  economic_by_state %>%
    slice_head(n = 10) %>%
    select(STATE, TOTAL_DAMAGE) %>%
    mutate(
      TOTAL_DAMAGE = scales::dollar(
        TOTAL_DAMAGE, accuracy = 1
      )
    ),
  n = 10
)

cat("\nECONOMIC DAMAGE BY MONTH\n")

print(
  economic_by_month %>%
    select(MONTH, TOTAL_DAMAGE) %>%
    mutate(
      TOTAL_DAMAGE = scales::dollar(
        TOTAL_DAMAGE, accuracy = 1
      )
    ),
  n = 12
)

cat("\nECONOMIC DAMAGE BY SEASON\n")

print(
  economic_by_season %>%
    select(SEASON, TOTAL_DAMAGE) %>%
    mutate(
      TOTAL_DAMAGE = scales::dollar(
        TOTAL_DAMAGE, accuracy = 1
      )
    ),
  n = 4
)

# Missing economic damage information

incomplete_damage <- sum(
  is.na(economic_data$PROPERTY_DAMAGE_USD) |
  is.na(economic_data$CROP_DAMAGE_USD)
)

incomplete_percentage <- (
  incomplete_damage / nrow(economic_data)
) * 100

cat("\n========================================\n")
cat("ECONOMIC DAMAGE DATA QUALITY\n")
cat("========================================\n")

cat(
  "Events with incomplete damage information:",
  incomplete_damage, "\n"
)

cat(
  "Percentage of incomplete damage records:",
  round(incomplete_percentage, 2), "%\n"
)

# Validate economic damage totals

expected_total <- sum(
  economic_data$TOTAL_DAMAGE_USD,
  na.rm = TRUE
)

stopifnot(
  isTRUE(all.equal(
    sum(economic_by_event$TOTAL_DAMAGE),
    expected_total
  )),
  isTRUE(all.equal(
    sum(economic_by_state$TOTAL_DAMAGE),
    expected_total
  )),
  isTRUE(all.equal(
    sum(economic_by_month$TOTAL_DAMAGE),
    expected_total
  )),
  isTRUE(all.equal(
    sum(economic_by_season$TOTAL_DAMAGE),
    expected_total
  )),
  isTRUE(all.equal(
    expected_total,
    17996610040
  )),
  sum(economic_by_event$TOTAL_EVENTS) == 69802,
  sum(economic_by_state$TOTAL_EVENTS) == 69802,
  sum(economic_by_month$TOTAL_EVENTS) == 69802,
  sum(economic_by_season$TOTAL_EVENTS) == 69802
)

# Save analytical tables

write_csv(
  economic_by_event,
  "output/economic_damage_by_event.csv"
)

write_csv(
  economic_by_state,
  "output/economic_damage_by_state.csv"
)

write_csv(
  economic_by_month,
  "output/economic_damage_by_month.csv"
)

write_csv(
  economic_by_season,
  "output/economic_damage_by_season.csv"
)

cat("\nTotal known reported economic damage:")

cat(
  scales::dollar(expected_total, accuracy = 1),
  "\n"
)

cat("\nEconomic damage tables saved successfully.\n")

cat("\n========================================\n")
cat("ECONOMIC DAMAGE ANALYSIS VALIDATION: PASS\n")
cat("========================================\n")


# ============================================================
# STEP 9: ECONOMIC DAMAGE VISUALIZATIONS
# ============================================================

cat("\n========================================\n")
cat("ECONOMIC DAMAGE VISUALIZATIONS\n")
cat("========================================\n")

library(ggplot2)
library(dplyr)
library(scales)

dir.create(
  "figures",
  showWarnings = FALSE,
  recursive = TRUE
)

# Economic damage axis formatting

damage_labels <- function(x) {
  scales::dollar(
    x / 1e9,
    suffix = "B",
    accuracy = 0.1
  )
}

# Format individual economic damage bar labels

format_damage_label <- function(x) {
  ifelse(
    x >= 1e9,
    sprintf("$%.2fB", x / 1e9),
    sprintf("$%.1fM", x / 1e6)
  )
}

# Consistent visualization theme

economic_theme <- theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(size = 18),
    plot.subtitle = element_text(size = 14),
    plot.caption = element_text(size = 11),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.position = "bottom"
  )

# Reusable horizontal bar chart function

create_damage_plot <- function(
    data, category, damage, title, fill_color) {

  ggplot(
    data,
    aes(
      x = reorder(
        .data[[category]],
        .data[[damage]]
      ),
      y = .data[[damage]]
    )
  ) +
    geom_col(fill = fill_color) +
    geom_text(
      aes(
        label = if (damage == "CROP_DAMAGE") {
            format_damage_label(.data[[damage]])
        } else {
            damage_labels(.data[[damage]])
        }
      ),
      hjust = -0.1,
      size = 4
    ) +
    coord_flip() +
    scale_y_continuous(
      labels = damage_labels,
      expand = expansion(mult = c(0, 0.18))
    ) +
    labs(
      title = title,
      subtitle = "NOAA Storm Events Database, 2024",
      x = ifelse(
        category == "STATE",
        "Geographic Area",
        "Weather Event Type"
      ),
      y = "Known Reported Damage (USD)",
      caption = "Source: NOAA Storm Events Database"
    ) +
    economic_theme
}

# ============================================================
# FIGURE 13: PROPERTY DAMAGE
# ============================================================

figure13 <- create_damage_plot(
  top_property_damage,
  "EVENT_TYPE",
  "PROPERTY_DAMAGE",
  "Top 10 Weather Event Types by Property Damage",
  "steelblue"
)

ggsave(
  "figures/figure13_property_damage.png",
  plot = figure13,
  width = 12,
  height = 7.2,
  dpi = 300
)

cat("Figure 13 saved successfully.\n")

# ============================================================
# FIGURE 14: CROP DAMAGE
# ============================================================

figure14 <- create_damage_plot(
  top_crop_damage,
  "EVENT_TYPE",
  "CROP_DAMAGE",
  "Top 10 Weather Event Types by Crop Damage",
  "darkgreen"
) +
  scale_y_continuous(
    labels = scales::label_dollar(
      scale = 1e-6,
      suffix = "M",
      accuracy = 0.1
    ),
    expand = expansion(mult = c(0, 0.18))
  )

ggsave(
  "figures/figure14_crop_damage.png",
  plot = figure14,
  width = 12,
  height = 7.2,
  dpi = 300
)

cat("Figure 14 saved successfully.\n")

# ============================================================
# FIGURE 15: COMBINED ECONOMIC DAMAGE
# ============================================================

combined_damage_plot <- top_total_damage %>%
  select(
    EVENT_TYPE,
    PROPERTY_DAMAGE,
    CROP_DAMAGE,
    TOTAL_DAMAGE
  ) %>%
  tidyr::pivot_longer(
    cols = c(PROPERTY_DAMAGE, CROP_DAMAGE),
    names_to = "DAMAGE_TYPE",
    values_to = "DAMAGE_USD"
  ) %>%
  mutate(
    DAMAGE_TYPE = recode(
      DAMAGE_TYPE,
      PROPERTY_DAMAGE = "Property Damage",
      CROP_DAMAGE = "Crop Damage"
    )
  )

figure15 <- ggplot(
  combined_damage_plot,
  aes(
    x = reorder(EVENT_TYPE, TOTAL_DAMAGE),
    y = DAMAGE_USD,
    fill = DAMAGE_TYPE
  )
) +
  geom_col() +
  coord_flip() +
  scale_fill_manual(
    values = c(
      "Property Damage" = "steelblue",
      "Crop Damage" = "darkgreen"
    )
  ) +
  scale_y_continuous(
    labels = damage_labels
  ) +
  labs(
    title = "Weather Events with the Greatest Economic Impact",
    subtitle = "Top 10 event types by combined economic damage, 2024",
    x = "Weather Event Type",
    y = "Known Reported Economic Damage (USD)",
    fill = "Damage Type",
    caption = "Source: NOAA Storm Events Database"
  ) +
  economic_theme

ggsave(
  "figures/figure15_combined_economic_damage.png",
  plot = figure15,
  width = 12,
  height = 7.2,
  dpi = 300
)

cat("Figure 15 saved successfully.\n")

# ============================================================
# FIGURE 16: GEOGRAPHIC ECONOMIC DAMAGE
# ============================================================

top_economic_states <- economic_by_state %>%
  slice_max(
    TOTAL_DAMAGE,
    n = 10,
    with_ties = FALSE
  )

figure16 <- create_damage_plot(
  top_economic_states,
  "STATE",
  "TOTAL_DAMAGE",
  "Top 10 Geographic Areas by Economic Damage",
  "firebrick"
)

ggsave(
  "figures/figure16_geographic_economic_damage.png",
  plot = figure16,
  width = 12,
  height = 7.2,
  dpi = 300
)

cat("Figure 16 saved successfully.\n")

# ============================================================
# FIGURE 17: MONTHLY ECONOMIC DAMAGE
# ============================================================

economic_month_plot <- economic_by_month %>%
  mutate(
    MONTH = factor(
      MONTH,
      levels = month.name
    )
  )

figure17 <- ggplot(
  economic_month_plot,
  aes(
    x = MONTH,
    y = TOTAL_DAMAGE
  )
) +
  geom_col(fill = "darkgreen") +
  geom_text(
    aes(
      label = scales::dollar(
        TOTAL_DAMAGE / 1e9,
        suffix = "B",
        accuracy = 0.01
      )
    ),
    vjust = -0.4,
    size = 3.8
  ) +
  scale_y_continuous(
    labels = damage_labels,
    expand = expansion(mult = c(0, 0.15))
  ) +
  labs(
    title = "Monthly Weather-Related Economic Damage",
    subtitle = "NOAA Storm Events Database, 2024",
    x = "Month",
    y = "Known Reported Economic Damage (USD)",
    caption = "Source: NOAA Storm Events Database"
  ) +
  economic_theme +
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    )
  )

ggsave(
  "figures/figure17_monthly_economic_damage.png",
  plot = figure17,
  width = 12,
  height = 7.2,
  dpi = 300
)

cat("Figure 17 saved successfully.\n")

# ============================================================
# FIGURE 18: SEASONAL ECONOMIC DAMAGE
# ============================================================

economic_season_plot <- economic_by_season %>%
  mutate(
    DAMAGE_PERCENTAGE =
      TOTAL_DAMAGE / sum(TOTAL_DAMAGE) * 100,
    DAMAGE_LABEL = paste0(
      scales::dollar(
        TOTAL_DAMAGE / 1e9,
        suffix = "B",
        accuracy = 0.01
      ),
      "\n(",
      round(DAMAGE_PERCENTAGE, 2),
      "%)"
    )
  )

figure18 <- ggplot(
  economic_season_plot,
  aes(
    x = SEASON,
    y = TOTAL_DAMAGE,
    fill = SEASON
  )
) +
  geom_col(width = 0.65) +
  geom_text(
    aes(label = DAMAGE_LABEL),
    vjust = -0.4,
    size = 4.5
  ) +
  scale_fill_manual(
    values = c(
      "Winter" = "steelblue",
      "Spring" = "forestgreen",
      "Summer" = "orange",
      "Fall" = "sienna"
    )
  ) +
  scale_y_continuous(
    labels = damage_labels,
    expand = expansion(mult = c(0, 0.20))
  ) +
  labs(
    title = "Weather-Related Economic Damage by Season",
    subtitle = "NOAA Storm Events Database, 2024",
    x = "Season",
    y = "Known Reported Economic Damage (USD)",
    caption = "Source: NOAA Storm Events Database"
  ) +
  economic_theme +
  theme(legend.position = "none")

ggsave(
  "figures/figure18_seasonal_economic_damage.png",
  plot = figure18,
  width = 12,
  height = 7.2,
  dpi = 300
)

cat("Figure 18 saved successfully.\n")

# ============================================================
# ECONOMIC VISUALIZATION VALIDATION
# ============================================================

expected_economic_figures <- c(
  "figures/figure13_property_damage.png",
  "figures/figure14_crop_damage.png",
  "figures/figure15_combined_economic_damage.png",
  "figures/figure16_geographic_economic_damage.png",
  "figures/figure17_monthly_economic_damage.png",
  "figures/figure18_seasonal_economic_damage.png"
)

stopifnot(
  all(file.exists(expected_economic_figures)),
  all(file.info(expected_economic_figures)$size > 0),
  nrow(top_property_damage) == 10,
  nrow(top_crop_damage) == 10,
  nrow(top_total_damage) == 10,
  nrow(top_economic_states) == 10,
  nrow(economic_month_plot) == 12,
  nrow(economic_season_plot) == 4,
  isTRUE(all.equal(
    sum(economic_season_plot$DAMAGE_PERCENTAGE),
    100
  ))
)

cat("\nAll six economic visualizations saved successfully.\n")

cat("\n========================================\n")
cat("ECONOMIC VISUALIZATION VALIDATION: PASS\n")
cat("========================================\n")
