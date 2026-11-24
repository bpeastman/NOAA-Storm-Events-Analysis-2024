# An Analysis of Weather Events and Their Impacts in the United States: 2024

## Author

Brian Punch-Eastman

## Project Overview

This research project analyzes weather events recorded in the 2024 NOAA Storm Events Database using R.

The study examines weather event frequency, population health impacts, geographic distribution, temporal patterns, and economic consequences.

## Research Questions

1. Which weather events are most harmful to population health?
2. How are weather events distributed geographically?
3. How do weather events and their impacts vary by month and season?
4. Which weather events have the greatest economic consequences?

## Data Source

NOAA National Centers for Environmental Information (NCEI):

https://www.ncei.noaa.gov/pub/data/swdi/stormevents/csvfiles/

The analysis integrates three datasets:

- Storm Events Details
- Storm Events Fatalities
- Storm Events Locations

## Key Findings

- Total weather event records: 69,802
- Unique weather event types: 50
- Geographic areas: 68
- Most frequent weather event: Thunderstorm Wind
- Leading cause of fatalities: Excessive Heat
- Leading cause of injuries: Tornado
- Most active geographic area: Texas
- Most active month: May
- Most active season: Summer
- Total reported casualties: 2,981
- Total known economic damage: $17,996,610,040
- Greatest economic impact: Hurricane (Typhoon)
- Economic damage occurring in fall: 80.17%

## Repository Structure

- Final_Project.R: Complete analytical R source code
- Final_Project.Rmd: Reproducible research report
- Final_Project.html: Rendered HTML report
- data/: NOAA source datasets
- figures/: Analytical visualizations
- output/: Processed datasets and analytical results
- README.md: Project documentation

## Software Requirements

- R 4.5.3
- Pandoc
- rmarkdown
- knitr
- dplyr
- tidyr
- ggplot2
- scales
- readr

## Reproducing the Analysis

Download or clone this repository.

Set the R working directory to the project root.

Install the required R packages and ensure Pandoc is available.

Run the following command in R:

```r
source("Final_Project.R")
```

To regenerate the complete research report:

```r
rmarkdown::render("Final_Project.Rmd")
```

The rendered report contains five selected figures and the complete analytical code appendix.

## Data Quality and Limitations

- The study covers one calendar year.
- Some records contain incomplete economic damage information.
- Economic totals represent known reported damage.
- Minor discrepancies exist between fatality records and event-level fatality counts.
- Geographic health impacts are not population-adjusted.
- Findings should not be interpreted as long-term climate trends.

## Reproducibility

The analysis was validated through dataset integration checks, unique event identifier verification, damage conversion testing, and successful HTML report generation in a fresh R session.

## Published Report

RPubs URL: Pending publication.
