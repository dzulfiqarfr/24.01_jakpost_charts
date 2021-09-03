Indonesia sees CO2 emission reduction in 2020, but still lags behind
peers in decoupling GDP growth, emissions
================

I prepared this analysis for a story looking at the progress (or lack
thereof) in Indonesia’s efforts to limit its fossil carbon dioxide (CO2)
emissions while developing the economy.

## Packages

``` r
library(tidyverse)
library(readxl)
library(httr)
library(jsonlite)
library(patchwork)
library(ggtext)
```

## CO2 emissions

A group of scientists published [two papers](/paper) estimating the
pandemic-induced reduction in CO2 emissions for 69 countries, including
Indonesia, that represent an overwhelming majority of the global
population and emissions.

They estimated daily emissions using a combination of energy, activity
and policy data. There is an absence of real-time CO2 emission data, so
they use proxy data that represent changes in activity such as road and
air traffic for six sectors from various sources like TOMTOM. And they
developed a confinement index (CI) to look at restrictions depending on
their stringency that would potentially affect CO2 emissions.

The estimate for daily change in emission for Indonesia is relatively
limited and has less nuance compared with the global observations. So we
will focus on the annual estimate and compare it with emission data from
1990 to 2019 from the
<a href="https://essd.copernicus.org/articles/12/3269/2020/" target="_blank">Global Carbon Project (GCP)</a>.

``` r
path_dir <- "data/"
path_emission_data <- "Le_Quere_update_with_GCB2020_time_series_1990_2020_v1.0.xlsx"

emission_raw <- read_excel(
  paste0(path_dir, path_emission_data),
  sheet = "data",
  na = ""
)

emission_tidy <- emission_raw %>% 
  rename(date = `MtCO2/yr`) %>% 
  mutate(date = as.integer(date)) %>% 
  dplyr::filter(!is.na(date))
#> Warning in mask$eval_all_mutate(quo): NAs introduced by coercion

emission_idn <- emission_tidy %>% 
  select(date, Indonesia) %>% 
  rename(emission = Indonesia) %>% 
  mutate(emission = as.double(emission))

tail(emission_idn)
#> # A tibble: 6 x 2
#>    date emission
#>   <int>    <dbl>
#> 1  2015     507.
#> 2  2016     568.
#> 3  2017     531.
#> 4  2018     577.
#> 5  2019     618.
#> 6  2020     559.
```

Indonesia’s CO2 emissions were estimated to fall to 558.9 million tons
in 2020 from 617.51 million tons a year earlier, according to the study.
To put this in perspective, we can calculate the percentage change in
emissions from year to year and plot it just to check whether we have
seen such a decline before.

Before creating the chart, we will create a function for a custom
ggplot2 theme.

``` r
color_primary <- "#757575"
color_primary_light <- "#E0E0E0"
color_annotation <- "#9E9E9E"

color_data_primary <- "black"
color_data_secondary <- "#E66439"
color_data_secondary_light <- "#F2AA61"

calculate_annotation_size <- function(base_size = 12, rel_size = 0.7) {
  text_annotation_size <- base_size * rel_size / ggplot2::.pt
  return(text_annotation_size)
}

theme_dfr <- function(..., 
                      base_size = 12, 
                      rel_size = 0.75,
                      color_primary = "#757575",
                      color_primary_light = "#E0E0E0") {
  
  theme(
    text = element_text(
      size = base_size, 
      color = color_primary
    ),
    axis.text = element_text(
      size = rel(rel_size), 
      color = color_primary
    ),
    axis.line = element_blank(),
    axis.ticks.x = element_line(color = color_primary_light),
    axis.ticks.y = element_blank(),
    panel.background = element_rect(fill = "#FFFFFF"),
    panel.grid.major.x  = element_blank(),
    panel.grid.major.y = element_line(color = color_primary_light),
    panel.grid.minor = element_blank(),
    plot.title = element_text(
      face = "bold", 
      size = rel(1),
      color = "#000000", 
      margin = margin(b = 5)
    ),
    plot.subtitle = element_markdown(
      size = rel(rel_size), 
      color = color_primary, 
      margin = margin(b = 5)
    ),
    plot.title.position = "plot",
    plot.caption = element_markdown(
      size = rel(0.7),
      color = "#9E9E9E",
      hjust = 0,
      margin = margin(t = 5)
    ),
    plot.caption.position = "plot",
    ...
  )
  
}
```

We can now create the chart.

``` r
emission_idn_trf <- emission_idn %>% 
  mutate(
    diff = emission - dplyr::lag(emission, 1),
    pct_chg = diff / dplyr::lag(emission, 1) * 100
  )

ggplot(emission_idn_trf, aes(date, pct_chg)) +
  geom_hline(yintercept = 0, color = "red") +
  geom_line() +
  labs(x = NULL, y = NULL) +
  theme_dfr()
#> Warning: Removed 1 row(s) containing missing values (geom_path).
```

<img src="README_files/figure-gfm/Calculate percentage change in emission-1.png" width="70%" style="display: block; margin: auto;" />

The chart shows that the emission reduction last year was the steepest
at least since 2013, when the economy faced a slowdown as the government
cut fuel subsidies and the country faced the taper tantrum. Overall, the
biggest decline since 1990 took place in 1998, when the Asian financial
crisis hit the country.

This suggests emissions tended to fall when the country faced economic
crises, including the one caused by the COVID-19 pandemic that we were
currently in.

We can plot the CO2 emissions to see how long did it take for emissions
to return to its precrisis level after the past crises.

``` r
ggplot(emission_idn_trf, aes(date, emission)) +
  geom_line() +
  labs(x = NULL, y = NULL) +
  theme_dfr()
```

<img src="README_files/figure-gfm/Plot emission-1.png" width="70%" style="display: block; margin: auto;" />

The chart shows that Indonesia’s emissions rebounded to its precrisis
level within three years after the Asian financial crisis. After the
slowdown in economic growth in 2013, the country’s emissions also
returned to its precrisis level within three years.

For COVID-19, scientists said it would be unlikely for emissions to
return to its prepandemic level in 2021 given the prolonged pandemic and
mobility restrictions.

Globally, crises driven by energy factors such as the oil crisis in the
1970s and 1980s did not result in a similar postcrisis rebound because
it led to some improvements in energy efficiency and the development of
alternative energy sources, according to the scientists.

## Economic growth and CO2 emissions

The emission data suggest that Indonesia has not managed to decouple its
economic growth and CO2 emissions. Meanwhile, some countries especially
developed economies have managed to grow their economy while reducing or
at least slowing down the emissions.

<a href="https://ourworldindata.org/co2-and-other-greenhouse-gas-emissions#can-we-make-progress-in-reducing-emissions" target="_blank">Our World in Data (OWID)</a>
has compiled the CO2 emission per capita and GDP per capita data from
the World Bank and GCP. They also publish the consumption-based CO2
emissions. We will take a look at this data to see how does Indonesia
compare with other countries in decoupling growth and emissions.

``` r
growth_co2_raw <- read_csv(paste0(path_dir, "co2-emissions-and-gdp.csv"))

growth_co2_cln <- growth_co2_raw %>% 
  rename(
    date = Year,
    entity = Entity,
    gdp_perCap = `GDP per capita, PPP (constant 2017 international $)`,
    emission_perCap = `Per capita CO2 emissions`,
    emission_adj_perCap = `Per capita consumption-based CO2 emissions`
  )

glimpse(growth_co2_cln)
#> Rows: 8,332
#> Columns: 6
#> $ entity              <chr> "Afghanistan", "Afghanistan", "Afghanistan", "Afgh~
#> $ Code                <chr> "AFG", "AFG", "AFG", "AFG", "AFG", "AFG", "AFG", "~
#> $ date                <dbl> 1990, 1991, 1992, 1993, 1994, 1995, 1996, 1997, 19~
#> $ gdp_perCap          <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 11~
#> $ emission_adj_perCap <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA~
#> $ emission_perCap     <dbl> 0.20973558, 0.18252245, 0.09522725, 0.08427989, 0.~
```

OWID provides the emission data up to 2019. We can add the 2020
estimates from the study for the available countries. But first we need
to calculate the CO2 emissions per capita for 2020.

To that end, we will import the population estimates for 2020 from the
<a href="https://population.un.org/wpp/" target="_blank">United Nations (UN)</a>.

``` r
path_population_data <- "AnnualTotPopMidYear-20210824014211.xlsx"

population_raw <- read_excel(
  paste0(path_dir, path_population_data),
  sheet = "Data",
  skip = 1,
  na = ""
)

population_tidy <- population_raw %>% 
  select(-Note) %>% 
  rename(entity = Location) %>% 
  pivot_longer(
    `1990`:`2020`,
    names_to = "population_year",
    names_transform = list(population_year = as.integer),
    values_to = "population"
  ) %>% 
  mutate(
    entity = str_replace(entity, "United States of America", "United States")
  )

emission_long <- emission_tidy %>% 
  rename(`United States` = USA) %>% 
  pivot_longer(
    2:ncol(.),
    names_to = "entity",
    names_transform = list(entity = as.character),
    values_to = "emission",
    values_transform = list(emission = as.double)
  )

emission_with_pop_year <- emission_long %>% 
  mutate(
    population_year = case_when(
      date < 1995 ~ 1990,
      date >= 1995 & date <= 1999 ~ 1995,
      date >= 2000 & date <= 2004 ~ 2000,
      date >= 2005 & date <= 2009 ~ 2005,
      date >= 2010 & date <= 2014 ~ 2010,
      date >= 2015 & date <= 2019 ~ 2015,
      date == 2020 ~ 2020
    )
  )

emission_population <- emission_with_pop_year %>% 
  left_join(population_tidy, by = c("entity", "population_year"))

emission_2020_perCap <- emission_population %>% 
  group_by(entity) %>% 
  mutate(
    population = population / 1000,
    emission_perCap = emission / population
  ) %>% 
  ungroup() %>% 
  select(entity, date, emission_perCap) %>% 
  dplyr::filter(date == 2020)

growth_co2_add <- growth_co2_cln %>% 
  left_join(emission_2020_perCap, by = c("entity", "date")) %>%
  rename(emission_perCap = emission_perCap.x) %>% 
  mutate(
    emission_perCap = case_when(
      date == 2020 ~ emission_perCap.y, 
      TRUE ~ emission_perCap
    )
  ) %>% 
  select(-emission_perCap.y)

glimpse(growth_co2_add)
#> Rows: 8,332
#> Columns: 6
#> $ entity              <chr> "Afghanistan", "Afghanistan", "Afghanistan", "Afgh~
#> $ Code                <chr> "AFG", "AFG", "AFG", "AFG", "AFG", "AFG", "AFG", "~
#> $ date                <dbl> 1990, 1991, 1992, 1993, 1994, 1995, 1996, 1997, 19~
#> $ gdp_perCap          <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 11~
#> $ emission_adj_perCap <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA~
#> $ emission_perCap     <dbl> 0.20973558, 0.18252245, 0.09522725, 0.08427989, 0.~
```

The CO2 emissions per capita for the 1990-2019 period we have calculated
do not precisely match the data from OWID. But they are pretty close.

Since the CO2 emission and GDP data have different units, we will index
them so we can compare them using the same chart. We will use 1990 as
the base year as it is the earliest date in our data.

``` r
growth_co2_index <- growth_co2_add %>% 
  group_by(entity) %>% 
  mutate(
    gdp_index = gdp_perCap / first(gdp_perCap) * 100,
    emission_index = emission_perCap / first(emission_perCap) * 100,
    emission_adj_index = emission_adj_perCap / first(emission_adj_perCap) * 100
  ) %>% 
  select(entity, Code, date, ends_with("_index")) %>% 
  pivot_longer(
    ends_with("_index"),
    names_to = "metrics",
    values_to = "index"
  ) %>% 
  ungroup()

glimpse(growth_co2_index)
#> Rows: 24,996
#> Columns: 5
#> $ entity  <chr> "Afghanistan", "Afghanistan", "Afghanistan", "Afghanistan", "A~
#> $ Code    <chr> "AFG", "AFG", "AFG", "AFG", "AFG", "AFG", "AFG", "AFG", "AFG",~
#> $ date    <dbl> 1990, 1990, 1990, 1991, 1991, 1991, 1992, 1992, 1992, 1993, 19~
#> $ metrics <chr> "gdp_index", "emission_index", "emission_adj_index", "gdp_inde~
#> $ index   <dbl> NA, 100.00000, NA, NA, 87.02503, NA, NA, 45.40348, NA, NA, 40.~
```

We will compare Indonesia with India, Turkey, Singapore, China and the
United States. These countries have fared better than Indonesia in
decoupling GDP growth and CO2 emissions. India and Turkey are relatively
more comparable for Indonesia.

We will create small multiples. For China, we will separate its y-axis
because it is an outlier in terms of GDP per capita growth. This will
prevent it from dwarfing the variation in data for other countries.

``` r
growth_co2_index_sub <- growth_co2_index %>% 
  dplyr::filter(Code %in% c("IDN", "SGP", "IND", "CHN", "USA", "TUR"))

growth_co2_index_list <- growth_co2_index_sub %>% split(.$entity)

plot_gdp_co2 <- function(df, name, x, y, color) {
  
  country <- first(df[[name]])
  
  y_axis_limits <- if (country == "China") {
    c(100, 1300)
  } else {
    c(0, 400)
  }
  
  y_axis_increment <- if (country == "China") {
    300
  } else {
    100
  }
  
  annotation_crises <- tribble(
    ~xmin, ~xmax,
    1997, 1998, 
    2007, 2008,
    2020, 2021
  )
  
  plot <- ggplot(df) +
    geom_hline(
      yintercept = 100, 
      color = "black",
      lwd = 0.25
    ) +
    geom_rect(
      data = annotation_crises,
      aes(
        xmin = xmin,
        xmax = xmax,
        ymin = if_else(country == "China", 100, 0),
        ymax = if_else(country == "China", 1300, 400)
      ),
      fill = color_annotation,
      alpha = 0.25
    ) +
    geom_vline(
      xintercept = 2015, 
      color = color_annotation, 
      lwd = 0.35,
      lty = "dashed"
    ) +
    # Create a rectangle annotation layer to provide a white box for legend
    # background in the first panel. The `legend.background` parameter has 
    # quite a wide margin, which interferes with the chart too much
    annotate(
      geom = "rect",
      xmin = if_else(country == "Indonesia", 1995, 1990),
      xmax = if_else(country == "Indonesia", 2019, 1990),
      ymin = if_else(country == "Indonesia", 250, 100),
      ymax = if_else(country == "Indonesia", 395, 100),
      fill = "white"
    ) +
    geom_line(
      aes(.data[[x]], .data[[y]], color = .data[[color]]),
      lwd = 0.75, 
      show.legend = if_else(country == "Indonesia", TRUE, FALSE)
    ) +
    geom_richtext(
      data = tibble(
        x = 1998, 
        y = 400, 
        label = if_else(country == "Turkey", "Shades<br>mark<br>crises", "")
      ),
      aes(x, y, label = label),
      size = calculate_annotation_size(rel_size = 0.65),
      hjust = 1,
      nudge_x = -1.5,
      nudge_y = -60,
      label.padding = unit(0, "lines"),
      fill = "white",
      label.color = NA,
      color = color_annotation
    ) +
    geom_richtext(
      data = tibble(
        x = 2015, 
        y = 400, 
        label = if_else(country == "Turkey", "Paris<br>Agreement", "")
      ),
      aes(x, y, label = label),
      size = calculate_annotation_size(),
      hjust = 1,
      nudge_x = -0.5,
      nudge_y = -50,
      label.padding = unit(0, "lines"),
      fill = "white",
      label.color = NA,
      color = color_annotation
    ) +
    scale_x_continuous(labels = c("1990", "2000", "'10", "'20")) +
    scale_y_continuous(
      breaks = seq(y_axis_limits[[1]], y_axis_limits[[2]], y_axis_increment),
      limits = y_axis_limits,
      position = "right"
    ) +
    scale_color_manual(
      values = c(
        "gdp_index" = color_data_primary,
        "emission_index" = color_data_secondary,
        "emission_adj_index" = color_data_secondary_light
      ),
      labels = c(
        "gdp_index" = "GDP",
        "emission_index" = "Production-based\nemissions",
        "emission_adj_index" = "Consumption-based\nemissions"
      )
    ) +
    labs(subtitle = country, x = NULL, y = NULL) +
    theme_dfr(
      legend.text = element_text(size = rel(0.7)),
      legend.title = element_blank(),
      legend.position = c(0.5, 0.81),
      legend.key.size = unit(0.25, "cm"),
      legend.margin = margin(0),
      legend.key = element_blank(),
      legend.background = element_blank()
    ) +
    theme(plot.subtitle = element_markdown(face = "bold"))
  
  return(plot)
  
}

plot_list <- growth_co2_index_list %>% 
  map(
    ~plot_gdp_co2(
      df = .,
      name = "entity",
      x = "date",
      y = "index",
      color = "metrics"
    )
  )

plot_list$Indonesia + 
  plot_list$Turkey + 
  plot_list$India +
  plot_list$Singapore +
  plot_list$`United States` +
  plot_list$China +
  plot_annotation(
    title = "Indonesia lags behind in decoupling economic growth and emissions",
    subtitle = paste0(
      "Real GDP per capita, and ",
      "production and consumption-based CO2 emissions per capita*<br>",
      "(1990 = 100)"
    ),
    caption = paste0(
      "\\*Production-based CO2 emissions in 2020 are estimates<br>",
      paste0(
        "Sources: Our World in Data; Le Quéré et al. 2021; ",
        "Global Carbon Project; UN; *The Jakarta Post* analysis<br>"
      ),
      "Chart: JP/Dzulfiqar Fathur Rahman"
    ),
    theme = theme_dfr(plot.margin = margin(0))
  )
#> Warning: Removed 2 row(s) containing missing values (geom_path).

#> Warning: Removed 2 row(s) containing missing values (geom_path).

#> Warning: Removed 2 row(s) containing missing values (geom_path).
#> Warning: Removed 3 row(s) containing missing values (geom_path).
#> Warning: Removed 2 row(s) containing missing values (geom_path).

#> Warning: Removed 2 row(s) containing missing values (geom_path).
```

<img src="README_files/figure-gfm/Plot comparison for Indonesia with peers-1.png" width="70%" style="display: block; margin: auto;" />

We can see in the chart that Indonesia has not been able to grow its
economy without slowing down the growth in CO2 emissions. India, Turkey
and China have somewhat slowed their emission growth while developing
their economy. Singapore and the US have managed to grow their economy
while reducing their CO2 emissions from the 1990 level, respectively.

## Planetary pressures-adjusted HDI

The UN Development Programme (UNDP) launched last year an experimental
indicator called
<a href="http://hdr.undp.org/en/content/planetary-pressures%E2%80%93adjusted-human-development-index-phdi" target="_blank">planetary pressures-adjusted human development index (PHDI)</a>.
This indicator reflects how a country’s carbon footprint and resource
use for consumption affect both its material and nonmaterial well-being.

The UNDP, however, only publishes the PHDI for 2019. So we have to
construct the PHDI for the 1990-2018 period. We can do so using similar
if not the same underlying data. They constructed the adjustment factor
for the PHDI using the territorial CO2 emission data from the GCP and
<a href="https://environmentlive.unep.org/downloader" target="_blank">material footprint data</a>
from the UN Environment Programme (UNEP).

First, we will import the HDI data from 1990 to 2019 from the UNDP via
its
<a href="http://hdr.undp.org/en/content/human-development-report-office-statistical-data-api" target="_blank">application programming interface (API)</a>.

``` r
hdi_url <- "http://ec2-54-174-131-205.compute-1.amazonaws.com/API/HDRO_API.php/"
hdi_api_queries <- "country_code=IDN/indicator_id=137506"

hdi_response <- GET(paste0(hdi_url, hdi_api_queries))

hdi_parsed <- hdi_response %>% 
  content(type = "text") %>% 
  fromJSON()

hdi_raw <- as_tibble(hdi_parsed$indicator_value$IDN$`137506`)

hdi_tidy <- hdi_raw %>% 
  pivot_longer(
    everything(), 
    names_to = "date",
    names_transform = list(date = as.integer),
    values_to = "hdi"
  )

glimpse(hdi_tidy)
#> Rows: 30
#> Columns: 2
#> $ date <int> 1990, 1991, 1992, 1993, 1994, 1995, 1996, 1997, 1998, 1999, 2000,~
#> $ hdi  <dbl> 0.523, 0.529, 0.534, 0.541, 0.550, 0.560, 0.573, 0.587, 0.586, 0.~
```

We can now import the material footprint data. The material footprint
represents the amount of raw material extraction such as biomass, metal
ores, nonmetal ores and fossil energy for consumption.

``` r
footprint_raw <- read_csv(
  paste0(path_dir, "unep_material-footprint.csv"),
  skip = 3
)

footprint_tidy <- footprint_raw %>% 
  rename(
    country = `COUNTRY NAME`,
    date = YEAR, 
    material_footprint_perCap = VALUE
  ) %>% 
  select(country,date, material_footprint_perCap)

glimpse(footprint_tidy)
#> Rows: 4,816
#> Columns: 3
#> $ country                   <chr> "Afghanistan", "Afghanistan", "Afghanistan",~
#> $ date                      <dbl> 1990, 1991, 1992, 1993, 1994, 1995, 1996, 19~
#> $ material_footprint_perCap <dbl> 2.42, 2.76, 2.03, 1.84, 1.57, 1.56, 1.42, 1.~
```

Having imported the material footprint data, we can now create the
adjustment factor for planetary pressures. This is just an arithmetic
mean of CO2 emission per capita and material footprint per capita
indices. The PHDI is a product of the HDI and the planetary pressure
adjustment factor.

To construct the emission and material footprint indices, the UNDP uses
a similar approach to calculate the education dimension index of the HDI
(see the
<a href="http://hdr.undp.org/sites/default/files/phdi_tn.pdf" target="_blank">technical note</a>).
But since the material footprint data are somewhat different from the
one the UNDP uses, we will have a different maximum value.

For Indonesia, the UNDP used the 2018 CO2 emission per capita data and
2017 material footprint per capita data. However, we will use more
recent emission data since we have them.

We still have to recycle the 2017 material footprint per capita figure
for the successive years due to absence of more recent data. This will
likely underestimate the result, in part because Indonesia’s material
footprint per capita has posted an upward trend since mid-2010s.

``` r
emission_idn <- growth_co2_cln %>% 
  dplyr::filter(entity == "Indonesia") %>% 
  select(date, emission_perCap)

emission_max <- growth_co2_cln %>% 
  dplyr::filter(emission_perCap == max(emission_perCap, na.rm = TRUE)) %>% 
  select(emission_perCap) %>% 
  deframe()

footprint_idn <- footprint_tidy %>% 
  dplyr::filter(country == "Indonesia") %>% 
  select(date, material_footprint_perCap)

footprint_max <- footprint_tidy %>% 
  dplyr::filter(
    material_footprint_perCap == max(material_footprint_perCap, na.rm = TRUE)
  ) %>% 
  select(material_footprint_perCap) %>% 
  deframe()

emission_footprint <- emission_idn %>% 
  left_join(footprint_idn)

calculate_index <- function(x, max) {
  index <- (max - x) / max
  return(index)
}

recycle_last_value <- function(x) {
  recycle <- case_when(
    is.na(x) ~ dplyr::lag(x, 1),
    TRUE ~ x
  )
  return(recycle)
}

planetary_pressure <- emission_footprint %>%
  mutate(
    material_footprint_perCap = recycle_last_value(material_footprint_perCap),
    material_footprint_perCap = recycle_last_value(material_footprint_perCap),
    emission_index = calculate_index(emission_perCap, max = emission_max),
    material_footprint_index = calculate_index(
      material_footprint_perCap,
      max = footprint_max
    ),
    adjustment_factor = (emission_index + material_footprint_index) / 2
  )

p_indices_hdi <- hdi_tidy %>% left_join(planetary_pressure, by = "date")

phdi <- p_indices_hdi %>% 
  mutate(
    phdi = hdi * adjustment_factor,
    diff = (hdi - phdi) / hdi * 100
  ) %>% 
  select(date, hdi, phdi, diff) 

tail(phdi)
#> # A tibble: 6 x 4
#>    date   hdi  phdi  diff
#>   <dbl> <dbl> <dbl> <dbl>
#> 1  2014 0.69  0.670  2.86
#> 2  2015 0.695 0.673  3.12
#> 3  2016 0.703 0.680  3.31
#> 4  2017 0.707 0.684  3.22
#> 5  2018 0.712 0.688  3.33
#> 6  2019 0.718 0.693  3.42
```

Our PHDI for 2019 does not precisely match the official figure. But they
are pretty close and we can still get similar insights. The 2019 HDI and
PHDI difference, for example, is slightly lower by 0.4 points than the
official figure.

One important takeaway from the PHDI is that it pushses down Indonesia
down to the medium human development group. Planetary pressures
discounted the country’s progress in healthcare, education and income
development. Moreover, the difference between the HDI and the PHDI
tended to get bigger in recent years.

We will plot the HDI and the PHDI in a line chart to show the trend and
the widening gap between the two metrics.

``` r
ggplot(phdi, aes(date)) +
  annotate(
    "rect",
    xmin = 1997,
    xmax = 1998,
    ymin = 0.45,
    ymax = 0.75,
    fill = color_annotation,
    alpha = 0.25
  ) +
  geom_hline(
    yintercept = c(0.55, 0.7),
    color = color_annotation,
    lty = "dashed"
  ) +
  geom_line(
    aes(y = hdi),
    color = color_data_primary, 
    lwd = 1
  ) +
  geom_line(
    aes(y = phdi),
    color = color_data_secondary,
    lwd = 1
  ) +
  geom_ribbon(
    aes(ymin = phdi, ymax = hdi),
    fill = color_data_secondary_light,
    alpha = 0.25
  ) +
  geom_richtext(
    data = tibble(
      x = c(2000, 2000),
      y = c(0.57, 0.625),
      label = c("Planetary pressures-adjusted<br>HDI", "HDI")
    ),
    aes(x, y, label = label),
    size = calculate_annotation_size(rel_size = 0.8),
    hjust = 0,
    label.padding = unit(0, "lines"),
    fill = NA,
    label.color = NA,
    color = c(color_data_secondary, color_data_primary),
    fontface = "bold"
  ) +
  geom_richtext(
    data = tibble(x = 1998, y = 0.75, label = "Asian financial crisis"),
    aes(x, y, label = label),
    size = calculate_annotation_size(rel_size = 0.8),
    hjust = 0,
    nudge_x = 0.1,
    nudge_y = -0.01,
    label.padding = unit(0, "lines"),
    fill = NA,
    label.color = NA,
    color = color_annotation
  ) +
  geom_richtext(
    data = tibble(x = 1990, y = 0.7, label = "High human development &uarr;"),
    aes(x, y, label = label),
    size = calculate_annotation_size(rel_size = 0.8),
    hjust = 0,
    nudge_x = -0.5,
    nudge_y = 0.01,
    label.padding = unit(0, "lines"),
    fill = NA,
    label.color = NA,
    color = color_annotation
  ) +
  geom_richtext(
    data = tibble(
      x = 1990, 
      y = 0.55, 
      label = "Medium human &uarr;<br>development"
    ),
    aes(x, y, label = label),
    size = calculate_annotation_size(rel_size = 0.8),
    hjust = 0,
    nudge_x = -0.5,
    nudge_y = 0.0175,
    label.padding = unit(0, "lines"),
    fill = NA,
    label.color = NA,
    color = color_annotation
  ) +
  scale_x_continuous(
    breaks = seq(1991, 2019, 4),
    labels = c("1991", "'95", "'99", "2003", "'07", paste0("'", seq(11, 19, 4)))
  ) +
  scale_y_continuous(
    breaks = seq(0.45, 0.75, 0.1),
    limits = c(0.45, 0.75),
    position = "right"
  ) +
  labs(
    title = "CO2 emissions, resource use lower Indonesia's human development",
    subtitle = paste0(
      "Human development index (HDI) and ",
      "planetary pressures-adjusted HDI"
    ),
    x = NULL, 
    y = NULL,
    caption = paste0(
      "Sources: UNDP; UNEP; Global Carbon Project; Our World in Data; ",
      "*The Jakarta Post* analysis<br>",
      "Chart: JP/Dzulfiqar Fathur Rahman"
    )
  ) +
  theme_dfr(
    rel_size = 0.8, 
    plot.margin = margin(0)
  ) 
#> Warning in text_info(label, fontkey, fontfamily, fontface, fontsize, cache):
#> unable to translate '<U+2191>png19.6' to native encoding
#> Warning in text_info_cache[[key]] <- info: unable to translate '<U+2191>png19.6'
#> to native encoding
#> Warning in text_info(label, fontkey, fontfamily, fontface, fontsize, cache):
#> unable to translate '<U+2191>png19.6' to native encoding
```

<img src="README_files/figure-gfm/Plot PHDI-1.png" width="70%" style="display: block; margin: auto;" />
