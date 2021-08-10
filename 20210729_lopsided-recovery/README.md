Unequal vaccine distribution leads to lopsided global recovery
================
2021-08-10

This is a documentation for the analysis behind a
<a href="https://www.thejakartapost.com/news/2021/08/01/slow-vaccination-takes-toll-on-indonesia-in-uneven-global-recovery.html" target="_blank">story</a>
on the July 2021 World Economic Outlook (WEO) from the International
Monetary Fund (IMF).

## Packages

``` r
library(tidyverse)
library(data.table)
library(readxl)
library(lubridate)
library(glue)
library(WDI)
library(ggbeeswarm)
library(ggtext)
library(ggrepel)
```

## World Economic Outlook

We can get the WEO data from the IMF’s
<a href="https://www.imf.org/en/Publications/WEO/Issues/2021/07/27/world-economic-outlook-update-july-2021#Data%20Tools" target="_blank">website</a>.
It is available in an Excel format. The file presents various
indicators, but we will focus on the outlook for real gross domestic
product (GDP) growth.

``` r
weo_raw <- read_excel(
  "data/IMF_202107_WEO_raw.xlsx",
  sheet = "Real GDP Growth",
  na = "",
  skip = 4
)

weo_cleaned <- weo_raw %>% 
  select_if(function(x) {!all(is.na(x))}) %>% 
  rename(
    country = 1,
    actual_2019 = 2,
    actual_2020 = 3,
    forecast_2021 = 4,
    forecast_2022 = 5,
    revision_2021 = 6,
    revision_2022 = 7
  ) %>% 
  dplyr::filter(!is.na(country), !is.na(actual_2019)) %>% 
  mutate(
    country = str_remove_all(country, "2/"),
    country = str_replace(country, "Korea", "South Korea")
  )

weo_actual_outlook <- weo_cleaned %>% 
  pivot_longer(
    2:5,
    names_to = c("outlook", "year"),
    names_sep = "_",
    values_to = "growth",
    values_transform = list(growth = as.double)
  ) %>% 
  select(!starts_with("revision"))

weo_revision <- weo_cleaned %>% 
  select(country, starts_with("revision")) %>% 
  pivot_longer(
    2:3,
    names_to = c(NA, "year"),
    names_sep = "_",
    values_to = "revision"
  )

weo_tidy <- weo_actual_outlook %>% 
  left_join(weo_revision, by = c("country", "year"))

glimpse(weo_tidy)
#> Rows: 120
#> Columns: 5
#> $ country  <chr> "Argentina", "Argentina", "Argentina", "Argentina", "Australi~
#> $ outlook  <chr> "actual", "actual", "forecast", "forecast", "actual", "actual~
#> $ year     <chr> "2019", "2020", "2021", "2022", "2019", "2020", "2021", "2022~
#> $ growth   <dbl> -2.1, -9.9, 6.4, 2.4, 1.9, -2.4, 5.3, 3.0, 1.4, -4.1, 5.3, 1.~
#> $ revision <dbl> NA, NA, 0.6, -0.1, NA, NA, 0.8, 0.2, NA, NA, 1.6, -0.7, NA, N~
```

There is an outlook for this year and 2022 in the data. But it contains
only 120 observations because the IMF provides an outlook for selected
countries.

The report came at a time when Indonesia was grappling with a resurgence
in cases and deaths driven by the highly transmissible delta variant. So
it will be interesting to see how the archipelagic country compares with
its peers. We can check the bottom 10 countries in terms of outlook
revision.

``` r
weo_tidy %>% 
  dplyr::filter(year == 2021) %>% 
  arrange(revision) %>% 
  head(10)
#> # A tibble: 10 x 5
#>    country        outlook  year  growth revision
#>    <chr>          <chr>    <chr>  <dbl>    <dbl>
#>  1 "India "       forecast 2021     9.5   -3    
#>  2 "Malaysia"     forecast 2021     4.7   -1.8  
#>  3 "Philippines"  forecast 2021     5.4   -1.5  
#>  4 "Japan"        forecast 2021     2.8   -0.5  
#>  5 "Saudi Arabia" forecast 2021     2.4   -0.5  
#>  6 "Thailand"     forecast 2021     2.1   -0.5  
#>  7 "Indonesia"    forecast 2021     3.9   -0.4  
#>  8 "China"        forecast 2021     8.1   -0.300
#>  9 "Netherlands"  forecast 2021     3.3   -0.200
#> 10 "Spain"        forecast 2021     6.2   -0.200
```

The Fund made the steepest downward revision at 3 percentage points for
India between April and July. It was followed by Malaysia and
Philippines. For Indonesia, the outlook was revised down by 0.4
percentage points.

The variation in outlook revision suggests the symptoms of a divergent
path in global economic recovery among countries, depending on not only
whether they are battling rising cases and deaths but also how fast they
are at inoculating their citizens. Hence, we will create a chart to show
it.

As usual, we will first create a custom ggplot2 theme.

``` r
theme_dfr <- function(..., base_size = 12, rel_size = 0.75) {
  
  theme(
    text = element_text(size = base_size, color = "#616161"),
    axis.title = element_blank(),
    axis.text = element_text(size = rel(rel_size)),
    axis.line.x = element_line(color = "black"),
    axis.ticks = element_blank(),
    panel.background = element_rect(fill = "white"),
    panel.grid.major.x  = element_blank(),
    panel.grid.major.y = element_line(color = "#E0E0E0"),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    plot.title = element_text(
      face = "bold", 
      color = "black", 
      size = rel(1)
    ),
    plot.subtitle = element_text(
      size = rel(rel_size),
      margin = margin(b = 12.5)
    ),
    plot.title.position = "plot",
    plot.caption = element_text(
      size = rel(rel_size),
      hjust = 0,
      margin = margin(t = 12.5)
    ),
    plot.caption.position = "plot",
    ...
  )
  
}
```

To show the variation in outlook revision, we will use a lollipop chart.
We will use different colors for countries that got an upward revision
and those that got a downward revision to emphasize the divergence. And
since there are dozens of countries, we will set the aspect ratio of our
chart to `1` so it doesn’t take up spaces.

``` r
weo_2021_revision <- weo_tidy %>%
  dplyr::filter(year == "2021") %>% 
  mutate(country = fct_reorder(country, revision))


ggplot(weo_2021_revision, aes(x = revision, y = country)) +
  geom_vline(xintercept = 0, color = "#E68F7E") +
  geom_segment(
    aes(xend = 0, yend = country, color = revision >= 0),
    lwd = 1.5,
    alpha = 0.25,
    show.legend = F
  ) +
  geom_point(
    aes(fill = revision >= 0),
    pch = 21,
    color = "white",
    size = 2.5
  ) +
  scale_x_continuous(
    breaks = seq(-3, 3),
    limits = c(-3, 3),
    position = "top"
  ) +
  scale_fill_manual(
    values = c("TRUE" = "#2477B3", "FALSE" = "#E66439"),
    labels = c("TRUE" = "Upward revision", "FALSE" = "Downward revision")
  ) +
  scale_color_manual(values = c("TRUE" = "#2477B3", "FALSE" = "#E66439")) +
  guides(fill = guide_legend(nrow = 1)) +
  labs(
    title = "Indonesia lags behind in lopsided global recovery",
    subtitle = str_c(
      "Revision in 2021 real GDP growth outlook between April and July ", 
      "(percentage point)"
    ),
    x = NULL,
    y = NULL,
    caption = "Source: International Monetary Fund (IMF)\nChart: @dzulfiqarfr"
  ) + 
  theme_dfr() +
  theme(
    axis.text.y = element_text(hjust = 0, color = "#616161"),
    axis.line.x = element_blank(),
    panel.grid.major.x = element_line(color = "#E0E0E0"),
    legend.title = element_blank(),
    legend.text = element_text(size = rel(0.75), color = "#616161"),
    legend.position = c(0.06, 1.085),
    legend.key = element_blank(),
    legend.key.height = unit(0.25, "cm"),
    legend.key.width = unit(0.1, "cm"),
    legend.background = element_blank(),
    plot.subtitle = element_text(margin = margin(b = 25), color = "#616161")
  )
```

<img src="README_files/figure-gfm/Plot WEO revision-1.png" width="70%" style="display: block; margin: auto;" />

## Vaccination campaign

The IMF stated that the latest WEO showed a split among countries based
on their vaccination rate. The higher the vacicnation rate the better
outlook they have.

We can check how far the real GDP growth outlook varies with vaccination
rate using vaccination data from
<a href="https://github.com/owid/covid-19-data/tree/master/public/data" target="_blank">Our World in Data (OWID)</a>.
We will take a look at the share of population partially vaccinated
against COVID-19 at the latest date available. This metric shows the
bare minimum in preventing people from contracting the virus.

``` r
url_owid <- "https://covid.ourworldindata.org/data/owid-covid-data.csv"

vax_raw <- fread(
  url_owid, 
  select = c("iso_code", "location", "date", "people_vaccinated_per_hundred")
)

# Remove observations by continent, which are denoted by "OWID" prefix in
# the `iso_code`
vax_latest <- vax_raw %>% 
  dplyr::filter(!str_detect(iso_code, "^OWID")) %>% 
  group_by(iso_code) %>% 
  dplyr::filter(!is.na(people_vaccinated_per_hundred)) %>%
  dplyr::filter(date == last(date)) %>% 
  ungroup()

glimpse(vax_latest)
#> Rows: 213
#> Columns: 4
#> $ iso_code                      <chr> "AFG", "ALB", "DZA", "AND", "AGO", "AIA"~
#> $ location                      <chr> "Afghanistan", "Albania", "Algeria", "An~
#> $ date                          <date> 2021-08-01, 2021-08-05, 2021-07-29, 202~
#> $ people_vaccinated_per_hundred <dbl> 1.96, 24.08, 7.80, 62.70, 2.95, 62.84, 3~
```

Having imported the vaccination data, we can now join it with the WEO
data.

``` r
# Call `parse_character()` to make sure values in `country` in WEO data 
# matches those in `location` in OWID data
weo_2021 <- weo_tidy %>% 
  dplyr::filter(year == 2021) %>% 
  mutate(country = parse_character(country))

weo_vax <- weo_2021 %>% 
  left_join(vax_latest, by = c("country" = "location"))

glimpse(weo_vax)
#> Rows: 30
#> Columns: 8
#> $ country                       <chr> "Argentina", "Australia", "Brazil", "Can~
#> $ outlook                       <chr> "forecast", "forecast", "forecast", "for~
#> $ year                          <chr> "2021", "2021", "2021", "2021", "2021", ~
#> $ growth                        <dbl> 6.4, 5.3, 5.3, 6.3, 8.1, 2.8, 5.8, 3.6, ~
#> $ revision                      <dbl> 0.6, 0.8, 1.6, 1.3, -0.3, 0.3, 0.0, 0.0,~
#> $ iso_code                      <chr> "ARG", "AUS", "BRA", "CAN", "CHN", "EGY"~
#> $ date                          <date> 2021-08-08, 2021-08-08, 2021-08-08, 202~
#> $ people_vaccinated_per_hundred <dbl> 57.70, 35.54, 52.50, 72.05, 43.21, 3.75,~
```

We can now create a scatter plot to see how real GDP growth outlook
varies with vaccination rate.

``` r
today <- format(Sys.Date(), "%b %d, %Y")

ggplot(weo_vax, aes(people_vaccinated_per_hundred, growth)) +
  geom_point(
    pch = 21,
    fill = "black",
    color = "white",
    size = 2.5,
    alpha = 0.75
  ) +
  geom_smooth(
    method = "lm",
    se = F,
    lwd = 1,
    lty = "dashed"
  ) +
  geom_text(
    aes(label = country),
    check_overlap = TRUE,
    size = 2.5,
    hjust = 0,
    nudge_x = 1,
    nudge_y = 0.25
  ) +
  scale_x_continuous(
    breaks = seq(0, 80, 20),
    limits = c(0, 80)
  ) +
  scale_y_continuous(
    breaks = seq(0, 10, 2),
    limits = c(0, 10),
    expand = c(0, 0),
    position = "right"
  ) +
  labs(
    title = "Better outlook for countries with higher vaccination rate",
    subtitle = str_c(
      "Real GDP growth outlook for 2021 & ",
      "share of population partially vaccinated against COVID-19*\n",
      "(percent)"
    ),
    x = "Share of population partially vaccinated*",
    y = "Real GDP growth outlook",
    caption = str_c(
      glue("*At latest date available as of {today}"),
      "\nSource: International Monetary Fund (IMF); Our World in Data",
      "\nChart: @dzulfiqarfr"
    )
  ) +
  theme_dfr() +
  theme(
    axis.title = element_text(size = rel(0.75)),
    panel.grid.major.x = element_line(color = "#E0E0E0")
  )
```

<img src="README_files/figure-gfm/Plot GDP outlook against vaccination rate-1.png" width="70%" style="display: block; margin: auto;" />

The scatter plot suggests a positive correlation between vaccination
rate and real GDP growth outlook. It also shows that countries with
higher vaccination rate tend to be richer countries, such as United
Kingdom, Canada, France, Germany and the United States.

We can see such a trend more clearly by looking at vaccination rate
across countries based on their income group. To group countries by
their income level, we can use the income classification data from the
World Bank using the **WDI** package.

``` r
wdi_country_raw <- as_tibble(WDI::WDI_data$country)

wdi_income_group <- wdi_country_raw %>% 
  select(iso3c, income)

glimpse(wdi_income_group)
#> Rows: 304
#> Columns: 2
#> $ iso3c  <chr> "ABW", "AFG", "AFR", "AGO", "ALB", "AND", "ANR", "ARB", "ARE", ~
#> $ income <chr> "High income", "Low income", "Aggregates", "Lower middle income~
```

We can now join the income classification data to the COVID-19
vaccination data. But not all countries in the vaccination data are
available in the income classification data. We will drop countries with
missing income classification.

``` r
vax_income_group <- vax_latest %>% 
  left_join(wdi_income_group, by = c("iso_code" = "iso3c")) %>%
  dplyr::filter(!is.na(income))

glimpse(vax_income_group)
#> Rows: 202
#> Columns: 5
#> $ iso_code                      <chr> "AFG", "ALB", "DZA", "AND", "AGO", "ATG"~
#> $ location                      <chr> "Afghanistan", "Albania", "Algeria", "An~
#> $ date                          <date> 2021-08-01, 2021-08-05, 2021-07-29, 202~
#> $ people_vaccinated_per_hundred <dbl> 1.96, 24.08, 7.80, 62.70, 2.95, 39.28, 5~
#> $ income                        <chr> "Low income", "Upper middle income", "Up~
```

We can now check the median vaccination rate by income group.

``` r
vax_income_group_median <- vax_income_group %>% 
  group_by(income) %>% 
  summarize(median_vax_rate = median(people_vaccinated_per_hundred)) %>% 
  ungroup() %>% 
  arrange(desc(median_vax_rate))

vax_income_group_median
#> # A tibble: 4 x 2
#>   income              median_vax_rate
#>   <chr>                         <dbl>
#> 1 High income                   61.6 
#> 2 Upper middle income           26.4 
#> 3 Lower middle income            8.69
#> 4 Low income                     1.22
```

The median share of people who have received their first shot was 61.65
percent for high income countries and 1.22 percent for low income
countries. This shows a very wide gap.

We can create one more chart to show the gap in vaccination rate by
income group. We will plot vaccination rate by country with income group
mapped to the x-axis. We will also show the median vaccination rate for
each income group using a cross bar.

``` r
annotation_country <- vax_income_group %>% 
  dplyr::filter(
    location %in% c(
      "Nepal",
      "Mongolia",
      "Nauru",
      "Gibraltar"
    )
  )

annotation_indonesia <- vax_income_group %>% 
  dplyr::filter(location == "Indonesia")

vax_income_group_sorted <- vax_income_group %>% 
  mutate(income = fct_reorder(income, people_vaccinated_per_hundred))

ggplot(vax_income_group_sorted, aes(income, people_vaccinated_per_hundred)) +
  geom_quasirandom(
    aes(fill = iso_code == "IDN"),
    pch = 21,
    color = "white",
    size = 2.25,
    show.legend = F,
    alpha = 0.9
  ) +
  stat_summary(
    geom = "crossbar",
    fun = median,
    lwd = 0.5,
    color = "#2477B3"
  ) +
  geom_richtext(
    data = annotation_country,
    aes(x = income, y = people_vaccinated_per_hundred, label = location),
    size = 2.75,
    hjust = 0,
    nudge_x = 0.075,
    fill = "white",
    label.color = NA,
    label.padding = grid::unit(rep(0, 4), "pt")
  ) +
  geom_richtext(
    data = annotation_indonesia,
    aes(income, people_vaccinated_per_hundred, label = location),
    size = 2.75,
    hjust = 1,
    vjust = 0,
    nudge_x = -0.1,
    fill = "white",
    color = "#E66439",
    fontface = "bold",
    label.color = NA,
    label.padding = grid::unit(rep(0, 4), "pt")
  ) +
  geom_text_repel(
    data = tibble(x = "Low income", y = 1.22, label = "Median"),
    aes(x, y, label = label),
    size = 2.75,
    color = "#2477B3",
    fontface = "bold",
    nudge_x = -0.25,
    nudge_y = 8,
    segment.color = "#2477B3",
    segment.curvature = -0.1,
    segment.ncp = 5,
    segment.size = 0.75
  ) +
  scale_y_continuous(
    breaks = seq(0, 120, 30),
    limits = c(0, 120),
    expand = c(0, 0),
    position = "right"
  ) +
  scale_x_discrete(
    labels  = c(
      "Low income",
      "Lower-middle income",
      "Upper-middle income",
      "High income"
    )
  ) +
  scale_fill_manual(values = c("FALSE" = "#55CBF2", "TRUE" = "#E66439")) +
  labs(
    title = "Vaccine rollout is much faster in richer countries",
    subtitle = str_c(
      "Share of population vaccinated against COVID-19* ", 
      "by income group (percent)"
    ),
    x = NULL,
    y = NULL,
    caption = str_c(
      glue("*At latest date available as of {today}"),
      "\nSource: Our World in Data; World Bank",
      "\nChart: @dzulfiqarfr"
    )
  ) +
  theme_dfr() +
  theme(axis.ticks.x = element_line(color = "black"))
```

<img src="README_files/figure-gfm/Plot vaccination rate by income group-1.png" width="70%" style="display: block; margin: auto;" />
