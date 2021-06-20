Pandemic’s impact on wage
================
2021-06-20

This is a documentation of how I analyzed the COVID-19 pandemic’s impact
on average net labor wage for [this
article](https://www.thejakartapost.com/news/2021/06/14/covid-19-takes-severe-toll-on-wages.html).

## Packages

Load the packages.

``` r
library(tidyverse)
library(readxl)
library(lubridate)
library(httr)
library(jsonlite)
library(WDI)
library(patchwork)
library(ggtext)
library(magick)
```

## Data

### Average wage

Collect the average wage data from Statistics Indonesia (BPS) through
its
<a href="https://webapi.bps.go.id/" target="_blank">application programming interface (API)</a>.

``` r
# api
## key
BPS_KEY <- Sys.getenv("BPS_KEY")

## url for dynamic table
base_url_dynamic <- "https://webapi.bps.go.id/v1/api/list"

## url for static table
base_url_static <- "https://webapi.bps.go.id/v1/api/view"

# request data
wage_req <- GET(
  base_url_dynamic,
  query = list(
    model = "data",
    domain = "0000",
    var = "1521",
    key = BPS_KEY
  )
)

# parse response
wage_parsed <- content(wage_req, "text") %>% 
  fromJSON()

# extract keys
## sector
key_sector <- as_tibble(wage_parsed$vervar)

## year
key_yr <- as_tibble(wage_parsed$tahun)

## month
key_mo <- as_tibble(wage_parsed$turtahun)

# extract data
wage_raw <- as_tibble(wage_parsed$datacontent)

# tidy, create date variable
wage_tidy <- wage_raw %>% 
  pivot_longer(everything(), names_to = "key", values_to = "wage") %>% 
  separate(key, into = c("sector", "date"), sep = "15210") %>% 
  mutate(
    yr = str_sub(date, 1, 3),
    yr = str_replace_all(yr, deframe(key_yr)),
    mo = str_sub(date, 4, 6),
    mo = str_replace_all(mo, c("189" = "02-01", "190" = "08-01")),
    date = ymd(str_c(yr, mo, sep = "-"))
  ) %>% 
  select(-c(yr, mo))

# add english label to sector keys
key_sector <- key_sector %>% 
  mutate(
    val = str_c("^", val, "$"),
    label_eng = c(
      "Agriculture, forestry and fishery",
      "Mining",
      "Manufacturing",
      "Electricity and gas procurement",
      "Water, waste and recycling management procurement",
      "Construction",
      "Wholesale and retail trade; car and motorcycle repair",
      "Transportation and warehouse",
      "Accommodation and food and beverage services",
      "Information and communication",
      "Financial and insurance services",
      "Real estate",
      "Corporate service",
      "Government, defense and social security administration",
      "Education service",
      "Healthcare and social activity services",
      "Other",
      "Overall"
    )
  )

# replace sector keys
wage_tidy <- wage_tidy %>% 
  mutate(sector = str_replace_all(sector, deframe(key_sector[, -2])))

# glimpse
glimpse(wage_tidy)
#> Rows: 234
#> Columns: 3
#> $ sector <chr> "Agriculture, forestry and fishery", "Agriculture, forestry and~
#> $ date   <date> 2015-02-01, 2015-08-01, 2016-02-01, 2016-08-01, 2017-02-01, 20~
#> $ wage   <int> 1195354, 1336045, 1429297, 1655385, 1751410, 1772196, 1761849, ~
```

Add average and minimum wage data by province. I had to use Tabula to
extract this data from
<a href="https://www.bps.go.id/pressrelease/2021/05/05/1815/februari-2021--tingkat-pengangguran-terbuka--tpt--sebesar-6-26-persen.html" target="_blank">the <em>Berita Resmi Statistik</em> (BRS) pdf</a>.

``` r
# import
wage_prov_raw <- read_csv("data/bps_wage-min-wage-prov_raw.csv")

# tidy
wage_prov_wide <- wage_prov_raw %>% 
  slice(-1) %>% 
  select(-6) %>% 
  rename(
    prov = 1,
    `2020-02-01` = 2,
    `2020-08-01` = 3,
    `2021-02-01` = 4,
    pct_chg_yoy_raw = 5,
    min_wage_2021 = 6
  ) %>% 
  mutate(pct_chg_yoy_raw = as.numeric(str_replace_all(pct_chg_yoy_raw, ",", ".")))

# correct data types
wage_prov_wide[, c(2:4, 6)] <- map(
  wage_prov_wide[, c(2:4, 6)],
  function(x) {as.numeric(str_remove_all(x, " "))}
)

# tidy
wage_prov_tidy <- wage_prov_wide %>% 
  pivot_longer(2:4, names_to = "date", values_to = "wage_nom") %>% 
  select(prov, date, wage_nom, pct_chg_yoy_raw, min_wage_2021) %>% 
  mutate(
    prov = str_to_upper(prov) %>% 
      str_replace_all(
        c(
          "JAWA BARAT" = "West Java",
          "JAWA TENGAH" = "Central Java",
          "JAWA TIMUR" = "East Java",
          "D.I. " = "",
          "DKI " = "",
          "KALIMANTAN BARAT" = "West Kalimantan",
          "KALIMANTAN SELATAN" = "South Kalimantan",
          "KALIMANTAN TENGAH" = "Central Kalimantan", 
          "KALIMANTAN TIMUR" = "East Kalimantan",
          "KALIMANTAN UTARA" = "North Kalimantan",
          "KEPULAUAN BANGKA BELITUNG" = "Bangka Belitung Islands",
          "KEPULAUAN RIAU" = "Riau Islands",
          "MALUKU UTARA" = "North Maluku",
          "NUSA TENGGARA BARAT" = "West Nusa Tenggara",
          "NUSA TENGGARA TIMUR" = "East Nusa Tenggara",
          "PAPUA BARAT" = "West Papua",
          "SULAWESI BARAT" = "West Sulawesi",
          "SULAWESI SELATAN" = "South Sulawesi",
          "SULAWESI TENGAH" = "Central Sulawesi",
          "SULAWESI TENGGARA" = "Southeast Sulawesi",
          "SULAWESI UTARA" = "North Sulawesi",
          "SUMATERA SELATAN" = "South Sumatra",
          "SUMATERA UTARA" = "North Sumatra",
          "SUMATERA BARAT" = "West Sumatra"
        )
      ) %>% 
      str_to_title(),
    pct_chg_yoy_raw = case_when(
      date != "2021-02-01" ~ 0,
      TRUE ~ pct_chg_yoy_raw
    ),
    date = ymd(date)
  )

# glimpse
glimpse(wage_prov_tidy)
#> Rows: 105
#> Columns: 5
#> $ prov            <chr> "Aceh", "Aceh", "Aceh", "North Sumatra", "North Sumatr~
#> $ date            <date> 2020-02-01, 2020-08-01, 2021-02-01, 2020-02-01, 2020-~
#> $ wage_nom        <dbl> 2549421, 2394965, 2317419, 2435961, 2384060, 2402795, ~
#> $ pct_chg_yoy_raw <dbl> 0.00, 0.00, -9.10, 0.00, 0.00, -1.36, 0.00, 0.00, 2.23~
#> $ min_wage_2021   <dbl> 3165031, 3165031, 3165031, 2499423, 2499423, 2499423, ~
```

Import the data on worker distribution by wage group. BPS published
<a href="https://bps.go.id/publication/2021/06/08/ccf5b352d7f42b9718b93f44/keadaan-pekerja-di-indonesia-februari-2021.html" target="_blank">this data in a pdf</a>,
so I had to use Tabula again and clean the data in a spreadsheet first.

``` r
# import
wage_group_raw <- read_csv("data/bps_wage-group-gender_raw.csv")

# tidy, convert wage group to million
wage_group_tidy <- wage_group_raw %>% 
  separate(
    urban_rural_male_female_2020,
    into = c("male_2020", "female_2020", "total_2020"),
    sep = "_"
  ) %>% 
  separate(
    urban_rural_male_female_2021,
    into = c("male_2021", "female_2021", "total_2021"),
    sep = "_"
  ) %>% 
  pivot_longer(2:ncol(.), names_to = "cat", values_to = "workers") %>% 
  separate(cat, into = c("gender", "year"), sep = "_") %>% 
  mutate(
    wage_group = str_replace_all(
      wage_group,
      c(
        "^< 200 000$" = "< 0.20",
        "^200 000 - 399 999$" = "0.20-0.39",
        "^400 000 - 599 999$" = "0.40-0.59",
        "^600 000 - 799 999$" = "0.60-0.79",
        "^800 000 - 999 999$" = "0.80-0.99",
        "^1 000 000 - 1 499 999$" = "1-1.49",
        "^1 500 000 - 1 999 999$" = "1.5-1.99",
        "^2 000 000 - 2 499 999$" = "2-2.49",
        "^2 000 000 \\+$" = "2+"
      )
    ),
    gender = as_factor(gender),
    year = as.numeric(year),
    workers = as.numeric(str_remove_all(workers, " "))
  )

# glimpse
glimpse(wage_group_tidy)
#> Rows: 54
#> Columns: 4
#> $ wage_group <chr> "< 0.20", "< 0.20", "< 0.20", "< 0.20", "< 0.20", "< 0.20",~
#> $ gender     <fct> male, female, total, male, female, total, male, female, tot~
#> $ year       <dbl> 2020, 2020, 2020, 2021, 2021, 2021, 2020, 2020, 2020, 2021,~
#> $ workers    <dbl> 205630, 421330, 626960, 281572, 486376, 767948, 741274, 140~
```

### Consumer price index

Add consumer price index (CPI) from the World Bank to deflate the
average wage.

``` r
# look up indicator
wdi_indicator <- WDIsearch("consumer price index") %>% 
  as_tibble() %>% 
  dplyr::filter(name == "Consumer price index (2010 = 100)")

# import data
cpi_raw <- WDI(wdi_indicator$indicator, country = "ID")

# tidy
cpi_tidy <- cpi_raw %>% 
  select(-c(1:2)) %>% 
  rename(cpi_2010 = 1) %>% 
  select(year, cpi_2010) %>% 
  arrange(year)

# glimpse
glimpse(cpi_tidy)
#> Rows: 61
#> Columns: 2
#> $ year     <int> 1960, 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1~
#> $ cpi_2010 <dbl> 0.0002776838, 0.0003157041, 0.0007305324, 0.0017964580, 0.003~
```

### Growth by sector

Add economic growth by sector from BPS’s API.

``` r
# request data
growth_sec_req <- GET(
  base_url_dynamic,
  query = list(
    model = "data",
    domain = "0000",
    var = "104",
    key = BPS_KEY
  )
)

# parse response
growth_sec_parsed <- content(growth_sec_req, "text") %>% 
  fromJSON()

# extract keys
## sectors
growth_key_sec <- as_tibble(growth_sec_parsed$vervar)

## year
growth_key_yr <- as_tibble(growth_sec_parsed$tahun)

## data
growth_sec_raw <- as_tibble(growth_sec_parsed$datacontent)

# separate keys, subset quarterly observations
growth_sec_tidy <- growth_sec_raw %>% 
  pivot_longer(1:ncol(.), names_to = "key", values_to = "pct_change_yoy") %>% 
  separate(key, into = c("sector", "period"), sep = "104") %>% 
  mutate(
    period_obs = str_sub(period, 1, 1),
    yr = str_sub(period, 2, 4),
    q = str_sub(period, 5, 6)
  ) %>% 
  dplyr::filter(period_obs == "5", q != "35")
#> Warning: Expected 2 pieces. Additional pieces discarded in 153 rows [8263, 8264,
#> 8265, 8266, 8267, 8268, 8269, 8270, 8271, 8272, 8273, 8274, 8275, 8276, 8277,
#> 8278, 8279, 8280, 8281, 8282, ...].

# replace year key
growth_sec_tidy$yr <- growth_sec_tidy$yr %>% 
  str_replace_all(deframe(growth_key_yr))

# replace quarter key
growth_sec_tidy$q <- growth_sec_tidy$q %>% 
  str_replace_all(
    c(
      "^31$" = "-01-01",
      "^32$" = "-04-01",
      "^33$" = "-07-01",
      "^34$" = "-10-01"
    )
  )

# create date variable
growth_sec_tidy <- growth_sec_tidy %>% 
  mutate(date = ymd(str_c(yr, q))) %>% 
  select(sector, date, pct_change_yoy)

# subset main sectors
## main sector key
growth_key_sec_main <- seq(11000, 27000, 1000)

## subset
growth_sec_tidy <- growth_sec_tidy %>% 
  dplyr::filter(sector %in% growth_key_sec_main)

# replace sector key
growth_sec_tidy$sector <- growth_sec_tidy$sector %>% 
  str_replace_all(
    c(
      "11000" = "Agriculture, forestry and fishery",
      "12000" = "Mining",
      "13000" = "Manufacturing",
      "14000" = "Electricity and gas procurement",
      "15000" = "Water, waste and recycling management procurement",
      "16000" = "Construction",
      "17000" = "Wholesale and retail trade; car and motorcycle repair",
      "18000" = "Transportation and warehouse",
      "19000" = "Accommodation and food and beverage services",
      "20000" = "Information and communication",
      "21000" = "Financial and insurance services",
      "22000" = "Real estate",
      "23000" = "Corporate service",
      "24000" = "Government, defense and social security administration",
      "25000" = "Education service",
      "26000" = "Healthcare and social activity services",
      "27000" = "Other"
    )
  )

# glimpse
glimpse(growth_sec_tidy)
#> Rows: 697
#> Columns: 3
#> $ sector         <chr> "Agriculture, forestry and fishery", "Agriculture, fore~
#> $ date           <date> 2011-01-01, 2011-04-01, 2011-07-01, 2011-10-01, 2012-0~
#> $ pct_change_yoy <dbl> 4.18, 4.95, 3.69, 2.89, 5.49, 4.21, 5.60, 2.78, 4.21, 4~
```

### Unemployment rate

Import the unemployment rate data, also from BPS’s API.

``` r
# request data
unemployment_req <- GET(
  base_url_dynamic,
  query = list(
    model = "data",
    domain = "0000",
    var = "529",
    key = BPS_KEY
  )
)

# parse response
unemployment_parsed <- content(unemployment_req, "text") %>% 
  fromJSON()

# extract keys
## activities
unemployment_key_act <- as_tibble(unemployment_parsed$vervar)

## year
unemployment_key_yr <- as_tibble(unemployment_parsed$tahun) 

# extract data
unemployment_raw <- as_tibble(unemployment_parsed$datacontent)

# tidy data
unemployment_tidy <- unemployment_raw %>% 
  pivot_longer(1:ncol(.), names_to = "key", values_to = "val") %>% 
  separate(key, into = c("key_act", "key_date"), sep = "5290") %>% 
  mutate(
    key_yr = case_when(
      str_detect(key_date, "^[8|9]") ~ as.numeric(str_sub(key_date, 1, 2)),
      str_detect(key_date, "^1") ~ as.numeric(str_sub(key_date, 1, 3))
    ),
    key_mo = case_when(
      str_detect(key_date, "^[8|9]") ~ as.numeric(str_sub(key_date, 3, 5)),
      str_detect(key_date, "^1") ~ as.numeric(str_sub(key_date, 4, 6))
    )
  ) %>% 
  select(key_act, key_date, key_yr, key_mo, val)

# add anchor to year key
unemployment_key_yr$val <- str_c("^", unemployment_key_yr$val, "$")

# replace year key
unemployment_tidy$key_yr <- unemployment_tidy$key_yr %>% 
  str_replace_all(deframe(unemployment_key_yr)) %>% 
  as.numeric()

# replace month key
unemployment_tidy$key_mo <- unemployment_tidy$key_mo %>% 
  str_replace_all(c("^189$" = "-02-01", "^190$" = "-08-01", "^191$" = "-01-01"))

# create date variable
unemployment_tidy <- unemployment_tidy %>% 
  mutate(date = ymd(str_c(key_yr, key_mo)), mo = month(date)) %>% 
  rename(yr = key_yr) %>% 
  select(key_act, date, yr, mo, val)

# add activity labels
unemployment_tidy <- unemployment_tidy %>% 
  mutate(act = str_replace_all(unemployment_tidy$key_act, deframe(unemployment_key_act))) %>% 
  select(key_act, act, date, yr, mo, val)

# subset unemployment rate
unemp_rate <- unemployment_tidy %>% 
  dplyr::filter(key_act == 6, yr >= 2005) %>% 
  select(-c(1, 2, 4)) %>%
  mutate(year = year(date)) %>% 
  dplyr::filter(year >= 2015) %>% 
  rename(unemp_rate = val) %>% 
  select(date, year, unemp_rate)

# glimpse
glimpse(unemp_rate)
#> Rows: 13
#> Columns: 3
#> $ date       <date> 2015-02-01, 2015-08-01, 2016-02-01, 2016-08-01, 2017-02-01~
#> $ year       <dbl> 2015, 2015, 2016, 2016, 2017, 2017, 2018, 2018, 2019, 2019,~
#> $ unemp_rate <dbl> 5.81, 6.18, 5.50, 5.61, 5.33, 5.50, 5.10, 5.30, 4.98, 5.23,~
```

## Plot

### Wage growth and unemployment rate

The average labor wage is negatively correlated with the unemployment
rate. We can first calculate the real wage growth and then plot it along
side the unemployment rate to compare changes in the two indicators.

``` r
# real wage growth
wage_trf <- wage_tidy %>% 
  mutate(month = month(date), year = year(date)) %>% 
  left_join(cpi_tidy, by = "year") %>% 
  mutate(cpi_decimal = cpi_2010 / 100, wage_real = wage / cpi_decimal) %>% 
  rename(wage_nom = wage) %>% 
  group_by(sector, month) %>% 
  mutate(
    diff_nom = wage_nom - dplyr::lag(wage_nom, 1),
    wage_nom_chg = diff_nom / dplyr::lag(wage_nom, 1) * 100,
    diff_real = wage_real - dplyr::lag(wage_real, 1),
    wage_real_chg = diff_real / dplyr::lag(wage_real, 1) * 100
  ) %>% 
  ungroup() %>% 
  select(
    sector, 
    date, 
    year, 
    wage_nom, 
    wage_real, 
    wage_nom_chg, 
    wage_real_chg
  )

# subset overall average wage
wage_overall <- wage_trf %>% 
  dplyr::filter(sector == "Overall")

# glimpse
glimpse(wage_overall)
#> Rows: 13
#> Columns: 7
#> $ sector        <chr> "Overall", "Overall", "Overall", "Overall", "Overall", "~
#> $ date          <date> 2015-02-01, 2015-08-01, 2016-02-01, 2016-08-01, 2017-02~
#> $ year          <dbl> 2015, 2015, 2016, 2016, 2017, 2017, 2018, 2018, 2019, 20~
#> $ wage_nom      <int> 1981725, 2069306, 2180577, 2552962, 2702590, 2742621, 26~
#> $ wage_real     <dbl> 1497891, 1564090, 1592061, 1863943, 1900791, 1928945, 18~
#> $ wage_nom_chg  <dbl> NA, NA, 10.034288, 23.372860, 23.939214, 7.428979, -1.79~
#> $ wage_real_chg <dbl> NA, NA, 6.28682206, 19.17111882, 19.39182088, 3.48735422~
```

Plot wage growth and unemployment rate.

``` r
# join wage, unemployment data
wage_unemp <- wage_overall %>% 
  left_join(unemp_rate, by = c("date", "year"))

# axis labels
## wage
x_axis_labs_wage <- seq(ymd("2016-02-01"), ymd("2020-08-01"), "6 month")

x_axis_labs_wage <- x_axis_labs_wage %>% 
  enframe() %>% 
  mutate(
    labs = case_when(name %% 2 == 0 ~ "A", TRUE ~ "F"),
    labs = case_when(
      value == first(value) ~ "Feb",
      value == value[2] ~ "Aug",
      TRUE ~ labs
    ),
    labs = case_when(
      name %% 2 != 0 ~ str_c(labs, "\n'", format(value, "%y")),
      TRUE ~ labs
    )
  )

## unemployment
x_axis_labs_unemp <- seq(ymd("2016-02-01"), ymd("2021-02-01"), "6 month")

x_axis_labs_unemp <- x_axis_labs_unemp %>% 
  enframe() %>% 
  mutate(
    labs = case_when(name %% 2 == 0 ~ "A", TRUE ~ "F"),
    labs = case_when(
      value == first(value) ~ "Feb",
      value == value[2] ~ "Aug",
      TRUE ~ labs
    ),
    labs = case_when(
      name %% 2 != 0 ~ str_c(labs, "\n'", format(value, "%y")),
      TRUE ~ labs
    )
  )

# wage growth plot
plot_wage <- wage_unemp %>% 
  dplyr::filter(!is.na(wage_real_chg)) %>% 
  ggplot(aes(date, wage_real_chg)) +
  geom_hline(yintercept = 0, color = "#FF8A80") +
  geom_vline(xintercept = ymd("2020-03-01"), color = "#90A4AE", lty = "dashed") +
  geom_line(color = "#009688", lwd = 1) +
  scale_x_continuous(
    breaks = seq(ymd("2016-02-01"), ymd("2020-08-01"), "6 month"),
    labels = x_axis_labs_wage$labs,
    limits = c(ymd("2016-02-01"), ymd("2020-08-01"))
  ) +
  scale_y_continuous(
    breaks = seq(-20, 20, 10),
    limits = c(-20, 20),
    expand = c(0, 0),
    position = "right"
  ) +
  labs(
    subtitle = "Real average net wage\n(annual percentage change)",
    x = NULL,
    y = NULL
  ) +
  annotate(
    "text",
    x = ymd("2020-02-01"),
    y = 15,
    label = "COVID-19\npandemic\n\u2192",
    size = 2.5,
    hjust = 1,
    color = "#90A4AE"
  ) +
  theme(
    text = element_text(size = 12),
    axis.text = element_text(size = rel(0.7)),
    axis.line.x = element_line(color = "black"),
    axis.ticks.y = element_blank(),
    panel.background = element_rect(fill = "white"),
    panel.grid.major.x  = element_blank(),
    panel.grid.major.y = element_line(color = "#CFD8DC"),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    plot.subtitle = element_text(size = rel(0.9), margin = margin(b = 12.5)),
    plot.margin = margin(r = 12.5)
  )

# unemployment rate plot
plot_unemp <- wage_unemp %>% 
  dplyr::filter(!str_detect(date, "2015")) %>% 
  ggplot(aes(date, unemp_rate)) +
  geom_vline(xintercept = ymd("2020-03-01"), color = "#90A4AE", lty = "dashed") +
  geom_line(color = "#009688", lwd = 1) +
  scale_x_continuous(
    breaks = seq(ymd("2016-02-01"), ymd("2021-02-01"), "6 month"),
    labels = x_axis_labs_unemp$labs,
    limits = c(ymd("2016-02-01"), ymd("2021-02-01"))
  ) +
  scale_y_continuous(
    breaks = seq(0, 10, 2),
    limits = c(0, 10),
    expand = c(0, 0),
    position = "right"
  ) +
  labs(
    subtitle = "Unemployment rate\n(percent)",
    x = NULL,
    y = NULL
  ) +
  theme(
    text = element_text(size = 12),
    axis.text = element_text(size = rel(0.7)),
    axis.line.x = element_line(color = "black"),
    axis.ticks.y = element_blank(),
    panel.background = element_rect(fill = "white"),
    panel.grid.major.x  = element_blank(),
    panel.grid.major.y = element_line(color = "#CFD8DC"),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    plot.subtitle = element_text(size = rel(0.9), margin = margin(b = 12.5)),
    plot.margin = margin(l = 12.5)
  )

# patchwork
plot_wage + plot_unemp +
  plot_annotation(
    title = "Fall in wages",
    caption = "Chart: @dzulfiqarfr | Source: Statistics Indonesia (BPS)",
    theme = theme(
      text = element_text(size = 12),
      plot.title = element_text(face = "bold", margin = margin(b = 25)),
      plot.caption = element_text(
        size = rel(0.8),
        color = "#757575",
        hjust = 0,
        margin = margin(t = 25)
      )
    )
  )
```

<img src="README_files/figure-gfm/plot wage growth and unemployment rate-1.png" width="70%" style="display: block; margin: auto;" />

### Wage growth and economic growth by sector

We can expect that the fall in average wage should be less severe in
sectors with a smaller contraction or even a higher growth.

``` r
# subset latest data of wage growth
wage_real_recent <- wage_trf %>% 
  dplyr::filter(sector != "Overall", date == ymd("2020-08-01")) %>% 
  select(sector, year, wage_nom_chg, wage_real_chg)

# subset latest data of gdp growth by sector
growth_sec_recent <- growth_sec_tidy %>% 
  dplyr::filter(date == ymd("2020-07-01")) %>% 
  mutate(year = year(date)) %>% 
  select(sector, year, pct_change_yoy) %>% 
  rename(econ_growth = 3)

# join
wage_gdp <- wage_real_recent %>% 
  left_join(growth_sec_recent, by = c("sector", "year"))

# plot annotations
filter_labs <- c(
  "Transportation and warehouse",
  "Accommodation and food and beverage services",
  "Construction",
  "Information and communication",
  "Healthcare and social activity services"
)

anno_labs <- wage_gdp %>% 
  dplyr::filter(sector %in% filter_labs) %>% 
  mutate(
    sector = str_replace_all(
      sector, 
      c(
        "Healthcare and social activity services" = "Healthcare and\nsocial services",
        "Accommodation and food and beverage services" = "Accommodation and\nF&B services",
        "Transportation and warehouse" = "Transportation and warehousing"
      )
    )
  )

# plot
ggplot(wage_gdp, aes(econ_growth, wage_real_chg)) +
  geom_hline(yintercept = 0, color = "#FF8A80") +
  geom_vline(xintercept = 0, color = "#FF8A80") +
  geom_point(
    pch = 21, 
    fill = "#009688",
    color = "white",
    alpha = 0.75,
    size = 3.5
  ) +
  geom_smooth(
    method = "lm",
    color = "black",
    lwd = 0.5,
    lty = "dashed",
    se = F
  ) +
  scale_x_continuous(
    breaks = seq(-20, 20, 10),
    limits = c(-20, 20),
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    breaks = seq(-20, 0, 4),
    limits = c(-20, 0),
    expand = c(0, 0),
    position = "right"
  ) +
  labs(
    title = "Wages fall across all sectors",
    subtitle = "Real average net wage (Aug '20) and economic growth (July-Sep '20), by sector\n(annual percentage change)",
    x = "\u2190 Deeper economic contraction | Higher econmic growth \u2192",
    y = "\u2190 Less severe decline in wages",
    caption = "Chart: @dzulfiqarfr | Source: Statistics Indonesia (BPS)"
  ) +
  geom_label(
    data = anno_labs, 
    aes(label = sector),
    hjust = 0,
    size = 2,
    color = "#009688",
    fill = "white",
    label.size = 0,
    label.padding = unit(0.05, "lines"),
    nudge_x = 0.5,
    nudge_y = 0.5
  ) +
  theme(
    text = element_text(size = 12),
    axis.title = element_text(size = rel(0.8)),
    axis.text = element_text(size = rel(0.8)),
    axis.ticks = element_blank(),
    panel.background = element_rect(fill = "white"),
    panel.grid.major = element_line(color = "#CFD8DC"),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    plot.title = element_text(face = "bold"),
    plot.title.position = "plot",
    plot.subtitle = element_text(size = rel(0.9), margin = margin(b = 25)),
    plot.caption = element_text(
      size = rel(0.8),
      color = "#757575",
      hjust = 0,
      margin = margin(t = 25)
    ),
    plot.caption.position = "plot"
  )
```

<img src="README_files/figure-gfm/wage growth and growth by sector-1.png" width="70%" style="display: block; margin: auto;" />

### Average and minimum wage by province

We can see that the average wage in some provinces was already below the
minimum wage even before the pandemic. With the pandemic battering
workers, some fell even further below the wage floor in nominal terms.

``` r
# reshape
wage_prov_wide <- wage_prov_tidy %>% 
  dplyr::filter(date != ymd("2020-08-01"), prov != "Indonesia") %>% 
  select(-pct_chg_yoy_raw) %>%
  mutate(
    wage_nom = wage_nom / 1000000,
    min_wage_2021 = min_wage_2021 / 1000000,
    prov = fct_reorder(prov, min_wage_2021)
  ) %>% 
  pivot_wider(names_from = date, values_from = wage_nom)

# plot annotation
anno_wage_min_wage <- tribble(
  ~prov, ~x, ~label,
  "Aceh", 2.317419, "Feb 2021",
  "South Sumatra", 2.256548, "Feb 2020",
  "Riau Islands", 3.005460, "2021 minimum wage"
)

# plot
ggplot(wage_prov_wide, aes(y = prov)) +
  geom_segment(
    aes(yend = prov, x = `2020-02-01`, xend = `2021-02-01`),
    color = "#B2DFDB",
    lwd = 2,
    alpha = 0.5
  ) +
  geom_point(
    aes(x = `2020-02-01`),
    shape = 21,
    fill = "#B2DFDB",
    color = "white",
    size = 2.5
  ) +
  geom_point(
    aes(x = `2021-02-01`),
    shape = 21,
    fill = "#26A69A",
    color = "white",
    size = 2.5
  ) +
  geom_point(
    aes(x = min_wage_2021),
    shape = 21,
    color = "white",
    fill = "#FF7043",
    size = 2.5
  ) +
  scale_x_continuous(
    breaks = seq(1, 5, 1),
    limits = c(1, 5),
    expand = c(0, 0),
    position = "top"
  ) +
  labs(
    title = "Wages in several provinces fall below the minimum wage",
    subtitle = "Average net wage and minimum wage, by province (million rupiah)",
    x = NULL,
    y = NULL,
    caption = "Chart: @dzulfiqarfr | Source: Statistics Indonesia (BPS)"
  ) +
  geom_richtext(
    data = anno_wage_min_wage,
    aes(y = prov, x = x, label = label),
    fill = "white",
    label.color = NA,
    label.padding = unit(c(0.05, 0.05, 0.05, 0.05), "lines"),
    hjust = 1,
    nudge_x = -0.05,
    size = 2.5,
    color = c("#26A69A", "#80CBC4", "#FF7043"),
    fontface = "bold"
  ) +
  theme(
    text = element_text(size = 12),
    axis.title = element_text(size = rel(0.8)),
    axis.text = element_text(size = rel(0.8)),
    axis.text.y = element_text(hjust = 0),
    axis.ticks = element_blank(),
    axis.line.y = element_line(color = "black"),
    panel.background = element_rect(fill = "white"),
    panel.grid.major.x = element_line(color = "#CFD8DC"),
    panel.grid.major.y = element_line(color = "#CFD8DC"),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    plot.title = element_text(face = "bold"),
    plot.title.position = "plot",
    plot.subtitle = element_text(size = rel(0.9), margin = margin(b = 25)),
    plot.caption = element_text(
      size = rel(0.8),
      color = "#757575",
      hjust = 0,
      margin = margin(t = 25)
    ),
    plot.caption.position = "plot"
  )
```

<img src="README_files/figure-gfm/wage by province-1.png" width="70%" style="display: block; margin: auto;" />

### Worker distribution by wage group

There was a contrast in the pandemic’s impact on worker distribution
between men and women. It was relatively more spread out among men.

We can also see that the increase in the share of female workers with a
net wage less than Rp 200,000 a month is over two times higher than that
of male workers.

``` r
# remove total
wage_group_sub <- wage_group_tidy %>% 
  dplyr::filter(gender != "total", wage_group != "total")

# subset total
wage_group_tot <- wage_group_tidy %>% 
  dplyr::filter(wage_group == "total") %>% 
  rename(total = workers)

# calculate proportion
wage_group_dis <- wage_group_sub %>% 
  left_join(wage_group_tot[-1], by = c("gender", "year")) %>% 
  group_by(wage_group, gender) %>% 
  mutate(
    diff = workers - dplyr::lag(workers, 1),
    chg = diff / dplyr::lag(workers, 1) * 100,
    prop = workers / total * 100,
    prop_diff = prop - dplyr::lag(prop, 1)
  )

# plot
wage_group_dis %>% 
  dplyr::filter(!is.na(prop_diff)) %>% 
  mutate(year = as_factor(year)) %>% 
  ggplot(aes(prop_diff, wage_group, fill = gender)) +
  geom_vline(xintercept = 0, color = "#FF8A80") +
  geom_col() +
  scale_x_continuous(
    breaks = seq(-1, 1, 0.25),
    labels = c(-1, seq(-0.75, 0.75, 0.25), 1),
    limits = c(-1, 1),
    expand = c(0, 0),
    position = "top"
  ) +
  scale_y_discrete(limits = rev) +
  scale_fill_manual(
    labels = c("Men", "Women"),
    values = c("male" = "#536DFE", "female" = "#FF4081")
  ) +
  labs(
    title = "Pandemic impact more spread out among men",
    subtitle = "Annual change in worker distribution (Feb '21), by gender and net wage group*\n(percentage point)",
    x = NULL,
    y = NULL,
    caption = "*Net wage group in million rupiah\n\nChart: @dzulfiqarfr | Source: Statistics Indonesia (BPS)"
  ) +
  facet_wrap(~ gender, scales = "free_y") +
  theme(
    text = element_text(size = 12),
    axis.text = element_text(size = rel(0.6)),
    axis.text.y = element_text(hjust = 0),
    axis.ticks.x = element_blank(),
    axis.line.y = element_line(color = "black"),
    legend.position = c(0.016, 1.2),
    legend.direction = "horizontal",
    legend.title = element_blank(),
    legend.text = element_text(size = 7.5),
    legend.key.height = unit(.25, "cm"),
    panel.background = element_rect(fill = "white"),
    panel.grid.major.x = element_line(color = "#CFD8DC"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.spacing = unit(1.5, "lines"),
    plot.title = element_text(face = "bold"),
    plot.title.position = "plot",
    plot.subtitle = element_text(size = rel(0.9), margin = margin(b = 35)),
    plot.caption = element_text(
      size = rel(0.8),
      color = "#757575",
      hjust = 0,
      margin = margin(t = 25)
    ),
    plot.caption.position = "plot",
    strip.background = element_blank(),
    strip.text = element_blank()
  )
```

<img src="README_files/figure-gfm/plot worker distribution by wage group-1.png" width="70%" style="display: block; margin: auto;" />