Home ownership and house prices
================
2021-06-28

I prepared this analysis for
<a href="https://www.thejakartapost.com/news/2021/06/28/indonesian-homeownership-slides-as-affordability-issues-arise.html" target="_blank">this article</a>
on how home ownership has fallen since 1999 in Indonesia as
affordability issues arise. Here, we take a look at home ownership,
house price and mortgage data.

## Packages

Load the packages.

``` r
library(tidyverse)
library(readxl)
library(tabulizer)
library(lubridate)
library(httr)
library(jsonlite)
library(gghighlight)
library(ggrepel)
library(patchwork)
```

## Data

### Home ownership

We collect the home ownership data from Statistics Indonesia’s (BPS)
<a href="https://webapi.bps.go.id/" target="_blank">application programming interface (API)</a>.
The data shows the share of households with a home of their own between
1999 and 2020 (as of the time of writing). You can read the metadata
<a href="https://bps.go.id/subject/29/perumahan.html#subjekViewTab1" target="_blank">here</a>.

Not all provinces have complete observations for the whole period, such
as North Kalimantan, which makes sense since this province was
established only in 2012.

There are also methodological differences in several years that
introduce issues to the data. In 2002, for example, in several
provinces, BPS surveyed only households in the capital city of that
province. This tends to overstate the decline in home ownership seen in
those provinces in 2002.

``` r
# api
## key
BPS_KEY <- Sys.getenv("BPS_KEY")

## url for dynamic table
base_url_dynamic <- "https://webapi.bps.go.id/v1/api/list"

# request data
ownership_req <- GET(
  base_url_dynamic,
  query = list(
    model = "data",
    domain = "0000",
    var = "849",
    key = BPS_KEY
  )
)

# parse response
ownership_parsed <- content(ownership_req, "text") %>% 
  fromJSON()

# extract keys
## province
key_prov <- as_tibble(ownership_parsed$vervar)

## year
key_yr <- as_tibble(ownership_parsed$tahun)

# extract data
ownership_raw <- as_tibble(ownership_parsed$datacontent)

# tidy
ownership_tidy <- ownership_raw %>% 
  pivot_longer(
    everything(), 
    names_to = "key",
    values_to = "home_ownership"
  ) %>% 
  separate(key, into = c("prov", "year"), sep = "8490") %>% 
  mutate(
    prov = str_replace_all(prov, deframe(key_prov)),
    prov = str_replace_all(
      prov,
      c(
        "JAWA BARAT" = "West Java",
        "JAWA TENGAH" = "Central Java",
        "JAWA TIMUR" = "East Java",
        "DI " = "",
        "DKI " = "",
        "KALIMANTAN BARAT" = "West Kalimantan",
        "KALIMANTAN SELATAN" = "South Kalimantan",
        "KALIMANTAN TENGAH" = "Central Kalimantan", 
        "KALIMANTAN TIMUR" = "East Kalimantan",
        "KALIMANTAN UTARA" = "North Kalimantan",
        "KEP. BANGKA BELITUNG" = "Bangka Belitung Islands",
        "KEP. RIAU" = "Riau Islands",
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
    year = str_remove_all(year, "0$"),
    year = as.numeric(str_replace_all(year, deframe(key_yr)))
  ) 

# glimpse
glimpse(ownership_tidy)
#> Rows: 723
#> Columns: 3
#> $ prov           <chr> "Indonesia", "Indonesia", "Indonesia", "Indonesia", "In~
#> $ year           <dbl> 1999, 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2~
#> $ home_ownership <dbl> 84.98, 84.68, 80.71, 82.47, 83.66, 82.38, 81.95, 81.24,~
```

### House price index

We use Bank Indonesia’s (BI) Residential Property Price Index (IHPR) to
look at how house prices have changed over time. I have downloaded the
data from
<a href="https://www.bi.go.id/id/publikasi/laporan/Pages/SHPR-Triwulan-I-2021.aspx" target="_blank">this link</a>.
The data shows how house prices in 18 cities across the country have
changed since 2002. The data is quarterly. You can read the metadata at
<a href="https://www.bi.go.id/id/statistik/Metadata/Survei/Documents/3-Metadata-SHPR-2016.pdf" target="_blank">this link</a>.

However, the data stretches only as far back as 2012 and there are
missing values for some cities.

``` r
# import
shpr_raw <- read_excel(
  "data/bi_shpr_raw.xlsx",
  sheet = "TABEL 2",
  skip = 6,
  na = c("", "-")
) %>% 
  select(-c(1, 4, ncol(.))) %>% 
  rename(cities = 1, house_type = 2)

# repair city column
shpr_filled <- shpr_raw %>% 
  mutate(
    cities = case_when(
      house_type == "PEKANBARU" ~ "Pekanbaru",
      house_type == "SAMARINDA" ~ "Samarinda",
      TRUE ~ str_to_title(cities)
    ),
    house_type = str_to_title(house_type)
  ) %>%
  fill(cities) %>% 
  mutate(
    cities = case_when(
      cities %in% c("Gabungan 16 Kota", "(Termasuk Jabodebek", "Dan Banten)") ~"Overall",
      TRUE ~ cities
    )
  ) %>% 
  dplyr::filter(house_type %in% c("Kecil", "Menengah", "Besar", "Total"))

# replace column names with date
latest_quarter <- floor_date(Sys.Date(), "quarter")

date <- seq(ymd("2012-01-01"), latest_quarter, "quarter")

date_annotated <- date %>%
  rep(., 2) %>% 
  enframe() %>% 
  mutate(
    value = case_when(
      name %in% seq(1, length(date)) ~ str_c(value, "_qtq"),
      TRUE ~ str_c(value, "_yoy")
    )
  ) %>% 
  deframe()

names(shpr_filled)[-c(1:2)] <- as.character(date_annotated)

# convert growth to character
shpr_filled[, 3:ncol(shpr_filled)] <- map(
  shpr_filled[, 3:ncol(shpr_filled)], 
  as.character
)

# tidy
shpr_tidy <- shpr_filled %>% 
  pivot_longer(3:ncol(.), names_to = "date", values_to = "shpr_growth") %>%
  separate(date, into = c("date", "change_term"), sep = "_") %>% 
  mutate(
    house_type = as_factor(house_type),
    date = ymd(date),
    shpr_growth = str_replace_all(shpr_growth, ",", "."),
    shpr_growth = parse_number(shpr_growth)
  )

# glimpse
glimpse(shpr_tidy)
#> Rows: 5,776
#> Columns: 5
#> $ cities      <chr> "Bandung", "Bandung", "Bandung", "Bandung", "Bandung", "Ba~
#> $ house_type  <fct> Kecil, Kecil, Kecil, Kecil, Kecil, Kecil, Kecil, Kecil, Ke~
#> $ date        <date> 2012-01-01, 2012-04-01, 2012-07-01, 2012-10-01, 2013-01-0~
#> $ change_term <chr> "qtq", "qtq", "qtq", "qtq", "qtq", "qtq", "qtq", "qtq", "q~
#> $ shpr_growth <dbl> 1.260145, 1.072153, 1.748940, 0.710000, 2.220000, 0.190000~
```

### Mortgages

We also take a look at the mortgage data BPS collected in 2019. The data
is disaggregated by province, income level, educational attainment, etc.
But we will only use the regional data to keep it straightforward. The
data is available in a pdf document and I have downloaded it from
<a href="https://bps.go.id/publication/2020/08/31/6a9e70d6154fde75499239e6/statistik-perumahan-dan-permukiman-2019.html" target="_blank">this link</a>.

For both average monthly payments and term data, there are a few missing
values for West Nusa Tenggara, Maluku and North Maluku.

We first read the average monthly payment data.

``` r
# import
mortgage_p_raw <- extract_tables(
  "data/bps_2020_report.pdf",
  pages = 62,
  output = "data.frame"
)

# tidy
## clean up
mortgage_p_raw_2 <- mortgage_p_raw %>% 
  pluck(1) %>% 
  slice(-c(1:3, nrow(.))) %>%
  select(1, ncol(.)) %>%
  rename(prov = 1, mortgage_payment = 2)

## separate collapsed observations into multiple rows
mortgage_p_raw_sub <- mortgage_p_raw_2 %>% 
  dplyr::filter(str_detect(mortgage_payment, " ")) %>% 
  mutate(
    prov = str_replace_all(prov, "Timur[ ]", "Timur_"),
    prov = str_replace_all(prov, "Tengah[ ]", "Tengah_"),
    mortgage_payment = str_replace_all(mortgage_payment, " ", "_")
  ) %>% 
  separate_rows(prov, mortgage_payment, sep = "_")

## replace column names, correct data type
mortgage_p_tidy <- mortgage_p_raw_2 %>% 
  dplyr::filter(!str_detect(mortgage_payment, " ")) %>%
  rbind(mortgage_p_raw_sub) %>% 
  mutate(
    prov = str_replace_all(prov, "Kep.", "Kepulauan"),
    prov = str_replace_all(
      str_to_upper(prov),
      c(
        "JAWA BARAT" = "West Java",
        "JAWA TENGAH" = "Central Java",
        "JAWA TIMUR" = "East Java",
        "DI " = "",
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
    mortgage_payment = str_remove_all(mortgage_payment, "\\."),
    mortgage_payment = as.numeric(str_replace_all(mortgage_payment, ",", "."))
  )
#> Warning in mask$eval_all_mutate(quo): NAs introduced by coercion

# glimpse
glimpse(mortgage_p_tidy)
#> Rows: 35
#> Columns: 2
#> $ prov             <chr> "Aceh", "North Sumatra", "West Sumatra", "Riau", "Jam~
#> $ mortgage_payment <dbl> 853775.4, 2021410.3, 790714.6, 2356089.0, 1888042.4, ~
```

Then we read the average mortgage term data.

``` r
# import
mortgage_t_raw <- extract_tables(
  "data/bps_2020_report.pdf",
  pages = 61,
  output = "data.frame"
)

# tidy
## clean up
mortgage_t_raw_2 <- mortgage_t_raw %>% 
  pluck(1) %>% 
  slice(-c(1:3, nrow(.))) %>%
  select(1, ncol(.)) %>%
  rename(prov = 1, mortgage_term = 2)

## separate collapsed observations into multiple rows
mortgage_t_raw_sub <- mortgage_t_raw_2 %>% 
  dplyr::filter(str_detect(mortgage_term, " ")) %>%
  mutate(
    prov = str_replace_all(prov, "Belitung[ ]", "Belitung_"),
    prov = str_replace_all(prov, "Timur[ ]", "Timur_"),
    prov = str_replace_all(prov, "Barat[ ]", "Barat_"),
    mortgage_term = str_replace_all(mortgage_term, " ", "_")
  ) %>% 
  separate_rows(prov, mortgage_term, sep = "_")

## replace column names, correct data type
mortgage_t_tidy <- mortgage_t_raw_2 %>% 
  dplyr::filter(!str_detect(mortgage_term, " ")) %>%
  rbind(mortgage_t_raw_sub) %>% 
  mutate(
    prov = str_replace_all(prov, "Kep.", "Kepulauan"),
    prov = str_replace_all(
      str_to_upper(prov),
      c(
        "JAWA BARAT" = "West Java",
        "JAWA TENGAH" = "Central Java",
        "JAWA TIMUR" = "East Java",
        "DI " = "",
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
    mortgage_term = str_remove_all(mortgage_term, "\\."),
    mortgage_term = as.numeric(str_replace_all(mortgage_term, ",", "."))
  )
#> Warning in mask$eval_all_mutate(quo): NAs introduced by coercion

# glimpse
glimpse(mortgage_t_tidy)
#> Rows: 35
#> Columns: 2
#> $ prov          <chr> "Aceh", "North Sumatra", "West Sumatra", "Riau", "Jambi"~
#> $ mortgage_term <dbl> 14.12, 13.83, 13.64, 12.06, 12.82, 12.61, 12.60, 13.80, ~
```

## Result

### Home ownership

To see how home ownership has changed over time, we need to index the
data. Here, I chose 1999 as the base year. Without an index chart, it
will be difficult to gauge the downward trend in Indonesian home
ownership since changes in the data are small enough.

I decided to filter out provinces with incomplete observations or have
methodological differences.

We can highlight the top two and bottom two provinces in terms of change
in home ownership and mute other provinces to give more nuance to the
chart.

As a result, we can see the steepest decline was recorded in Jakarta,
followed by Bali. Jakarta and Denpasar, the capital of Bali, are very
unaffordable, as reflected in the ratio between house prices and median
income in those cities that was higher than that of New York in the
United States. Conversely, the highest increase was seen in Bengkulu,
followed by Jambi.

To help readers see the end-point of the index chart, we can add a bar
chart showing home ownership by province in 2020, our most recent
observation.

However, since the variations in the data are small, we can try to look
at the bottom 10 provinces instead. In the bar chart, we can see more
variations and how Jakarta, a province with the lowest home ownership,
compares with other provinces.

``` r
# index
ownership_index <- ownership_tidy %>%
  group_by(prov) %>% 
  mutate(index = (home_ownership / first(home_ownership)) * 100) %>% 
  ungroup()

# drop provinces with incomplete observations
ownership_count <- ownership_tidy %>% 
  count(prov, sort = T) %>% 
  dplyr::filter(n >= 22, prov != "Papua")

# annotations
anno_prov <- tribble(
  ~prov, ~x, ~y,
  "National", 2019, 91,
  "Bengkulu", 2019, 112,
  "Jambi", 2020, 102,
  "Bali", 2019, 82,
  "Jakarta", 2018, 70.5
)

anno_other_prov <- tibble(
  prov = "Other selected\nprovinces",
  x = 2013,
  y = 98
)

# plot
## index
plot_index <- ownership_index %>% 
  dplyr::filter(prov %in% ownership_count$prov) %>% 
  ggplot(aes(year, index)) +
  geom_hline(yintercept = 100, color = "#E68F7E") +
  geom_line(aes(color = prov), lwd = 0.75, show.legend = F) +
  geom_point(
    aes(1999, 100),
    pch = 21, 
    color = "white",
    fill = "black",
    size = 1.5
  ) +
  scale_x_continuous(
    breaks = seq(1999, 2020, 3),
    labels = c("1999", "2002", "'05", "'08", str_c("'", seq(11, 20, 3)))
  ) +
  scale_y_continuous(
    breaks = seq(40, 120, 20),
    limits = c(40, 120),
    expand = c(0, 0),
    position = "right"
  ) +
  gghighlight(
    prov %in% c("Indonesia", "Jakarta", "Bali", "Bengkulu", "Jambi"),
    label_key = F,
    unhighlighted_params = list(color = "grey", alpha = 0.25)
  ) +
  scale_color_manual(
    values = c(
      "Indonesia" = "#E65639",
      "Jakarta" = "#4CCDD9",
      "Bali" = "#3DB4CC",
      "Jambi" = "#309BBF",
      "Bengkulu" = "#127DB3"
    )
  ) +
  geom_text(
    data = anno_prov,
    aes(x, y, label = prov),
    color = c("#E65639", "#127DB3", "#309BBF", "#3DB4CC", "#4CCDD9"),
    size = 2.25,
    hjust = 1,
    fontface = "bold"
  ) +
  geom_text_repel(
    data = anno_other_prov,
    aes(x, y, label = prov),
    size = 2.25,
    color = "#757575",
    hjust = 1,
    nudge_x = -2,
    nudge_y = 10,
    segment.curvature = -0.25,
    segment.ncp = 3
  ) +
  labs(subtitle = "1999 = 100", x = NULL, y = NULL) +
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
    plot.subtitle = element_text(size = rel(0.8), margin = margin(b = 5)),
    plot.margin = margin(r = 12.5)
  )
#> Warning: Tried to calculate with group_by(), but the calculation failed.
#> Falling back to ungrouped filter operation...
#> Warning: Could not calculate the predicate for layer 1; ignored

## lowest home ownership
plot_bottom10 <- ownership_tidy %>%
  dplyr::filter(year == 2020) %>% 
  mutate(prov = fct_reorder(prov, home_ownership)) %>% 
  arrange(home_ownership) %>% 
  head(10) %>% 
  ggplot(aes(home_ownership, prov)) +
  geom_col(fill = "#127DB3", color = "white", width = .6) +
  scale_x_continuous(
    breaks = seq(0, 80, 20),
    limits = c(0, 80),
    expand = c(0, 0),
    position = "top"
  ) +
  labs(subtitle = "Bottom 10 provinces in 2020 (percent)", x = NULL, y = NULL) +
  theme(
    text = element_text(size = 12),
    axis.title = element_blank(),
    axis.text = element_text(size = rel(0.7)),
    axis.text.y = element_text(hjust = 0),
    axis.ticks = element_blank(),
    axis.line.y = element_line(color = "black"),
    panel.background = element_rect(fill = "white"),
    panel.grid.major.x = element_line(color = "#CFD8DC"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    plot.subtitle = element_text(size = rel(0.8), margin = margin(b = 5)),
    plot.title.position = "plot",
    plot.margin = margin(l = 12.5, r = 5)
  )

# patchwork
plot_index + plot_bottom10 +
  plot_annotation(
    title = "Home ownership has fallen since 1999",
    subtitle = "Share of households with a home of their own, by province",
    caption = "Chart: @dzulfiqarfr | Source: Statistics Indonesia (BPS)",
    theme = theme(
      text = element_text(size = 12),
      plot.title = element_text(face = "bold", size = rel(1.1)),
      plot.subtitle = element_text(size = rel(0.9), margin = margin(b = 25)),
      plot.caption = element_text(
        size = rel(0.7),
        color = "#757575",
        hjust = 0,
        margin = margin(t = 25)
      )
    )
  ) +
  plot_layout(widths = c(2, 1))
```

<img src="README_files/figure-gfm/home ownership plot-1.png" width="70%" style="display: block; margin: auto;" />

### House price index

While BI has broken down the house price index by house type, we will
only look at the overall house price index. We also subset the annual
percentage changes to find out how house prices have moved from year to
year.

Here, we can see that the index has plateaued, recording a slower growth
after it peaked in 2013. In Bandar Lampung, the index even showed annual
declines.

``` r
# subset total, annual growth
shpr_sub <- shpr_tidy %>%
  dplyr::filter(house_type == "Total", change_term == "yoy")

# cities to highlight
highlight_cities <- c(
  "Overall", 
  "Manado", 
  "Jabodebek-Banten", 
  "Bandar Lampung"
)

# annotations
anno_cities <- shpr_sub %>% 
  dplyr::filter(cities %in% highlight_cities, date == last(date)) %>% 
  mutate(
    shpr_growth = case_when(
      cities == "Overall" ~ 1.75,
      cities == "Jabodebek-Banten" ~ 0.25,
      TRUE ~ shpr_growth
    ),
    cities = case_when(
      cities == "Jabodebek-Banten" ~ "Greater Jakarta* & Banten",
      cities == "Overall" ~ "National",
      TRUE ~ cities
    )
  )

anno_covid <- tribble(
  ~label, ~x, ~y,
  "COVID-19\npandemic\n\u2192", ymd("2020-02-01"), 18
)

# plot
ggplot(shpr_sub, aes(date, shpr_growth)) +
  geom_hline(yintercept = 0, color = "#E68F7E") +
  geom_vline(
    xintercept = ymd("2020-03-01"),
    color = "#90A4AE", 
    lty = "dashed"
  ) +
  geom_line(aes(color = cities), lwd = 0.75, show.legend = F) +
  gghighlight(
    cities %in% highlight_cities,
    label_key = F,
    unhighlighted_params = list(color = "grey", alpha = 0.25)
  ) +
  scale_x_date(
    date_breaks = "1 year",
    date_labels = "'%y",
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    breaks = seq(-12, 24, 6),
    limits = c(-12, 24),
    expand = c(0, 0),
    sec.axis = dup_axis(
      breaks = anno_cities$shpr_growth,
      labels = anno_cities$cities,
      name = NULL,
      guide = guide_axis()
    )
  ) +
  scale_color_manual(
    values = c(
      "Overall" = "#E65639",
      "Bandar Lampung" = "#4CCDD9",
      "Jabodebek-Banten" = "#3DB4CC",
      "Manado" = "#127DB3"
    )
  ) +
  labs(
    title = "House prices have grown slower",
    subtitle = "Residential property price index, by city (annual percentage change)",
    x = NULL,
    y = NULL,
    caption = "*Excluding Tangerang\n\nChart: @dzulfiqarfr | Source: Bank Indonesia (BI)"
  ) +
  annotate(
    "text",
    x = ymd("2018-07-01"),
    y = 8,
    label = "Other selected cities",
    color = "#757575",
    size = 2.5
  ) +
  geom_label(
    data = anno_covid,
    aes(x, y, label = label),
    size = 2.25,
    hjust = 1,
    color = "#90A4AE",
     fill = "white",
    label.size = 0,
    label.padding = unit(0.025, "lines")
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
    plot.title = element_text(size = rel(1.1), face = "bold"),
    plot.subtitle = element_text(size = rel(0.9), margin = margin(b = 25)),
    plot.title.position = "plot",
    plot.caption = element_text(
      size = rel(0.7),
      color = "#757575",
      hjust = 0,
      margin = margin(t = 25)
    ),
    plot.caption.position = "plot"
  )
#> Warning: Tried to calculate with group_by(), but the calculation failed.
#> Falling back to ungrouped filter operation...
#> Warning: Could not calculate the predicate for layer 1, layer 2; ignored
#> Warning: Removed 72 row(s) containing missing values (geom_path).
```

<img src="README_files/figure-gfm/house price index plot-1.png" width="70%" style="display: block; margin: auto;" />

### Mortgage

The amount of monthly mortgage payments one has to make largely depends
on the term one chooses. So we can expect there is a linear relationship
between the average term and the average monthly payments. Monthly
payments tend to be smaller for mortgages with a longer term. That said,
we can look at the data through a scatter plot by mapping the average
term on the x-axis and average monthly payments on the y-axis.

Here, we can see the linear fit does quite a good job at showing the
negative relationship between mortgage term and payments. Jakarta,
Yogyakarta and East Nusa Tenggara are clearly outliers.

I think it is safe to say that the average monthly payments in Jakarta
and Yogyakarta are pretty expensive relative to other provinces, given
the average term.

``` r
# join mortgage payment, term data
mortgage_pl <- mortgage_p_tidy %>% 
  left_join(mortgage_t_tidy, by = "prov") %>% 
  dplyr::filter(!is.na(mortgage_payment), prov != "Indonesia")

# annotations
## provinces to highlight
highlight_prov <- c(
  "Jakarta", 
  "Yogyakarta", 
  "Bali",
  "East Nusa Tenggara",
  "Bangka Belitung Islands",
  "Lampung"
)

anno_prov_mortgage <- mortgage_pl %>% 
  dplyr::filter(prov %in% highlight_prov) %>%
  mutate(
    prov = case_when(
      prov == "West Sulawesi" ~ "West\nSulawesi",
      TRUE ~ prov
    ),
    mortgage_payment = mortgage_payment / 1000000
  )

# plot  
mortgage_pl %>% 
  mutate(mortgage_payment = mortgage_payment / 1000000) %>% 
  ggplot(aes(mortgage_term, mortgage_payment)) +
  geom_point(
    pch = 21,
    fill = "#1D8EBF",
    color = "white",
    alpha = 0.75,
    size = 3.5
  ) +
  geom_smooth(
    method = "lm",
    color = "black",
    lwd = 0.5,
    lty = "dashed",
    se = F) +
  scale_x_continuous(
    breaks = seq(8, 20, 2),
    limits = c(8, 20)
  ) +
  scale_y_continuous(
    breaks = seq(0, 6, 1),
    limits = c(0, 6.5),
    expand = c(0, 0),
    position = "right"
  ) +
  labs(
    title = "Monthly payments in Jakarta, Yogyakarta are unusually expensive",
    subtitle = "Average monthly mortgage payments and term in 2019, by province*",
    y = "Monthly payment\n(million rupiah)",
    x = "Term\n(years)",
    caption = "*Excluding West Nusa Tenggara, Maluku and North Maluku\n\nChart: @dzulfiqarfr | Source: Statistics Indonesia (BPS)"
  ) +
  geom_label(
    data = anno_prov_mortgage,
    aes(x = mortgage_term, y = mortgage_payment, label = prov), 
    hjust = 1, 
    size = 2.25,
    fill = "white",
    label.size = 0,
    label.padding = unit(0.025, "lines"),
    nudge_x = -0.2,
    nudge_y = 0.15
  ) +
  annotate(
    "text",
    y = 0.45,
    x = 15.15,
    label = "West\nSulawesi",
    size = 2.25,
    hjust = 1
  ) +
  theme(
    text = element_text(size = 12),
    axis.title = element_text(size = rel(0.75)),
    axis.text = element_text(size = rel(0.75)),
    axis.line.x = element_line(color = "black"),
    axis.ticks.y = element_blank(),
    panel.background = element_rect(fill = "white"),
    panel.grid.major.x  = element_line(color = "#CFD8DC"),
    panel.grid.major.y = element_line(color = "#CFD8DC"),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    plot.title = element_text(size = rel(1.1), face = "bold"),
    plot.subtitle = element_text(size = rel(0.9), margin = margin(b = 25)),
    plot.title.position = "plot",
    plot.caption = element_text(
      size = rel(0.7),
      color = "#757575",
      hjust = 0,
      margin = margin(t = 25)
    ),
    plot.caption.position = "plot"
  )
#> Warning: Removed 18 rows containing missing values (geom_smooth).
```

<img src="README_files/figure-gfm/mortgage plot-1.png" width="70%" style="display: block; margin: auto;" />
