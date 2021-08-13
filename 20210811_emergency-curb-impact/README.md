Emergency curbs reverse recovery in consumer confidence and retail sales
================

I prepared this analysis for a
<a href="https://www.thejakartapost.com/news/2021/08/12/emergency-curbs-reverse-recovery-in-consumer-confidence-retail-sales.html" target="_blank">story</a>
on the impact of COVID-19 emergency curbs on the economy. We will take a
look at the consumer confidence index and retail sales index. We will
also analyze community mobility data to see how people’s visits to
retail and recreation places change before, during and after the
government tightens movement restrictions.

## Packages

``` r
library(tidyverse)
library(data.table)
library(readxl)
library(lubridate)
library(zoo)
library(httr)
library(jsonlite)
library(ggrepel)
library(ggtext)
library(patchwork)
```

## Consumer confidence

We can get the consumer confidence index data from
<a href="https://www.bi.go.id/id/publikasi/laporan/Pages/SK-Juli-2021.aspx" target="_blank">Bank Indonesia (BI)</a>.

``` r
# unzip("data/SK.zip", exdir = "data")

cci_overall_raw <- read_excel(
  "data/SK.xlsx",
  sheet = "Tabel 1",
  skip = 5,
  na = c("-", "")
)

cci_overall_clean <- cci_overall_raw %>% 
  select(-c(KETERANGAN:...3)) %>%
  dplyr::filter(...4 == "Indeks Keyakinan Konsumen (IKK)") %>% 
  select_if(function(x) {!all(is.na(x))}) %>% 
  select(-c(...4, ncol(.)))

cci_date <- seq(ymd("2012-01-01"), ymd("2021-07-01"), by = "1 month")

names(cci_overall_clean)[1:ncol(cci_overall_clean)] <- as.character(cci_date)

cci_overall_tidy <- cci_overall_clean %>% 
  pivot_longer(
    everything(),
    names_to = "date",
    values_to = "cci",
    names_transform = list(date = ymd),
    values_transform = list(cci = as.double)
  ) %>% 
  mutate(cci = round(cci, 2))

glimpse(cci_overall_tidy)
#> Rows: 115
#> Columns: 2
#> $ date <date> 2012-01-01, 2012-02-01, 2012-03-01, 2012-04-01, 2012-05-01, 2012~
#> $ cci  <dbl> 119.20, 111.70, 107.30, 102.50, 109.00, 114.40, 113.50, 115.70, 1~
```

We have read the overall consumer confidence index data, which stretch
as far back as 2012. We can now check the July figure to see how the
emergency curbs, which the government implemented starting from July 3,
affected consumer confidence.

``` r
tail(cci_overall_tidy, 6)
#> # A tibble: 6 x 2
#>   date         cci
#>   <date>     <dbl>
#> 1 2021-02-01  85.8
#> 2 2021-03-01  93.4
#> 3 2021-04-01 101. 
#> 4 2021-05-01 104. 
#> 5 2021-06-01 107. 
#> 6 2021-07-01  80.2
```

The consumer confidence index fell to 80.20 in July. This is below the
100-point threshold separating optimistic and pessimistic territory.

BI also provides consumer confidence broken down by income group.
Although they usually move together, it’s not always clear cut which
income group is the least or most optimistic.

So we will also import the consumer confidence by income group data.

``` r
cci_income_raw <- read_excel(
  "data/SK.xlsx",
  sheet = "Tabel 2",
  skip = 7,
  na = c("-", "")
)

income_group_name_en <- c(
  "Pengeluaran Rp1 - 2 juta" = "Rp 1-2 million",
  "Pengeluaran Rp2,1 - 3 juta" = "Rp 2.1-3 million",
  "Pengeluaran Rp3,1 - 4 juta" = "Rp 3.1-4 million",
  "Pengeluaran Rp4,1 - 5 juta" = "Rp 4.1-5 million",
  "Pengeluaran >Rp5 juta" = "Above Rp 5 million"
)

cci_income_clean <- cci_income_raw %>%
  fill(...3) %>% 
  dplyr::filter(...3 == "Indeks Keyakinan Konsumen (IKK)", !is.na(...4)) %>% 
  select(-c(KETERANGAN:...3, ncol(.))) %>% 
  select_if(function(x) {!all(is.na(x))}) %>%
  rename(income_group = ...4) %>% 
  mutate(
    income_group_id = c("1_2", "2.1_3", "3.1_4", "4.1_5", "above_5"),
    income_group = str_replace_all(income_group, income_group_name_en)
  ) %>% 
  select(income_group_id, income_group, `2012`:ncol(.))

names(cci_income_clean)[3:ncol(cci_income_clean)] <- as.character(cci_date)

cci_income_tidy <- cci_income_clean %>% 
  pivot_longer(
    3:ncol(.),
    names_to = "date",
    names_transform = list(date = ymd),
    values_to = "cci",
    values_transform = list(cci = as.double)
  ) %>% 
  mutate(cci = round(cci, 2))

glimpse(cci_income_tidy)
#> Rows: 575
#> Columns: 4
#> $ income_group_id <chr> "1_2", "1_2", "1_2", "1_2", "1_2", "1_2", "1_2", "1_2"~
#> $ income_group    <chr> "Rp 1-2 million", "Rp 1-2 million", "Rp 1-2 million", ~
#> $ date            <date> 2012-01-01, 2012-02-01, 2012-03-01, 2012-04-01, 2012-~
#> $ cci             <dbl> 122.29, 116.25, 112.80, 109.84, 112.70, 120.29, 122.55~
```

Now we can check confidence among consumers depending on their income
group.

``` r
cci_income_tidy %>% 
  dplyr::filter(date == last(date)) %>% 
  arrange(cci)
#> # A tibble: 5 x 4
#>   income_group_id income_group       date         cci
#>   <chr>           <chr>              <date>     <dbl>
#> 1 1_2             Rp 1-2 million     2021-07-01  74.8
#> 2 2.1_3           Rp 2.1-3 million   2021-07-01  80.6
#> 3 4.1_5           Rp 4.1-5 million   2021-07-01  80.7
#> 4 3.1_4           Rp 3.1-4 million   2021-07-01  81.5
#> 5 above_5         Above Rp 5 million 2021-07-01  83.8
```

Confidence across all income groups fell into the pessimistic territory.
But consumers with income between Rp 1 million and Rp 2 million posted
the lowest confidence at 74.80.

We will create a line chart to show how the latest restriction affected
consumer confidence. But we will first define a function to create a
custom ggplot2 theme.

``` r
color_primary <- "#757575"
color_primary_light <- "#E0E0E0"
color_annotation <- "#9E9E9E"

theme_dfr <- function(..., 
                      base_size = 12, 
                      rel_size = 0.75,
                      color_primary = "#757575",
                      color_primary_light = "#E0E0E0") {
  
  
  
  theme(
    text = element_text(size = base_size, color = color_primary),
    axis.text = element_text(size = rel(rel_size), color = color_primary),
    axis.line = element_blank(),
    axis.ticks.x = element_line(color = color_primary_light),
    axis.ticks.y = element_blank(),
    panel.background = element_rect(fill = "#FFFFFF"),
    panel.grid.major.x  = element_blank(),
    panel.grid.major.y = element_line(color = color_primary_light),
    panel.grid.minor = element_blank(),
    plot.title = element_text(
      face = "bold", 
      color = "#000000", 
      size = rel(1),
      margin = margin(b = 5)
    ),
    plot.subtitle = element_text(
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

Since we are not interested in the entire historical trend in the data,
we will plot only figures in the past five years and thus, drop
observations prior to 2017.

``` r
cci_overall_filtered <- cci_overall_tidy %>% 
  mutate(year = year(date)) %>% 
  dplyr::filter(year >= 2017) %>% 
  select(-year)

annotation_text_confidence <- tribble(
  ~x, ~y, ~label,
  ymd("2017-06-01"), 110, "Optimistic \u2191",
  ymd("2017-06-01"), 90, "Pessimistic \u2193"
)

chart_cci <- ggplot(cci_overall_filtered, aes(date, cci)) +
  annotate(
    "rect",
    xmin = ymd("2020-04-01"),
    xmax = ymd("2020-05-01"),
    ymin = 40, 
    ymax = 140,
    fill = color_annotation,
    alpha = 0.25
  ) +
  annotate(
    "rect",
    xmin = ymd("2021-07-01"),
    xmax = ymd("2021-08-01"),
    ymin = 40, 
    ymax = 140,
    fill = color_annotation,
    alpha = 0.25
  ) +
  geom_hline(yintercept = 100, color = "#E68F7E") +
  geom_vline(
    xintercept = ymd("2020-03-01"), 
    color = color_annotation, 
    lty = "dashed"
  ) +
  geom_line(color = "#2477B3", lwd = 0.75) +
  scale_x_date(
    breaks = seq(ymd("2017-01-01"), ymd("2021-01-01"), by = "1 year"),
    labels = c("2017", str_c("'", seq(18, 21)))
  ) +
  scale_y_continuous(
    breaks = seq(40, 140, 20),
    limits = c(40, 140),
    position = "right"
  ) +
  geom_text(
    data = annotation_text_confidence,
    aes(x, y, label = label),
    size = 3,
    hjust = 0,
    vjust = 0.5,
    color = color_annotation
  ) +
  geom_text(
    data = tibble(x = ymd("2020-03-01"), y = 135, label = "COVID-19 pandemic"),
    aes(x, y, label = label),
    size = 3,
    hjust = 1,
    vjust = 0.5,
    nudge_x = -25,
    color = color_annotation
  ) +
  geom_text_repel(
    data = tibble(
      x = ymd("2020-05-01"), 
      y = 70, 
      label = "Large-scale\nsocial restrictions"
    ),
    aes(x, y, label = label),
    size = 3,
    hjust = 1,
    vjust = 0.5,
    nudge_x = -175,
    color = color_annotation
  ) +
  geom_text_repel(
    data = tibble(
      x = ymd("2021-08-01"), 
      y = 130, 
      label = "Emergency\ncurbs"
    ),
    aes(x, y, label = label),
    size = 2.75,
    hjust = 1,
    vjust = 0.5,
    nudge_x = -75,
    color = color_annotation
  ) +
  labs(
    subtitle = "Consumer confidence index",
    x = NULL,
    y = NULL
  ) +
  theme_dfr()

chart_cci
```

<img src="README_files/figure-gfm/Plot overall consumer confidence-1.png" width="70%" style="display: block; margin: auto;" />

The chart helps us see clearly that consumer confidence fell drastically
when regional administrations imposed the large-scale social
restrictions during the first few months of the pandemic last year and
when the government tightened mobility restrictions in July. More
importantly, the emergency curbs halted in July the recovery progress
for the previous three months.

## Retail sales

We can also download the retail sales index data from
<a href="https://www.bi.go.id/id/publikasi/laporan/Pages/SPE_Juni_2021.aspx" target="_blank">BI</a>.

``` r
# unzip("data/spe.zip", exdir = "data")

rsi_complete_raw <- read_excel(
  "data/spe.xlsx", 
  sheet = "Tabel 1",
  skip = 3
) 

rsi_complete_clean <- rsi_complete_raw %>% 
  dplyr::filter(!is.na(`2012`), !is.na(DESKRIPSI)) %>% 
  select_if(function(x) {!all(is.na(x))}) %>% 
  select(-DESCRIPTION) %>% 
  rename(category = DESKRIPSI)

# follow the original order for easier `mutate()`-ing
rsi_category_name_en <- rsi_complete_clean %>% 
  select(category) %>% 
  mutate(
    category_en = c(
      "Vehicle spare parts and accessories",
      "Food, beverages and tobacco",
      "Vehicle fuels",
      "Information and communication tools",
      "Other household supplies",
      "Cultural and recreational goods",
      "Other goods",
      "Clothing",
      "Total"
    )
  )

rsi_date <- seq(ymd("2012-01-01"), ymd("2021-07-01"), by = "1 month")

names(rsi_complete_clean)[2:ncol(rsi_complete_clean)] <- as.character(rsi_date)

rsi_complete_tidy <- rsi_complete_clean %>% 
  pivot_longer(
    2:ncol(.),
    names_to = "date",
    names_transform = list(date = ymd),
    values_to = "rsi",
    values_transform = list(rsi = as.double)
  ) %>% 
  mutate(
    category = str_replace_all(category, deframe(rsi_category_name_en)),
    rsi = round(rsi, 2)
  )

glimpse(rsi_complete_tidy)
#> Rows: 1,035
#> Columns: 3
#> $ category <chr> "Vehicle spare parts and accessories", "Vehicle spare parts a~
#> $ date     <date> 2012-01-01, 2012-02-01, 2012-03-01, 2012-04-01, 2012-05-01, ~
#> $ rsi      <dbl> 95.46, 86.88, 93.03, 86.91, 93.11, 98.54, 104.06, 95.66, 97.1~
```

Having imported the data, we can now calculate the annual percentage
change in the retail sales index.

``` r
rsi_complete_chg <- rsi_complete_tidy %>% 
  mutate(month = month(date)) %>% 
  group_by(category, month) %>% 
  mutate(
    diff_yoy = rsi - dplyr::lag(rsi, 1),
    pct_chg_yoy = round(diff_yoy / dplyr::lag(rsi, 1) * 100, digits = 2)
  ) %>% 
  ungroup() %>% 
  select(-c(month, diff_yoy))

glimpse(rsi_complete_chg)
#> Rows: 1,035
#> Columns: 4
#> $ category    <chr> "Vehicle spare parts and accessories", "Vehicle spare part~
#> $ date        <date> 2012-01-01, 2012-02-01, 2012-03-01, 2012-04-01, 2012-05-0~
#> $ rsi         <dbl> 95.46, 86.88, 93.03, 86.91, 93.11, 98.54, 104.06, 95.66, 9~
#> $ pct_chg_yoy <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 8.77, 14.8~
```

We can now check the latest annual percentage change in retail sales
index across categories.

``` r
rsi_complete_chg %>% 
  dplyr::filter(date == last(date)) %>% 
  arrange(desc(pct_chg_yoy))
#> # A tibble: 9 x 4
#>   category                            date         rsi pct_chg_yoy
#>   <chr>                               <date>     <dbl>       <dbl>
#> 1 Vehicle fuels                       2021-07-01  66.7        9.11
#> 2 Food, beverages and tobacco         2021-07-01 232         -0.67
#> 3 Total                               2021-07-01 182.        -6.23
#> 4 Other household supplies            2021-07-01 126.        -7.1 
#> 5 Vehicle spare parts and accessories 2021-07-01  93.4      -11.5 
#> 6 Other goods                         2021-07-01  68.4      -12.0 
#> 7 Clothing                            2021-07-01  51.6      -17.0 
#> 8 Cultural and recreational goods     2021-07-01  56.5      -22.3 
#> 9 Information and communication tools 2021-07-01 177.       -33.8
```

Aside from vehicle fuels, the retail sales posted sharp declines in July
from a year earlier. The overall retail sales index was estimated to
fall by 6.23 percent year-on-year (yoy). The steepest fall was recorded
in information and communication tools.

We can create a line chart to show the latest trend and see how it
compares with the one during large-scale social restrictions last year.
Like with the consumer confidence chart, we will filter out observations
prior to 2017. We will also plot only the overall retail sales index.

``` r
rsi_total_filtered <- rsi_complete_chg %>% 
  mutate(year = year(date)) %>% 
  dplyr::filter(year >= 2017, category == "Total") %>% 
  select(-c(category, year))

chart_rsi <- ggplot(rsi_total_filtered, aes(date, pct_chg_yoy)) +
  annotate(
    "rect",
    xmin = ymd("2020-04-01"),
    xmax = ymd("2020-05-01"),
    ymin = -45, 
    ymax = 30,
    fill = color_annotation,
    alpha = 0.25
  ) +
  annotate(
    "rect",
    xmin = ymd("2021-07-01"),
    xmax = ymd("2021-08-01"),
    ymin = -45, 
    ymax = 30,
    fill = color_annotation,
    alpha = 0.25
  ) +
  geom_hline(yintercept = 0, color = "#E68F7E") +
  geom_vline(
    xintercept = ymd("2020-03-01"), 
    color = color_annotation, 
    lty = "dashed"
  ) +
  geom_line(color = "#2477B3", lwd = 0.75) +
  scale_x_date(
    breaks = seq(ymd("2017-01-01"), ymd("2021-01-01"), by = "1 year"),
    labels = c("2017", str_c("'", seq(18, 21)))
  ) +
  scale_y_continuous(
    breaks = seq(-45, 30, 15),
    limits = c(-45, 30),
    position = "right"
  ) +
  labs(
    subtitle = "Retail sales index* (annual percentage change)",
    x = NULL,
    y = NULL
  ) +
  theme_dfr()

chart_rsi
```

<img src="README_files/figure-gfm/Plot retail sales-1.png" width="70%" style="display: block; margin: auto;" />

We can see from the chart that the fall in retail sales was not as steep
as it was during the large-scale social restrictions. However, the
recent tightening of movement restrictions also reversed the recovery
trend.

We can join the consumer confidence and retail sales charts to make a
single chart.

``` r
chart_cci + chart_rsi +
  plot_annotation(
    title = "Emergency curbs reverse recovery in consumer confidence, retail sales",
    caption = str_c(
      "\\*Last figure (July 2021) is an estimate<br>",
      "Source: Bank Indonesia (BI); *The Jakarta Post* analysis<br>",
      "Chart: JP/Dzulfiqar Fathur Rahman"
    ),
    theme = theme_dfr()
  )
```

<img src="README_files/figure-gfm/Patchwork consumer confidence and retail sales charts-1.png" width="70%" style="display: block; margin: auto;" />

## Mobility to retail and recreation places

The charts on consumer confidence and retail sales suggest both metrics
tend to go down when the government tightens mobility restrictions. One
way to test this assumption is to check it with community mobility data
from
<a href="https://www.google.com/covid19/mobility/" target="_blank">Google</a>.

``` r
path_mobility_data <- "data/mobility_indonesia_raw.csv"

if (!file.exists(path_mobility_data)) {
  
  url_mobility <- "https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv"
  
  mobility_global_raw <- fread(
    url_mobility,
    select = c(
      "country_region",
      "sub_region_1",
      "date", 
      "retail_and_recreation_percent_change_from_baseline"
    )
  )
  
  mobility_idn_raw <- mobility_global_raw %>% 
    dplyr::filter(country_region == "Indonesia")
  
  write_csv(mobility_idn_raw, "data/mobility_indonesia_raw.csv")
  
} else {
  
  mobility_idn_raw <- fread(path_mobility_data)
  
}

glimpse(mobility_idn_raw)
#> Rows: 18,935
#> Columns: 4
#> $ country_region                                     <chr> "Indonesia", "Indon~
#> $ sub_region_1                                       <chr> "", "", "", "", "",~
#> $ date                                               <date> 2020-02-15, 2020-0~
#> $ retail_and_recreation_percent_change_from_baseline <int> -2, -3, -3, -3, -3,~
```

After reading the mobility data, we need to smooth it to seven-day
moving averages to separate the movement trend from day-to-day
fluctuations.

``` r
# Rename provinces to ensure consistency with other data to enable joins
mobility_idn_avg <- mobility_idn_raw %>% 
  mutate(
    across(everything(), function(x) {na_if(x, "")}),
    sub_region_1 = if_else(is.na(sub_region_1), "National", sub_region_1),
    sub_region_1 = str_remove_all(sub_region_1, "Special Region of "),
    sub_region_1 = str_replace_all(
      sub_region_1, 
      "^South East Sulawesi$", 
      "Southeast Sulawesi"
    )
  ) %>% 
  group_by(sub_region_1) %>% 
  mutate(
    seven_day_average = rollmean(
      retail_and_recreation_percent_change_from_baseline,
      k = 7, 
      fill = NA, 
      align = "right"
    )
  ) %>% 
  ungroup()

glimpse(mobility_idn_avg)
#> Rows: 18,935
#> Columns: 5
#> $ country_region                                     <chr> "Indonesia", "Indon~
#> $ sub_region_1                                       <chr> "National", "Nation~
#> $ date                                               <date> 2020-02-15, 2020-0~
#> $ retail_and_recreation_percent_change_from_baseline <int> -2, -3, -3, -3, -3,~
#> $ seven_day_average                                  <dbl> NA, NA, NA, NA, NA,~
```

We can plot the smoothed mobility at national level to gauge the
movement trend nationwide.

``` r
mobility_ntl_avg <- mobility_idn_avg %>% 
  dplyr::filter(sub_region_1 == "National")

ggplot(mobility_ntl_avg, aes(date, seven_day_average)) +
  geom_line(show.legend = FALSE) +
  scale_x_date(
    breaks = seq(ymd("2020-03-01"), ymd("2021-08-01"), by = "1 month"),
    labels = c(
      "Mar\n2020",
      format(seq(ymd("2020-04-01"), ymd("2020-12-01"), by = "1 month"), "%b"),
      "Jan\n'21",
      format(seq(ymd("2021-02-01"), ymd("2021-08-01"), by = "1 month"), "%b")
    )
  ) +
  labs(x = NULL, y = NULL) +
  theme_dfr()
#> Warning: Removed 6 row(s) containing missing values (geom_path).
```

<img src="README_files/figure-gfm/Plot mobility to retail and recreation places-1.png" width="70%" style="display: block; margin: auto;" />

The mobility data suggest that people reduced their visits to retail and
recreation places even before the government started tightening the
restrictions as the pandemic began to worsen. In July, the surge in
cases and deaths led to a reverse in mobility from a brief moment of a
return to the pre-pandemic level between April and June. But the decline
was not as steep as during the large-scale social restrictions last
year.

Now that the government has gradually relaxed the restrictions and cases
started declining in some places, more people started going to shopping
centers and other recreation places again.

But the trend is somewhat different from one province to another as
cases and deaths remained high in some places. We can see this trend if
we compare mobility by region.

To simplify the analysis, we will group provinces into Java and Bali,
and other. We will then take the average mobility for each region. For
better comparison, we need to weight the average mobility by population.

We can get population data from
<a href="https://bps.go.id/indicator/12/1886/1/jumlah-penduduk-hasil-proyeksi-menurut-provinsi-dan-jenis-kelamin.html" target="_blank">Statistics Indonesia (BPS)</a>.
The agency provides population estimates for the country broken down by
sex and province from the 2015 Intercensal Survey (SUPAS). We can get
this data through the agency’s application programming interface (API).

``` r
api_key_bps <- Sys.getenv("api_key_bps")

# Store var id for querying population data and separating `id` column later
var_id <- "1886"

population_req <- GET(
  "https://webapi.bps.go.id/v1/api/list",
  query = list(
    model = "data",
    domain = "0000",
    var = var_id,
    key = api_key_bps
  )
)

population_parsed <- content(population_req, type = "text") %>% 
  fromJSON()

# Add anchor to id values to prevent errors when replacing them with the labels
add_anchor <- function(x, list) {
  
  df <- as_tibble(list[[x]])
  
  df_anchored <- df %>% 
    mutate(val = str_c("^", as.character(val), "$"))
  
}

# `turvar` contains sex values, `vervar` province values, `tahun` year
id_names <- c("turvar", "vervar", "tahun")

id_list <- map(id_names, add_anchor, list = population_parsed)

id_sex <- id_list[[1]]

id_province <- id_list[[2]]

id_year <- id_list[[3]]

# Follow the order in the `id_province` for easier `mutate()`-ing
province_name_en <- c(
  "Indonesia",
  "Aceh",
  "North Sumatra",
  "West Sumatra",
  "Riau",
  "Jambi",
  "South Sumatra",
  "Bengkulu",
  "Lampung",
  "Bangka Belitung Islands",
  "Riau Islands",
  "Jakarta",
  "West Java",
  "Central Java",
  "Yogyakarta",
  "East Java",
  "Banten",
  "Bali",
  "West Nusa Tenggara",
  "East Nusa Tenggara",
  "West Kalimantan",
  "Central Kalimantan",
  "South Kalimantan",
  "East Kalimantan",
  "North Kalimantan",
  "North Sulawesi",
  "Central Sulawesi",
  "South Sulawesi",
  "Southeast Sulawesi",
  "Gorontalo",
  "West Sulawesi",
  "Maluku",
  "North Maluku",
  "West Papua",
  "Papua"
)

id_province_en <- id_province %>% 
  mutate(
    province_en = province_name_en,
    island_group_1 = case_when(
      province_en == "Indonesia" ~ "Indonesia",
      str_detect(val, "\\^[1|2]") ~ "Sumatra",
      str_detect(val, "\\^3") ~ "Java",
      str_detect(val, "\\^5") ~ "Bali & Nusa Tenggara",
      str_detect(val, "\\^6") ~ "Kalimantan",
      str_detect(val, "\\^7") ~ "Sulawesi",
      str_detect(val, "\\^[8|9]") ~ "Maluku & Papua"
    ),
    island_group_2 = case_when(
      province_en == "Indonesia" ~ "Indonesia",
      str_detect(val, "\\^[3|5]") ~ "Java & Bali",
      TRUE ~ "Other"
    )
  )

id_sex_en <- id_sex %>% 
  mutate(sex_en = c("Men", "Women", "Total")) %>% 
  select(-label)

population_raw <- as_tibble(population_parsed$datacontent)

population_tidy <- population_raw %>% 
  pivot_longer(
    everything(),
    names_to = "id",
    values_to = "population"
  ) %>% 
  separate(
    id,
    into = c("province_id", "sex_year_id"),
    sep = var_id
  ) %>% 
  mutate(
    province = str_replace_all(
      province_id, 
      deframe(id_province_en[, c(1, 3)])
    ),
    island_group_1 = str_replace_all(
      province_id, 
      deframe(id_province_en[, c(1, 4)])
    ),
    island_group_2 = str_replace_all(
      province_id, 
      deframe(id_province_en[, c(1, 5)])
    ),
    sex = str_sub(sex_year_id, 1, 3),
    sex = str_replace_all(sex, deframe(id_sex_en)),
    year = str_sub(sex_year_id, 4, 6),
    year = str_replace_all(year, deframe(id_year))
  ) %>% 
  select(
    province_id,
    province, 
    island_group_1,
    island_group_2, 
    sex, 
    year, 
    population
  )

glimpse(population_tidy)
#> Rows: 630
#> Columns: 7
#> $ province_id    <chr> "9999", "9999", "9999", "9999", "9999", "9999", "9999",~
#> $ province       <chr> "Indonesia", "Indonesia", "Indonesia", "Indonesia", "In~
#> $ island_group_1 <chr> "Indonesia", "Indonesia", "Indonesia", "Indonesia", "In~
#> $ island_group_2 <chr> "Indonesia", "Indonesia", "Indonesia", "Indonesia", "In~
#> $ sex            <chr> "Men", "Men", "Men", "Men", "Men", "Men", "Women", "Wom~
#> $ year           <chr> "2015", "2016", "2017", "2018", "2019", "2020", "2015",~
#> $ population     <dbl> 128483.4, 129910.2, 131310.6, 132683.0, 134025.6, 13533~
```

We will use the estimates for overall population in 2020. After
filtering the data, we can then calculate each region’s share of
population.

``` r
population_island_2020 <- population_tidy %>% 
  dplyr::filter(
    island_group_2 != "Indonesia",
    sex == "Total",
    year == "2020"
  ) %>% 
  group_by(island_group_2) %>% 
  summarize(population = sum(population)  ) %>% 
  ungroup() %>% 
  mutate(
    total = sum(population),
    population_share = population / total
  ) %>% 
  select(-total)

glimpse(population_island_2020)
#> Rows: 2
#> Columns: 3
#> $ island_group_2   <chr> "Java & Bali", "Other"
#> $ population       <dbl> 166803.9, 102799.5
#> $ population_share <dbl> 0.618701, 0.381299
```

Now, we can start computing the average mobility for each region and use
the population data to calculate the weighted average.

``` r
mobility_province <- mobility_idn_avg %>% 
  dplyr::filter(sub_region_1 != "National")

# Add anchor to province names to prevent errors when replacing it with
# island group names
id_province_en_anchored <- id_province_en %>% 
  mutate(province_en = str_c("^", province_en, "$"))

mobility_island <- mobility_province %>% 
  mutate(
    island_group_2 = str_replace_all(
      sub_region_1, 
      deframe(id_province_en_anchored[, c(3, 5)])
    )
  ) %>% 
  group_by(island_group_2, date) %>% 
  summarize(
    retail_recreation_mobility = mean(
      retail_and_recreation_percent_change_from_baseline, 
      na.rm = TRUE
    )
  ) %>% 
  ungroup()

mobility_population <- mobility_island %>% 
  left_join(population_island_2020, by = "island_group_2")

mobility_island_weighted <- mobility_population %>% 
  group_by(island_group_2) %>% 
  mutate(
    retail_recreation_mob_w = retail_recreation_mobility * population_share,
    seven_day_average = rollmean(
      retail_recreation_mob_w,
      k = 7, 
      fill = NA, 
      align = "right"
    )
  ) %>% 
  ungroup()

glimpse(mobility_island_weighted)
#> Rows: 1,082
#> Columns: 7
#> $ island_group_2             <chr> "Java & Bali", "Java & Bali", "Java & Bali"~
#> $ date                       <date> 2020-02-15, 2020-02-16, 2020-02-17, 2020-0~
#> $ retail_recreation_mobility <dbl> 0.0000000, -2.8888889, -3.5555556, -4.00000~
#> $ population                 <dbl> 166803.9, 166803.9, 166803.9, 166803.9, 166~
#> $ population_share           <dbl> 0.618701, 0.618701, 0.618701, 0.618701, 0.6~
#> $ retail_recreation_mob_w    <dbl> 0.00000000, -1.78735852, -2.19982587, -2.47~
#> $ seven_day_average          <dbl> NA, NA, NA, NA, NA, NA, -1.9444889, -1.8953~
```

We can now plot the weighted average of the number of visitors to retail
and recreation places by region.

``` r
chart_mobility_weighted <- ggplot(
  mobility_island_weighted, 
  aes(date, seven_day_average, color = island_group_2)
) +
  annotate(
    "rect",
    xmin = ymd("2020-04-01"),
    xmax = ymd("2020-05-01"),
    ymin = -40, 
    ymax = 10,
    fill = color_annotation,
    alpha = 0.25
  ) +
  annotate(
    "rect",
    xmin = ymd("2021-07-01"),
    xmax = last(mobility_island_weighted$date),
    ymin = -40, 
    ymax = 10,
    fill = color_annotation,
    alpha = 0.25
  ) +
  geom_hline(yintercept = 0, color = "#E68F7E") +
  geom_vline(
    xintercept = ymd("2020-03-02"), 
    color = color_annotation, 
    lty = "dashed"
  ) +
  geom_line(
    lwd = 0.75,
    show.legend = FALSE
  ) +
  scale_x_date(
    breaks = seq(ymd("2020-03-01"), ymd("2021-08-01"), by = "3 month"),
    labels = c("Mar\n2020", "Jun", "Sep", "Dec", "Mar\n'21", "Jun")
  ) +
  scale_y_continuous(
    breaks = seq(-40, 10, 10),
    limits = c(-40, 10),
    position = "right"
  ) +
  scale_color_manual(
    values = c("Java & Bali" = "#2477B3", "Other" = "#55CBF2")
  ) +
  geom_text(
    data = tibble(
      x = ymd("2020-03-01"),
      y = 7.5, 
      label = "COVID-19 pandemic"
    ),
    aes(x, y, label = label),
    size = 3,
    hjust = 0,
    vjust = 0.5,
    nudge_x = 5,
    color = color_annotation
  ) +
  geom_text_repel(
    data = tibble(
      x = ymd("2020-04-01"), 
      y = -35, 
      label = "Large-scale\nsocial restrictions"
    ),
    aes(x, y, label = label),
    size = 3,
    hjust = 0,
    vjust = 0.5,
    nudge_x = 50,
    color = color_annotation
  ) +
  geom_text_repel(
    data = tibble(
      x = ymd("2021-08-01"), 
      y = -25, 
      label = "Emergency curbs"
    ),
    aes(x, y, label = label),
    size = 3,
    hjust = 1,
    vjust = 0.5,
    nudge_x = -50,
    color = color_annotation
  ) +
  geom_text(
    data = tibble(
      x = rep(ymd("2021-05-01"), 2),
      y = c(4, -18.5),
      label = c("Other", "Java & Bali")
    ),
    aes(x, y, label = label),
    size = 3,
    hjust = 0.5,
    vjust = 0.5,
    color = c("#55CBF2", "#2477B3"),
    fontface = "bold"
  ) +
  labs(
    subtitle = "By region**", 
    x = NULL, 
    y = NULL
  ) +
  theme_dfr()

chart_mobility_weighted
#> Warning: Removed 12 row(s) containing missing values (geom_path).
```

<img src="README_files/figure-gfm/Plot weighted mobility by island group-1.png" width="70%" style="display: block; margin: auto;" />

The chart shows that visits to retail and recreation places in Java and
Bali fell more drastically than in other regions. This is expected given
the difference in stringency of containment measures between the two
regions. But we can see a similar trend, namely people reduced their
visits even before the government tightened mobility restrictions.

<a href="https://voxeu.org/article/covid-social-distancing-driven-mostly-voluntary-demobilisation" target="_blank">This column</a>
also finds that mobility falls regardless of restrictions in advanced
economies such as the United States, Italy and the United Kingdom.

## Mobility and COVID-19 cases

The relationship between mobility and infections seem to go both ways.
But since the data show that people tend to reduce their visits outside
when the pandemic worsens, we can analyze how mobility varies with
confirmed COVID-19 cases among provinces. We can obtain Indonesia’s
pandemic data from the government’s API. We will take a look at
cumulative confirmed cases.

``` r
path_covid_data <- "data/covid_province_raw.csv"

if (!file.exists(path_covid_data)) {
  
  url_covid <- "https://data.covid19.go.id/public/api/prov.json"
  
  covid_parsed <- fromJSON(url_covid)
  
  covid_raw <- as_tibble(covid_parsed$list_data)
  
  covid_raw %>% 
    select(-c(jenis_kelamin, kelompok_umur, lokasi, penambahan)) %>% 
    write_csv(path_covid_data)
  
} else {
  
  covid_raw <- read_csv(path_covid_data)
  
}

# Add anchor to prevent errors when replacing province names
id_province_idn_anchored <- id_province_en %>% 
  mutate(label = str_c("^", label, "$"))

# Change province names for consistency and prevent errors when joing data
covid_tidy <- covid_raw %>% 
  select(key, jumlah_kasus) %>% 
  mutate(
    key = str_replace_all(
      key,
      c("KEPULAUAN" = "KEP.", "DAERAH ISTIMEWA" = "DI")
    ),
    key = str_replace_all(key, deframe(id_province_idn_anchored[, c(2, 3)]))
  ) %>% 
  dplyr::filter(key != "PROVINSI JAWA TENGAH") # This observation seems like a bug

glimpse(covid_tidy)
#> Rows: 34
#> Columns: 2
#> $ key          <chr> "Jakarta", "West Java", "Central Java", "East Java", "Eas~
#> $ jumlah_kasus <dbl> 833654, 643567, 423915, 342852, 133825, 131901, 121861, 1~
```

Again, to better compare cases between one province and another, we
first need to adjust for population.

``` r
population_province_2020 <- population_tidy %>% 
  dplyr::filter(
    province != "Indonesia",
    sex == "Total",
    year == "2020"
  )

covid_population <- covid_tidy %>% 
  left_join(population_province_2020, by = c("key" = "province")) %>% 
  select(-c(province_id, sex, year))

covid_adjusted <- covid_population %>% 
  mutate(cases_per_hundred_people = jumlah_kasus / population * 100)

glimpse(covid_adjusted)
#> Rows: 34
#> Columns: 6
#> $ key                      <chr> "Jakarta", "West Java", "Central Java", "East~
#> $ jumlah_kasus             <dbl> 833654, 643567, 423915, 342852, 133825, 13190~
#> $ island_group_1           <chr> "Java", "Java", "Java", "Java", "Kalimantan",~
#> $ island_group_2           <chr> "Java & Bali", "Java & Bali", "Java & Bali", ~
#> $ population               <dbl> 10576.4, 49565.2, 34738.2, 39955.9, 3664.7, 3~
#> $ cases_per_hundred_people <dbl> 7882.2094, 1298.4251, 1220.3137, 858.0760, 36~
```

We can fit a linear model to see how far can COVID-19 cases explain the
variation in the number of visitors to places like restaurants and
shopping centers. We will use the smoothed mobility and the natural log
of cumulative coronavirus cases per 100 people.

``` r
mobility_latest <- mobility_province %>% 
  dplyr::filter(date == last(date))

mobility_covid <- mobility_latest %>% 
  left_join(covid_adjusted, by = c("sub_region_1" = "key"))

mobility_covid_lm <- lm(
  seven_day_average ~ log(cases_per_hundred_people), 
  data = mobility_covid
)

summary(mobility_covid_lm)
#> 
#> Call:
#> lm(formula = seven_day_average ~ log(cases_per_hundred_people), 
#>     data = mobility_covid)
#> 
#> Residuals:
#>      Min       1Q   Median       3Q      Max 
#> -29.0144  -5.8993   0.8927   4.4132  23.5420 
#> 
#> Coefficients:
#>                               Estimate Std. Error t value Pr(>|t|)    
#> (Intercept)                     58.663     18.489   3.173  0.00332 ** 
#> log(cases_per_hundred_people)   -9.478      2.612  -3.629  0.00098 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Residual standard error: 10.17 on 32 degrees of freedom
#> Multiple R-squared:  0.2916, Adjusted R-squared:  0.2694 
#> F-statistic: 13.17 on 1 and 32 DF,  p-value: 0.0009799
```

We get a statistically significant result, but the *R<sup>2</sup>* is
not satisfying. Still, this is enough to make us slightly more confident
in the assumption that mobility varies with coronavirus cases.

Now, we can make another chart to plot smoothed mobility against
coronavirus cases per 100 people. To account for density, we will map
the COVID-19 cases to an x-axis in log scale.

``` r
annotation_text_province <- mobility_covid %>% 
  dplyr::filter(sub_region_1 %in% c("Bali", "Jakarta", "Gorontalo", "Aceh"))

chart_covid_mobitliy <- ggplot(
  mobility_covid, 
  aes(cases_per_hundred_people, seven_day_average)
) +
  geom_hline(yintercept = 0, color = "#E68F7E") +
  geom_point(
    pch = 21,
    fill = "#36A3D9",
    color = "white",
    alpha = 0.75,
    size = 3
  ) +
  geom_text(
    data = annotation_text_province, 
    aes(label = sub_region_1),
    size = 3,
    hjust = 1,
    vjust = 0.5,
    color = "#36A3D9",
    nudge_x = 0.025,
    nudge_y = 5
  ) +
  geom_smooth(
    method = "lm",
    se = FALSE,
    lty = "dashed",
    color = "#2477B3",
    lwd = 0.75
  ) +
  scale_x_log10(
    breaks = c(400, 1000, 4000, 10000),
    labels = c(400, "1,000", "4,000", "10,000"),
    limits = c(400, 10000)
  ) +
  scale_y_continuous(
    breaks = seq(-50, 25, 25),
    limits = c(-50, 25)
  ) +
  labs(
    subtitle = "By province (at latest date available)",
    x = "COVID-19 cases per 100 people (log scale)",
    y = "Mobility"
  ) +
  theme_dfr() +
  theme(
    axis.title = element_text(size = rel(0.75)),
    panel.grid.major.x = element_line(color = "#E0E0E0")
  )

chart_covid_mobitliy
```

<img src="README_files/figure-gfm/Plot COVID cases and mobility-1.png" width="70%" style="display: block; margin: auto;" />

The scatter plot shows that mobility tends to be closer to pre-pandemic
levels in provinces with fewer cases, after adjusting for population.
The linear model, which we fit using `geom_smooth()`, is good enough to
show the relationship between the two variables.

We can join the scatter plot with the mobility by region chart we have
made earlier to make one final chart.

``` r
chart_mobility_weighted + chart_covid_mobitliy +
  plot_annotation(
    title = "Mobility falls before govt tightens restrictions as cases rise",
    subtitle = str_c(
      "Number of visitors to retail and recreation places* ",
      "(percentage change)"
    ),
    caption = str_c(
      "\\*Change from January-February baseline smoothed to seven-day moving average ",
      "\\*\\*Weighted by population<br>",
      "Source: Google; BPS; BNPB; *The Jakarta Post* analysis<br>",
      "Chart: JP/Dzulfiqar Fathur Rahman"
    ),
    theme = theme_dfr()
  )
#> Warning: Removed 12 row(s) containing missing values (geom_path).
```

<img src="README_files/figure-gfm/Patchwork mobility charts-1.png" width="70%" style="display: block; margin: auto;" />
