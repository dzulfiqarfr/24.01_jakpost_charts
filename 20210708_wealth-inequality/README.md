Wealth gap in Indonesia narrows during pandemic
================
2021-07-11

I prepared this analysis for The Jakarta Post’s story on how the
pandemic affects Indonesia’s wealth gap. The TLDR is as follows: wealth
inequality in Indonesia was estimated to narrow between 2019 and 2020 as
stocks, which typically made up the majority of rich adults’ wealth,
declined.

This is based on the latest global wealth report released in June 2021
by Swiss investment bank Credit Suisse and authored by economists
Anthony Shorrocks, James Davies and Rodrigo Lluberas.

The report provides estimates for both wealth level and distribution
among adults for many countries including Indonesia. The wealth level
estimate spans as far back as 2000. That said, this is probably the most
comprehensive wealth inequality data for Indonesia out there.

The data quality for Indonesia is described as “fair” by the authors.
Data for Indonesia are a result of estimates based on the Indonesian
Family Life Survey (IFLS).

For wealth level, the best data source is actually the household balance
sheet, but this is available mostly in rich countries and less so in
Africa, Asia and Latin America.

The authors also take into account data from the billionaire list
published by Forbes to address accuracy issues for wealth at the top of
the distribution.

For more details, you can read the first chapter of the
[databook](/data/global-wealth-databook-2021.pdf).

I have downloaded all data and you can do so from
<a href="https://www.credit-suisse.com/about-us/en/reports-research/global-wealth-report.html" target="_blank">this link</a>.

## Packages

Load the packages.

``` r
library(tidyverse)
library(lubridate)
library(tabulizer)
library(fs)
library(ggrepel)
library(ggtext)
```

## Wealth level from 2000 to 2020

We will first take a look at the wealth level data from 2000 to 2020.
The data is available in Tabel 2-2 in the databook, which is a pdf
document. So we have to extract the data from tables on pages that
contain observations on Indonesia.

``` r
# path to databook
path_databook_2021 <- "data/global-wealth-databook-2021.pdf"

# area of tables on pages containing observations on indonesia.
# result from `locate_areas(path_databook_2021, pages = 106)`:      
area_wealth_level <- list(
  c(
    top = 53.98713, 
    left = 26.58371, 
    bottom = 701.83269, 
    right = 565.82232
  )
)

# extract the wealth level data
wealth_level_raw <- extract_tables(
  path_databook_2021,
  # change the page numbers to 25, 108 for complete observations and remove the increment
  pages = seq(26, 106, 4), 
  area = area_wealth_level,
  guess = F,
  output = "data.frame"
)

# clean column names
wealth_level_cln <- wealth_level_raw %>% 
  map_df(pluck) %>%
  rename(
    country = 1,
    adult_pop = 2,
    adult_share = 3,
    wealth_total = 4,
    wealth_share = 5,
    wealth_perAdult = 6,
    wealth_fin_perAdult = 7,
    wealth_nonFin_perAdult = 8,
    debt_perAdult = 9,
    wealth_median = 10,
    method = 11
  ) %>%
  slice(-c(1:7))

# correct data types
wealth_level_cln[, 2:10] <- map(
  wealth_level_cln[, 2:10], 
  function(x) {as.numeric(str_remove_all(x, ","))}
)
#> Warning in .f(.x[[i]], ...): NAs introduced by coercion

#> Warning in .f(.x[[i]], ...): NAs introduced by coercion

#> Warning in .f(.x[[i]], ...): NAs introduced by coercion

#> Warning in .f(.x[[i]], ...): NAs introduced by coercion

#> Warning in .f(.x[[i]], ...): NAs introduced by coercion

#> Warning in .f(.x[[i]], ...): NAs introduced by coercion

#> Warning in .f(.x[[i]], ...): NAs introduced by coercion

#> Warning in .f(.x[[i]], ...): NAs introduced by coercion

#> Warning in .f(.x[[i]], ...): NAs introduced by coercion

# remove rows containing table headers
wealth_level_cln <- wealth_level_cln %>% 
  mutate(across(everything(), function(x) {na_if(x, "")})) %>% 
  dplyr::filter(!is.na(wealth_perAdult))

# add year variable. this works because we extract the data in a chronological order 
wealth_level_tidy <- wealth_level_cln %>% 
  group_by(country) %>% 
  mutate(year = seq(2000, 2020)) %>% 
  ungroup()

# subset observations on indonesia
wealth_level_id <- wealth_level_tidy %>% 
  dplyr::filter(country == "Indonesia")

# write indonesia's wealth level data to a csv for a backup
write_csv(wealth_level_id, "data/idn_wealth-level_cleaned.csv")

# glimpse
glimpse(wealth_level_id)
#> Rows: 21
#> Columns: 12
#> $ country                <chr> "Indonesia", "Indonesia", "Indonesia", "Indones~
#> $ adult_pop              <dbl> 125684, 128333, 130847, 133203, 135440, 138113,~
#> $ adult_share            <dbl> 3.35, 3.36, 3.36, 3.36, 3.35, 3.35, 3.36, 3.37,~
#> $ wealth_total           <dbl> 368, 358, 506, 650, 684, 720, 875, 1006, 911, 1~
#> $ wealth_share           <dbl> 0.31, 0.31, 0.39, 0.43, 0.39, 0.39, 0.42, 0.43,~
#> $ wealth_perAdult        <dbl> 2928, 2789, 3868, 4883, 5050, 5211, 6194, 6977,~
#> $ wealth_fin_perAdult    <dbl> 656, 360, 521, 746, 878, 856, 1132, 1631, 1398,~
#> $ wealth_nonFin_perAdult <dbl> 2320, 2476, 3417, 4233, 4283, 4488, 5227, 5534,~
#> $ debt_perAdult          <dbl> 48, 47, 71, 96, 111, 132, 165, 188, 201, 271, 3~
#> $ wealth_median          <dbl> 701, 754, 1094, 1443, 1482, 1506, 1815, 1827, 2~
#> $ method                 <chr> "Original data", "Regression", "Regression", "R~
#> $ year                   <int> 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007,~
```

We have 21 observations and 12 variables. We will look mostly into
wealth per adult, wealth components and the median wealth. The `method`
column tells us how the authors estimate the wealth level.

With the data we have extracted, we can now see how the average wealth
level at current exchange rates has changed over the period. We will
also calculate the contribution of financial and nonfinancial assets, as
well as debts to the wealth growth.

``` r
# calculate wealth growth
wealth_chg_overall <- wealth_level_id %>% 
  mutate(
    wealth_diff = wealth_perAdult - dplyr::lag(wealth_perAdult, 1),
    wealth_chg = wealth_diff / dplyr::lag(wealth_perAdult, 1) * 100
  ) %>% 
  select(year, wealth_chg) %>% 
  dplyr::filter(!is.na(wealth_chg))

# glimpse
glimpse(wealth_chg_overall)
#> Rows: 20
#> Columns: 2
#> $ year       <int> 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010,~
#> $ wealth_chg <dbl> -4.74726776, 38.68770169, 26.24095140, 3.42002867, 3.188118~

# function to calculate the weighted growth of each asset component
calculate_wg <- function(x, y) {
  
  # base
  x_lag <- dplyr::lag(x, 1)
  
  # annual change
  x_chg <- (x - x_lag) / x_lag * 100
  
  # proportion
  x_prop <- x / y * 100
  
  # weigh change by proportion
  x_wg <- x_chg * dplyr::lag(x_prop, 1) / 100
  
  return(x_wg)
  
}

# calculate weighted growth
wealth_chg_comp <- wealth_level_id %>% 
  mutate(
    fin_asset_wg = calculate_wg(wealth_fin_perAdult, wealth_perAdult),
    nonFin_asset_wg = calculate_wg(wealth_nonFin_perAdult, wealth_perAdult),
    debt_wg = calculate_wg(debt_perAdult, wealth_perAdult) * -1
  ) %>% 
  select(year, fin_asset_wg, nonFin_asset_wg, debt_wg) %>% 
  pivot_longer(
    2:ncol(.), 
    names_to = "asset_component", 
    values_to = "weighted_growth"
  ) %>% 
  dplyr::filter(!is.na(weighted_growth))

# glimpse
glimpse(wealth_chg_comp) 
#> Rows: 60
#> Columns: 3
#> $ year            <int> 2001, 2001, 2001, 2002, 2002, 2002, 2003, 2003, 2003, ~
#> $ asset_component <chr> "fin_asset_wg", "nonFin_asset_wg", "debt_wg", "fin_ass~
#> $ weighted_growth <dbl> -10.10928962, 5.32786885, 0.03415301, 5.77267838, 33.7~
```

In line with the global trend, the average wealth grew by 2.2 percent
between 2019 and 2020, driven by nonfinancial assets. But it was slower
than a year earlier. The growth in wealth level was related to the rise
in asset prices as a result of policy responses to the pandemic,
according to the authors. (At smoothed exchange rates, the average
wealth grew by 3 percent.)

We can now plot the change in wealth level between 2000 and 2020 to see
the trend over time.

``` r
# custom ggplot2 theme
theme_dfr <- function(..., base_size = 12, rel_size = 0.8) {
  
  theme(
    text = element_text(size = base_size),
    axis.title = element_blank(),
    axis.text = element_text(size = rel(rel_size)),
    axis.ticks.y = element_blank(),
    axis.line.x = element_line(color = "black"),
    panel.background = element_rect(fill = "white"),
    panel.grid.major.x  = element_blank(),
    panel.grid.major.y = element_line(color = "#CFD8DC"),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    plot.title = element_text(face = "bold", size = rel(1)),
    plot.subtitle = element_text(size = rel(0.9), margin = margin(b = 25)),
    plot.title.position = "plot",
    plot.caption = element_text(
      size = rel(rel_size),
      color = "#757575",
      hjust = 0,
      margin = margin(t = 25)
    ),
    plot.caption.position = "plot",
    ...
  )
  
}

# anotations
anno_gfc_cvd <- tribble(
  ~x, ~y, ~label,
  2008, 30, "Global\nfinancial\ncrisis",
  2020, 30, "COVID-19\npandemic"
)

anno_wealth_chg <- tribble(
  ~x, ~y, ~label,
  2016.5, 12, "Net wealth\ngrowth"
)

# plot
wealth_chg_comp %>% 
  dplyr::filter(!is.na(weighted_growth)) %>% 
  mutate(asset_component = fct_reorder(asset_component, weighted_growth)) %>% 
  ggplot(aes(year, weighted_growth)) +
  geom_hline(yintercept = 0, color = "#E68F7E") +
  geom_vline(
    xintercept = 2008,
    color = "#90A4AE", 
    lwd = 0.5,
    lty = "dashed"
  ) +
  geom_vline(
    xintercept = 2020,
    color = "#90A4AE",
    lwd = 0.5,
    lty = "dashed"
  ) +
  geom_col(aes(fill = asset_component), width = 0.6) +
  geom_line(
    data = wealth_chg_overall, 
    aes(y = wealth_chg),
    color = "#263238",
    lwd = 0.75
  ) +
  geom_label(
    data = anno_gfc_cvd,
    aes(x, y, label = label),
    size = 3,
    hjust = 1,
    nudge_x = -0.25,
    color = "#90A4AE",
    fill = "white",
    label.size = 0,
    label.padding = unit(0.025, "lines")
  ) +
  geom_text_repel(
    data = anno_wealth_chg,
    aes(x, y, label = label),
    size = 3,
    color = "#263238",
    hjust = 1,
    nudge_x = -1.25,
    nudge_y = 5,
    segment.curvature = -0.1,
    segment.ncp = 1
  ) +
  scale_x_continuous(
    breaks = seq(2001, 2020, 3), 
    labels = c(
      2001, 
      str_c("'", c(c("04", "07"), seq(10, 19, 3)))
    )
  ) +
  scale_y_continuous(
    breaks = seq(-30, 40, 10),
    limits = c(-30, 40),
    expand = c(0, 0),
    position = "right"
  ) +
  scale_fill_manual(
    values = c(
      "nonFin_asset_wg" = "#127DB3",
      "fin_asset_wg" = "#36B3D9",
      "debt_wg" = "grey"
    ),
    labels = c(
      "nonFin_asset_wg" = "Nonfinancial assets",
      "fin_asset_wg" = "Financial assets",
      "debt_wg" = "Debt"
    )
  ) +
  labs(
    title = "Indonesia sees slower wealth growth, relies on nonfinancial assets",
    subtitle = "Wealth per adult and its composition (annual percentage change)",
    x = NULL,
    y = NULL,
    caption = "Chart: @dzulfiqarfr | Source: Credit Suisse"
  ) +
  theme_dfr() +
  theme(
    plot.subtitle = element_text(margin = margin(b = 37.5)),
    legend.text = element_text(size = rel(0.8)),
    legend.title = element_blank(),
    legend.key.size = unit(0.25, "cm"),
    legend.direction = "horizontal",
    legend.position = c(0.275, 1.075)
  )
```

<img src="README_files/figure-gfm/plot wealth growth and its components-1.png" width="70%" style="display: block; margin: auto;" />

Indonesia’s wealth comprised mostly of nonfinancial assets, which were
typically houses or land. We can see that nonfinancial assets led the
wealth growth in the country in most cases, with a few exceptions such
as in 2008 and 2013 (see also
<a href="https://www.thejakartapost.com/news/2021/06/28/indonesian-homeownership-slides-as-affordability-issues-arise.html" target="_blank">my article on home ownership</a>).

## Wealth distribution

We can actually guess the wealth distribution by comparing the average
and the median wealth provided in `wealth_level_id`.

``` r
wealth_level_id %>% 
  tail(1) %>% 
  select(year, wealth_perAdult, wealth_median) %>% 
  mutate(mean_to_median = wealth_perAdult / wealth_median)
#> # A tibble: 1 x 4
#>    year wealth_perAdult wealth_median mean_to_median
#>   <int>           <dbl>         <dbl>          <dbl>
#> 1  2020           17693          4693           3.77
```

The average wealth is nearly four times higher than the median wealth in
2020. This suggests that the wealth distribution is skewed toward the
richest adults.

The report provides a wealth distribution data by decile and also gives
estimates for wealth share of the top 5 percent and top 1 percent.
However, it is importatnt to remember that the estimates for Indonesia
are based on a survey data and thus, the report may understate the
wealth share of the top groups since they are less likely to respond to
the survey and their financial assets are likely to be underreported.

To see how the pandemic has impacted wealth distribution, we will look
at the wealth distribution for 2019 and 2020. The data are presented in
Table 7-5 in the 2019 databook and Table 4-5 in the 2021 databook.

``` r
# function to clean wealth share data
clean_wealth_share <- function(x, year) {
  
  x_cln <- x %>% 
    .[[1]] %>% 
    slice(-c(1:2)) %>% 
    separate(Wealth.decile, into = c("dec_4", "dec_5"), sep = "\\s") %>% 
    rename(
      country = 1,
      dec_1 = 2,
      dec_2 = 3,
      dec_3 = 4,
      dec_6 = 7,
      dec_7 = 8,
      dec_8 = 9,
      dec_9 = 10,
      top_10 = 11,
      top_5 = 12,
      top_1 = 13
    ) %>% 
    mutate(year = year) %>% 
    select(country, year, dec_1:top_1)
  
  x_cln[, 3:ncol(x_cln)] <- map(x_cln[, 3:ncol(x_cln)], as.numeric)
  
  return(x_cln)
  
}

# 2019 ----
## path to 2019 databook
path_databook_2019 <- "data/global-wealth-databook-2019.pdf"

## extract the 2019 wealth distribution data
wealth_share_19_raw <- extract_tables(
  path_databook_2019,
  pages = 168,
  # the area below is obtained from `locate_area()`
  area = list(
    c(
      top = 53.35751,
      left = 35.84341,
      bottom = 544.34924,
      right = 559.47660
    )
  ),
  guess = F,
  output = "data.frame"
) %>% 
  pluck()

## clean the data
wealth_share_19_cln <- clean_wealth_share(wealth_share_19_raw, 2019)

# 2020 ----
## extract the 2020 wealth distribution data
wealth_share_20_raw <- extract_tables(
  path_databook_2021,
  pages = 136,
  area = list(c(
    top = 57.97499,
    left = 32.70321,
    bottom = 539.73176,
    right = 559.41660 
  )),
  guess = F,
  output = "data.frame"
) %>% 
  pluck()

## clean the data
wealth_share_20_cln <- clean_wealth_share(wealth_share_20_raw, 2020)

# merge 2019 and 2020 wealth distribution data ----
wealth_share_cln <- wealth_share_19_cln %>% 
  rbind(wealth_share_20_cln) %>% 
  arrange(country, year)

# subset observations on indonesia
wealth_share_id <- wealth_share_cln %>% 
  dplyr::filter(country == "Indonesia")

# write indonesia's wealth distribution data to a csv for a backup
write_csv(wealth_share_id, "data/idn_wealth-share_cleaned.csv")

# glimpse
glimpse(wealth_share_id)
#> Rows: 2
#> Columns: 14
#> $ country <chr> "Indonesia", "Indonesia"
#> $ year    <dbl> 2019, 2020
#> $ dec_1   <dbl> -0.1, -0.1
#> $ dec_2   <dbl> 0.2, 0.3
#> $ dec_3   <dbl> 0.5, 0.6
#> $ dec_4   <dbl> 0.9, 1.3
#> $ dec_5   <dbl> 1.5, 2.2
#> $ dec_6   <dbl> 2.2, 3.3
#> $ dec_7   <dbl> 3.2, 5.1
#> $ dec_8   <dbl> 5.9, 8.3
#> $ dec_9   <dbl> 11.7, 12.8
#> $ top_10  <dbl> 74.1, 66.2
#> $ top_5   <dbl> 63.7, 55.5
#> $ top_1   <dbl> 44.6, 36.6
```

We have learned that the top groups control the majority of wealth in
Indonesia. But the wealth share of the top 10 percent, for example,
shrank to 66.2 percent in 2020 from 74.1 percent a year earlier.

We can draw the Lorenz curves to see the changes in the overall wealth
distribution.

``` r
# calculate cumulative wealth share
wealth_dis <- wealth_share_id %>%
  select(-c(top_5, top_1)) %>% 
  pivot_longer(3:ncol(.), names_to = "decile", values_to = "wealth_share") %>% 
  mutate(
    decile = str_remove_all(decile, c("dec_")),
    decile = as.numeric(str_remove_all(decile, c("top_")))
  ) %>% 
  group_by(year) %>% 
  mutate(wealth_share_cum = cumsum(wealth_share)) %>% 
  ungroup()

# annotations
anno_wealth_dis_yr <- tribble(
  ~x, ~y, ~label,
  9, 20, "2019",
  8.25, 40, "2020"
)

anno_bottom_40 <- tribble(
  ~x, ~y, ~label,
  4, 2.1, "Wealth share of the bottom 40 percent\nexpanded to 2.1 percent from 1.5 percent"
)

area_ineq <- tibble(
  x = c(0, seq(1, 10)),
  ymin = c(0, wealth_dis[wealth_dis$year == 2020, ] %>% select(wealth_share_cum) %>%  .[[1]]),
  ymax = seq(0, 100, 10)
)

area_fall_ineq <- wealth_dis %>% 
  select(decile, year, wealth_share_cum) %>% 
  pivot_wider(names_from = year, values_from = wealth_share_cum)

anno_area_ineq <- tribble(
  ~x, ~y, ~label,
  5.25, 47.5, "Shaded area represents inequality"
)

# plot
wealth_dis %>% 
  ggplot() +
  geom_richtext(
    data = anno_area_ineq,
    aes(x, y, label = label),
    angle = 19,
    size = 3,
    color = "#90A4AE",
    fill = "white",
    label.color = NA,
    label.padding = grid::unit(rep(0, 4), "pt")
  ) +
  geom_ribbon(
    data = area_ineq,
    aes(x = x, ymin = ymin, ymax = ymax),
    fill = "#90A4AE",
    alpha = 0.125
  ) +
  geom_ribbon(
    data = area_fall_ineq,
    aes(decile, ymin = `2019`, ymax = `2020`),
    fill = "#36B3D9",
    alpha = 0.125
  ) +
  geom_abline(
    slope = 10,
    color = "black",
    lwd = 0.5,
    lty = "dashed"
  ) +
  geom_line(
    aes(decile, wealth_share_cum, color = as_factor(year)),
    lwd = 1,
    show.legend = F
  ) +
  scale_x_continuous(
    breaks = seq(1, 10),
    limits = c(1, 10),
    labels = seq(10, 100, 10),
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    breaks = seq(0, 100, 25),
    limits = c(-0.5, 100.1),
    expand = c(0, 0),
    position = "right"
  ) +
  scale_color_manual(values = c("2019" = "#36B3D9", "2020" = "#127DB3")
  ) +
  labs(
    title = "Wealth gap narrows during pandemic",
    subtitle = "Wealth distribution in Indonesia (percent)",
    x = "Cumulative share of adult population from poorest to wealthiest",
    y = "Cumulative share of wealth",
    caption = "Chart: @dzulfiqarfr | Source: Credit Suisse"
  ) +
  annotate(
    "text",
    x = 5,
    y = 57.5,
    label = "Equality",
    angle = 20,
    size = 3,
    color = "black"
  ) +
  geom_text(
    data = anno_wealth_dis_yr,
    aes(x, y, label = label),
    hjust = 0,
    size = 3,
    color = c("#36B3D9", "#127DB3"),
    fontface = "bold"
  ) +
  geom_text_repel(
    data = anno_bottom_40,
    aes(x, y, label = label),
    color = "#90A4AE",
    segment.color = "#90A4AE",
    size = 2.75,
    hjust = 0,
    nudge_x = -1.35,
    nudge_y = 15
  ) +
  theme_dfr() +
  theme(axis.title = element_text(size = rel(0.8)))
```

<img src="README_files/figure-gfm/plot wealth distribution-1.png" width="70%" style="display: block; margin: auto;" />

The curves are very low, especially if we compare them to the curves for
income distribution. In fact, the authors mentioned that wealth and
income inequality are correlated.

While seeing changes in wealth distribution from 2019 to 2020 is
helpful, we also need to know how the latest trend compares with changes
in wealth inequality in the previous years. To that end, we can read the
gini coefficients provided by the report. The databook provides gini
coefficients for Indonesia from 2010 until 2020.

``` r
# directory
dir <- "data"

# file list
# rename the files with a consisent naming convention first
databooks <-  fs::dir_ls(dir)

# filter out files other than the databooks
databooks <- databooks %>% 
  enframe() %>% 
  dplyr::filter(!str_detect(name, ".csv$")) %>% 
  deframe()

# pages containing gini data in each databook
page_num <- tribble(
  ~databook, ~page_num,
  2010, 122,
  2011, 148,
  2012, 152,
  2013, 148,
  2014, 149,
  2015, 151,
  2016, 150,
  2017, 159,
  2018, 157,
  2019, 169,
  2021, 137
)

# extract the gini data
gini_raw <- map(
  databooks,
  function(x) {
    
    year <- str_sub(x, 29, 32)
    
    page_num <- page_num %>% 
      dplyr::filter(databook == year) %>% 
      select(page_num) %>% 
      deframe()
    
    gini_raw <- extract_tables(
      x,
      pages = page_num,
      area = list(
        c(
          top = 75,
          left = 35,
          bottom = 721,
          right = 567
        )
      ),
      guess = F,
      output = "data.frame"
    ) %>% 
      .[[1]] %>% 
      as_tibble() %>% 
      rename(country = 1) %>% 
      dplyr::filter(str_detect(country, "Indonesia")) %>% 
      select(country, ncol(.)) %>% 
      rename(gini = ncol(.)) %>% 
      mutate(year = year)
    
    return(gini_raw)
    
  }
)

# tidy the data
gini_tidy <- gini_raw %>% 
  map_df(pluck) %>% 
  mutate(
    country = "Indonesia",
    gini = as.numeric(case_when(
      str_length(gini) > 4 ~ str_sub(gini, -4, -1),
      TRUE ~ gini
    )),
    year = if_else(year == "2021", 2020, as.numeric(year))
  ) %>% 
  select(country, year, gini)

# write the gini data to a csv for a backup
write_csv(gini_tidy, "data/idn_gini_cleaned.csv")

# glimpse
glimpse(gini_tidy)
#> Rows: 11
#> Columns: 3
#> $ country <chr> "Indonesia", "Indonesia", "Indonesia", "Indonesia", "Indonesia~
#> $ year    <dbl> 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 20~
#> $ gini    <dbl> 77.3, 81.2, 82.0, 82.8, 84.0, 84.7, 84.0, 83.7, 84.0, 83.3, 77~
```

The gini ratio fell to 0.777 last year from 0.833 in 2019.

We can now plot the gini data to see the trend in the past decade.

``` r
ggplot(gini_tidy, aes(year, gini)) +
  geom_vline(
    xintercept = 2020,
    color = "#90A4AE",
    lwd = 0.5,
    lty = "dashed"
  ) +
  geom_line(lwd = 1, color = "#127DB3") +
  scale_x_continuous(
    breaks = seq(2010, 2020),
    labels = c(2010, str_c("'", seq(11, 20)))
  ) +
  scale_y_continuous(
    breaks = seq(70, 90, 5),
    limits = c(70, 90),
    expand = c(0, 0),
    position = "right"
  ) +
  annotate(
    "text",
    x = 2019.9,
    y = 88.5,
    label = "COVID-19\npandemic",
    color = "#90A4AE",
    size = 3,
    hjust = 1
  ) +
  labs(
    title = "Wealth inequality falls to 2010 level",
    subtitle = "Gini ratio for wealth inequality in Indonesia (percent)",
    caption = "Chart: @dzulfiqarfr | Source: Credit Suisse"
  ) +
  theme_dfr()
```

<img src="README_files/figure-gfm/plot gini-1.png" width="70%" style="display: block; margin: auto;" />

The chart reveals that Indonesia’s gini figure fell to a level last seen
in 2010. Wealth inequality rose from 2010 until mid-2010s and it started
plateauing since.

## Wealth share at the top and asset prices

It is quite tempting to dismiss whatever we have just learned from the
data, perhaps due to our intuition based on the direct economic impact
of the pandemic. Moreover, wealth inequality was actually rising
globally.

But if we look at the Indonesian stock market and house price index, it
actually makes sense. The top groups typically accumulate their wealth
through financial assets such as equities, while those in the middle
through houses. The Jakarta Composite Index (JCI) had not fully
recovered by the end of last year. Meanwhile, the pandemic has not
resulted in a decline in house prices, although the growth was slowing,
as reflected in Bank Indonesia’s residential property price index.

We can check this assumption with the *limited* data provided in the
latest databook. Tabel 1-4 provides changes in exchange rates against
the US dollar, share price index and house price index. The authors
source the data from Thomson Reuters.

``` r
# extract the share price data from the databook
share_price_raw <- extract_tables(
  path_databook_2021,
  pages = 14,
  area = list(
    c(
      top = 81.91151,
      left = 222.16248,
      bottom = 749.30413,
      right = 387.00744  
    )
  ),
  guess = F,
  output = "data.frame"
) %>% 
  pluck()

# correct column names
share_price_tidy <- share_price_raw %>% 
  .[[1]] %>% 
  rename(country = 1, share_price_chg = 2)

# glimpse
glimpse(share_price_tidy)
#> Rows: 57
#> Columns: 2
#> $ country         <chr> "Turkey", "Korea", "Denmark", "Taiwan", "Bangladesh", ~
#> $ share_price_chg <dbl> 42.2, 32.5, 28.6, 23.3, 23.3, 22.9, 19.6, 18.0, 17.9, ~
```

There are share price data for 57 countries, including Indonesia.

We will now merge the share price data with the wealth distribution data
for the top 10 percent.

``` r
# subset wealth share of the top 10 percent
top_10_wealth_share <- wealth_share_cln %>% 
  select(country, year, top_10) %>% 
  dplyr::filter(year == 2020)

# join top wealth share, share price index data
top_wealth_and_stock <- top_10_wealth_share %>% 
  left_join(share_price_tidy, by = "country")

# glimpse
glimpse(top_wealth_and_stock)
#> Rows: 40
#> Columns: 4
#> $ country         <chr> "Australia", "Austria", "Belgium", "Canada", "Chile", ~
#> $ year            <dbl> 2020, 2020, 2020, 2020, 2020, 2020, 2020, 2020, 2020, ~
#> $ top_10          <dbl> 51.4, 57.4, 44.7, 56.4, 67.1, 59.9, 71.6, 67.3, 56.0, ~
#> $ share_price_chg <dbl> -0.8, -13.5, -10.0, 2.8, -14.9, 22.9, NA, -4.6, 28.6, ~
```

After joining the data, we are left with just 37 countries. Not all
countries with wealth distribution data also have share price index data
in the report. Needless to say this is a very small sample.

We can perform a simple linear regression just to gauge the relationship
between share price movement and wealth share of the top 10 percent.

``` r
reg_top_wealth_stock <- lm(top_10 ~ share_price_chg, data = top_wealth_and_stock)

summary(reg_top_wealth_stock)
#> 
#> Call:
#> lm(formula = top_10 ~ share_price_chg, data = top_wealth_and_stock)
#> 
#> Residuals:
#>     Min      1Q  Median      3Q     Max 
#> -15.021  -6.373  -1.914   6.278  21.720 
#> 
#> Coefficients:
#>                 Estimate Std. Error t value Pr(>|t|)    
#> (Intercept)      60.8853     1.3928  43.715   <2e-16 ***
#> share_price_chg   0.1164     0.0963   1.209    0.234    
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Residual standard error: 8.554 on 37 degrees of freedom
#>   (1 observation deleted due to missingness)
#> Multiple R-squared:  0.03799,    Adjusted R-squared:  0.01199 
#> F-statistic: 1.461 on 1 and 37 DF,  p-value: 0.2344
```

The result suggests we can expect the wealth share of top 10 percent to
vary by 0.11 percentage points for every change in the share price
movement. But it is not statistically significant, which is expected in
part due to our limited data. The *t*-value, for one, is pretty small.

``` r
ggplot(top_wealth_and_stock, aes(share_price_chg, top_10)) +
  geom_vline(xintercept = 0, color = "#E68F7E") +
  geom_point(size = 2.5) +
  geom_smooth(method = "lm", se = T, lty = "dashed") +
  geom_text_repel(
    aes(label = country), 
    check_overlap = T,
    max.overlaps = 6,
    hjust = 1,
    size = 3
  ) +
  scale_x_continuous(
    breaks = seq(-20, 40, 10),
    limits = c(-20, 40)
  ) +
  scale_y_continuous(
    breaks = seq(40, 90, 10),
    limits = c(40, 90),
    expand = c(0, 0),
    position = "right"
  ) +
  labs(
    subtitle = "Wealth share of top 10 percent and share price index movement in 2020 (percent)",
    x = "Change in share price",
    y = "Wealth share of top 10 percent",
    caption = "Chart: @dzulfiqarfr | Source: Credit Suisse"
  ) +
  theme_dfr() +
  theme(
    axis.title = element_text(size = rel(0.8)),
    panel.grid.major.x = element_line(color = "#CFD8DC")
  )
#> Warning: Ignoring unknown parameters: check_overlap
#> Warning: Removed 2 rows containing non-finite values (stat_smooth).
#> Warning: Removed 2 rows containing missing values (geom_point).
#> Warning: Removed 2 rows containing missing values (geom_text_repel).
#> Warning: ggrepel: 11 unlabeled data points (too many overlaps). Consider
#> increasing max.overlaps
```

<img src="README_files/figure-gfm/plot top wealth share and stock price movement-1.png" width="70%" style="display: block; margin: auto;" />

Of course, if we have more, better data, we will have a better
understanding of the relationship between wealth distribution among the
top group and share price movements during the pandemic. We may also
want to take into account monetary policy of each country as a response
to the pandemic.

But that’s for another time – and other people.
