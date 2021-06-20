Population distribution
================
2021-06-20

This is a documentation of how I analyzed population distribution over
time across Indonesia. I prepared this analysis for
<a href="https://www.thejakartapost.com/news/2021/05/23/indonesia-remains-java-centric-despite-jokowis-infrastructure-campaign.html" target="_blank">this article</a>.

## Packages

Load the packages.

``` r
library(tidyverse)
library(lubridate)
library(httr)
library(jsonlite)
library(patchwork)
library(sf)
library(nusantr)
library(magick)
library(viridis)
```

## Data

### Population

I have downloaded the population data from Statistics Indonesia (BPS).
The population data for 1971-2010 period is available at
<a href="https://www.bps.go.id/statictable/2009/02/20/1267/jumlah-penduduk-hasil-sensus-penduduk-sp-dan-survei-penduduk-antar-sensus-supas-menurut-provinsi-1971---2015.html" target="_blank">this link</a>
and the latest population data at
<a href="https://www.bps.go.id/pressrelease/2021/01/21/1854/hasil-sensus-penduduk-2020.html" target="_blank">this link</a>.
I had to extract the latest data from a table in the *Berita Resmi
Statistik* (BRS) pdf using Tabula.

``` r
# import

## 1971-2010
pop_7110_raw <- read_csv(
  "data/bps_pop-1971-2010_raw.csv",
  skip = 3,
  na = c("", "-")
) %>%
  rename(prov = 1) %>%
  dplyr::filter(!str_detect(prov, "Catatan|Sumber"))
#> Warning: Missing column names filled in: 'X1' [1]

## 2020
pop_2020_raw <- read_csv("data/bps_pop-2020_raw.csv", skip = 2) %>%
  select(-c(2:3, ncol(.))) %>%
  rename(prov = 1, `2020` = 2)
#> Warning: Missing column names filled in: 'X5' [5]

## correct data type
pop_2020_raw <- pop_2020_raw %>%
  mutate(`2020` = as.numeric(str_remove_all(`2020`, " ")))

# join all data
pop_raw <- pop_2020_raw %>%
  left_join(pop_7110_raw, by = "prov")

# glimpse
glimpse(pop_raw)
#> Rows: 35
#> Columns: 8
#> $ prov   <chr> "Aceh", "Sumatera Utara", "Sumatera Barat", "Riau", "Jambi", "S~
#> $ `2020` <dbl> 5274871, 14799361, 5534472, 6394087, 3548228, 8467432, 2010670,~
#> $ `1971` <dbl> 2008595, 6621831, 2793196, 1641545, 1006084, 3440573, 519316, 2~
#> $ `1980` <dbl> 2611271, 8360894, 3406816, 2168535, 1445994, 4629801, 768064, 4~
#> $ `1990` <dbl> 3416156, 10256027, 4000207, 3303976, 2020568, 6313074, 1179122,~
#> $ `1995` <dbl> 3847583, 11114667, 4323170, 3900534, 2369959, 7207545, 1409117,~
#> $ `2000` <dbl> 3930905, 11649655, 4248931, 4957627, 2413846, 6899675, 1567432,~
#> $ `2010` <dbl> 4494410, 12982204, 4846909, 5538367, 3092265, 7450394, 1715518,~
```

Subset the total population figures.

``` r
# subset total
pop_total <- pop_raw %>%
  dplyr::filter(prov == "INDONESIA") %>%
  select(-1) %>%
  pivot_longer(everything(), names_to = "yr", values_to = "pop_total")

# glimpse
glimpse(pop_total)
#> Rows: 7
#> Columns: 2
#> $ yr        <chr> "2020", "1971", "1980", "1990", "1995", "2000", "2010"
#> $ pop_total <dbl> 270203917, 119208229, 147490298, 179378946, 194754808, 20626~
```

To see the population distribution by island, we need to create vectors
of island groups to group the observations. Then, we calculate the
population distribution.

``` r
# create island groups
java <- c(
  "DKI Jakarta",
  "Jawa Tengah",
  "Jawa Timur",
  "Jawa Barat",
  "DI Yogyakarta",
  "Banten"
)

kalimantan <- c(
  "Kalimantan Tengah",
  "Kalimantan Timur",
  "Kalimantan Barat",
  "Kalimantan Selatan",
  "Kalimantan Utara"
)

sulawesi <- c(
  "Sulawesi Tengah",
  "Sulawesi Tenggara",
  "Sulawesi Barat",
  "Sulawesi Utara",
  "Sulawesi Selatan",
  "Gorontalo"
)

sumatera <- c(
  "Aceh",
  "Sumatera Barat",
  "Jambi",
  "Bengkulu",
  "Kepulauan Bangka Belitung",
  "Sumatera Utara",
  "Riau",
  "Sumatera Selatan",
  "Lampung",
  "Kepulauan Riau"
)

bali_nt <- c(
  "Bali",
  "Nusa Tenggara Timur",
  "Nusa Tenggara Barat"
)

maluku_papua <- c(
  "Maluku Utara",
  "Maluku",
  "Papua",
  "Papua Barat"
)

# tidy, add island group, total pop
pop_tidy <- pop_raw %>%
  dplyr::filter(prov != "INDONESIA") %>%
  mutate(
    island_group = as_factor(
      case_when(
        prov %in% java ~ "Java",
        prov %in% sumatera ~ "Sumatra",
        prov %in% sulawesi ~ "Sulawesi",
        prov %in% kalimantan ~ "Kalimantan",
        prov %in% bali_nt ~ "Bali & Nusa Tenggara",
        TRUE ~ "Maluku & Papua"
      )
    )
  ) %>%
  pivot_longer(2:(ncol(.) -1), names_to = "yr", values_to = "pop") %>%
  left_join(pop_total, by = "yr") %>%
  mutate(yr = as.numeric(yr)) %>%
  arrange(island_group, prov, yr)

# calculate population distribution by island group
pop_trf <- pop_tidy %>%
  group_by(island_group, yr) %>%
  mutate(
    pop_island = sum(pop, na.rm = T),
    pop_dis = pop_island / pop_total * 100
  ) %>%
  ungroup(yr) %>%
  mutate(pop_dis_chg = pop_dis - dplyr::lag(pop_dis, 1)) %>%
  ungroup() %>%
  rename(pop_prov = pop)

# glimpse
glimpse(pop_trf)
#> Rows: 238
#> Columns: 8
#> $ prov         <chr> "Aceh", "Aceh", "Aceh", "Aceh", "Aceh", "Aceh", "Aceh", "~
#> $ island_group <fct> Sumatra, Sumatra, Sumatra, Sumatra, Sumatra, Sumatra, Sum~
#> $ yr           <dbl> 1971, 1980, 1990, 1995, 2000, 2010, 2020, 1971, 1980, 199~
#> $ pop_prov     <dbl> 2008595, 2611271, 3416156, 3847583, 3930905, 4494410, 527~
#> $ pop_total    <dbl> 119208229, 147490298, 179378946, 194754808, 206264595, 23~
#> $ pop_island   <dbl> 20808148, 28016160, 36506703, 40830334, 43309707, 5063093~
#> $ pop_dis      <dbl> 17.45529, 18.99526, 20.35172, 20.96499, 20.99716, 21.3056~
#> $ pop_dis_chg  <dbl> NA, 1.53996125, 1.35646499, 0.61327289, 0.03216589, 0.308~
```

### GDP

We now read the gross regional domestic product (GRDP) data to see how
the population distribution follows the country’s development pattern.

We can import the GRDP at constant 2010 prices data through
<a href="https://webapi.bps.go.id/" target="_blank">BPS’s application programming interface (API)</a>.

``` r
# api

## key
BPS_KEY <- Sys.getenv("BPS_KEY")

## url
base_url <- "https://webapi.bps.go.id/v1/api/list"

# request data
grdp_req <- GET(
  base_url,
  query = list(
    model = "data",
    domain = "0000",
    var = "533",
    key = BPS_KEY
  )
)

# parse response
grdp_parsed <- content(grdp_req, "text") %>%
  fromJSON()

# component key
key_comp <- as_tibble(grdp_parsed$turvar)

# province key
key_prov <- as_tibble(grdp_parsed$vervar)

# year key
key_yr <- as_tibble(grdp_parsed$tahun)

# data
grdp_raw <- as_tibble(grdp_parsed$datacontent)

# tidy
grdp_tidy <- grdp_raw %>%
  pivot_longer(everything(), names_to = "key", values_to = "grdp") %>%
  separate(key, into = c("prov", "other"), sep = "533") %>%
  mutate(
    comp = case_when(
      str_detect(other, "^6") ~ str_sub(other, 1, 3),
      !str_detect(other, "^6") ~ str_sub(other, 1, 4)
    ),
    yr = case_when(
      str_detect(other, "^6") ~ str_sub(other, 4, 6),
      !str_detect(other, "^6") ~ str_sub(other, 5, 7)
    ),
    q = case_when(
      str_detect(other, "^6") ~ str_sub(other, 7, 8),
      !str_detect(other, "^6") ~ str_sub(other, 8, 9)
    )
  ) %>%
  dplyr::filter(comp %in% c("694", "1550"), q == "35") %>%
  select(prov, yr, q, grdp)
#> Warning: Expected 2 pieces. Additional pieces discarded in 245 rows [42, 98,
#> 154, 210, 266, 322, 378, 448, 504, 560, 616, 672, 728, 784, 854, 910, 966, 1022,
#> 1078, 1134, ...].

# tidy province names
key_prov$label <- key_prov$label %>%
  str_replace_all("KEP.", "Kepulauan") %>%
  str_to_title() %>%
  str_replace_all(c("Dki" = "DKI", "Di" = "DI"))

# create date variable, replace province key
grdp_tidy <- grdp_tidy %>%
  mutate(
    prov = str_replace_all(prov, deframe(key_prov)),
    yr = str_replace_all(yr, deframe(key_yr))
  ) %>%
  select(prov, yr, grdp)

# subset national gdp
gdp_ntl <- grdp_tidy %>%
  dplyr::filter(prov == "34 Provinsi") %>%
  select(-1) %>%
  rename(gdp_ntl = grdp)

# add island group, total gdp
grdp_tidy <- grdp_tidy %>%
  mutate(
    island_group = as_factor(case_when(
      prov %in% java ~ "Java",
      prov %in% sumatera ~ "Sumatra",
      prov %in% sulawesi ~ "Sulawesi",
      prov %in% kalimantan ~ "Kalimantan",
      prov %in% bali_nt ~ "Bali & Nusa Tenggara",
      prov %in% maluku_papua ~ "Maluku & Papua"
    ))
  ) %>%
  left_join(gdp_ntl, by = "yr")

# calculate gdp distribution by island group
grdp_trf <- grdp_tidy %>%
  dplyr::filter(prov != "34 Provinsi") %>%
  mutate(yr = as.numeric(yr)) %>%
  group_by(island_group, yr) %>%
  mutate(
    grdp_island = sum(grdp, na.rm = T),
    grdp_dis = grdp_island / gdp_ntl * 100
  ) %>%
  ungroup(yr) %>%
  mutate(grdp_dis_chg = grdp_dis - dplyr::lag(grdp_dis, 1)) %>%
  ungroup()

# glimpse
glimpse(grdp_trf)
#> Rows: 374
#> Columns: 8
#> $ prov         <chr> "Aceh", "Aceh", "Aceh", "Aceh", "Aceh", "Aceh", "Aceh", "~
#> $ yr           <dbl> 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 201~
#> $ grdp         <dbl> 101545237, 104874211, 108914898, 111755827, 113490359, 11~
#> $ island_group <fct> Sumatra, Sumatra, Sumatra, Sumatra, Sumatra, Sumatra, Sum~
#> $ gdp_ntl      <dbl> 6864133129, 7286914843, 7735785460, 8177822287, 860363597~
#> $ grdp_island  <dbl> 1536556815, 1631726678, 1725535859, 1810953378, 189416981~
#> $ grdp_dis     <dbl> 22.38530, 22.39256, 22.30589, 22.14469, 22.01592, 21.7101~
#> $ grdp_dis_chg <dbl> NA, 0.007258305, -0.086667797, -0.161201031, -0.128767460~
```

### Migration

We can also check the net lifetime migration, which partly contributes
to a province’s population growth. BPS published
<a href="https://bps.go.id/publication/2020/12/02/725d484ca73434e95d4d4b9d/profil-migran-hasil-survei-sosial-ekonomi-nasional-2019.html" target="_blank">the latest migrant profile data</a>
in December 2020. It was based on the National Economic and Social
Survey (Susenas) in March 2019.

I have downloaded the data, extracted it using Tabula and cleaned it a
little in a spreadsheet.

``` r
# import
lft_mig_raw <- read_csv("data/bps_lifetime-migration_cleaned.csv")

# tidy
lft_mig_tidy <- lft_mig_raw %>%
  separate(var, into = c("prov", "fig"), sep = "_") %>%
  mutate(
    fig = str_remove_all(fig, " "),
    fig = str_replace_all(fig, ",", "_")
  ) %>%
  separate(
    fig,
    into = c("lft_in", "lft_in_prop", "lft_out", "lft_out_prop", "lft_net", "lft_net_prop"),
    sep = "_"
  )

# correct data type
lft_mig_tidy[, 2:ncol(lft_mig_tidy)] <- lapply(
  lft_mig_tidy[, 2:ncol(lft_mig_tidy)],
  function(x) {as.numeric(x)}
)

# glimpse
glimpse(lft_mig_tidy)
#> Rows: 34
#> Columns: 7
#> $ prov         <chr> "Aceh", "Sumatera Utara", "Sumatera Barat", "Riau", "Jamb~
#> $ lft_in       <dbl> 232693, 586721, 405024, 1998658, 688349, 974161, 372758, ~
#> $ lft_in_prop  <dbl> 4.4, 4.0, 7.5, 28.8, 19.1, 11.5, 18.8, 16.6, 14.8, 46.4, ~
#> $ lft_out      <dbl> 307845, 2341095, 1232335, 409949, 210704, 896299, 149944,~
#> $ lft_out_prop <dbl> 5.7, 14.4, 19.7, 7.7, 6.7, 10.7, 8.5, 11.1, 7.4, 10.7, 32~
#> $ lft_net      <dbl> -75152, -1754374, -827311, 1588709, 477645, 77862, 222814~
#> $ lft_net_prop <dbl> -1.4, -11.4, -14.2, 25.9, 14.2, 0.9, 11.9, 6.4, 8.3, 50.0~
```

## Plot

Plot the population and GDP distribution over time.

``` r
# population distribution ----

## remove duplicates, 1995 observation from inter-census population survey, reorder island group
pop_cleaned <- pop_trf %>%
  mutate(island_group = fct_reorder(island_group, pop_dis)) %>%
  select(island_group, yr, pop_dis) %>%
  group_by(island_group) %>%
  dplyr::filter(!duplicated(yr), yr != 1995) %>%
  ungroup()

## population distribution plot
plot_pop_dis <- ggplot(pop_cleaned, aes(yr, pop_dis, fill = island_group)) +
  geom_col(position = "stack", width = 6.5) +
  scale_x_continuous(breaks = c(1971, seq(1980, 2020, 10))) +
  scale_y_continuous(
    limits = c(0, 100),
    expand = c(0, 0),
    position = "right"
  ) +
  labs(subtitle = "Population") +
  scale_fill_viridis(discrete = T, option = "cividis", direction = -1) +
  theme(
    text = element_text(size = 12),
    axis.title = element_blank(),
    axis.text = element_text(size = rel(0.75)),
    axis.ticks.y = element_blank(),
    axis.line.x = element_line(color = "black"),
    legend.title = element_blank(),
    legend.text = element_text(size = 6),
    legend.key = element_rect(fill = "transparent"),
    legend.key.size = unit(0.3, "cm"),
    legend.position = c(0.485, 1.15),
    legend.direction = "horizontal",
    panel.background = element_rect(fill = "white"),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "#CFD8DC"),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    plot.subtitle = element_text(size = rel(0.9), margin = margin(b = 35))
  )

# gdp distribution ----

## remove duplicates, reorder island group
grdp_cleaned <- grdp_trf %>%
  mutate(island_group = fct_reorder(island_group, grdp_dis)) %>%
  select(island_group, yr, grdp_dis, grdp_dis_chg) %>%
  group_by(island_group) %>%
  dplyr::filter(!duplicated(yr)) %>%
  ungroup()

## gdp distribution plot
plot_gdp_dis <- ggplot(
  grdp_cleaned, 
  aes(yr, grdp_dis, fill = island_group)
) +
  geom_col(position = "stack", show.legend = T, width = 0.65) +
  scale_x_continuous(breaks = seq(2010, 2020, 2)) +
  scale_y_continuous(expand = c(0, 0), position = "right") +
  labs(subtitle = "GDP") +
  scale_fill_viridis(discrete = T, option = "inferno", direction = -1) +
  theme(
    text = element_text(size = 12),
    axis.title = element_blank(),
    axis.text = element_text(size = rel(0.75)),
    axis.ticks.y = element_blank(),
    axis.line.x = element_line(color = "black"),
    legend.title = element_blank(),
    legend.text = element_text(size = 6),
    legend.key = element_rect(fill = "transparent"),
    legend.key.size = unit(0.3, "cm"),
    legend.position = c(0.45, 1.15),
    legend.direction = "horizontal",
    panel.background = element_rect(fill = "white"),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "#CFD8DC"),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    plot.subtitle = element_text(size = rel(0.9), margin = margin(b = 35))
  )

# patchwork ----
plot_pop_dis + plot_gdp_dis +
  plot_annotation(
    title = "Population concentration follows development pattern",
    subtitle = "Distribution by island group (percent)",
    caption = "Chart: @dzulfiqarfr | Data: Statistics Indonesia (BPS)",
    theme = theme(
      text = element_text(size = 12),
      plot.title = element_text(face = "bold"),
      plot.subtitle = element_text(margin = margin(b = 15)),
      plot.caption = element_text(
        size = rel(0.8),
        color = "#37474F",
        hjust = 0,
        margin = margin(t = 15)
      )
    )
  )
```

<img src="README_files/figure-gfm/population and gdp distribution plot-1.png" width="70%" style="display: block; margin: auto;" />

Plot the net lifetime migration.

``` r
# indonesian map
id_map <- id_map("indonesia")

# join map and migration data
mig_map <- id_map %>% 
  left_join(lft_mig_tidy, by = c("provinsi" = "prov")) %>% 
  select(provinsi, geometry, lft_net_prop)

# plot
ggplot(mig_map) +
  geom_sf(aes(fill = lft_net_prop), color = "white", lwd = .25) +
  labs(
    title = "Distance still matters",
    subtitle = "Net lifetime migration relative to population in 2019 (percent)",
    caption = "Chart: @dzulfiqarfr | Source: Statistics Indonesia (BPS)"
  ) +
  scale_fill_fermenter(
    breaks = c(-15, -10, -5, 0, 5, 10, 15),
    expand = c(0, 0),
    palette = "RdBu", 
    direction = 1
  ) +
  coord_sf() +
  theme_void() +
  theme(
    text = element_text(size = 12),
    legend.position = c(.85, 1),
    legend.direction = "horizontal",
    legend.title = element_blank(),
    legend.text = element_text(size = 7.5),
    legend.key.height = unit(.25, "cm"),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(margin = margin(b = 15)),
    plot.caption = element_text(
      size = rel(0.8),
      color = "#37474F",
      hjust = 0,
      margin = margin(t = 15)
    )
  )
```

<img src="README_files/figure-gfm/net lifetime migration plot-1.png" width="70%" style="display: block; margin: auto;" />

Plot the share of incoming migrants against the GRDP per person. Also,
adds population data.

``` r
# subset latest grdp data, convert to billion
grdp_latest <- grdp_tidy %>% 
  dplyr::filter(prov != "34 Provinsi", yr == "2020") %>% 
  select(-c(gdp_ntl, yr))

# extract net lifetime migration relative to population
lft_in_prop <- lft_mig_tidy %>% 
  select(prov, lft_in_prop)

# latest population data
pop_latest <- pop_2020_raw %>% 
  dplyr::filter(prov != "INDONESIA") %>% 
  rename(pop = 2)

# join all data
grdp_mig_pop <- grdp_latest %>% 
  left_join(lft_in_prop) %>% 
  left_join(pop_latest) %>% 
  select(prov, island_group, pop, grdp, lft_in_prop)

# calculate grdp per capita, reorder island group by population
grdpPercap_mig_pop <- grdp_mig_pop %>% 
  mutate(
    grdp_perCap = grdp / pop, 
    island_group = fct_reorder(island_group, pop)
  )

# plot
ggplot(grdpPercap_mig_pop, aes(grdp_perCap, lft_in_prop)) +
  geom_point(
    aes(fill = island_group, size = pop),
    pch = 21,
    color = "white",
    alpha = 0.5
  ) +
  geom_smooth(
    method = "lm",
    color = "black",
    lwd = 0.5,
    lty = "dashed",
    se = F
  ) +
  scale_x_log10(
    breaks = c(10, 20, 50, 100, 200),
    limits = c(10, 200)
  ) +
  scale_y_continuous(
    breaks = seq(0, 50, 10),
    limits = c(0, 50),
    expand = c(0, 0),
    position = "right"
  ) +
  scale_fill_brewer(palette = "Dark2", direction = -1) +
  scale_size(range = c(5, 10)) +
  guides(
    size = "none",
    fill = guide_legend(ncol = 1, reverse = T)
  ) +
  labs(
    title = "Expecting higher income",
    subtitle = "GRDP per capita (2020) & incoming lifetime migrants relative to population (2019).\nCircle size represents population",
    x = "GRDP per capita\n(million rupiah, log scale)",
    y = "Share of incoming lifetime migrants\n(percent)",
    caption = "Chart: @dzulfiqarfr | Source: Statistics Indonesia (BPS)"
  ) +
  theme(
    text = element_text(size = 12),
    axis.title = element_text(size = rel(0.8)),
    axis.text = element_text(size = rel(0.8)),
    axis.ticks.y = element_blank(),
    axis.line.x = element_line(color = "black"),
    legend.title = element_blank(),
    legend.text = element_text(size = 7.5),
    legend.key = element_rect(fill = "transparent"),
    legend.key.size = unit(0.3, "cm"),
    legend.position = c(0.05, 0.95),
    legend.justification = c(0, 1),
    legend.direction = "horizontal",
    panel.background = element_rect(fill = "white"),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "#CFD8DC"),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = rel(0.95), margin = margin(b = 15)),
    plot.caption = element_text(
      size = rel(0.8),
      color = "#37474F",
      hjust = 0,
      margin = margin(t = 15)
    )
  )
#> Warning: Removed 2 rows containing missing values (geom_smooth).
```

<img src="README_files/figure-gfm/migration and grdp-1.png" width="70%" style="display: block; margin: auto;" />

For the publication, I used [Datawrapper](https://datawrapper.de) to map
the share of net lifetime migration and plot the share of incoming
lifetime migrants against GRDP per capita.