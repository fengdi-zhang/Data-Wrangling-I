tidy_data
================

``` r
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.2 ──
    ## ✔ ggplot2 3.3.6      ✔ purrr   0.3.4 
    ## ✔ tibble  3.1.8      ✔ dplyr   1.0.10
    ## ✔ tidyr   1.2.0      ✔ stringr 1.4.1 
    ## ✔ readr   2.1.2      ✔ forcats 0.5.2 
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()

``` r
options(tibble.print_min = 5)
```

``` r
pulse_df = 
  haven::read_sas("./data/data_import_examples/public_pulse_data.sas7bdat") %>%
  janitor::clean_names() %>%
  pivot_longer(
    bdi_score_bl:bdi_score_12m, #cols to combine
    names_to = "visit", #cols become var visit
    values_to = "bdi", #values become var bdi
    names_prefix = "bdi_score_" #drops this prefix in new var
  ) %>%
  mutate(
    visit = replace(visit, visit == "bl", "00m"), #replace bl in visit to 00m
    visit = factor(visit) #convert visit to factor var
  ) %>%
  arrange(id, visit) #order by id and then visit
```

``` r
litter_df = read_csv("./data/data_import_examples/FAS_litters.csv") %>%
  janitor::clean_names() %>%
  select(litter_number, gd0_weight, gd18_weight) %>%
  pivot_longer(
    gd0_weight:gd18_weight,
    names_to= "gd",
    values_to = "weight"
  ) %>%
  mutate(gd = recode(gd, "gd0_weight" = 0, "gd18_weight" = 18)) #in col gd, convert gd0_weight to 0 and gd18_weight to 18
```

    ## Rows: 49 Columns: 8
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (2): Group, Litter Number
    ## dbl (6): GD0 weight, GD18 weight, GD of Birth, Pups born alive, Pups dead @ ...
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
analysis_result = tibble(
  group = c("treatment", "treatment", "placebo", "placebo"),
  time = c("pre", "post", "pre", "post"),
  mean = c(4, 8, 3.5, 4)
)
```

``` r
analysis_result_wide = pivot_wider(
  analysis_result,
  names_from = "time", #col names are from var time
  values_from = "mean" #values in df are from var mean
)
```

``` r
fellowship_ring = 
  readxl::read_excel("./data/data_import_examples/LotR_Words.xlsx", range = "B3:D6") %>%
  mutate(movie = "fellowship_ring")

two_towers = 
  readxl::read_excel("./data/data_import_examples/LotR_Words.xlsx", range = "F3:H6") %>%
  mutate(movie = "two_towers")

return_king = 
  readxl::read_excel("./data/data_import_examples/LotR_Words.xlsx", range = "J3:L6") %>%
  mutate(movie = "return_king")
```

``` r
lotr_tidy = 
  bind_rows(fellowship_ring, two_towers, return_king) %>% #bind rows of the 3 df because they all have same col name
  janitor::clean_names() %>%
  pivot_longer(
    female:male,
    names_to = "gender",
    values_to = "words") %>%
  mutate(race = str_to_lower(race)) %>%
  select(movie, everything())
```

``` r
pup_data = 
  read_csv("./data/data_import_examples/FAS_pups.csv") %>%
  janitor::clean_names() %>%
  mutate(
    sex = recode(sex, `1` = "male", `2` = "female"), #`1` means looking for num 1
    sex = factor(sex)
  )
```

    ## Rows: 313 Columns: 6
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (1): Litter Number
    ## dbl (5): Sex, PD ears, PD eyes, PD pivot, PD walk
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
litter_data = 
  read_csv("./data/data_import_examples/FAS_litters.csv") %>%
  janitor::clean_names() %>%
  separate(group, into = c("dose", "day_of_tx"), sep = 3) %>% #separate after the third character
  mutate(
    wt_gain = gd18_weight - gd0_weight,
    dose = str_to_lower(dose)
  )
```

    ## Rows: 49 Columns: 8
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (2): Group, Litter Number
    ## dbl (6): GD0 weight, GD18 weight, GD of Birth, Pups born alive, Pups dead @ ...
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
fas_data = 
  left_join(pup_data, litter_data, by = "litter_number")
```
