Data Import
================

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.2 ──
    ## ✔ ggplot2 3.3.6      ✔ purrr   0.3.4 
    ## ✔ tibble  3.1.8      ✔ dplyr   1.0.10
    ## ✔ tidyr   1.2.0      ✔ stringr 1.4.1 
    ## ✔ readr   2.1.2      ✔ forcats 0.5.2 
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()

## Data Import: CSVs

Let’s import data using the `readr` package.

``` r
litters_df = read_csv("data/data_import_examples/FAS_litters.csv")
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
litters_df = janitor::clean_names(litters_df)
#package::function lets you use a function from a package
#janitor::clean_names cleans variable names, convert column names to lower snake case
```

\#Looking at data

``` r
litters_df #show whole df
```

    ## # A tibble: 49 × 8
    ##    group litter_number   gd0_weight gd18_weight gd_of_…¹ pups_…² pups_…³ pups_…⁴
    ##    <chr> <chr>                <dbl>       <dbl>    <dbl>   <dbl>   <dbl>   <dbl>
    ##  1 Con7  #85                   19.7        34.7       20       3       4       3
    ##  2 Con7  #1/2/95/2             27          42         19       8       0       7
    ##  3 Con7  #5/5/3/83/3-3         26          41.4       19       6       0       5
    ##  4 Con7  #5/4/2/95/2           28.5        44.1       19       5       1       4
    ##  5 Con7  #4/2/95/3-3           NA          NA         20       6       0       6
    ##  6 Con7  #2/2/95/3-2           NA          NA         20       6       0       4
    ##  7 Con7  #1/5/3/83/3-3/2       NA          NA         20       9       0       9
    ##  8 Con8  #3/83/3-3             NA          NA         20       9       1       8
    ##  9 Con8  #2/95/3               NA          NA         20       8       0       8
    ## 10 Con8  #3/5/2/2/95           28.5        NA         20       8       0       8
    ## # … with 39 more rows, and abbreviated variable names ¹​gd_of_birth,
    ## #   ²​pups_born_alive, ³​pups_dead_birth, ⁴​pups_survive

``` r
tail(litters_df, 5) #show last 5 rows
```

    ## # A tibble: 5 × 8
    ##   group litter_number gd0_weight gd18_weight gd_of_birth pups_…¹ pups_…² pups_…³
    ##   <chr> <chr>              <dbl>       <dbl>       <dbl>   <dbl>   <dbl>   <dbl>
    ## 1 Low8  #100                20          39.2          20       8       0       7
    ## 2 Low8  #4/84               21.8        35.2          20       4       0       4
    ## 3 Low8  #108                25.6        47.5          20       8       0       7
    ## 4 Low8  #99                 23.5        39            20       6       0       5
    ## 5 Low8  #110                25.5        42.7          20       7       0       6
    ## # … with abbreviated variable names ¹​pups_born_alive, ²​pups_dead_birth,
    ## #   ³​pups_survive

``` r
skimr::skim(litters_df)
```

|                                                  |            |
|:-------------------------------------------------|:-----------|
| Name                                             | litters_df |
| Number of rows                                   | 49         |
| Number of columns                                | 8          |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_   |            |
| Column type frequency:                           |            |
| character                                        | 2          |
| numeric                                          | 6          |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ |            |
| Group variables                                  | None       |

Data summary

**Variable type: character**

| skim_variable | n_missing | complete_rate | min | max | empty | n_unique | whitespace |
|:--------------|----------:|--------------:|----:|----:|------:|---------:|-----------:|
| group         |         0 |             1 |   4 |   4 |     0 |        6 |          0 |
| litter_number |         0 |             1 |   3 |  15 |     0 |       49 |          0 |

**Variable type: numeric**

| skim_variable   | n_missing | complete_rate |  mean |   sd |   p0 |   p25 |   p50 |   p75 | p100 | hist  |
|:----------------|----------:|--------------:|------:|-----:|-----:|------:|------:|------:|-----:|:------|
| gd0_weight      |        15 |          0.69 | 24.38 | 3.28 | 17.0 | 22.30 | 24.10 | 26.67 | 33.4 | ▃▇▇▆▁ |
| gd18_weight     |        17 |          0.65 | 41.52 | 4.05 | 33.4 | 38.88 | 42.25 | 43.80 | 52.7 | ▃▃▇▂▁ |
| gd_of_birth     |         0 |          1.00 | 19.65 | 0.48 | 19.0 | 19.00 | 20.00 | 20.00 | 20.0 | ▅▁▁▁▇ |
| pups_born_alive |         0 |          1.00 |  7.35 | 1.76 |  3.0 |  6.00 |  8.00 |  8.00 | 11.0 | ▁▃▂▇▁ |
| pups_dead_birth |         0 |          1.00 |  0.33 | 0.75 |  0.0 |  0.00 |  0.00 |  0.00 |  4.0 | ▇▂▁▁▁ |
| pups_survive    |         0 |          1.00 |  6.41 | 2.05 |  1.0 |  5.00 |  7.00 |  8.00 |  9.0 | ▁▃▂▇▇ |

\#Other file types

We need to read in an excel spreadsheet..

``` r
mlb_df = read_excel("data/data_import_examples/mlb11.xlsx")
head(mlb_df, 5)
```

    ## # A tibble: 5 × 12
    ##   team          runs at_bats  hits homer…¹ bat_avg strik…² stole…³  wins new_o…⁴
    ##   <chr>        <dbl>   <dbl> <dbl>   <dbl>   <dbl>   <dbl>   <dbl> <dbl>   <dbl>
    ## 1 Texas Range…   855    5659  1599     210   0.283     930     143    96   0.34 
    ## 2 Boston Red …   875    5710  1600     203   0.28     1108     102    90   0.349
    ## 3 Detroit Tig…   787    5563  1540     169   0.277    1143      49    95   0.34 
    ## 4 Kansas City…   730    5672  1560     129   0.275    1006     153    71   0.329
    ## 5 St. Louis C…   762    5532  1513     162   0.273     978      57    90   0.341
    ## # … with 2 more variables: new_slug <dbl>, new_obs <dbl>, and abbreviated
    ## #   variable names ¹​homeruns, ²​strikeouts, ³​stolen_bases, ⁴​new_onbase

``` r
lotR_words_df = read_excel(
  "data/data_import_examples/LotR_Words.xlsx", 
  range = "B3:D6" #only read part of the file
  )
```

## Still more formats

Read in a SAS dataset.

``` r
pulse_df = read_sas("data/data_import_examples/public_pulse_data.sas7bdat")
```

## Data export

``` r
write_csv(lotR_words_df, file = "data/lotR_words_df.csv")
```

## Why not base r?

``` r
dont_do_this_df = read.csv("data/data_import_examples/FAS_litters.csv")
#don't do it! Use read_csv instead
```
