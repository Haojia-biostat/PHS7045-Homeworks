Homework 1b - Data Wrangling
================
Haojia Li
2/8/23

The learning objectives are to conduct data wrangling and generate some
summary statistics.

You will need to download two datasets from
https://github.com/USCbiostats/data-science-data. The
[individual](https://raw.githubusercontent.com/USCbiostats/data-science-data/master/01_chs/chs_individual.csv)
and
[regional](https://raw.githubusercontent.com/USCbiostats/data-science-data/master/01_chs/chs_regional.csv)
CHS datasets in `01_chs`. The individual data includes personal and
health characteristics of children in 12 communities across Southern
California. The regional data include air quality measurements at the
community level. Once downloaded, you can merge these datasets using the
location variable. Once combined, you will need to do the following:

``` r
# load data.table
library(data.table)
# download data
individual <- fread("https://raw.githubusercontent.com/USCbiostats/data-science-data/master/01_chs/chs_individual.csv")
regional <- fread("https://raw.githubusercontent.com/USCbiostats/data-science-data/master/01_chs/chs_regional.csv")
# merge data
intersect(colnames(individual), colnames(regional))
```

    [1] "townname"

``` r
dat <- merge(individual, regional, all.x = T)
```

1.  After merging the data, make sure you don’t have any duplicates by
    counting the number of rows. Make sure it matches.

In the case of missing values, impute data using the average within the
variables “male” and “hispanic.” If you are interested (and feel
adventurous) in the theme of Data Imputation, take a look at this paper
on “Multiple Imputation” using the Amelia R package
[here](https://gking.harvard.edu/files/gking/files/amelia_jss.pdf).

``` r
# number of obs
nrow(dat)
```

    [1] 1200

``` r
# number of unique individual
length(unique(dat$sid))
```

    [1] 1200

``` r
# impute missing values in male and hispanic
mean_male <- mean(dat$male, na.rm = T)
mean_hispanic <- mean(dat$hispanic, na.rm = T)
dat[, male     := fifelse(male == NA_integer_, mean_male, male)]
dat[, hispanic := fifelse(hispanic == NA_integer_, mean_hispanic, hispanic)]
```

2.  Create a new categorical variable named “obesity_level” using the
    BMI measurement (underweight BMI\<14; normal BMI 14-22; overweight
    BMI 22-24; obese BMI\>24). To make sure the variable is rightly
    coded, create a summary table that contains the minimum BMI, maximum
    BMI, and the total number of observations per category.

``` r
dat[, obesity_level := cut(bmi, c(0, 14, 22, 24, Inf), labels = c("underweight", "normal", "overweight", "obese"))]
obesity_summary <- dat[, .(min_bmi = min(bmi), max_bmi = max(bmi), nobs = .N), by = obesity_level]
setorder(obesity_summary, min_bmi)
obesity_summary
```

       obesity_level  min_bmi  max_bmi nobs
    1:          <NA>       NA       NA   89
    2:   underweight 11.29640 13.98601   35
    3:        normal 14.00380 21.96387  886
    4:    overweight 22.02353 23.99650   87
    5:         obese 24.00647 41.26613  103

3.  Create another categorical variable named “smoke_gas_exposure” that
    summarizes “Second Hand Smoke” and “Gas Stove.” The variable should
    have four categories in total.

``` r
dat[, smoke_gas_exposure := 
      ifelse(smoke == 1 & gasstove == 1, "second hand smoke, gas stove",
             ifelse(smoke == 1 & gasstove == 0, "second hand smoke, no gas stove",
                    ifelse(smoke == 0 & gasstove == 1, "no second hand smoke, gas stove",
                           ifelse(smoke == 0 & gasstove == 0, "no second hand smoke, no gas stove",
                                  NA_character_))))]
table(dat$smoke_gas_exposure, useNA = "i")
```


       no second hand smoke, gas stove no second hand smoke, no gas stove 
                                   739                                214 
          second hand smoke, gas stove    second hand smoke, no gas stove 
                                   151                                 36 
                                  <NA> 
                                    60 

4.  Create four summary tables showing the average (or proportion, if
    binary) and sd of “Forced expiratory volume in 1 second (ml)” and
    asthma indicator by town, sex, obesity level, and
    “smoke_gas_exposure.”

``` r
# average and sd of Forced expiratory volume in 1 second (ml)
dat[, .(average = mean(fev, na.rm = T), std_dev = sd(fev, na.rm = T)), by = townname]
```

             townname  average  std_dev
     1:        Alpine 2089.014 298.2039
     2:    Atascadero 2079.374 331.8782
     3: Lake Elsinore 2039.787 317.6112
     4:  Lake Gregory 2091.665 337.8286
     5:     Lancaster 2002.550 337.1053
     6:        Lompoc 2038.227 367.4474
     7:    Long Beach 1983.896 330.6271
     8:     Mira Loma 1984.726 336.6416
     9:     Riverside 1986.212 289.7415
    10:     San Dimas 2027.806 321.9740
    11:   Santa Maria 2022.553 330.0457
    12:        Upland 2027.284 357.2010

``` r
dat[, .(average = mean(fev, na.rm = T), std_dev = sd(fev, na.rm = T)), by = male]
```

       male  average  std_dev
    1:   NA 2031.265 330.6684

``` r
dat[, .(average = mean(fev, na.rm = T), std_dev = sd(fev, na.rm = T)), by = obesity_level]
```

       obesity_level  average  std_dev
    1:        normal 1997.974 309.4085
    2:    overweight 2224.322 317.4261
    3:         obese 2269.295 325.5054
    4:          <NA>      NaN       NA
    5:   underweight 1686.800 300.0803

``` r
dat[, .(average = mean(fev, na.rm = T), std_dev = sd(fev, na.rm = T)), by = smoke_gas_exposure]
```

                       smoke_gas_exposure  average  std_dev
    1: no second hand smoke, no gas stove 2059.943 342.5625
    2:                               <NA> 1999.783 364.9553
    3:    second hand smoke, no gas stove 2064.346 333.2266
    4:    no second hand smoke, gas stove 2026.308 328.1240
    5:       second hand smoke, gas stove 2019.974 313.2327

``` r
# proportion of asthma
dat[, .(proportion = mean(asthma, na.rm = T) * 100), by = townname]
```

             townname proportion
     1:        Alpine   11.34021
     2:    Atascadero   25.51020
     3: Lake Elsinore   12.63158
     4:  Lake Gregory   15.15152
     5:     Lancaster   16.49485
     6:        Lompoc   11.34021
     7:    Long Beach   13.54167
     8:     Mira Loma   15.78947
     9:     Riverside   11.00000
    10:     San Dimas   17.17172
    11:   Santa Maria   13.40206
    12:        Upland   12.12121

``` r
dat[, .(proportion = mean(asthma, na.rm = T) * 100), by = male]
```

       male proportion
    1:   NA   14.62789

``` r
dat[, .(proportion = mean(asthma, na.rm = T) * 100), by = obesity_level]
```

       obesity_level proportion
    1:        normal  14.104046
    2:    overweight  16.470588
    3:         obese  21.000000
    4:          <NA>  13.095238
    5:   underweight   8.571429

``` r
dat[, .(proportion = mean(asthma, na.rm = T) * 100), by = smoke_gas_exposure]
```

                       smoke_gas_exposure proportion
    1: no second hand smoke, no gas stove   14.76190
    2:                               <NA>   14.89362
    3:    second hand smoke, no gas stove   17.14286
    4:    no second hand smoke, gas stove   14.77428
    5:       second hand smoke, gas stove   13.01370
