Homework 9
================
Naomi Seula Lee
2022-04-19

-   <a href="#research-question" id="toc-research-question">Research
    Question</a>
-   <a href="#variables" id="toc-variables">Variables</a>
    -   <a href="#import-data" id="toc-import-data">Import Data</a>
    -   <a href="#variable-summary" id="toc-variable-summary">Variable
        Summary</a>
-   <a href="#model" id="toc-model">Model</a>
    -   <a href="#running-stan" id="toc-running-stan">Running Stan</a>
-   <a href="#results" id="toc-results">Results</a>

``` r
library(haven)   # for opening dta file
library(ggplot2)  # for plots
library(magrittr)  # for `%>%` operator
```

    ## Warning: package 'magrittr' was built under R version 4.0.5

``` r
library(here)
library(readxl)  # for reading excel files
```

    ## Warning: package 'readxl' was built under R version 4.0.5

``` r
library(modelsummary)  # for summarizing data
```

    ## Warning: package 'modelsummary' was built under R version 4.0.5

``` r
library(rstan)
rstan_options(auto_write = TRUE)  # save compiled STAN object
options(mc.cores = 2)  # use two cores
library(posterior)
```

    ## Warning: package 'posterior' was built under R version 4.0.5

``` r
library(bayesplot)
theme_set(theme_classic() +
    theme(panel.grid.major.y = element_line(color = "grey92")))
```

# Research Question

> Do we observe higher PM readings for months in which forest fires were
> frequently observed?

For our analysis project, we aim to estimate the missing values for
Particulate Matter(PM) records in Bogota, Columbia. In Bogota, Columbia,
forest fire is prevalent in the neighboring regions, and such forest
fire frequently happens in February and March. Forest fire is expected
to increase the PM concentrate measure, and thus, we first want to check
whether we observe higher PM readings for the months with frequent
forest fire.

# Variables

-   `pm10`: Particulate matter 10
    (micrograms/![m^3](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;m%5E3 "m^3"))
-   `pm25`: Particulate matter 2.5
    (micrograms/![m^3](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;m%5E3 "m^3"))
-   `myday`: Day of the year in format ddmmyyyy
-   `month`: Month of the year
-   `rain`: Rainfall (mm)
-   `Station_ID`: Pollutant tracking station ID
-   `tmp`: Temperature at 20m from ground level (degrees in Celsius)
-   `quadrant`: Quadrant in which the station is located (1 = NE, 2 =
    SE, 3 = SW, 4 = NW)
-   `tot`: Total number of forest fires within 1,500km X 1,500km from
    the center of Bogota, Columbia
-   `qtot`: `tot` by quadrant
-   `max`: Daily max forest Fire Radiative Power (FRP)
-   `qmax`: `tot` by quadrant
-   `fire`: A dummy variable indicating months that forest fires were
    frequently observed

## Import Data

``` r
data <- read_dta("/Users/seulalee/Desktop/Spring 2022/PSYC 573/Project/bayes_final1.dta")
```

## Variable Summary

``` r
datasummary(daily_avg_pm10  * 
                (N + Mean + SD + Min + Max + Histogram) ~
                factor(fire, labels = c("Non_Fire", "Fire")),
            data = data)
```

|                |           | Non_Fire |     Fire |
|:---------------|:----------|---------:|---------:|
| daily_avg_pm10 | N         |    11934 |     2301 |
|                | Mean      |    32.99 |    45.23 |
|                | SD        |    20.46 |    23.55 |
|                | Min       |     0.00 |     0.00 |
|                | Max       |   136.43 |   119.72 |
|                | Histogram |   ▄▇▇▅▂▁ | ▄▁▅▇▆▅▃▁ |

``` r
datasummary(daily_avg_pm25  * 
                (N + Mean + SD + Min + Max + Histogram) ~
                factor(fire, labels = c("Non_Fire", "Fire")),
            data = data)
```

|                |           | Non_Fire |  Fire |
|:---------------|:----------|---------:|------:|
| daily_avg_pm25 | N         |    10560 |  2061 |
|                | Mean      |    16.55 | 26.33 |
|                | SD        |     9.62 | 10.10 |
|                | Min       |     0.00 |  0.20 |
|                | Max       |    64.49 | 78.93 |
|                | Histogram |   ▃▇▆▄▂▁ | ▃▇▇▄▂ |

From the variable summary, we can see that months with frequent forest
fire (fire=1) has higher records for both PM10 and PM2.5 measure. In the
following part, we will use PM10 values to see whether there actually
exist meaningful differences between months with and without frequent
forest fire.

# Model

Let
![Y](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;Y "Y")
= PM10,
![F](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;F "F")
= Forest Fire

Model:

![\begin{aligned}
    Y\_{i, F = 0} & \sim N(\mu_1, \sigma_1) \\\\
    Y\_{i, F = 1} & \sim N(\mu_2, \sigma_2)
  \end{aligned}](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%5Cbegin%7Baligned%7D%0A%20%20%20%20Y_%7Bi%2C%20F%20%3D%200%7D%20%26%20%5Csim%20N%28%5Cmu_1%2C%20%5Csigma_1%29%20%5C%5C%0A%20%20%20%20Y_%7Bi%2C%20F%20%3D%201%7D%20%26%20%5Csim%20N%28%5Cmu_2%2C%20%5Csigma_2%29%0A%20%20%5Cend%7Baligned%7D "\begin{aligned}
    Y_{i, F = 0} & \sim N(\mu_1, \sigma_1) \\
    Y_{i, F = 1} & \sim N(\mu_2, \sigma_2)
  \end{aligned}")

Prior:

![\begin{aligned}
    \mu_1 & \sim N(36, 20) \\\\
    \mu_2 & \sim N(45, 23) \\\\
    \sigma_1 & \sim N^+(0, 2) \\\\
    \sigma_2 & \sim N^+(0, 2)
  \end{aligned}](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%5Cbegin%7Baligned%7D%0A%20%20%20%20%5Cmu_1%20%26%20%5Csim%20N%2836%2C%2020%29%20%5C%5C%0A%20%20%20%20%5Cmu_2%20%26%20%5Csim%20N%2845%2C%2023%29%20%5C%5C%0A%20%20%20%20%5Csigma_1%20%26%20%5Csim%20N%5E%2B%280%2C%202%29%20%5C%5C%0A%20%20%20%20%5Csigma_2%20%26%20%5Csim%20N%5E%2B%280%2C%202%29%0A%20%20%5Cend%7Baligned%7D "\begin{aligned}
    \mu_1 & \sim N(36, 20) \\
    \mu_2 & \sim N(45, 23) \\
    \sigma_1 & \sim N^+(0, 2) \\
    \sigma_2 & \sim N^+(0, 2)
  \end{aligned}")

## Running Stan

We used 4 chains, each with 4,000 iterations (first 2,000 as warm-ups).

``` r
# 1. form the data list for Stan
stan_dat <- with(data,
    list(N1 = sum(fire == 0),
         N2 = sum(fire == 1),
         y1 = daily_avg_pm10[which(fire == 0)],
         y2 = daily_avg_pm10[which(fire == 1)])
)
# 2. Run Stan
m1 <- stan(
    file = here("Project", "normal_2group.stan"),
    data = stan_dat,
    seed = 2834,  # for reproducibility
    iter = 4000
)
```

    ## Warning in readLines(file, warn = TRUE): incomplete final line found on '/Users/
    ## seulalee/Desktop/Spring 2022/PSYC 573/Project/normal_2group.stan'

# Results

``` r
print(m1, pars = c("mu1", "mu2", "sigma1", "sigma2"))
```

    ## Inference for Stan model: normal_2group.
    ## 4 chains, each with iter=4000; warmup=2000; thin=1; 
    ## post-warmup draws per chain=2000, total post-warmup draws=8000.
    ## 
    ##         mean se_mean   sd  2.5%   25%   50%   75% 97.5% n_eff Rhat
    ## mu1    31.92    0.00 0.18 31.56 31.79 31.92 32.04 32.26  7471    1
    ## mu2    37.14    0.01 0.46 36.23 36.83 37.14 37.46 38.04  7366    1
    ## sigma1 20.15    0.00 0.13 19.90 20.06 20.14 20.23 20.40  7794    1
    ## sigma2 22.54    0.00 0.30 21.96 22.33 22.54 22.75 23.14  7110    1
    ## 
    ## Samples were drawn using NUTS(diag_e) at Tue Apr 19 22:26:39 2022.
    ## For each parameter, n_eff is a crude measure of effective sample size,
    ## and Rhat is the potential scale reduction factor on split chains (at 
    ## convergence, Rhat=1).

As shown in the graph below, the chains mixed well.

``` r
mcmc_rank_hist(m1, pars = c("mu1", "mu2", "sigma1", "sigma2"))
```

![](HW9_NaomiLee_files/figure-gfm/rank-hist-m1-1.png)<!-- -->

``` r
summ_m1 <- as_draws_df(m1) %>%
    subset_draws(variable = c("mu1", "mu2", "sigma1", "sigma2")) %>%
    mutate_variables(`mu2 - mu1` = mu2 - mu1) %>%
    summarise_draws()
knitr::kable(summ_m1, digits = 2)
```

<table>
<thead>
<tr>
<th style="text-align:left;">
variable
</th>
<th style="text-align:right;">
mean
</th>
<th style="text-align:right;">
median
</th>
<th style="text-align:right;">
sd
</th>
<th style="text-align:right;">
mad
</th>
<th style="text-align:right;">
q5
</th>
<th style="text-align:right;">
q95
</th>
<th style="text-align:right;">
rhat
</th>
<th style="text-align:right;">
ess_bulk
</th>
<th style="text-align:right;">
ess_tail
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
mu1
</td>
<td style="text-align:right;">
31.92
</td>
<td style="text-align:right;">
31.92
</td>
<td style="text-align:right;">
0.18
</td>
<td style="text-align:right;">
0.18
</td>
<td style="text-align:right;">
31.62
</td>
<td style="text-align:right;">
32.21
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
7505.51
</td>
<td style="text-align:right;">
5984.33
</td>
</tr>
<tr>
<td style="text-align:left;">
mu2
</td>
<td style="text-align:right;">
37.14
</td>
<td style="text-align:right;">
37.14
</td>
<td style="text-align:right;">
0.46
</td>
<td style="text-align:right;">
0.46
</td>
<td style="text-align:right;">
36.37
</td>
<td style="text-align:right;">
37.90
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
7364.23
</td>
<td style="text-align:right;">
6089.46
</td>
</tr>
<tr>
<td style="text-align:left;">
sigma1
</td>
<td style="text-align:right;">
20.15
</td>
<td style="text-align:right;">
20.14
</td>
<td style="text-align:right;">
0.13
</td>
<td style="text-align:right;">
0.13
</td>
<td style="text-align:right;">
19.93
</td>
<td style="text-align:right;">
20.36
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
7804.50
</td>
<td style="text-align:right;">
6279.05
</td>
</tr>
<tr>
<td style="text-align:left;">
sigma2
</td>
<td style="text-align:right;">
22.54
</td>
<td style="text-align:right;">
22.54
</td>
<td style="text-align:right;">
0.30
</td>
<td style="text-align:right;">
0.31
</td>
<td style="text-align:right;">
22.05
</td>
<td style="text-align:right;">
23.05
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
7116.07
</td>
<td style="text-align:right;">
6187.50
</td>
</tr>
<tr>
<td style="text-align:left;">
mu2 - mu1
</td>
<td style="text-align:right;">
5.23
</td>
<td style="text-align:right;">
5.22
</td>
<td style="text-align:right;">
0.49
</td>
<td style="text-align:right;">
0.49
</td>
<td style="text-align:right;">
4.42
</td>
<td style="text-align:right;">
6.03
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
7321.33
</td>
<td style="text-align:right;">
6132.71
</td>
</tr>
</tbody>
</table>

The following table shows the posterior distributions of
![\mu_1](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%5Cmu_1 "\mu_1"),
![\mu_2](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%5Cmu_2 "\mu_2"),
![\sigma_1](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%5Csigma_1 "\sigma_1"),
![\sigma_2](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%5Csigma_2 "\sigma_2"),
and
![\mu_2 - \mu_1](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%5Cmu_2%20-%20%5Cmu_1 "\mu_2 - \mu_1").
As the last row `mu2-mu1` value suggest, the months with frequent forest
fire has higher PM readings than the months without frequent forest fire
with the difference of 5.23

``` r
mcmc_dens(m1, 
          pars = c("mu1","sigma1","mu2","sigma2"))
```

![](HW9_NaomiLee_files/figure-gfm/mcmc_dens-1.png)<!-- -->

The analysis showed that on average, months with frequent forest fire
showed higher PM concentrate values than the months that did not observe
frequent forest fire, with a posterior mean of 5.23 and a 90% CI of
\[4.42, 6.03\].
