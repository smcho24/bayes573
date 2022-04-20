---
title: "homework 9"
author: "Seongmoon Cho"
date: "2022-04-19"
output:
  github_document:
    toc: true
    math_method: webtex
---




```r
library(haven)   # for opening dta file
library(ggplot2)  # for plots
library(magrittr)  # for `%>%` operator
library(here)
library(readxl)  # for reading excel files
library(modelsummary)  # for summarizing data
library(rstan)
rstan_options(auto_write = TRUE)  # save compiled STAN object
options(mc.cores = 2)  # use two cores
library(posterior)
library(bayesplot)
theme_set(theme_classic() +
    theme(panel.grid.major.y = element_line(color = "grey92")))
```

# Research Question

> Do we observe higher PM readings for months in which forest fires were frequently observed?


# Variables

- `pm10`: Particulate matter 10 (micrograms/$m^3$)
- `pm25`: Particulate matter 2.5 (micrograms/$m^3$)
- `myday`: Day of the year in format ddmmyyyy
- `month`: Month of the year
- `rain`: Rainfall (mm)
- `Station_ID`: Pollutant tracking station ID
- `tmp`: Temperature at 20m from ground level (degrees in Celsius)
- `quadrant`: Quadrant in which the station is located (1 = NE, 2 = SE, 3 = SW, 4 = NW)
- `tot`: Total number of forest fires within 1,500km X 1,500km from the center of Bogota, Columbia
- `qtot`: `tot` by quadrant
- `max`: Daily max forest Fire Radiative Power (FRP)
- `qmax`: `tot` by quadrant
- `fire`: A dummy variable indicating months that forest fires were frequently observed

## Import Data


```r
data <- read_dta(here("bayes_final1.dta"))
```

## Variable Summary


```r
datasummary(daily_avg_pm10  * 
                (N + Mean + SD + Min + Max + Histogram) ~
                factor(fire, labels = c("Non_Fire", "Fire")),
            data = data)
```



|               |          |                                  Non_Fire|                                                      Fire|
|:--------------|:---------|-----------------------------------------:|---------------------------------------------------------:|
|daily_avg_pm10 |N         |                                     11934|                                                      2301|
|               |Mean      |                                     32.99|                                                     45.23|
|               |SD        |                                     20.46|                                                     23.55|
|               |Min       |                                      0.00|                                                      0.00|
|               |Max       |                                    136.43|                                                    119.72|
|               |Histogram | _<U+2587><U+2587><U+2585><U+2582><U+2581>| _<U+2581><U+2585><U+2587><U+2586><U+2585><U+2583><U+2581>|

# Model

Let $Y$ = PM10, $G$ = Forest Fire

Model:
$$
  \begin{aligned}
    Y_{i, G = 0} & \sim N(\mu_1, \sigma_1) \\
    Y_{i, G = 1} & \sim N(\mu_2, \sigma_2)
  \end{aligned}
$$

Prior:
$$
  \begin{aligned}
    \mu_1 & \sim N(36, 20) \\
    \mu_2 & \sim N(45, 23) \\
    \sigma_1 & \sim N^+(0, 2) \\
    \sigma_2 & \sim N^+(0, 2)
  \end{aligned}
$$

## Running Stan

We used 4 chains, each with 4,000 iterations (first 2,000 as warm-ups). 


```r
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
    seed = 20220419,  # for reproducibility
    iter = 4000
)
```

```
## Warning in normalizePath(path.expand(path), winslash, mustWork): path[1]="C:/SMC/USC_PhD/PhD_Courses/
## PSYC573_Bayesian Data Analysis/final/Project/normal_2group.stan": The system cannot find the path specified
```

```
## Warning in file(fname, "rt"): cannot open file 'C:\SMC\USC_PhD\PhD_Courses\PSYC573_Bayesian Data
## Analysis\final\Project\normal_2group.stan': No such file or directory
```

```
## Error in get_model_strcode(file, model_code): cannot open model file "C:\SMC\USC_PhD\PhD_Courses\PSYC573_Bayesian Data Analysis\final\Project\normal_2group.stan"
```

# Results

```r
print(m1, pars = c("mu1", "mu2", "sigma1", "sigma2"))
```

```
## Inference for Stan model: normal_2group.
## 4 chains, each with iter=4000; warmup=2000; thin=1; 
## post-warmup draws per chain=2000, total post-warmup draws=8000.
## 
##         mean se_mean   sd  2.5%   25%   50%   75% 97.5% n_eff Rhat
## mu1    31.92    0.00 0.18 31.57 31.80 31.92 32.04 32.27  7931    1
## mu2    37.14    0.01 0.46 36.24 36.83 37.15 37.46 38.02  7725    1
## sigma1 20.15    0.00 0.13 19.90 20.06 20.15 20.23 20.40  7841    1
## sigma2 22.54    0.00 0.30 21.96 22.34 22.54 22.75 23.14  7596    1
## 
## Samples were drawn using NUTS(diag_e) at Tue Apr 19 22:42:55 2022.
## For each parameter, n_eff is a crude measure of effective sample size,
## and Rhat is the potential scale reduction factor on split chains (at 
## convergence, Rhat=1).
```


As shown in the graph below, the chains mixed well.


```r
mcmc_rank_hist(m1, pars = c("mu1", "mu2", "sigma1", "sigma2"))
```

![plot of chunk rank-hist-m1](figure/rank-hist-m1-1.png)

The following table shows the posterior distributions of $\mu_1$, $\mu_2$, $\sigma_1$, $\sigma_2$, and $\mu_2 - \mu_1$.


```r
summ_m1 <- as_draws_df(m1) %>%
    subset_draws(variable = c("mu1", "mu2", "sigma1", "sigma2")) %>%
    mutate_variables(`mu2 - mu1` = mu2 - mu1) %>%
    summarise_draws()
knitr::kable(summ_m1, digits = 2)
```

<table>
 <thead>
  <tr>
   <th style="text-align:left;"> variable </th>
   <th style="text-align:right;"> mean </th>
   <th style="text-align:right;"> median </th>
   <th style="text-align:right;"> sd </th>
   <th style="text-align:right;"> mad </th>
   <th style="text-align:right;"> q5 </th>
   <th style="text-align:right;"> q95 </th>
   <th style="text-align:right;"> rhat </th>
   <th style="text-align:right;"> ess_bulk </th>
   <th style="text-align:right;"> ess_tail </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> mu1 </td>
   <td style="text-align:right;"> 31.92 </td>
   <td style="text-align:right;"> 31.92 </td>
   <td style="text-align:right;"> 0.18 </td>
   <td style="text-align:right;"> 0.17 </td>
   <td style="text-align:right;"> 31.62 </td>
   <td style="text-align:right;"> 32.21 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 7937.92 </td>
   <td style="text-align:right;"> 6303.37 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> mu2 </td>
   <td style="text-align:right;"> 37.14 </td>
   <td style="text-align:right;"> 37.15 </td>
   <td style="text-align:right;"> 0.46 </td>
   <td style="text-align:right;"> 0.47 </td>
   <td style="text-align:right;"> 36.39 </td>
   <td style="text-align:right;"> 37.89 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 7738.86 </td>
   <td style="text-align:right;"> 6615.03 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> sigma1 </td>
   <td style="text-align:right;"> 20.15 </td>
   <td style="text-align:right;"> 20.15 </td>
   <td style="text-align:right;"> 0.13 </td>
   <td style="text-align:right;"> 0.12 </td>
   <td style="text-align:right;"> 19.94 </td>
   <td style="text-align:right;"> 20.36 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 7866.02 </td>
   <td style="text-align:right;"> 5649.64 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> sigma2 </td>
   <td style="text-align:right;"> 22.54 </td>
   <td style="text-align:right;"> 22.54 </td>
   <td style="text-align:right;"> 0.30 </td>
   <td style="text-align:right;"> 0.30 </td>
   <td style="text-align:right;"> 22.06 </td>
   <td style="text-align:right;"> 23.04 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 7634.57 </td>
   <td style="text-align:right;"> 6098.18 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> mu2 - mu1 </td>
   <td style="text-align:right;"> 5.22 </td>
   <td style="text-align:right;"> 5.22 </td>
   <td style="text-align:right;"> 0.49 </td>
   <td style="text-align:right;"> 0.49 </td>
   <td style="text-align:right;"> 4.41 </td>
   <td style="text-align:right;"> 6.02 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 7747.00 </td>
   <td style="text-align:right;"> 6239.31 </td>
  </tr>
</tbody>
</table>


```r
mcmc_dens(m1, 
          pars = c("mu1","sigma1","mu2","sigma2"))
```

![plot of chunk mcmc_dens](figure/mcmc_dens-1.png)

The analysis showed that on average, months with frequent forest fire showed higher PM concentrate than months that did not observe frequent forest fire, with a posterior mean of 5.22 and a 90% CI of [4.41, 6.02].
