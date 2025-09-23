Trust Badge Study Results
================
Cameron Bale
2025-09-23

- [Lots of Data Cleaning](#lots-of-data-cleaning)
- [Main Analysis](#main-analysis)

Import libraries.

``` r
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.4     ✔ readr     2.1.5
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.1
    ## ✔ ggplot2   3.5.1     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.4     ✔ tidyr     1.3.1
    ## ✔ purrr     1.0.4     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

Import data.

``` r
survey <- read_csv("../Data/trust_badge_study.csv")
```

    ## Rows: 265 Columns: 47
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (47): StartDate, EndDate, Status, IPAddress, Progress, Duration (in seco...
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

## Lots of Data Cleaning

Remove first two rows which contain question text and descriptions.

``` r
survey <- survey |>
  slice(-c(1:2))
```

Remove our test responses (i.e., only keep observations with a Prolific
ID).

``` r
survey <- survey |>
  filter(!is.na(PROLIFIC_PID))
```

Remove participants that failed the attention check of whether the
reviewer was verified.

``` r
survey <- survey |>
  # remove those who said they can't remember verification status
  filter(QID11 != "I can't remember") |>
  # create binary variable for reponse to question of whether reviewer was verified
  # then convert treatment group indicator to numeric variable
  # then compare treatment group assignment to binary response variable, pass if they match
  mutate(manipulation_check_binary = if_else(QID11 == "Yes", 1, 0),
         Treat = as.numeric(Treat),                                 
         passed_manipulation_check = if_else(Treat == manipulation_check_binary, 1, 0)) |>
  filter(passed_manipulation_check == 1)
```

Check survey completion time. Remove participants who took less than 90
seconds to complete.

``` r
survey <- survey |>
  filter(`Duration (in seconds)` < 90)
```

Create numeric versions of Likert scale questions. We’ll use the
function below, and apply it to the `Q10_*` questions.

``` r
likert_converter <- function(x){
  factor_x <- factor(x, levels = c("Strongly disagree", "Disagree", "Somewhat disagree", "Neither agree nor disagree", "Somewhat agree", "Agree", "Strongly agree"))
  numeric_x <- as.numeric(factor_x)
  return(numeric_x)
}
```

Check alignment of factor levels and numeric values. `Strongly disagree`
= 1, and `Strongly agree` = 7.

``` r
# factor(survey$QID10_1, levels = c("Strongly disagree", "Disagree", "Somewhat disagree", "Neither agree nor disagree", "Somewhat agree", "Agree", "Strongly agree"))
# likert_converter(survey$QID10_1)
```

Perform conversion.

``` r
survey <- survey |>
  mutate(across(c("QID10_1", "QID10_2", "QID10_3", "QID10_4", "QID10_5"), likert_converter))
```

Rename Likert scale questions.

``` r
survey <- survey |>
  rename("purchase" = "QID10_1",
         "helpful" = "QID10_2",
         "review_trust" = "QID10_3",
         "reviewer_trust" = "QID10_4",
         "platform_trust" = "QID10_5")
```

Check for text responses to demographic questions.

``` r
# no one entered additional sex identifier
survey |>
  filter(!is.na(Q47_4_TEXT))
```

    ## # A tibble: 0 × 49
    ## # ℹ 49 variables: StartDate <chr>, EndDate <chr>, Status <chr>,
    ## #   IPAddress <chr>, Progress <chr>, Duration (in seconds) <chr>,
    ## #   Finished <chr>, RecordedDate <chr>, ResponseId <chr>,
    ## #   RecipientLastName <chr>, RecipientFirstName <chr>, RecipientEmail <chr>,
    ## #   ExternalReference <chr>, LocationLatitude <chr>, LocationLongitude <chr>,
    ## #   DistributionChannel <chr>, UserLanguage <chr>, Prolific ID <chr>,
    ## #   Q18_Browser <chr>, Q18_Version <chr>, Q18_Operating System <chr>, …

``` r
# a few people entered additional ethnicity values
# excluding the person who said test
survey |>
  filter(!is.na(QID1219256734_6_TEXT))
```

    ## # A tibble: 7 × 49
    ##   StartDate    EndDate Status IPAddress Progress Duration (in seconds…¹ Finished
    ##   <chr>        <chr>   <chr>  <chr>     <chr>    <chr>                  <chr>   
    ## 1 2025-09-22 … 2025-0… IP Ad… 128.187.… 100      160                    True    
    ## 2 2025-09-22 … 2025-0… IP Ad… 72.240.1… 100      174                    True    
    ## 3 2025-09-22 … 2025-0… IP Ad… 73.220.8… 100      157                    True    
    ## 4 2025-09-22 … 2025-0… IP Ad… 76.128.3… 100      393                    True    
    ## 5 2025-09-22 … 2025-0… IP Ad… 174.50.8… 100      186                    True    
    ## 6 2025-09-22 … 2025-0… IP Ad… 188.64.3… 100      173                    True    
    ## 7 2025-09-23 … 2025-0… IP Ad… 76.30.64… 100      155                    True    
    ## # ℹ abbreviated name: ¹​`Duration (in seconds)`
    ## # ℹ 42 more variables: RecordedDate <chr>, ResponseId <chr>,
    ## #   RecipientLastName <chr>, RecipientFirstName <chr>, RecipientEmail <chr>,
    ## #   ExternalReference <chr>, LocationLatitude <chr>, LocationLongitude <chr>,
    ## #   DistributionChannel <chr>, UserLanguage <chr>, `Prolific ID` <chr>,
    ## #   Q18_Browser <chr>, Q18_Version <chr>, `Q18_Operating System` <chr>,
    ## #   Q18_Resolution <chr>, `QID41_First Click` <chr>, …

Excluding the person who answered `test` to the ethnicity question.

``` r
survey <- survey |>
  filter(is.na(QID1219256734_6_TEXT) | QID1219256734_6_TEXT != "test")
```

Renaming demographic questions.

``` r
survey <- survey |>
  rename("education" = "QID1219256731",
         "ethnicity" = "QID1219256734",
         "num_online_purchase" = "QID39",
         "age_group" = "Q46",
         "sex" = "Q47",
         "income" = "Q48")
```

Convert number of online purchases to numeric and age group to a factor.

``` r
survey <- survey |>
  mutate(num_online_purchase = as.numeric(factor(num_online_purchase, levels = c("1", "2", "3", "4", "5 or more"))),
         age_group = factor(age_group, levels = c("18-24 years old", "25-34 years old", "35-44 years old", "45-54 years old", "55-64 years old", "65+ years old")))
```

Check how many participants we have in each experimental condition.

``` r
survey |>
  group_by(Treat) |>
  summarize(num_obs = n())
```

    ## # A tibble: 2 × 2
    ##   Treat num_obs
    ##   <dbl>   <int>
    ## 1     0     100
    ## 2     1     118

## Main Analysis

Compare group means.

``` r
survey |>
  group_by(Treat) |>
  summarize(mean_purchase = mean(purchase),
            mean_helpful = mean(helpful),
            mean_review_trust = mean(review_trust),
            mean_reviewer_trust = mean(reviewer_trust),
            mean_platform_trust = mean(platform_trust))
```

    ## # A tibble: 2 × 6
    ##   Treat mean_purchase mean_helpful mean_review_trust mean_reviewer_trust
    ##   <dbl>         <dbl>        <dbl>             <dbl>               <dbl>
    ## 1     0          4.71         5.31              4.99                4.85
    ## 2     1          5.25         5.86              5.84                5.85
    ## # ℹ 1 more variable: mean_platform_trust <dbl>

#### T-tests for differences in mean ratings.

Make separate dataframes for each condition.

``` r
treated <- survey |>
  filter(Treat == 1)

control <- survey |>
  filter(Treat == 0)
```

Purchase intentions.

``` r
t.test(x = treated$purchase, control$purchase, alternative = "greater")
```

    ## 
    ##  Welch Two Sample t-test
    ## 
    ## data:  treated$purchase and control$purchase
    ## t = 2.911, df = 205.96, p-value = 0.002
    ## alternative hypothesis: true difference in means is greater than 0
    ## 95 percent confidence interval:
    ##  0.231663      Inf
    ## sample estimates:
    ## mean of x mean of y 
    ##  5.245763  4.710000

Helpfulness.

``` r
t.test(x = treated$helpful, control$helpful, alternative = "greater")
```

    ## 
    ##  Welch Two Sample t-test
    ## 
    ## data:  treated$helpful and control$helpful
    ## t = 3.5652, df = 197.89, p-value = 0.0002279
    ## alternative hypothesis: true difference in means is greater than 0
    ## 95 percent confidence interval:
    ##  0.2928744       Inf
    ## sample estimates:
    ## mean of x mean of y 
    ##  5.855932  5.310000

Review trust.

``` r
t.test(x = treated$review_trust, control$review_trust, alternative = "greater")
```

    ## 
    ##  Welch Two Sample t-test
    ## 
    ## data:  treated$review_trust and control$review_trust
    ## t = 5.7349, df = 188.2, p-value = 1.916e-08
    ## alternative hypothesis: true difference in means is greater than 0
    ## 95 percent confidence interval:
    ##  0.6042794       Inf
    ## sample estimates:
    ## mean of x mean of y 
    ##  5.838983  4.990000

Reviewer trust.

``` r
t.test(x = treated$reviewer_trust, control$reviewer_trust, alternative = "greater")
```

    ## 
    ##  Welch Two Sample t-test
    ## 
    ## data:  treated$reviewer_trust and control$reviewer_trust
    ## t = 6.5328, df = 180.53, p-value = 3.183e-10
    ## alternative hypothesis: true difference in means is greater than 0
    ## 95 percent confidence interval:
    ##  0.7450201       Inf
    ## sample estimates:
    ## mean of x mean of y 
    ##  5.847458  4.850000

Platform trust.

``` r
t.test(x = treated$platform_trust, control$platform_trust, alternative = "greater")
```

    ## 
    ##  Welch Two Sample t-test
    ## 
    ## data:  treated$platform_trust and control$platform_trust
    ## t = 3.4504, df = 207.1, p-value = 0.0003392
    ## alternative hypothesis: true difference in means is greater than 0
    ## 95 percent confidence interval:
    ##  0.2629552       Inf
    ## sample estimates:
    ## mean of x mean of y 
    ##  5.474576  4.970000

#### Regressions for ratings based on treatment and the number of online purchases.

Purchase regression.

``` r
purchase_model <- lm(purchase ~ num_online_purchase + Treat, data = survey)
summary(purchase_model)
```

    ## 
    ## Call:
    ## lm(formula = purchase ~ num_online_purchase + Treat, data = survey)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -4.5210 -0.5890  0.3134  0.8691  2.3134 
    ## 
    ## Coefficients:
    ##                     Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)          4.49151    0.21881  20.527  < 2e-16 ***
    ## num_online_purchase  0.09754    0.07701   1.267  0.20668    
    ## Treat                0.54181    0.18302   2.960  0.00342 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.346 on 215 degrees of freedom
    ## Multiple R-squared:  0.04521,    Adjusted R-squared:  0.03632 
    ## F-statistic:  5.09 on 2 and 215 DF,  p-value: 0.006924

Helpfulness regression.

``` r
helpful_model <- lm(helpful ~ num_online_purchase + Treat, data = survey)
summary(helpful_model)
```

    ## 
    ## Call:
    ## lm(formula = helpful ~ num_online_purchase + Treat, data = survey)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -4.1980 -0.4463  0.1601  0.7794  1.8020 
    ## 
    ## Coefficients:
    ##                     Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)          5.10774    0.18062  28.279  < 2e-16 ***
    ## num_online_purchase  0.09030    0.06357   1.420 0.156950    
    ## Treat                0.55153    0.15108   3.651 0.000329 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.111 on 215 degrees of freedom
    ## Multiple R-squared:  0.06556,    Adjusted R-squared:  0.05687 
    ## F-statistic: 7.542 on 2 and 215 DF,  p-value: 0.0006829

Review trust regression.

``` r
review_trust_model <- lm(review_trust ~ num_online_purchase + Treat, data = survey)
summary(review_trust_model)
```

    ## 
    ## Call:
    ## lm(formula = review_trust ~ num_online_purchase + Treat, data = survey)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -3.9652 -0.7171  0.1383  0.8689  2.1383 
    ## 
    ## Coefficients:
    ##                     Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)          4.75815    0.17305  27.496  < 2e-16 ***
    ## num_online_purchase  0.10351    0.06091   1.699   0.0907 .  
    ## Treat                0.85540    0.14475   5.910 1.33e-08 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.065 on 215 degrees of freedom
    ## Multiple R-squared:  0.1479, Adjusted R-squared:   0.14 
    ## F-statistic: 18.66 on 2 and 215 DF,  p-value: 3.377e-08

Reviewer trust regression.

``` r
reviewer_trust_model <- lm(reviewer_trust ~ num_online_purchase + Treat, data = survey)
summary(reviewer_trust_model)
```

    ## 
    ## Call:
    ## lm(formula = reviewer_trust ~ num_online_purchase + Treat, data = survey)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -3.8359 -0.8186  0.1641  1.0359  2.2304 
    ## 
    ## Coefficients:
    ##                     Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)          4.70480    0.17832  26.384  < 2e-16 ***
    ## num_online_purchase  0.06482    0.06276   1.033    0.303    
    ## Treat                1.00148    0.14915   6.714 1.66e-10 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.097 on 215 degrees of freedom
    ## Multiple R-squared:  0.1757, Adjusted R-squared:  0.168 
    ## F-statistic: 22.91 on 2 and 215 DF,  p-value: 9.562e-10

Platform trust regression.

``` r
platform_trust_model <- lm(platform_trust ~ num_online_purchase + Treat, data = survey)
summary(platform_trust_model)
```

    ## 
    ## Call:
    ## lm(formula = platform_trust ~ num_online_purchase + Treat, data = survey)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -3.9600 -0.9182  0.0400  0.9461  2.0818 
    ## 
    ## Coefficients:
    ##                     Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)          4.87649    0.17451  27.943  < 2e-16 ***
    ## num_online_purchase  0.04175    0.06142   0.680 0.497440    
    ## Treat                0.50717    0.14597   3.474 0.000619 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.074 on 215 degrees of freedom
    ## Multiple R-squared:  0.05461,    Adjusted R-squared:  0.04581 
    ## F-statistic: 6.209 on 2 and 215 DF,  p-value: 0.00239
