---
title: Matching, missing data, a quasi-experiment, and causal inference--Oh my!
author: A. Solomon Kurz
date: '2025-02-02'
excerpt: "I'm finally dipping my does into causal inference for quasi-experiments, and my first use case has missing data. In this post we practice propensity score matching with multiply-imputed data sets, and how to compute the average treatment effect for the treated (ATT) with g-computation."
tags:
  - ANCOVA
  - ANHECOVA
  - ATT
  - binary
  - binomial
  - causal inference
  - g-computation
  - GLM
  - logistic regression
  - matching
  - mice
  - missing data
  - potential outcomes
  - quasi-experiment
  - R
  - standardization
  - tidyverse
  - tutorial
draft: false
layout: single
featured: no
bibliography: /Users/solomonkurz/Dropbox/blogdown/content/blog/my_blog.bib
biblio-style: apalike
csl: /Users/solomonkurz/Dropbox/blogdown/content/blog/apa.csl  
link-citations: yes
---

## Context

I have a project for work where the goal is to compare two non-randomized groups. One group received and experimental intervention, and we’d like to compare them to their peers who were not offered the same intervention. We have access to data from folks from the broader population during the same time period, so the goal isn’t so bad as far as quasi-experiments go.[^1] The team would like to improve the comparisons by using matching, and we have some missing data issues in the matching covariate set. I haven’t done an analysis quite like this before, so this post is a walk through of the basic statistical procedure as I see it.

You are welcome to give feedback in the comments section.

### I make assumptions.

For this post, I’m presuming readers have a passing familiarity with the following:

#### Regression.

You’ll want to be familiar with single-level regression, from the perspective of the generalized linear model. For frequentist resources, I recommend the texts by Ismay & Kim ([2022](#ref-ismay2022StatisticalInference)) and Roback & Legler ([2021](#ref-roback2021beyond)). For the Bayesians in the room, I recommend the texts by Gelman and colleagues ([2020](#ref-gelmanRegressionOtherStories2020)), Kruschke ([2015](#ref-kruschkeDoingBayesianData2015)), or McElreath ([2015](#ref-mcelreathStatisticalRethinkingBayesian2015), [2020](#ref-mcelreathStatisticalRethinkingBayesian2020)). In this post, we’ll mainly rely on the frequentist paradigm.

#### Missing data.

You should be familiar with contemporary missing data theory, particularly multiple imputation (MI). You can find brief overviews in the texts by McElreath and Gelman et al, above. For a deeper dive, I recommend Enders ([2022](#ref-enders2022applied)), Little & Rubin ([2019](#ref-little2019statistical)), or van Buuren ([2018](#ref-vanbuurenFlexibleImputationMissing2018)). I will walk through the imputation steps in some detail, but this will not be a full-blown MI tutorial.

#### Experimental design.

I’m assuming readers have a basic background in experimental design. At a minimum, you’ll want to have a sense of why it’s harder to make comparisons between non-randomized groups than it is between those formed by random assignment. If these ideas are new, I recommend Shadish et al. ([2002](#ref-shadish2002Experimental)) or Kazdin ([2017](#ref-kazdin2017ResearchDesign)). You might also check out Taback ([2022](#ref-taback2022DesignAndAnalysis)), and its free companion website at <https://designexptr.org/index.html>.

#### Matching for causal inference.

Of the various skills required for these analysis, causal inference with matching is the new part for me, which means I’ll walk through those parts of the analysis with more attention to detail and pedagogy than others. For the basic matching workflow, I’ll be relying heavily on the documentation for the **MatchIt** ([Ho et al., 2011](#ref-ho2011MatchIt)) and **MatchThem** ([Pishgar et al., 2021](#ref-pishgar2021MatchThem)) packages. I also found the papers by Stuart ([2010](#ref-stuart2010matching)) and Greifer & Stuart ([2021](#ref-greifer2021matching)) were helpful introductions to the paradigm. For general introductions to causal inference, consider texts like Brumback ([2022](#ref-brumback2022Fundamentals)), Hernán & Robins ([2020](#ref-hernan2020CausalInference)), or Imbens & Rubin ([2015](#ref-imbensCausalInferenceStatistics2015)). If you prefer freely-accessible ebooks, check out Cunningham ([2021](#ref-cunningham2021causal)).

#### **R**.

All code will be in **R** ([R Core Team, 2022](#ref-R-base)). Data wrangling and plotting will rely heavily on the **tidyverse** ([Wickham et al., 2019](#ref-wickhamWelcomeTidyverse2019); [Wickham, 2022](#ref-R-tidyverse)). We multiply impute the missing values with the **mice** package ([van Buuren & Groothuis-Oudshoorn, 2011](#ref-mice2011), [2021](#ref-R-mice)). The MI data sets are matched with propensity scores with the **MatchThem** package, and we assess matching balance with functions from **cobalt** ([Greifer, 2024](#ref-R-cobalt)). Finally, we compute the causal estimand itself with help from **marginaleffects** ([Arel-Bundock, 2023](#ref-R-marginaleffects)).

Load the packages.

``` r
# Load
library(tidyverse)
library(mice)
library(MatchThem)
library(cobalt)
library(marginaleffects)

# Remove ggplot gridlines
theme_set(
  theme_gray() +
    theme(panel.grid = element_blank())
  )
```

#### We need data.

The structure of our workflow will largely follow the extend example from Pishgar et al. ([2021](#ref-pishgar2021MatchThem)). As did they, here we load the `osteoarthritis` data. If you’d like to dive deeply into the data documentation, execute `?osteoarthritis` in your console.

``` r
data("osteoarthritis")

# What?
glimpse(osteoarthritis)
```

    ## Rows: 2,585
    ## Columns: 7
    ## $ AGE <int> 69, 71, 72, 75, 72, 61, 76, 79, 68, 77, 77, 63, 64, 64, 72, 62, 70…
    ## $ SEX <fct> 1, 2, 2, 2, 1, 1, 1, 1, 1, 2, 2, 2, 1, 1, 1, 2, 2, 1, 1, 2, 1, 2, …
    ## $ BMI <dbl> 29.8, 22.7, 30.7, 23.5, 25.9, 36.5, 25.1, 31.8, 27.4, 32.0, 22.9, …
    ## $ RAC <fct> 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 2, 1, 1, 2, …
    ## $ SMK <fct> 1, 1, 0, 0, 0, 1, 1, 0, 1, 0, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 0, …
    ## $ OSP <fct> 0, 1, 1, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, …
    ## $ KOA <fct> 1, 0, 0, 0, 1, 1, 0, 1, 1, NA, 1, 0, 0, 0, 0, 0, 1, 0, 0, NA, 0, 1…

Here we make a few adjustments to the data. First, I generally prefer the **tidyverse** style of lower-case variable names (see [Wickham, 2020](#ref-wickhamTidyverseStyleGuide2020)). Second, I generally like my lower-case names to use whole words such as `race`, rather than abbreviations, such as `rac`. Third, I prefer my 2-level nominal variables in a dummy-code format. Here we make those changes and save the results as `d`.

``` r
d <- osteoarthritis |> 
  rename_all(.funs = str_to_lower) |>
  rename(male = sex,
         race = rac,
         smoker = smk) |> 
  mutate(male   = as.integer(male)   - 1,
         smoker = as.integer(smoker) - 1,
         koa    = as.integer(koa)    - 1)

# What?
head(d)
```

    ##   age male  bmi race smoker osp koa
    ## 2  69    0 29.8    1      1   0   1
    ## 3  71    1 22.7    1      1   1   0
    ## 5  72    1 30.7    1      0   1   0
    ## 6  75    1 23.5    1      0   0   0
    ## 8  72    0 25.9    1      0   0   1
    ## 9  61    0 36.5    2      1   0   1

As in Pishgar et al. ([2021](#ref-pishgar2021MatchThem)), the criterion variable is knee osteoarthritis in the follow-up time period, `koa`. The focal *treatment* variable[^2] is osteoporosis at baseline, `osp`. The remaining variables are potential confounders. Thus the research question for this blog is: *What is the causal effect of baseline osteoporosis status on follow-up knee osteoarthritis status?* For my real-world use case, the research question is more like: *What is the causal effect of our new treatment, relative to business as usual?*

## Missing data

When you have missing data, you might get a high-level summary of the missing data patterns with the `md.pattern()` function.

``` r
md.pattern(d)
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-4-1.png" width="672" />

    ##      age male osp bmi race smoker koa    
    ## 2363   1    1   1   1    1      1   1   0
    ## 191    1    1   1   1    1      1   0   1
    ## 27     1    1   1   1    1      0   1   1
    ## 1      1    1   1   1    1      0   0   2
    ## 1      1    1   1   1    0      1   1   1
    ## 1      1    1   1   1    0      1   0   2
    ## 1      1    1   1   0    1      1   1   1
    ##        0    0   0   1    2     28 193 224

The top row indicates most cases (2,363 out of 2,585) had no missing data. Though we have no missingness in the focal exposure `osp`–arguably the most important variable from a missing data perspective–, we have missingness on the criterion `koa` and on some of the potential confounders. Importantly, some of these variables are categorical, which means multiple imputation is the go-to solution here. There has also been some discussion of how one might best handle missing data for matching methods in the methods literature, and it appears the methodologists have been recommending the MI approach in recent years (e.g., [Leyrat et al., 2019](#ref-leyrat2019propensity); [Nguyen & Stuart, 2024](#ref-nguyen2024multiple); [Zhao et al., 2021](#ref-zhao2021propensity)).

## Impute with `mice()`

We’ll start the imputation discussion with an initial default[^3] call to `mice()`, the primary function from the **mice** package, and we save the results as `imputed.datasets`.[^4]

``` r
imputed.datasets <- mice(d, print = FALSE)
```

The default imputation method for `mice()` is generally predictive mean matching (execute `imputed.datasets$method`), which is attractive in how simple and general it is. If you’re not familiar with predictive mean matching, see [Section 3.4](https://stefvanbuuren.name/fimd/sec-pmm.html) in van Buuren ([2018](#ref-vanbuurenFlexibleImputationMissing2018)) for an introduction. I believe there are some concerns predictive mean matching doesn’t work as well with smaller sample sizes, but I think the `\(N = 2{,}585\)` data set here is large enough to discard those worries. Note though, the `mice()` function used polytomous logistic regression (`method = "polyreg"`) for the factor variable `race`, which I think is a great choice for that variable.

By default, all variables are used to predict the missingness of all other variables in a simple linear model (execute `imputed.datasets$predictorMatrix`). However, there is some concern in the RCT literature that such a simple model may be inappropriate in that it would not be robust to possible treatment-by-covariate interactions, which could bias the results ([Sullivan et al., 2018](#ref-sullivan2018should)).[^5] This leaves us with two basic solutions: expand the predictor matrix to include all possible `osp`-by-covariate interactions, or impute the data sets separately by the two levels of `osp`. With a small number of rows, the second option might be worrisome, but I think splitting the data set into `\(n_{{\text{osp}} = 0} = 2{,}106\)` and `\(n_{{\text{osp}} = 1} = 479\)` should be fine.

Toward that end, we first make two new versions of the `d` data set. The new `d0` subset only contains the cases for which `osp == 0`; the `d1` subset only contains the remaining cases for which `osp == 1`.

``` r
d0 <- d |> 
  filter(osp == 0)

d1 <- d |> 
  filter(osp == 1)

# Sample sizes
nrow(d0)
```

    ## [1] 2106

``` r
nrow(d1)
```

    ## [1] 479

Now we make two new `imputed.datasets` objects, one for each of the new data subsets.

``` r
imputed.datasets0 <- mice(d0, print = FALSE)
imputed.datasets1 <- mice(d1, print = FALSE)
```

If you look at the new `predictorMatrix` for either, you’ll note that the `osp` column has changed to a vector of `0`’s. As an example, here we look at the matrix for `imputed.datasets0`.

``` r
imputed.datasets0$predictorMatrix
```

    ##        age male bmi race smoker osp koa
    ## age      0    1   1    1      1   0   1
    ## male     1    0   1    1      1   0   1
    ## bmi      1    1   0    1      1   0   1
    ## race     1    1   1    0      1   0   1
    ## smoker   1    1   1    1      0   0   1
    ## osp      1    1   1    1      1   0   1
    ## koa      1    1   1    1      1   0   0

The `mice()` function noticed that `osp` was now a constant within each data subset, and it correctly removed that “variable” from the predictor matrix of the other variables. This is exactly what one would hope for, and it’s a testament to the foresight of the **mice** package team.

To combine the two `imputed.datasets0` and `imputed.datasets1` objects, you can use the `rbind()` function.

``` r
imputed.datasets <- rbind(imputed.datasets0, imputed.datasets1)

# What?
imputed.datasets |> 
  str(max.level = 1, give.attr = FALSE)
```

    ## List of 22
    ##  $ data           :'data.frame':	2585 obs. of  7 variables:
    ##  $ imp            :List of 7
    ##  $ m              : num 5
    ##  $ where          : logi [1:2585, 1:7] FALSE FALSE FALSE FALSE FALSE FALSE ...
    ##  $ blocks         :List of 7
    ##  $ call           :List of 2
    ##  $ nmis           : Named int [1:7] 0 0 1 2 28 0 193
    ##  $ method         : Named chr [1:7] "" "" "pmm" "polyreg" ...
    ##  $ predictorMatrix: num [1:7, 1:7] 0 1 1 1 1 1 1 1 0 1 ...
    ##  $ visitSequence  : chr [1:7] "age" "male" "bmi" "race" ...
    ##  $ formulas       :List of 7
    ##  $ post           : Named chr [1:7] "" "" "" "" ...
    ##  $ blots          :List of 7
    ##  $ ignore         : logi [1:2585] FALSE FALSE FALSE FALSE FALSE FALSE ...
    ##  $ seed           : logi NA
    ##  $ iteration      : num 5
    ##  $ lastSeedValue  : int [1:626] 10403 144 572103155 -1806920652 -375284517 464361388 1131814530 1688459416 -1207814412 1902940732 ...
    ##  $ chainMean      : num [1:7, 1:5, 1:5] NaN NaN NaN NaN 0.536 ...
    ##  $ chainVar       : num [1:7, 1:5, 1:5] NA NA NA NA 0.268 ...
    ##  $ loggedEvents   :'data.frame':	1 obs. of  5 variables:
    ##  $ version        :Classes 'package_version', 'numeric_version'  hidden list of 1
    ##  $ date           : Date[1:1], format: "2025-02-03"

In the `str()` output, you’ll note how the data frame in the `data` section now has 2,585 rows (i.e., the full sample size), and that correct number of cases is also echoed in the `where` and `ignore` sections. `rbind()` worked!

By default, `mice()` only imputes five data sets (i.e., `m = 5`). In my use case, it’d be better if we had a larger number like 20 or 100. For the sake of practice in this post, we’ll use `m = 10`. I’d also like to make my results more reproducible by setting my seed value by way of the `seed` argument. Finally, I continue setting `print = FALSE` for silent printing.

Here’s the full updated workflow.

``` r
# Impute separately by `osp`
imputed.datasets0 <- mice(d0, m = 10, print = FALSE, seed = 0)
imputed.datasets1 <- mice(d1, m = 10, print = FALSE, seed = 1)

# Combine
imputed.datasets <- rbind(imputed.datasets0, imputed.datasets1)
```

It can be wise to check the trace plots for the imputed variables with `plot()`.

``` r
plot(imputed.datasets)
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-14-1.png" width="672" />

To my eye, these look pretty good.

Once you’re imputed the data sets, it’s also a good idea to take a look at the results with summary statistics and/or plots. As a first step, you can extract the MI data sets with the `complete()` function. By setting `action = "long"`, all MI data sets will be returned in a long format with respect to the imputation number (`.imp`). Setting `include = TRUE` returns the results for the original un-imputed data set (`.imp == 0`), too.

``` r
imputed.datasets |> 
  complete(action = "long", include = TRUE) |> 
  glimpse()
```

    ## Rows: 28,435
    ## Columns: 9
    ## $ .imp   <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
    ## $ .id    <int> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, …
    ## $ age    <int> 69, 75, 72, 61, 76, 79, 68, 64, 64, 72, 62, 70, 72, 68, 73, 66,…
    ## $ male   <dbl> 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 1, 0, 1, 1, 0, 0, …
    ## $ bmi    <dbl> 29.8, 23.5, 25.9, 36.5, 25.1, 31.8, 27.4, 36.0, 33.1, 30.0, 29.…
    ## $ race   <fct> 1, 1, 1, 2, 1, 1, 1, 2, 1, 1, 1, 1, 1, 2, 1, 2, 1, 1, 1, 1, 1, …
    ## $ smoker <dbl> 1, 0, 0, 1, 1, 0, 1, 1, 1, 1, 1, 0, 1, 1, 1, 0, 0, 1, 0, 0, 0, …
    ## $ osp    <fct> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
    ## $ koa    <dbl> 1, 0, 1, 1, 0, 1, 1, 0, 0, 0, 0, 1, 0, 0, 0, 1, 1, NA, NA, 0, 0…

Now you can summarize each variable with missingness, as desired. For example, here are the counts for the criterion `koa`, by imputation.

``` r
imputed.datasets |> 
  complete(action = "long", include = TRUE) |> 
  count(koa, .imp) |> 
  pivot_wider(names_from = koa, values_from = n)
```

    ## # A tibble: 11 × 4
    ##     .imp   `0`   `1`  `NA`
    ##    <int> <int> <int> <int>
    ##  1     0  1197  1195   193
    ##  2     1  1295  1290    NA
    ##  3     2  1283  1302    NA
    ##  4     3  1289  1296    NA
    ##  5     4  1300  1285    NA
    ##  6     5  1285  1300    NA
    ##  7     6  1286  1299    NA
    ##  8     7  1286  1299    NA
    ##  9     8  1286  1299    NA
    ## 10     9  1284  1301    NA
    ## 11    10  1290  1295    NA

Here we compute the means of the standard deviations for `bmi` across the imputations, and then compare those statistics with the statistics for the complete cases in a faceted histogram.

``` r
imputed.datasets |> 
  complete(action = "long", include = TRUE) |> 
  group_by(.imp) |> 
  summarise(mean = mean(bmi, na.rm = TRUE),
            sd = sd(bmi, na.rm = TRUE)) |> 
  pivot_longer(cols = mean:sd) |> 
  mutate(type = ifelse(.imp == 0, "complete case", "imputation")) |> 
  
  ggplot(aes(x = value)) +
  geom_histogram(aes(fill = type),
                 bins = 8) +
  geom_rug(aes(color = type)) +
  scale_color_viridis_d(NULL, option = "A", begin = 0.2, end = 0.6) +
  scale_fill_viridis_d(NULL, option = "A", begin = 0.2, end = 0.6) +
  xlab(NULL) +
  facet_wrap(~ name, scales = "free_x")
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-17-1.png" width="576" />

You might also want to plot the imputed data themselves. The **mice** package has a few plotting functions to aid with this, such as `bwplot()`, `densityplot()`, and `xyplot()`. The `densityplot()` function is good for displaying continuous variables, though it won’t work well in our case because `bmi` is the only continuous variable with missingness, and with a single missing case `densityplot()` won’t be able to select the bandwidth appropriately.

The `bwplot()` method, however, can be of use for `bmi` in our example. The blue blox and whisker plot on the left is for the complete cases only. Had we had more missing values, we would have had 10 red box and whisker plots to its right for the remaining levels of `.imp`. But in this case because we have a single case with missingness for `bmi`, its exact value gets displayed by a red colored dot.

``` r
bwplot(imputed.datasets, bmi ~ .imp )
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-19-1.png" width="384" />

The `xyplot()` function is good for bivariate plots. For example, here we plot the distribution of the imputed `bmi` values on the y axis by their distribution of `age` values on the x axis. The case with the imputed `bmi` value is color coded red. Note how the upper left panel is for `.imp == 0`, the complete cases only.

``` r
xyplot(imputed.datasets, bmi ~ age | .imp, pch = c(1, 20))
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-20-1.png" width="576" />

You could also always make your own custom missing data visualizations with the output from `complete(imputed.datasets, action = "long", include = TRUE)` and the **ggplot2** functions of your choosing.

### Parting thoughts.

For a modest `\(N\)` data set with so few columns, it’s not a big deal to just use all variables to predict all other variables for the imputation step, and we’ll be using all variables in the matching procedure to come, too. In my real-world use case, we’ll have many more columns to choose from. In the abstract, the more data the better! But as your columns increase, the predictor matrix for the imputation step increases, too, and you might find yourself running into computation problems. If and when that arises, it might be wise to think about reducing the columns in your data set, and/or manually adjusting the predictor matrix. From a purely missing data perspective, you can find a lot of guidance in Enders ([2022](#ref-enders2022applied)), particularly from his discussion of Collins et al’s ([2001](#ref-collins2001comparison)) typology.

## Match

Now we match. Were we using a single data set, we might match with the `matchit()` function from **MatchIt**. But since we have MI data sets, we instead use the `matchthem()` function from **MatchThem**. Note the name of the `datasets` argument (typically `data` in most functions) further indicates `matchthem()` is designed for MI data sets.

In the `formula` argument, we use the confounder variables to predict the *treatment* variable `osp`. Here I use a simple approach where all confounder only have lower-order terms. But you might also consider adding interactions among the confounders, or even adding polynomial terms (see [Zhao et al., 2021](#ref-zhao2021propensity) for an extended example).

The default for the `method` argument is `"nearest"` for *nearest neighbor matching*. In a personal consultation, Noah Greifer recommended I use the *genetic matching* approach for my use case, and so we use it here. You can learn about the various matching methods in Greifer’s [*Matching Methods* vignette](https://CRAN.R-project.org/package=MatchIt/vignettes/matching-methods.html), and you can specifically learn about the genetic algorithm used when you set `methoc = "genetic"` [here](https://CRAN.R-project.org/package=MatchIt/vignettes/matching-methods.html#genetic-matching-method-genetic). In short, the genetic algorithm optimizes balance among the confounders using the scaled generalized Mahalanobis distance via functions from the **Matching** package ([Sekhon, 2011](#ref-sekhon2011multivariate)).

Note the `pop.size` argument. If you run the code without that argument, you’ll get a warning message from **Matching** that the optimization parameters are at their default values, and that `pop.size` in particular might should be increased from its default setting of `100`. At the moment, I do not have a deep grasp of this setting, but for the sake of practice I have increased it to `200`.

So far, my experience is the genetic algorithm with an increased `pop.size` setting is computationally expensive. Execute code like this with care.

``` r
t0 <- Sys.time()

matched.datasets <- matchthem(
  datasets = imputed.datasets,
  osp ~ age + male + bmi + race + smoker,
  method = "genetic",
  pop.size = 200)

t1 <- Sys.time()
t1 - t0
```

    ## Time difference of 9.274688 mins

Note the `Sys.time()` calls and the `t1 - t0` algebra at the bottom of the code block. This is a method I often use to keep track of the computation time for longer operations. In this case this code block took just under ten minutes to complete on my laptop.[^6] YMMV.

The output of `matchthem()` is an object of class `mimids`, which is a list of 4.

``` r
class(matched.datasets)
```

    ## [1] "mimids"

``` r
str(matched.datasets, max.level = 1, give.attr = FALSE)
```

    ## List of 4
    ##  $ call    : language matchthem(formula = osp ~ age + male + bmi + race + smoker, datasets = imputed.datasets,      method = "genetic",| __truncated__
    ##  $ object  :List of 22
    ##  $ models  :List of 10
    ##  $ approach: chr "within"

### Assess balance.

We match data from quasi-experiments to better balance the confounder sets in a way that would ideally mimic the kind of balance you’d get by random assignment in an RCT. Therefore we might inspect the degree of balance after the matching step to get a sense of how well we did. Here we do so with tables and plots. We start with tables.

#### Tables.

One way to assess how well the matching step balanced the data is simply with the `summary()` function. If you set `improvement = TRUE`, you will also get information about how more more balanced the adjusted data set is compares to the original unadjusted data. However, this produces a lot of output and we will take a more focused approach instead.

``` r
# Execute this on your own
summary(matched.datasets, improvement = TRUE)
```

The `bal.tab()` function from **cobalt** returns a focused table of contrasts for each covariate. These are computed across all MI data sets, and then summarized in three columns: one for the minimum, mean, and maximum value of the statistic.

``` r
bal.tab(matched.datasets)
```

    ## Balance summary across all imputations
    ##              Type Min.Diff.Adj Mean.Diff.Adj Max.Diff.Adj
    ## distance Distance       0.0158        0.0449       0.0538
    ## age       Contin.      -0.0029        0.0137       0.0433
    ## male       Binary       0.0000        0.0000       0.0000
    ## bmi       Contin.      -0.0530       -0.0422      -0.0086
    ## race_0     Binary       0.0000        0.0000       0.0000
    ## race_1     Binary       0.0000        0.0000       0.0000
    ## race_2     Binary       0.0000        0.0000       0.0000
    ## race_3     Binary       0.0000        0.0000       0.0000
    ## smoker     Binary      -0.0042       -0.0004       0.0000
    ## 
    ## Average sample sizes across imputations
    ##              0   1
    ## All       2106 479
    ## Matched    479 479
    ## Unmatched 1627   0

Notice the `Type` column differentiates between continuous and binary predictors (and further note how the `race` predictor is broken into categories). The `Type` column is important because the metrics for the contrasts in the remaining three columns depend on the type of predictor. The contrasts for the continuous predictors are standardized mean differences (SMD, i.e., Cohen’s `\(d\)`’s). The binary variables are raw differences in proportion (i.e., “risk” differences in the jargon often used in biostatistics and the medical literatures).

I don’t love the mixing of these metrics, and an alternative is to make separate tables by type. When you’re working with matched MI data sets, the only way to do that is to use the alternative formula syntax in `bal.tab()`. The trick is to input the `imputed.datasets` object to the `data` argument, and to input the `matched.datasets` object to the `weights` argument.

``` r
# Continuous (SMD contrasts)
bal.tab(osp ~ age + bmi, 
        data = imputed.datasets,
        weights = matched.datasets)
```

    ## Balance summary across all imputations
    ##        Type Min.Diff.Adj Mean.Diff.Adj Max.Diff.Adj
    ## age Contin.      -0.0029        0.0137       0.0433
    ## bmi Contin.      -0.0530       -0.0422      -0.0086
    ## 
    ## Average effective sample sizes across imputations
    ##               0   1
    ## Unadjusted 2106 479
    ## Adjusted    479 479

``` r
# Binary (raw proportion contrasts)
bal.tab(osp ~ male + race + smoker, 
        data = imputed.datasets,
        weights = matched.datasets)
```

    ## Balance summary across all imputations
    ##          Type Min.Diff.Adj Mean.Diff.Adj Max.Diff.Adj
    ## male   Binary       0.0000        0.0000            0
    ## race_0 Binary       0.0000        0.0000            0
    ## race_1 Binary       0.0000        0.0000            0
    ## race_2 Binary       0.0000        0.0000            0
    ## race_3 Binary       0.0000        0.0000            0
    ## smoker Binary      -0.0042       -0.0004            0
    ## 
    ## Average effective sample sizes across imputations
    ##               0   1
    ## Unadjusted 2106 479
    ## Adjusted    479 479

In this case all the contrasts look great. The SMD’s are all below the conventional threshold for a *small* difference,[^7] and the proportion contrasts are about as close to zero as you could hope for.

Also note in the secondary tables we can see that whereas the total sample size for the `osp == 0` cases was 2,106, only 479 cases were matched with with the 479 available cases in the `osp == 1`. This is as expected.

#### Plots.

We can also get a plot representation of this table with a love plot via `love.plot()`. When working with MI data sets, you’ll want to set the `which.imp` argument which specifies which of the MI data sets get displayed in the plot. One way to do this is to feed a vector of integers. In this case, we set `which.imp = 1:3` to plot the first three MI data sets.

``` r
love.plot(matched.datasets, which.imp = 1:3, thresholds = 0.2)
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-27-1.png" width="672" />

You have the contrasts on the x axis, and the various predictor variables on the y. The contrasts are color coded by whether they’re for the original unadjusted data, or the adjusted matched data. As with the `bal.tab()` tables above, the metrics of the contrasts differ based on whether the predictors are continuous or binary.

Another approach with these plots is to set `which.imp = .none`, which collapses across all MI data sets.

``` r
love.plot(matched.datasets, which.imp = .none, thresholds = 0.2)
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-28-1.png" width="480" />

Further, if you would like to separate the continuous and binary predictors, you can use the alternative formula syntax similar to the `bal.tab()` tables, above. Here’s an example with the continuous variables and the `which.imp = 1:3` setting.

``` r
love.plot(osp ~ age + bmi, 
          data = imputed.datasets,
          weights = matched.datasets, 
          which.imp = 1:3, thresholds = 0.2) +
  xlim(-1, 1)
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-29-1.png" width="672" />

Notice how because `love.plot()` returns a ggplot object, we can further adjust the settings with other **ggplot2** functions, like `xlim()`.

You can continue to narrow your diagnostic focus by using `bal.plot()` to compare the distributions of specific variables by the treatment variable (`osp`) and whether the solutions are unadjusted or adjusted. As we are working with MI data sets, we’ll also want to continue setting the `which.imp` argument. Here we make overlaid density plots for propensity score `distance` for the first four MI data sets.

``` r
bal.plot(matched.datasets, var.name = 'distance', which = "both", which.imp = 1:4)
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-31-1.png" width="672" />

I find the similarity in the adjusted solutions pretty impressive.

Here we collapse across all the MI data sets and compare the `male` dummy with a faceted histogram.

``` r
bal.plot(matched.datasets, var.name = 'male', which = "both", which.imp = .none)
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-32-1.png" width="480" />

On average, the balance is perfect in the adjusted data.

### But what have we done?

Before we move forward with fitting the substantive model, we might linger a while on the output of our matching step.

Much like we used the `mice::complete()` function to extract the imputed data sets from our `imputed.datasets` mids object above, the **MatchThem** package comes with a variant of the `complete()` function that extracts MI data sets from a mimids object. By setting `action = "long"`, we extract all MI data sets in the long format, where each is indexed by an `.imp` column.

``` r
matched.datasets.long <- complete(matched.datasets, action = "long") 

# What?
matched.datasets.long |> 
  glimpse()
```

    ## Rows: 25,850
    ## Columns: 12
    ## $ .imp     <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1…
    ## $ .id      <int> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18…
    ## $ age      <int> 69, 75, 72, 61, 76, 79, 68, 64, 64, 72, 62, 70, 72, 68, 73, 6…
    ## $ male     <dbl> 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 1, 0, 1, 1, 0, 0…
    ## $ bmi      <dbl> 29.8, 23.5, 25.9, 36.5, 25.1, 31.8, 27.4, 36.0, 33.1, 30.0, 2…
    ## $ race     <fct> 1, 1, 1, 2, 1, 1, 1, 2, 1, 1, 1, 1, 1, 2, 1, 2, 1, 1, 1, 1, 1…
    ## $ smoker   <dbl> 1, 0, 0, 1, 1, 0, 1, 1, 1, 1, 1, 0, 1, 1, 1, 0, 0, 1, 0, 0, 0…
    ## $ osp      <fct> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
    ## $ koa      <dbl> 1, 0, 1, 1, 0, 1, 1, 0, 0, 0, 0, 1, 0, 0, 0, 1, 1, 0, 1, 0, 0…
    ## $ weights  <dbl> 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
    ## $ subclass <fct> NA, 376, NA, 141, NA, NA, NA, NA, NA, NA, 169, NA, NA, NA, NA…
    ## $ distance <dbl> 0.028437764, 0.465112074, 0.052341221, 0.006110089, 0.0598369…

Note the three new columns at the end. The `weights` column includes the matching weights. In this case, the values are `0` for unmatched cases and `1` for matched cases.

``` r
matched.datasets.long |> 
  distinct(weights)
```

    ##   weights
    ## 1       0
    ## 2       1

The `subclass` column contains the indices for the matched pairs, saved as a factor. Each level of the factor includes two cases from the data, and these were matched based on their covariate set with the genetic algorithm above.

``` r
matched.datasets.long |> 
  filter(.imp == 1) |> 
  count(subclass) |> 
  glimpse()
```

    ## Rows: 480
    ## Columns: 2
    ## $ subclass <fct> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18…
    ## $ n        <int> 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2…

You’ll also note every time `weights == 0`, we have missingness for `subclass`. Those cases that were not matched (i.e., no `subclass` factor level) have an exact zero weight in the substantive analyses.

``` r
matched.datasets.long |> 
  filter(weights == 0) |> 
  distinct(subclass)
```

    ##   subclass
    ## 1     <NA>

Finally, we have the `distance` column. Those values are the propensity scores, which range from zero to one. In a well-matched data set, the `distance` distributions for the two intervention groups mirror one another, and this was the case for our example based on the `bal.plot()` plot from a couple code blocks above.

To give a further sense of the matched pairs and their propensity scores, here’s a plot of the first 30 pairs from the first MI data set.

``` r
matched.datasets.long |> 
  filter(.imp == 1) |> 
  filter(subclass %in% 1:30) |> 
  mutate(osp = factor(osp)) |> 
  
  ggplot(aes(x = distance, y  = subclass, group = subclass)) +
  geom_line() +
  geom_point(aes(color = osp)) +
  scale_x_continuous("distance (i.e., propensity score)", limits = c(0, NA)) +
  scale_color_viridis_d(end = 0.6)
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-38-1.png" width="576" />

Each matched pair contains one of each of the two levels of the treatment variable `osp`. Look how closely the members of each of the pairs are on their propensity scores. The matching wasn’t exact, but it was very close.

## Fit the primary model and compute the estimand

We’re finally ready to fit the substantive model. Technically we could just fit the simple univariable model, which we might also call an ANOVA. In formula syntax, that would be:

`koa ~ osp`.

Given our matching procedure, this simple model would be valid. However, there’s no inherent reason for us not to condition the substantive model on the covariate set. Doing so can help further control for confounding, particularly in the presence of imperfect matching, and it can also increase the precision of the estimate[^8] (see [Greifer & Stuart, 2021](#ref-greifer2021matching)). One approach would be the simple expansion:

`koa ~ osp + age + male + bmi + race + smoker`,

which is often described as an ANCOVA. But we could go even further to protect against any treatment-by-covariate interactions with the fuller

`koa ~ osp * (age + male + bmi + race + smoker)`.

In some of the more recent literature, this has been called an ANHECOVA, an analysis of *heterogeneous* covariance (e.g., [Ye et al., 2022](#ref-ye2022toward)).

In practice, we use the `with()` function to fit the ANHECOVA model to the matched MI data sets.

``` r
matched.models <- with(
  data = matched.datasets,
  expr = glm(koa ~ osp * (age + male + bmi + race + smoker), 
             family = binomial))
```

Here’s the summary of the pooled model parameters. As with a typical MI data analysis, you pool the results across the MI data sets with Rubin’s rules using the `pool()` function.

``` r
matched.models |> 
  pool() |> 
  summary(conf.int = TRUE)
```

    ##           term     estimate    std.error   statistic        df      p.value
    ## 1  (Intercept)  -4.98809313   2.05143801 -2.43151053 478.21271 1.540227e-02
    ## 2         osp1 -16.15915175 479.84319812 -0.03367590 939.91137 9.731427e-01
    ## 3          age   0.02616364   0.01886925  1.38657571 370.98420 1.664038e-01
    ## 4         male  -0.22699036   0.44725227 -0.50752198  78.60782 6.132096e-01
    ## 5          bmi   0.11715216   0.02828944  4.14119691 160.74018 5.566346e-05
    ## 6        race1   0.22400485   1.39529652  0.16054283 181.91644 8.726316e-01
    ## 7        race2   0.51516851   1.45871665  0.35316558 153.85396 7.244477e-01
    ## 8        race3   0.03447518   1.85872612  0.01854775 191.62459 9.852212e-01
    ## 9       smoker  -0.27925667   0.20827985 -1.34077622 310.11309 1.809744e-01
    ## 10    osp1:age   0.02061808   0.02607307  0.79078092 488.44712 4.294556e-01
    ## 11   osp1:male   0.87792221   0.62092728  1.41388894 114.95901 1.600969e-01
    ## 12    osp1:bmi  -0.02576603   0.03808613 -0.67651983 196.10577 4.995078e-01
    ## 13  osp1:race1  14.27235099 479.83808498  0.02974410 939.91137 9.762775e-01
    ## 14  osp1:race2  14.44694881 479.83844551  0.03010794 939.91137 9.759874e-01
    ## 15  osp1:race3  16.40606723 479.84099244  0.03419063 939.91137 9.727324e-01
    ## 16 osp1:smoker   0.34161400   0.29188009  1.17039159 389.36103 2.425593e-01
    ##            2.5 %       97.5 %
    ## 1    -9.01903969  -0.95714657
    ## 2  -957.84716384 925.52886035
    ## 3    -0.01094045   0.06326773
    ## 4    -1.11729281   0.66331209
    ## 5     0.06128525   0.17301907
    ## 6    -2.52904095   2.97705065
    ## 7    -2.36653049   3.39686750
    ## 8    -3.63171524   3.70066561
    ## 9    -0.68907708   0.13056374
    ## 10   -0.03061113   0.07184730
    ## 11   -0.35201989   2.10786431
    ## 12   -0.10087701   0.04934496
    ## 13 -927.40562663 955.95032860
    ## 14 -927.23173633 956.12563395
    ## 15 -925.27761624 958.08975071
    ## 16   -0.23224426   0.91547226

However, in the *MatchIt: Getting Started* vignette, Greifer cautioned:

> The outcome model coefficients and tests should not be interpreted or reported.

But rather, one should only report and interpret the causal estimand, which in our case is the ATT, the *average treatment effect for the treated*.[^9] We can compute the ATT with g-computation using the `avg_comparisons()` function from the **marginaleffects** package.[^10] Note how we can request cluster-robust standard errors with pair membership as the clustering variable by setting `vcov = ~subclass`. By setting `by = "osp"`, we compute both the ATU[^11] and the ATT. Our focus will be the ATT.

``` r
avg_comparisons(matched.models,
                variables = "osp",
                # cluster-robust standard errors for most analyses, 
                # with pair membership as the clustering variable
                vcov = ~subclass,
                by = "osp")
```

    ## Warning in get.dfcom(object, dfcom): Infinite sample size assumed.

    ## 
    ##  Term osp Estimate Std. Error     t Pr(>|t|)   S   2.5 % 97.5 %  Df
    ##   osp   0  -0.0352     0.0329 -1.07    0.285 1.8 -0.0999 0.0295 433
    ##   osp   1  -0.0338     0.0327 -1.04    0.301 1.7 -0.0980 0.0303 426
    ## 
    ## Type:  response 
    ## Comparison: 1 - 0
    ## Columns: term, contrast, osp, estimate, std.error, s.value, df, statistic, p.value, conf.low, conf.high

The second row of the output, for which `osp == 1`, is where we see the summary for the ATT. By default, `avg_comparisons()` puts this in a probability-difference metric, though other metrics can be called with the `comparison` and/or `transform` arguments. My understanding is this summary also automatically pools the results using Rubin’s rules. Substantively, the probability difference is -0.03, 95% CI \[-0.10, 0.03\], which is about as close to zero as you could ask for. This is the effect size you would report in a white paper.

## Thank the reviewer

I’d like to publicly acknowledge and thank

- [Julia Rohrer](https://juliarohrer.com/)

for her kind efforts reviewing the draft of this post. Do note the final editorial decisions were my own.

## Session info

``` r
sessionInfo()
```

    ## R version 4.4.2 (2024-10-31)
    ## Platform: aarch64-apple-darwin20
    ## Running under: macOS Ventura 13.4
    ## 
    ## Matrix products: default
    ## BLAS:   /Library/Frameworks/R.framework/Versions/4.4-arm64/Resources/lib/libRblas.0.dylib 
    ## LAPACK: /Library/Frameworks/R.framework/Versions/4.4-arm64/Resources/lib/libRlapack.dylib;  LAPACK version 3.12.0
    ## 
    ## locale:
    ## [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
    ## 
    ## time zone: America/Chicago
    ## tzcode source: internal
    ## 
    ## attached base packages:
    ## [1] stats     graphics  grDevices utils     datasets  methods   base     
    ## 
    ## other attached packages:
    ##  [1] marginaleffects_0.24.0 cobalt_4.5.5           MatchThem_1.2.1       
    ##  [4] mice_3.16.0            lubridate_1.9.3        forcats_1.0.0         
    ##  [7] stringr_1.5.1          dplyr_1.1.4            purrr_1.0.2           
    ## [10] readr_2.1.5            tidyr_1.3.1            tibble_3.2.1          
    ## [13] ggplot2_3.5.1          tidyverse_2.0.0       
    ## 
    ## loaded via a namespace (and not attached):
    ##  [1] tidyselect_1.2.1  viridisLite_0.4.2 farver_2.1.2      fastmap_1.1.1    
    ##  [5] blogdown_1.20     digest_0.6.37     rpart_4.1.23      timechange_0.3.0 
    ##  [9] lifecycle_1.0.4   survival_3.7-0    magrittr_2.0.3    compiler_4.4.2   
    ## [13] rlang_1.1.4       sass_0.4.9        tools_4.4.2       yaml_2.3.8       
    ## [17] data.table_1.16.2 knitr_1.49        labeling_0.4.3    withr_3.0.2      
    ## [21] nnet_7.3-19       grid_4.4.2        jomo_2.7-6        colorspace_2.1-1 
    ## [25] scales_1.3.0      iterators_1.0.14  MASS_7.3-61       insight_0.20.4   
    ## [29] cli_3.6.3         survey_4.4-2      rmarkdown_2.29    crayon_1.5.3     
    ## [33] generics_0.1.3    rstudioapi_0.16.0 tzdb_0.4.0        minqa_1.2.6      
    ## [37] DBI_1.2.3         cachem_1.0.8      splines_4.4.2     mitools_2.4      
    ## [41] vctrs_0.6.5       WeightIt_1.3.2    boot_1.3-31       glmnet_4.1-8     
    ## [45] Matrix_1.7-1      sandwich_3.1-1    jsonlite_1.8.9    bookdown_0.40    
    ## [49] hms_1.1.3         mitml_0.4-5       foreach_1.5.2     jquerylib_0.1.4  
    ## [53] glue_1.8.0        nloptr_2.0.3      pan_1.9           chk_0.10.0       
    ## [57] codetools_0.2-20  stringi_1.8.4     shape_1.4.6.1     gtable_0.3.6     
    ## [61] lme4_1.1-35.3     munsell_0.5.1     pillar_1.10.1     htmltools_0.5.8.1
    ## [65] R6_2.5.1          evaluate_1.0.1    lattice_0.22-6    backports_1.5.0  
    ## [69] MatchIt_4.7.0     broom_1.0.7       bslib_0.7.0       Rcpp_1.0.13-1    
    ## [73] nlme_3.1-166      checkmate_2.3.2   xfun_0.49         zoo_1.8-12       
    ## [77] pkgconfig_2.0.3

## References

<div id="refs" class="references csl-bib-body hanging-indent" entry-spacing="0" line-spacing="2">

<div id="ref-R-marginaleffects" class="csl-entry">

Arel-Bundock, V. (2023). *<span class="nocase">marginaleffects</span>: Predictions, Comparisons, Slopes, Marginal Means, and Hypothesis Tests* \[Manual\]. [https://vincentarelbundock.github.io/ marginaleffects/ https://github.com/vincentarelbundock/ marginaleffects](https://vincentarelbundock.github.io/ marginaleffects/ https://github.com/vincentarelbundock/ marginaleffects)

</div>

<div id="ref-brumback2022Fundamentals" class="csl-entry">

Brumback, B. A. (2022). *Fundamentals of causal inference with R*. Chapman & Hall/CRC. <https://www.routledge.com/Fundamentals-of-Causal-Inference-With-R/Brumback/p/book/9780367705053>

</div>

<div id="ref-cohenStatisticalPowerAnalysis1988a" class="csl-entry">

Cohen, J. (1988). *Statistical power analysis for the behavioral sciences*. L. Erlbaum Associates. <https://www.worldcat.org/title/statistical-power-analysis-for-the-behavioral-sciences/oclc/17877467>

</div>

<div id="ref-collins2001comparison" class="csl-entry">

Collins, L. M., Schafer, J. L., & Kam, C.-M. (2001). A comparison of inclusive and restrictive strategies in modern missing data procedures. *Psychological Methods*, *6*(4), 330–351. <https://doi.org/10.1037/1082-989x.6.4.330>

</div>

<div id="ref-cunningham2021causal" class="csl-entry">

Cunningham, S. (2021). *Causal inference: The mixtape*. Yale University Press. <https://mixtape.scunning.com/>

</div>

<div id="ref-enders2022applied" class="csl-entry">

Enders, C. K. (2022). *Applied missing data analysis* (Second Edition). Guilford Press. <http://www.appliedmissingdata.com/>

</div>

<div id="ref-gelmanRegressionOtherStories2020" class="csl-entry">

Gelman, A., Hill, J., & Vehtari, A. (2020). *Regression and other stories*. Cambridge University Press. <https://doi.org/10.1017/9781139161879>

</div>

<div id="ref-R-cobalt" class="csl-entry">

Greifer, N. (2024). *<span class="nocase">cobalt</span>: Covariate balance tables and plots* \[Manual\]. <https://CRAN.R-project.org/package=cobalt>

</div>

<div id="ref-greifer2021matching" class="csl-entry">

Greifer, N., & Stuart, E. A. (2021). Matching methods for confounder adjustment: An addition to the epidemiologist’s toolbox. *Epidemiologic Reviews*, *43*(1), 118–129. <https://doi.org/10.1093/epirev/mxab003>

</div>

<div id="ref-hernan2020CausalInference" class="csl-entry">

Hernán, M. A., & Robins, J. M. (2020). *Causal inference: What if*. Boca Raton: Chapman & Hall/CRC. <https://www.hsph.harvard.edu/miguel-hernan/causal-inference-book/>

</div>

<div id="ref-ho2011MatchIt" class="csl-entry">

Ho, D. E., Imai, K., King, G., & Stuart, E. A. (2011). MatchIt: Nonparametric preprocessing for parametric causal inference. *Journal of Statistical Software*, *42*(8), 1–28. <https://doi.org/10.18637/jss.v042.i08>

</div>

<div id="ref-imbensCausalInferenceStatistics2015" class="csl-entry">

Imbens, G. W., & Rubin, D. B. (2015). *Causal inference in statistics, social, and biomedical sciences: An Introduction*. Cambridge University Press. <https://doi.org/10.1017/CBO9781139025751>

</div>

<div id="ref-ismay2022StatisticalInference" class="csl-entry">

Ismay, C., & Kim, A. Y. (2022). *Statistical inference via data science; A moderndive into R and the tidyverse*. <https://moderndive.com/>

</div>

<div id="ref-kazdin2017ResearchDesign" class="csl-entry">

Kazdin, A. E. (2017). *Research design in clinical psychology, 5th Edition*. Pearson. <https://www.pearson.com/>

</div>

<div id="ref-kruschkeDoingBayesianData2015" class="csl-entry">

Kruschke, J. K. (2015). *Doing Bayesian data analysis: A tutorial with R, JAGS, and Stan*. Academic Press. <https://sites.google.com/site/doingbayesiandataanalysis/>

</div>

<div id="ref-leyrat2019propensity" class="csl-entry">

Leyrat, C., Seaman, S. R., White, I. R., Douglas, I., Smeeth, L., Kim, J., Resche-Rigon, M., Carpenter, J. R., & Williamson, E. J. (2019). Propensity score analysis with partially observed covariates: How should multiple imputation be used? *Statistical Methods in Medical Research*, *28*(1), 3–19. <https://doi.org/10.1177/0962280217713032>

</div>

<div id="ref-little2019statistical" class="csl-entry">

Little, R. J., & Rubin, D. B. (2019). *Statistical analysis with missing data* (3rd ed., Vol. 793). John Wiley & Sons. <https://www.wiley.com/en-us/Statistical+Analysis+with+Missing+Data%2C+3rd+Edition-p-9780470526798>

</div>

<div id="ref-mcelreathStatisticalRethinkingBayesian2015" class="csl-entry">

McElreath, R. (2015). *Statistical rethinking: A Bayesian course with examples in R and Stan*. CRC press. <https://xcelab.net/rm/statistical-rethinking/>

</div>

<div id="ref-mcelreathStatisticalRethinkingBayesian2020" class="csl-entry">

McElreath, R. (2020). *Statistical rethinking: A Bayesian course with examples in R and Stan* (Second Edition). CRC Press. <https://xcelab.net/rm/statistical-rethinking/>

</div>

<div id="ref-nguyen2024multiple" class="csl-entry">

Nguyen, T. Q., & Stuart, E. A. (2024). Multiple imputation for propensity score analysis with covariates missing at random: Some clarity on "within" and "across" methods. *American Journal of Epidemiology*, *193*(10), 1470–1476. <https://doi.org/10.1093/aje/kwae105>

</div>

<div id="ref-pishgar2021MatchThem" class="csl-entry">

Pishgar, F., Greifer, N., Leyrat, C., & Stuart, E. (2021). MatchThem:: Matching and weighting after multiple imputation. *The R Journal*. <https://doi.org/10.32614/RJ-2021-073>

</div>

<div id="ref-R-base" class="csl-entry">

R Core Team. (2022). *R: A language and environment for statistical computing*. R Foundation for Statistical Computing. <https://www.R-project.org/>

</div>

<div id="ref-roback2021beyond" class="csl-entry">

Roback, P., & Legler, J. (2021). *Beyond multiple linear regression: Applied generalized linear models and multilevel models in R*. CRC Press. <https://bookdown.org/roback/bookdown-BeyondMLR/>

</div>

<div id="ref-sekhon2011multivariate" class="csl-entry">

Sekhon, J. S. (2011). Multivariate and propensity score matching software with automated balance optimization: The matching package for R. *Journal of Statistical Software*, *42*(7), 1–52. <https://doi.org/10.18637/jss.v042.i07>

</div>

<div id="ref-shadish2002Experimental" class="csl-entry">

Shadish, W. R., Cook, T. D., & Campbell, D. T. (2002). *Experimental and quasi-experimental designs for generalized causal inference*. Houghton, Mifflin and Company.

</div>

<div id="ref-stuart2010matching" class="csl-entry">

Stuart, E. A. (2010). Matching methods for causal inference: A review and a look forward. *Statistical Science: A Review Journal of the Institute of Mathematical Statistics*, *25*(1), 1–25. <https://doi.org/10.1214/09-STS313>

</div>

<div id="ref-sullivan2018should" class="csl-entry">

Sullivan, T. R., White, I. R., Salter, A. B., Ryan, P., & Lee, K. J. (2018). Should multiple imputation be the method of choice for handling missing data in randomized trials? *Statistical Methods in Medical Research*, *27*(9), 2610–2626. <https://doi.org/10.1177/0962280216683570>

</div>

<div id="ref-taback2022DesignAndAnalysis" class="csl-entry">

Taback, N. (2022). *Design and analysis of experiments and observational studies using R*. Chapman and Hall/CRC. <https://doi.org/10.1201/9781003033691>

</div>

<div id="ref-vanbuurenFlexibleImputationMissing2018" class="csl-entry">

van Buuren, S. (2018). *Flexible imputation of missing data* (Second Edition). CRC Press. <https://stefvanbuuren.name/fimd/>

</div>

<div id="ref-mice2011" class="csl-entry">

van Buuren, S., & Groothuis-Oudshoorn, K. (2011). <span class="nocase">mice</span>: Multivariate imputation by chained equations in R. *Journal of Statistical Software*, *45*(3), 1–67. <https://www.jstatsoft.org/v45/i03/>

</div>

<div id="ref-R-mice" class="csl-entry">

van Buuren, S., & Groothuis-Oudshoorn, K. (2021). *<span class="nocase">mice</span>: Multivariate imputation by chained equations* \[Manual\]. <https://CRAN.R-project.org/package=mice>

</div>

<div id="ref-wickhamTidyverseStyleGuide2020" class="csl-entry">

Wickham, H. (2020). *The tidyverse style guide*. <https://style.tidyverse.org/>

</div>

<div id="ref-R-tidyverse" class="csl-entry">

Wickham, H. (2022). *<span class="nocase">tidyverse</span>: Easily install and load the ’tidyverse’*. <https://CRAN.R-project.org/package=tidyverse>

</div>

<div id="ref-wickhamWelcomeTidyverse2019" class="csl-entry">

Wickham, H., Averick, M., Bryan, J., Chang, W., McGowan, L. D., François, R., Grolemund, G., Hayes, A., Henry, L., Hester, J., Kuhn, M., Pedersen, T. L., Miller, E., Bache, S. M., Müller, K., Ooms, J., Robinson, D., Seidel, D. P., Spinu, V., … Yutani, H. (2019). Welcome to the tidyverse. *Journal of Open Source Software*, *4*(43), 1686. <https://doi.org/10.21105/joss.01686>

</div>

<div id="ref-ye2022toward" class="csl-entry">

Ye, T., Shao, J., Yi, Y., & Zhao, Q. (2022). Toward better practice of covariate adjustment in analyzing randomized clinical trials. *Journal of the American Statistical Association*, 1–13. <https://doi.org/10.1080/01621459.2022.2049278>

</div>

<div id="ref-zhao2021propensity" class="csl-entry">

Zhao, Q.-Y., Luo, J.-C., Su, Y., Zhang, Y.-J., Tu, G.-W., & Luo, Z. (2021). Propensity score matching with R: Conventional methods and new features. *Annals of Translational Medicine*, *9*(9). <https://doi.org/10.21037/atm-20-3998>

</div>

</div>

[^1]: Here I’m using the term *quasi-experiment* to mean an intervention that shares the major features of a randomized experiment, such as the causal intervention preceding the outcome, but that lacks random assignment to condition. For more background on this use of the term, see Chapter 1 in Shadish et al. ([2002](#ref-shadish2002Experimental)).

[^2]: Often called an *exposure* variable in the causal-inference literature. I don’t know for sure, but I imagine the term has its roots in the epidemiological literature.

[^3]: Technically this isn’t a pure default call since we’re setting `print = FALSE` to suppress the print history. But this is small potatoes and all the other settings are at their defaults, some of which we’ll be changing shortly.

[^4]: I’m just going to continue paying homage to Pishgar et al. ([2021](#ref-pishgar2021MatchThem)) by reusing many of the object names from their paper.

[^5]: Though we don’t have randomly-assigned groups in this example, I believe the basic concern is the same. The two groups may have different correlations among the covariates, and we want an imputation method that can handle such a pattern.

[^6]: 2023 M2 chip MacBook Pro

[^7]: That is, they’re below `\(d = 0.2\)`. See Cohen ([1988](#ref-cohenStatisticalPowerAnalysis1988a))

[^8]: i.e., increase statistical power by decreasing the standard error.

[^9]: If you’re not familiar with the ATT, it was discussed a bit in Greifer & Stuart ([2021](#ref-greifer2021matching)), though instead by the term “average exposure effect in the exposed.” Greifer recommend the ATT for my real-world use case, which is why I use it here.

[^10]: If you’re not familiar with g-computation, boy do I have the blog series for you. Start [here](https://solomonkurz.netlify.app/blog/2023-04-12-boost-your-power-with-baseline-covariates/). You’ll want to read the first three posts. Sorry, but some topics take a little effort to walk out.

[^11]: That is, the *average treatment effect for the untreated*.
