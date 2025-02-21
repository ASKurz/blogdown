---
title: Example power analysis report, II
author: A. Solomon Kurz
date: '2022-03-05'
draft: false
excerpt: "In an earlier post, I gave an example of what a power analysis report could look like for a multilevel model. At my day job, I was recently asked for a rush-job power analysis that required a multilevel model of a different kind and it seemed like a good opportunity to share another example."
layout: single
tags:
- multilevel
- power
- lme4
- R
- tidyverse
- tutorial
lastmod: '2022-03-05T08:45:58-06:00'
featured: no
bibliography: /Users/solomonkurz/Dropbox/blogdown/content/blog/my_blog.bib
biblio-style: apalike
csl: /Users/solomonkurz/Dropbox/blogdown/content/blog/apa.csl  
link-citations: yes

disable_codefolding: false
codefolding_show: hide
codefolding_nobutton: false
---

## Preamble

In [an earlier post](https://solomonkurz.netlify.app/blog/2021-07-02-example-power-analysis-report/), I gave an example of what a power analysis report could look like for a multilevel model. At my day job, I was recently asked for a rush-job power analysis that required a multilevel model of a different kind and it seemed like a good opportunity to share another example.

For the sake of confidentiality, some of the original content will be omitted or slightly altered. But the overall workflow is about `\(95\%\)` faithful to the original report I submitted to my boss. To understand this report, you should know:

-   my boss has some experience fitting multilevel models, but they’re not a stats jock;
-   we had good pilot data; and
-   this document was meant for internal purposes only.

The pilot data were from `\(N = 50\)` persons, collected on pre- and post-treatment assessment. We had a `\(4\%\)` missing data rate a pre-treatment and `\(10\%\)` missing data a post-treatment, which became an important factor in my analytic strategy. The larger study this power analysis is designed to inform would follow the same basic structure.

Okay, we’re ready for the report. I’ll wrap this post up with a few comments in an [afterward](#afterward) section.

## Executive summary

A total sample size of `\(N = \mathbf{218}\)` is the minimum number to reliably detect a *small* size effect size (i.e., Cohen’s `\(d = 0.2\)`). This recommendation assumes

-   a pre-post single-group design, and
-   a `\(7\%\)` overall missing data rate.

The small effect size estimates and the missing data rate are based on pilot data collected in early `\(2022\)`.

The remainder of this report details how I came to these conclusions. For full transparency, I will supplement prose with the statistical code used for all computations. By default, the code is hidden in this document. However, if you are interested in the code, you should be able to make it appear by selecting “Show All Code” in the dropdown menu from the “Code” button on the upper-right corner.

## Statistical framework and Cohen’s `\(d\)`

In this report, Cohen’s `\(d\)` is meant to indicate a standardized mean difference. The `\(d = 0.2\)` is the conventional cutt-off value for a *small* effect size (see [Cohen, 1988](#ref-cohenStatisticalPowerAnalysis1988a)). In a conventional single-group pre-post study, the formula for `\(d\)` is

$$
d = \frac{\bar y_\text{post} - \bar y_\text{pre}}{s_\text{pre}},
$$

where `\(\bar y_\text{pre}\)` and `\(\bar y_\text{post}\)` are the sample means at each time point and `\(s_\text{pre}\)` is the sample standard deviation at the baseline pre-intervention assessment. The major limitation of this approach is it does not accommodate missing data and our pilot data indicated we should expect about `\(7\%\)` of values will be missing. However, analyzing the data within a multilevel model using a full-information estimator (e.g., maximum likelihood) will allow us to estimate the effect size in the presence of missing values. Under this strategy, the model would follow the equation

$$
`\begin{align*}
y_{it} & \sim \mathcal N(\mu_{it}, \sigma_\epsilon) \\
\mu_{it} & = \beta_0 + \beta_1 \text{time}_{it} + u_{0i} \\
u_{0i} & \sim \mathcal N(0, \sigma_0),
\end{align*}`
$$

where the outcome variable `\(y\)` varies across `\(i\)` persons and over `\(t\)` time. Presuming the data are approximately continuous, they are modeled as normally distributed with a conditional mean `\(\mu_{it}\)`. The `\(\beta_0\)` parameter is the mean value at the pre-treatment baseline assessment and the `\(\beta_1\)` parameter is the difference at the post-treatment assessment. Person-specific differences are captured in the model by `\(u_{0i}\)`, which is itself modeled as normally distributed with a mean of zero and standard deviation `\(\sigma_0\)`. The remaining variance is captured by `\(\sigma_\epsilon\)`, the within-person variation term.

Following the methodological framework developed by Feingold ([2009](#ref-feingoldEffectSizeForGMA2009), [2013](#ref-feingoldARegressionFramework2013)), we can use the results from this model to compute the pre-post standardized mean difference effect size as

$$
d = \frac{\beta_1}{s_\text{pre}}.
$$

A major advantage of this approach is it also returns model-based `\(95\%\)` confidence intervals for the estimate of `\(d\)`.

## Power from simulation

For studies following simple designs, there are known formulas for deriving statistical power. However, these formulas are not robust to missing data, which our pilot trial suggests we will have in a larger-scale study. In such a case, one can determine the statistical power for a study with a simulation-based power analysis. With this approach, one must:

1.  Determine the characteristics of the data you anticipate collecting.
2.  Define the primary statistical model and parameter(s) of interest.
3.  Select a sample size.
4.  Build a custom simulation function that will iteratively
    1.  simulate data resembling the real data you intend to collect,
    2.  fit a statistical model to each data simulation, and
    3.  summarize the parameter(s) of interest in each model.
5.  Iterate a large number of times with the chosen settings to ensure sable results.
6.  Summarize the results.
7.  Iterate across different sample sizes and effect sizes, as needed.

In our case, the parameter of interest is `\(\beta_1\)` from the statistical model outlined in the previous section and the smallest effect size we would reliably like to detect is `\(d = 0.2\)`.

Here is the **R** code for the simulation:

``` r
# load the necessary packages
library(faux)
library(tidyverse)
library(lme4)
library(broom.mixed)

# define the simulation function
sim_lmer <- function(seed = 1, n_group = 20, d = 0.2) {
  
  # set the seed 
  set.seed(seed)
  
  # simulate the data
  d_sim <- rnorm_multi(
    n = n_group,
    mu = c(0, d),
    sd = c(1, 1), 
    r = .5, 
    varnames = list("pre", "post")
  ) %>% 
    mutate(id = 1:n(),
           # simulate missingness
           pre  = ifelse(rbinom(n = n(), size = 1, prob = .04) == 1, NA, pre),
           post = ifelse(rbinom(n = n(), size = 1, prob = .10) == 1, NA, post)) %>% 
    pivot_longer(-id, values_to = "y") %>% 
    mutate(time = ifelse(name == "pre", 0, 1))
  
  # fit the model
  fit_sim <- lmer(
    data = d_sim,
    y ~ 1 + time + (1 | id)
  )
  
  # summarize the results for beta_1
  tidy(fit_sim, conf.int = TRUE) %>% 
    filter(term == "time")
  
}

# run the simulation 1000 times with a sample size of 218
sim218 <- tibble(seed = 1:1000) %>% 
  mutate(tidy = map(seed, sim_lmer, n_group = 218)) %>% 
  unnest(tidy)

# summarize the overall power results
z_critical <- qnorm(p = .975, mean = 0, sd = 1)

sim218 %>% 
  summarise(p = mean(statistic > z_critical))
```

After running the simulation several times with different values of `\(N\)`, we determined a total sample size of `\(N = 218\)` was sufficient to meet the conventional `\(.8\)` power threshold.

## Session information

The make these analyses more reproducible, here is the session information on the software used to make this report and the analyses herein.

``` r
sessionInfo()
```

    ## R version 4.2.0 (2022-04-22)
    ## Platform: x86_64-apple-darwin17.0 (64-bit)
    ## Running under: macOS Big Sur/Monterey 10.16
    ## 
    ## Matrix products: default
    ## BLAS:   /Library/Frameworks/R.framework/Versions/4.2/Resources/lib/libRblas.0.dylib
    ## LAPACK: /Library/Frameworks/R.framework/Versions/4.2/Resources/lib/libRlapack.dylib
    ## 
    ## locale:
    ## [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
    ## 
    ## attached base packages:
    ## [1] stats     graphics  grDevices utils     datasets  methods   base     
    ## 
    ## loaded via a namespace (and not attached):
    ##  [1] bookdown_0.28   digest_0.6.30   R6_2.5.1        jsonlite_1.8.3 
    ##  [5] magrittr_2.0.3  evaluate_0.18   blogdown_1.15   stringi_1.7.8  
    ##  [9] cachem_1.0.6    rlang_1.0.6     cli_3.4.1       rstudioapi_0.13
    ## [13] jquerylib_0.1.4 bslib_0.4.0     rmarkdown_2.16  tools_4.2.0    
    ## [17] stringr_1.4.1   xfun_0.35       yaml_2.3.5      fastmap_1.1.0  
    ## [21] compiler_4.2.0  htmltools_0.5.3 knitr_1.40      sass_0.4.2

## Afterward

I presented the original report with an HTML document, which used the R Markdown [code folding](https://community.rstudio.com/t/notebook-with-code-folding-hide-by-default/55845) option, which hid my code, by default. Since I’m not aware of a good way to use code folding with **blogdown** blog posts, here you see the code in all its glory.

This is *another* example of a frequentist power analysis. Had this not been a rush job, I would have considered running a Bayesian power analysis where the model used weakly-regularizing priors. So it goes…

As to the statistical model, had there not been missing data at the pre-treatment assessment, I might have opted for a simpler single-level model. Another consideration, though, is my boss likes multilevel models and since it seemed like a good option here, why not? As to the simulation code, you may have noticed I used the `rnorm_multi()` function from the [**faux** package](https://github.com/debruine/faux) ([DeBruine, 2021](#ref-R-faux)), which I find handy for data with small numbers of time points. In `rnorm_multi()`, I set the correlation between the two time points at `\(\rho = .5\)`, which was based on the simple correlation from the pilot data.

In retrospect, it might have been a good idea to have considered simulating the data with a slightly larger standard deviation for the post-treatment assessment. My experience is it’s not uncommon for variation to increase over time. I could have used the sample standard deviations from the pilot data as a guide. Now that I’ve written this in a blog post, perhaps it’ll occur to me more readily next time.

The analysis from the pilot data suggested a post-treatment `\(d = 0.35\)`, `\(95\% \text{ CI} [0.05, 0.65]\)`. In the power analysis, we used `\(d = 0.2\)` because (a) we wanted a conservative estimate and (b) it was the smallest effect size we were interested in reliably detecting.

Relatedly, the results of this quick-and-dirty power analysis are focused in that I provide a single number ($N = 218$) required to reliably detect a single effect size ($d = 0.2$) for a single set of assumptions about missing data ($4\%$ at pre-treatment and `\(10\%\)` at post-treatment). A slower and less-dirty analysis could return the required sample size for different combinations of effect sizes and missing-data rates. You might present such information with a series of power-curve plots or nicely-formatted charts.

Had I intended to share a report like this for a broader audience, possibly as supplemental material for a paper, I might have explained my custom `sim_lmer()` code. Since this was originally meant for internal use, my main goal was to present the results with an extra bit of transparency and so that even if I somehow lost my original files, I would always have the basics of the code documented in the report I emailed to my boss.

I should also add that even though it isn’t spelled out in the code, I came to the `\(N = 218\)` figure by iterating over several different sample size options. It seemed unnecessary to clutter up the file with those details.

## References

<div id="refs" class="references csl-bib-body hanging-indent" line-spacing="2">

<div id="ref-cohenStatisticalPowerAnalysis1988a" class="csl-entry">

Cohen, J. (1988). *Statistical power analysis for the behavioral sciences*. L. Erlbaum Associates. <https://www.worldcat.org/title/statistical-power-analysis-for-the-behavioral-sciences/oclc/17877467>

</div>

<div id="ref-R-faux" class="csl-entry">

DeBruine, L. (2021). *<span class="nocase">faux</span>: Simulation for factorial designs* \[Manual\]. <https://github.com/debruine/faux>

</div>

<div id="ref-feingoldEffectSizeForGMA2009" class="csl-entry">

Feingold, A. (2009). Effect sizes for growth-modeling analysis for controlled clinical trials in the same metric as for classical analysis. *Psychological Methods*, *14*(1), 43. <https://doi.org/10.1037/a0014699>

</div>

<div id="ref-feingoldARegressionFramework2013" class="csl-entry">

Feingold, A. (2013). A regression framework for effect size assessments in longitudinal modeling of group differences. *Review of General Psychology*, *17*(1), 111–121. <https://doi.org/10.1037/a0030048>

</div>

</div>
