---
title: Sexy up your logistic regression model with logit dotplots
author: A. Solomon Kurz
date: '2021-09-22'
draft: false
excerpt: "The major shortcoming in typical logistic regression line plots is they usually don't show the data due to overplottong across the *y*-axis. Happily, new developments with Matthew Kay's **ggdist** package make it easy to show your data when you plot your logistic regression curves. In this post I'll show you how."
layout: single
tags:
- binomial
- ggdist
- logistic regression
- plot
- R
- tidyverse
- tutorial
lastmod: '2021-09-22T09:40:23-05:00'
featured: no
bibliography: /Users/solomonkurz/Dropbox/blogdown/content/post/my_blog.bib
biblio-style: apalike
csl: /Users/solomonkurz/Dropbox/blogdown/content/post/apa.csl  
link-citations: yes
---

## What

When you fit a logistic regression model, there are a lot of ways to display the results. One of the least inspiring ways is to report a summary of the coefficients in prose or within a table. A more artistic approach is to show the fitted line in a plot, which often looks nice due to the curvy nature of logistic regression lines. The major shortcoming in typical logistic regression line plots is they usually don’t show the data due to overplottong across the `\(y\)`-axis. Happily, new developments with Matthew Kay’s ([2021](#ref-R-ggdist)) [**ggdist** package](https://mjskay.github.io/ggdist/) make it easy to show your data when you plot your logistic regression curves. In this post I’ll show you how.

### I make assumptions.

For this post, I’m presuming some background knowledge:

-   You should be familiar with logistic regression. For introductions, I recommend Roback and Legler’s ([2021](#ref-roback2021beyond)) online text or James, Witten, Hastie, and Tibshirani’s ([2021](#ref-james2021AnIntroduction)) online text. Both texts are written from a frequentist perspective, which is also the framework we’ll be using in this blog post. For Bayesian introductions to logistic regression, I recommend either edition of McElreath’s text ([2020](#ref-mcelreathStatisticalRethinkingBayesian2020), [2015](#ref-mcelreathStatisticalRethinkingBayesian2015)); Kruschke’s ([2015](#ref-kruschkeDoingBayesianData2015)) text; or Gelman, Hill, and Vehtari’s ([2020](#ref-gelmanRegressionOtherStories2020)) text.

-   All code is in **R** ([R Core Team, 2022](#ref-R-base)). Data wrangling and plotting were done with help from the **tidyverse** ([Wickham et al., 2019](#ref-wickhamWelcomeTidyverse2019); [Wickham, 2022](#ref-R-tidyverse)) and **broom** ([Robinson et al., 2022](#ref-R-broom)). The data are from the [**fivethirtyeight** package](https://github.com/debruine/faux) ([Kim et al., 2018](#ref-fivethirtyeight2018), [2020](#ref-R-fivethirtyeight)).

Here we load our primary **R** packages.

``` r
library(tidyverse)
library(fivethirtyeight)
library(broom)
library(ggdist)
```

### We need data.

In this post, we’ll be working with the `bechdel` data set. From the documentation, we read these are “the raw data behind the story ‘[The Dollar-And-Cents Case Against Hollywood’s Exclusion of Women](https://fivethirtyeight.com/features/the-dollar-and-cents-case-against-hollywoods-exclusion-of-women/).’”

``` r
data(bechdel)

glimpse(bechdel)
```

    ## Rows: 1,794
    ## Columns: 15
    ## $ year          <int> 2013, 2012, 2013, 2013, 2013, 2013, 2013, 2013, 2013, 20…
    ## $ imdb          <chr> "tt1711425", "tt1343727", "tt2024544", "tt1272878", "tt0…
    ## $ title         <chr> "21 & Over", "Dredd 3D", "12 Years a Slave", "2 Guns", "…
    ## $ test          <chr> "notalk", "ok-disagree", "notalk-disagree", "notalk", "m…
    ## $ clean_test    <ord> notalk, ok, notalk, notalk, men, men, notalk, ok, ok, no…
    ## $ binary        <chr> "FAIL", "PASS", "FAIL", "FAIL", "FAIL", "FAIL", "FAIL", …
    ## $ budget        <int> 13000000, 45000000, 20000000, 61000000, 40000000, 225000…
    ## $ domgross      <dbl> 25682380, 13414714, 53107035, 75612460, 95020213, 383624…
    ## $ intgross      <dbl> 42195766, 40868994, 158607035, 132493015, 95020213, 1458…
    ## $ code          <chr> "2013FAIL", "2012PASS", "2013FAIL", "2013FAIL", "2013FAI…
    ## $ budget_2013   <int> 13000000, 45658735, 20000000, 61000000, 40000000, 225000…
    ## $ domgross_2013 <dbl> 25682380, 13611086, 53107035, 75612460, 95020213, 383624…
    ## $ intgross_2013 <dbl> 42195766, 41467257, 158607035, 132493015, 95020213, 1458…
    ## $ period_code   <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,…
    ## $ decade_code   <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,…

The data were collected on Hollywood movies made between 1970 and 2013.

``` r
bechdel %>% 
  pull(year) %>% 
  range()
```

    ## [1] 1970 2013

Our focal variable will be `binary`, which indicates whether a given movie passed the Bechdel test. Of the `\(1{,}794\)` movies in the data set, just under half of them passed.

``` r
bechdel %>% 
  count(binary) %>% 
  mutate(percent = 100 * n / sum(n))
```

    ## # A tibble: 2 × 3
    ##   binary     n percent
    ##   <chr>  <int>   <dbl>
    ## 1 FAIL     991    55.2
    ## 2 PASS     803    44.8

Our sole predictor variable will be `budget_2013`, each movie’s budget as expressed in 2013 dollars.

``` r
bechdel %>% 
  ggplot(aes(x = budget_2013)) +
  geom_histogram() +
  facet_wrap(~ binary, ncol = 1)
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-5-1.png" width="672" />

To make our lives a little easier, we’ll convert the character variable `binary` into a conventional `\(0/1\)` numeric variable called `pass`.

``` r
# compute
bechdel <- bechdel  %>% 
  mutate(pass = ifelse(binary == "FAIL", 0, 1)) 

# compare
bechdel %>% 
  select(binary, pass) %>% 
  head()
```

    ## # A tibble: 6 × 2
    ##   binary  pass
    ##   <chr>  <dbl>
    ## 1 FAIL       0
    ## 2 PASS       1
    ## 3 FAIL       0
    ## 4 FAIL       0
    ## 5 FAIL       0
    ## 6 FAIL       0

## Model

We can express our statistical model in formal notation as

$$
`\begin{align*}
\text{pass}_i & \sim \operatorname{Binomial}(n = 1, p_i) \\
\operatorname{logit}(p_i) & = \beta_0 + \beta_1 \text{budget_2013}_i,
\end{align*}`
$$

where we use the conventional logit link to ensure the binomial probabilities are restricted within the bounds of zero and one. We can fit such a model with the base **R** `glm()` function like so.

``` r
fit <- glm(
  data = bechdel,
  family = binomial,
  pass ~ 1 + budget_2013)
```

A conventional way to present the results would in a coefficient table, the rudiments of which we can get from the `broom::tidy()` function.

``` r
tidy(fit) %>% 
  knitr::kable()
```

| term        |  estimate | std.error | statistic |   p.value |
|:------------|----------:|----------:|----------:|----------:|
| (Intercept) | 0.1113148 | 0.0689661 |  1.614051 | 0.1065163 |
| budget_2013 | 0.0000000 | 0.0000000 | -6.249724 | 0.0000000 |

Because of the scale of the `budget_2013` variable, its point estimate and standard errors are both very small. To give a little perspective, here is the expected decrease in log-odds for a budget increase in $\$100{,}000{,}000$.

``` r
c(coef(fit)[2], confint(fit)[2, ]) * 1e8
```

    ## budget_2013       2.5 %      97.5 % 
    ##  -0.5972374  -0.7875178  -0.4126709

Note how we added in the 95% confidence intervals for good measure.

## Line plots

Now we have interpreted the model in the dullest way possible, with a table and in prose, let’s practice plotting the results. First, we’ll use the widely-used method of displaying only the fitted line.

### Fitted line w/o data.

We can use the `predict()` function along with some post-processing strategies from [Gavin Simpson](https://twitter.com/ucfagls)’s fine blog post, [*Confidence intervals for GLMs*](https://fromthebottomoftheheap.net/2018/12/10/confidence-intervals-for-glms/), to prepare the data necessary for making our plot.

``` r
# define the new data
nd <- tibble(budget_2013 = seq(from = 0, to = 500000000, length.out = 100))

p <-
  # compute the fitted lines and SE's
  predict(fit,
          newdata = nd,
          type = "link",
          se.fit = TRUE) %>% 
  # wrangle
  data.frame() %>% 
  mutate(ll = fit - 1.96 * se.fit,
         ul = fit + 1.96 * se.fit) %>% 
  select(-residual.scale, -se.fit) %>% 
  mutate_all(plogis) %>%
  bind_cols(nd)

# what have we done?
glimpse(p)
```

    ## Rows: 100
    ## Columns: 4
    ## $ fit         <dbl> 0.5278000, 0.5202767, 0.5127442, 0.5052059, 0.4976652, 0.4…
    ## $ ll          <dbl> 0.4940356, 0.4881515, 0.4821772, 0.4760998, 0.4699043, 0.4…
    ## $ ul          <dbl> 0.5613120, 0.5522351, 0.5432161, 0.5342767, 0.5254405, 0.5…
    ## $ budget_2013 <dbl> 0, 5050505, 10101010, 15151515, 20202020, 25252525, 303030…

Here’s a conventional line plot for our logistic regression model.

``` r
p %>% 
  ggplot(aes(x = budget_2013, y = fit)) +
  geom_ribbon(aes(ymin = ll, ymax = ul),
              alpha = 1/2) +
  geom_line() +
  scale_y_continuous("probability of passing", 
                     expand = c(0, 0), limits = 0:1)
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-11-1.png" width="672" />

The fitted line is in black and the semitransparent grey ribbon marks of the 95% confidence intervals. The plot does a nice job showing how movies with larger budgets tend to do a worse job passing the Bechdel test.

### Improve the visualization by adding data.

If you wanted to add the data to our plot, a naïve approach might be to use `geom_point()`.

``` r
p %>% 
  ggplot(aes(x = budget_2013, y = fit)) +
  geom_ribbon(aes(ymin = ll, ymax = ul),
              alpha = 1/2) +
  geom_line() +
  geom_point(data = bechdel,
             aes(y = pass),
             alpha = 1/2) +
  scale_y_continuous("probability of passing", 
                     expand = expansion(mult = 0.01))
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-12-1.png" width="672" />

Even by making the dots semitransparent with the `alpha` parameter, the overplotting issue makes it very difficult to make sense of the data. One of the approaches favored by Gelman and colleagues ([2020](#ref-gelmanRegressionOtherStories2020)) is to add a little vertical jittering. We can do that with `geom_jitter()`.

``` r
p %>% 
  ggplot(aes(x = budget_2013, y = fit)) +
  geom_ribbon(aes(ymin = ll, ymax = ul),
              alpha = 1/2) +
  geom_line() +
  geom_jitter(data = bechdel,
              aes(y = pass),
              size = 1/4, alpha = 1/2, height = 0.05) +
  scale_y_continuous("probability of passing", 
                     expand = c(0, 0))
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-13-1.png" width="672" />

Though a big improvement, this approach still doesn’t do the best job depicting the distribution of the `budget_2013` values. If possible, it would be better to explicitly depict the `budget_2013` distributions for each level of `pass` with something more like histograms. In his blogpost, [*Using R to make sense of the generalised linear model*](https://www.barelysignificant.com/post/glm/), [Ladislas Nalborczyk](https://twitter.com/lnalborczyk) showed how you could do so with a custom function he named `logit_dotplot()`, the source code for which you can find [here](https://github.com/lnalborczyk/lnalborczyk.github.io/blob/master/code/logit_dotplot.R) on his GitHub. Since Nalborczyk’s post, this kind of functionality has since been built into Kay’s **ggdist** package. Here’s what it looks like.

``` r
p %>% 
  ggplot(aes(x = budget_2013)) +
  geom_ribbon(aes(ymin = ll, ymax = ul),
              alpha = 1/2) +
  geom_line(aes(y = fit)) +
  stat_dots(data = bechdel,
            aes(y = pass, side = ifelse(pass == 0, "top", "bottom")),
            scale = 1/3) +
  scale_y_continuous("probability of passing",
                     expand = c(0, 0))
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-14-1.png" width="672" />

With the `stat_dots()` function, we added dotplots, which are nifty alternatives to histograms which display each data value as an individual dot. With the `side` argument, we used a conditional statement to tell `stat_dots()` we wanted some of the `budget_2013` to be displayed on the bottom and other of those values to be displayed on the top. With the `scale` argument, we indicated how much of the total space within the range of the `\(y\)`-axis we wanted the dot plot distributions to take up.

For kicks and giggles, here’s a more polished version of what such a plot could look like.

``` r
p %>% 
  ggplot(aes(x = budget_2013)) +
  geom_ribbon(aes(ymin = ll, ymax = ul),
              alpha = 1/2) +
  geom_line(aes(y = fit)) +
  stat_dots(data = bechdel %>% 
              mutate(binary = factor(binary, levels = c("PASS", "FAIL"))),
            aes(y = pass, 
                side = ifelse(pass == 0, "top", "bottom"),
                color = binary),
            scale = 0.4, shape = 19) +
  scale_color_manual("Bechdel test", values = c("#009E73", "#D55E00")) +
  scale_x_continuous("budget (in 2013 dollars)",
                     breaks = c(0, 1e8, 2e8, 3e8, 4e8),
                     labels = c(0, str_c(1:4 * 100, " mill")),
                     expand = c(0, 0), limits = c(0, 48e7)) +
  scale_y_continuous("probability of passing",
                     expand = c(0, 0)) +
  theme(panel.grid = element_blank())
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-15-1.png" width="672" />

Other distributional forms are possible, too. For example, here we set `slab_type = "histogram"` within the `stat_slab()` function to swap out the dotplots for histograms.

``` r
p %>% 
  ggplot(aes(x = budget_2013)) +
  geom_ribbon(aes(ymin = ll, ymax = ul),
              alpha = 1/2) +
  geom_line(aes(y = fit)) +
  # the magic lives here
  stat_slab(data = bechdel %>% 
              mutate(binary = factor(binary, levels = c("PASS", "FAIL"))),
            aes(y = pass, 
                side = ifelse(pass == 0, "top", "bottom"),
                fill = binary, color = binary),
            slab_type = "histogram",
            scale = 0.4, breaks = 40, size = 1/2) +
  scale_fill_manual("Bechdel test", values = c(alpha("#009E73", .7), alpha("#D55E00", .7))) +
  scale_color_manual("Bechdel test", values = c("#009E73", "#D55E00")) +
  scale_x_continuous("budget (in 2013 dollars)",
                     breaks = c(0, 1e8, 2e8, 3e8, 4e8),
                     labels = c(0, str_c(1:4 * 100, " mill")),
                     expand = c(0, 0), limits = c(0, 48e7)) +
  scale_y_continuous("probability of passing",
                     expand = c(0, 0)) +
  theme(panel.grid = element_blank())
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-16-1.png" width="672" />

That’s a wrap, friends. No more lonely logistic curves absent data. Flaunt those sexy data with **ggdist**.

## Session info

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
    ## other attached packages:
    ##  [1] ggdist_3.2.0          broom_1.0.1           fivethirtyeight_0.6.2
    ##  [4] forcats_0.5.1         stringr_1.4.1         dplyr_1.0.10         
    ##  [7] purrr_0.3.4           readr_2.1.2           tidyr_1.2.1          
    ## [10] tibble_3.1.8          ggplot2_3.4.0         tidyverse_1.3.2      
    ## 
    ## loaded via a namespace (and not attached):
    ##  [1] lubridate_1.8.0      assertthat_0.2.1     digest_0.6.30       
    ##  [4] utf8_1.2.2           R6_2.5.1             cellranger_1.1.0    
    ##  [7] backports_1.4.1      reprex_2.0.2         evaluate_0.18       
    ## [10] highr_0.9            httr_1.4.4           blogdown_1.15       
    ## [13] pillar_1.8.1         rlang_1.0.6          googlesheets4_1.0.1 
    ## [16] readxl_1.4.1         rstudioapi_0.13      jquerylib_0.1.4     
    ## [19] rmarkdown_2.16       labeling_0.4.2       googledrive_2.0.0   
    ## [22] munsell_0.5.0        compiler_4.2.0       modelr_0.1.8        
    ## [25] xfun_0.35            pkgconfig_2.0.3      htmltools_0.5.3     
    ## [28] tidyselect_1.1.2     bookdown_0.28        fansi_1.0.3         
    ## [31] crayon_1.5.2         tzdb_0.3.0           dbplyr_2.2.1        
    ## [34] withr_2.5.0          MASS_7.3-58.1        distributional_0.3.1
    ## [37] grid_4.2.0           jsonlite_1.8.3       gtable_0.3.1        
    ## [40] lifecycle_1.0.3      DBI_1.1.3            magrittr_2.0.3      
    ## [43] scales_1.2.1         cli_3.4.1            stringi_1.7.8       
    ## [46] cachem_1.0.6         farver_2.1.1         fs_1.5.2            
    ## [49] xml2_1.3.3           bslib_0.4.0          ellipsis_0.3.2      
    ## [52] generics_0.1.3       vctrs_0.5.0          tools_4.2.0         
    ## [55] glue_1.6.2           hms_1.1.1            fastmap_1.1.0       
    ## [58] yaml_2.3.5           colorspace_2.0-3     gargle_1.2.0        
    ## [61] rvest_1.0.2          knitr_1.40           haven_2.5.1         
    ## [64] sass_0.4.2

## References

<div id="refs" class="references csl-bib-body hanging-indent" line-spacing="2">

<div id="ref-gelmanRegressionOtherStories2020" class="csl-entry">

Gelman, A., Hill, J., & Vehtari, A. (2020). *Regression and other stories*. Cambridge University Press. <https://doi.org/10.1017/9781139161879>

</div>

<div id="ref-james2021AnIntroduction" class="csl-entry">

James, G., Witten, D., Hastie, T., & Tibshirani, R. (2021). *An introduction to statistical learning with applications in R* (Second Edition). Springer. <https://web.stanford.edu/~hastie/ISLRv2_website.pdf>

</div>

<div id="ref-R-ggdist" class="csl-entry">

Kay, M. (2021). *<span class="nocase">ggdist</span>: Visualizations of distributions and uncertainty* \[Manual\]. <https://CRAN.R-project.org/package=ggdist>

</div>

<div id="ref-fivethirtyeight2018" class="csl-entry">

Kim, A. Y., Ismay, C., & Chunn, J. (2018). The fivethirtyeight R package: ’Tame data’ principles for introductory statistics and data science courses. *Technology Innovations in Statistics Education*, *11*(1). <https://escholarship.org/uc/item/0rx1231m>

</div>

<div id="ref-R-fivethirtyeight" class="csl-entry">

Kim, A. Y., Ismay, C., & Chunn, J. (2020). *<span class="nocase">fivethirtyeight</span>: Data and code behind the stories and interactives at FiveThirtyEight* \[Manual\]. <https://github.com/rudeboybert/fivethirtyeight>

</div>

<div id="ref-kruschkeDoingBayesianData2015" class="csl-entry">

Kruschke, J. K. (2015). *Doing Bayesian data analysis: A tutorial with R, JAGS, and Stan*. Academic Press. <https://sites.google.com/site/doingbayesiandataanalysis/>

</div>

<div id="ref-mcelreathStatisticalRethinkingBayesian2020" class="csl-entry">

McElreath, R. (2020). *Statistical rethinking: A Bayesian course with examples in R and Stan* (Second Edition). CRC Press. <https://xcelab.net/rm/statistical-rethinking/>

</div>

<div id="ref-mcelreathStatisticalRethinkingBayesian2015" class="csl-entry">

McElreath, R. (2015). *Statistical rethinking: A Bayesian course with examples in R and Stan*. CRC press. <https://xcelab.net/rm/statistical-rethinking/>

</div>

<div id="ref-R-base" class="csl-entry">

R Core Team. (2022). *R: A language and environment for statistical computing*. R Foundation for Statistical Computing. <https://www.R-project.org/>

</div>

<div id="ref-roback2021beyond" class="csl-entry">

Roback, P., & Legler, J. (2021). *Beyond multiple linear regression: Applied generalized linear models and multilevel models in R*. CRC Press. <https://bookdown.org/roback/bookdown-BeyondMLR/>

</div>

<div id="ref-R-broom" class="csl-entry">

Robinson, D., Hayes, A., & Couch, S. (2022). *<span class="nocase">broom</span>: Convert statistical objects into tidy tibbles* \[Manual\]. <https://CRAN.R-project.org/package=broom>

</div>

<div id="ref-R-tidyverse" class="csl-entry">

Wickham, H. (2022). *<span class="nocase">tidyverse</span>: Easily install and load the ’tidyverse’*. <https://CRAN.R-project.org/package=tidyverse>

</div>

<div id="ref-wickhamWelcomeTidyverse2019" class="csl-entry">

Wickham, H., Averick, M., Bryan, J., Chang, W., McGowan, L. D., François, R., Grolemund, G., Hayes, A., Henry, L., Hester, J., Kuhn, M., Pedersen, T. L., Miller, E., Bache, S. M., Müller, K., Ooms, J., Robinson, D., Seidel, D. P., Spinu, V., … Yutani, H. (2019). Welcome to the tidyverse. *Journal of Open Source Software*, *4*(43), 1686. <https://doi.org/10.21105/joss.01686>

</div>

</div>
