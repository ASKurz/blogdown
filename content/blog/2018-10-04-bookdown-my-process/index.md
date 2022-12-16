---
title: bookdown, My Process
author: A. Solomon Kurz
date: '2018-10-04'
draft: false
excerpt: "The purpose of this post is to give readers a sense of how I used bookdown to make my first ebooks. I propose there are three fundamental skill sets you need basic fluency in before playing with bookdown: (a) R and R Studio, (b) scripts and R Markdown files, and (c) Git and GitHub."
layout: single
tags:
- Bayesian
- bookdown
- brms
- Git
- GitHub
- Markdown
- R
- Statistical Rethinking
- tidyverse
- tutorial
lastmod: '2021-04-21T13:20:06-07:00'
bibliography: /Users/solomonkurz/Dropbox/blogdown/content/blog/my_blog.bib
biblio-style: apalike
csl: /Users/solomonkurz/Dropbox/blogdown/content/blog/apa.csl  
link-citations: yes
---

## tl;dr

I just self-published a book-length version of my project [*Statistical Rethinking* with brms, ggplot2, and the tidyverse](https://bookdown.org/content/3890/). By using Yihui Xie’s [bookdown package](https://bookdown.org), I was able to do it for free. If you’ve never heard of it, bookdown enables [R](https://bookdown.org/rdpeng/rprogdatascience/history-and-overview-of-r.html#what-is-r) users to write books and other long-form articles with [R Markdown](https://rmarkdown.rstudio.com). You can save your bookdown products in a variety of formats (e.g., PDF, HTML) and publish them in several ways, too. The purpose of this post is to give readers a sense of how I used bookdown to make my project. I propose there are three fundamental skill sets you need basic fluency in before playing with bookdown. Those three are

-   R and R Studio,
-   Scripts and R Markdown files, and
-   Git and GitHub.

## Start with [R](https://cran.r-project.org)

First things first. Since bookdown is a package for use in the R environment, you’re going to have to use R. If you’re unfamiliar with it, [R is a freely-available programming language particularly well-suited for data analysis](https://bookdown.org/rdpeng/rprogdatascience/history-and-overview-of-r.html). If you’ve not used R before, learning how to self-publish books is a great incentive to start learning. But unless you already have a background in programming, I think bookdown is poorly-suited for novices. R newbies should check out Roger Peng’s [*R Programming for Data Science*](https://bookdown.org/rdpeng/rprogdatascience/) or Grolemund and Wickham’s [*R for Data Science*](http://r4ds.had.co.nz). Both are freely available online and, as it would turn out, made with bookdown. Also, new users should be aware that although you can interact with R directly, there are a variety of other ways to interface with R. I recommend using [R Studio](https://www.rstudio.com). You can find some nice reasons, [here](https://www.theanalysisfactor.com/the-advantages-of-rstudio/). For basic instructions on how to install R and R Studio, you might start [here](http://r4ds.had.co.nz/introduction.html#prerequisites). And if you prefer video tutorials to help you with the installation, just do a simple search in your favorite video-sharing website and several should pop up.

Personally, I started using R—via R Studio—during the 2015/2016 winter break before taking a spring semester statistics course based around an R package. \[In case you’re curious, it was a structural equation modeling course based around a [text by Beaujean](https://blogs.baylor.edu/rlatentvariable/) which featured the [lavaan package](http://lavaan.ugent.be)\]. At the time, I was already familiar with structural equation modeling, so the course was a nice opportunity to learn R. In addition, I was concurrently enrolled in a course on multilevel modeling based on [Singer and Willet’s classic text](http://gseacademic.harvard.edu/alda/). The professor of that course primarily used SAS to teach the material, but he was flexible and allowed me to do the work with R, instead. So that was my introduction to R–a semester of immersion in [\#rstats](https://twitter.com/search?q=%23rstats&src=typd). Here are some other [tips on how to learn R](https://www.r-bloggers.com/the-5-most-effective-ways-to-learn-r/).

## bookdown uses [Markdown](https://daringfireball.net/projects/markdown/syntax)

If you work with R through R Studio, you can do a handful of things through dropdowns. But really, if you’re going to be using R, you’re going to be coding. As it turns out, there are a variety of ways to code in R. One of the most basic ways is via the [console](https://support.rstudio.com/hc/en-us/articles/200404846-Working-in-the-Console), which I’m not going to cover in any detail.

The console is fine for quick operations, but you’re going to want to do most of your coding in some kind of a script. R Studio allows users to save and execute code in script files, which you can learn more about [here](http://r4ds.had.co.nz/workflow-scripts.html). Basic script files are nice in that they allow you to both save and annotate your code.

However, the annotation options in R Studio script files are limited. After using R Studio scripts for about a year, I learned about [R Notebooks](https://rmarkdown.rstudio.com/r_notebooks). These are special files that allow you to intermingle your R code with prose and the results of the code. R Notebooks also allow users to transform the working documents into professional-looking reports in various formats (e.g., PDF, HTML). And unlike the primitive annotation options with simple script files, R Notebooks use Markdown to allow users to format their prose with things like headers, italicized font, insert hyperlinks, and even embed images. So [Markdown](https://daringfireball.net/projects/markdown/), then, is a simple language that allows for many of those functions.

Within the R Studio environment, you can use Markdown with two basic file types: [R Markdown](https://rmarkdown.rstudio.com/lesson-1.html) files and [R Notebook](https://rmarkdown.rstudio.com/r_notebooks) files. R Notebook files are just special kinds of R Markdown files that have, IMO, a better interface. That is, R Notebooks are the newer nicer version of R Markdown files. The main point here is that when I say “bookdown uses Markdown”, I’m pointing out that one of the important skills you’ll want to develop before making content with bookdown is how to use Markdown within R Studio. It’s not terribly complicated to learn, and you can get an overview of the basics [here](https://www.rstudio.com/wp-content/uploads/2015/02/rmarkdown-cheatsheet.pdf) or [here](http://r4ds.had.co.nz/r-markdown.html) or [here](https://bookdown.org/yihui/bookdown/markdown-syntax.html), or an exhaustive treatment [here](https://bookdown.org/yihui/rmarkdown/).

If you’re a novice, it’ll take you a few days, weeks, or months to get a firm grasp of R. Not so with R Markdown files. You’ll have the basics of those down in an afternoon. That said, I had been an R Notebook user for more than a year before trying my hand at bookdown.

The first big edition of my [*Statistical Rethinking* with brms, ggplot2, and the tidyverse](https://bookdown.org/content/3890/) project came in the form of R Notebook files and their HTML counterparts stored in one of my projects on the [Open Science Framework](https://osf.io/?gclid=EAIaIQobChMI2ZDP9svj3QIVQ7nACh2rYQIHEAAYASAAEgL7avD_BwE). I don’t update it very often, but you can still find it [here](https://osf.io/97t6w/). If you’re not familiar with it, the OSF is a [“free, open source web application that connects and supports the research workflow, enabling scientists to increase the efficiency and effectiveness of their research.”](https://osf.io/4znzp/wiki/home/) In addition to their wiki, you might check out some of their [video tutorials](https://cos.io/our-services/training-services/cos-training-tutorials/).

## You’ll need [GitHub](https://github.com), too

I’m actually not sure whether you need to know how to use Git and GitHub to use bookdown. In his authoritative book, [*bookdown: Authoring Books and Technical Documents with R Markdown*](https://bookdown.org/yihui/bookdown/), Yihui Xie mentioned GitHub in every chapter. If you go to your favorite video-sharing website to look for instructional videos on bookdown, you’ll see the instructors take GitHub as a given, too. If you’re stubborn and have enough ingenuity, you might find a way to successfully use bookdown without GitHub, but you may as well go with the flow on this one.

If you’ve never heard of it before, [Git is a system for version control](https://git-scm.com/book/en/v2/Getting-Started-About-Version-Control). By version control, I mean a system by which you can keep track of changes to your code, over time. Even if you don’t have a background in programming, consider a scenario where you had to keep track of many versions of a writing project, perhaps saving your files as `first_draft.docx`, `second_draft.docx`, `final_draft.docx`, `final_draft_2.docx`… This was your own make-shift attempt at version control for writing. I’ve seen a lot of introductory material recommend Git and GitHub by leading with version control. And indeed, they do serve that purpose. But IMO, leading with version control is a rhetorical mistake when talking to non-programmers. I haven’t found Git and GitHub the most intuitive and if version control was the only benefit, they wouldn’t be worth the effort. But there are other good reasons to learn.

IMO, the best reason to learn Git and GitHub is because they allow you to make your work publicly available. When you just use Git, the work stays on your computer. But GitHub allows you to save your files online, too. This makes it easy for others to review them and give you feedback. GitHub also allows you to save things like data files online. So if you’re a working scientist, Git and GitHub might allow you to make a site—a repository—to house the de-identified data and statistical code for one of your projects. It’s another way to do open science. In addition, you can repurpose GitHub to work as blog or an analytic portfolio. And if you’d like to use bookdown, Git and GitHub will be a part of how you manage the files for your projects and make your work more accessible to others.

If you’re new to all this, you could probably blindly follow along with the steps in Yihui Xie’s bookdown [manual](https://bookdown.org/yihui/bookdown/) or any of the online video tutorials. But I suspect that’d be pretty confusing. Before attempting a bookdown project, spend some time getting comfortable with Git and GitHub, first. The best introduction to the topic I’ve seen is Jenny Bryan’s [*Happy Git and GitHub for the useR*](http://happygitwithr.com), which, you guessed it, is also freely available and powered by bookdown.

As I hinted, I found Git and GitHub baffling, at first. I checked out a few online video tutorials, but found them of little help. It really was Bryan’s [book](http://happygitwithr.com) that finally got me going. And I’m glad I did. I’ve been slowly working with GitHub for about a year—here’s [my profile](https://github.com/ASKurz)—and my first major project was putting together the files for the individual chapters in the [Statistical_Rethinking_with_brms_ggplot2_and_the_tidyverse](https://github.com/ASKurz/Statistical_Rethinking_with_brms_ggplot2_and_the_tidyverse) project. They originally lived as R Notebook files, eventually rendered in a [GitHub-friendly .md file format](https://rmarkdown.rstudio.com/github_document_format.html). After a while, I started playing around with `README`-only projects, which are basically a poor man’s GitHub version of blog posts (e.g., check out [this one](https://github.com/ASKurz/James-Stein-and-Bayesian-partial-pooling)). For me, and probably for your future bookdown projects, the most important GitHub skills to learn are commits, pushes, and forkes.

I’d fooled around with GitHub a tiny bit before launching my [*Statistical Rethinking* with brms, ggplot2, and the tidyverse](https://bookdown.org/content/3890/) project on the OSF. But it was confusing and after an hour or two of trying to make sense of it, I gave up and just figured the OSF would be good enough. After folks started noticing the project, I got a few comments that it’d be more accessible on GitHub. That was what finally influenced me to buckle down learn it in earnest. I’m still a little clunky with it, but I’m functional enough to do things like [make this blog](https://bookdown.org/yihui/blogdown/). With a little patience and practice, you can get there, too.

## Let Yihui Xie guide you

So far we’ve covered

-   R and R Studio
-   Scripts and R Markdown files
-   Git and GitHub

You don’t have become an expert, but you’ll need to become roughly fluent in all three to make good use of bookdown. Basically, if you are able to load data into R, document a rudimentary analysis in an R Notebook file, and then share the project in a non-embarrassing way in GitHub, you’re ready to use bookdown.

I’ve already mentioned it, but the authoritative work on bookdown is Yihui Xie’s [*bookdown: Authoring Books and Technical Documents with R Markdown*](https://bookdown.org/yihui/bookdown/). Yihui Xie, of course, is the author of the package. It’s probably best to just start there, going bit by bit. He also gave an RStudio webinar, [*Authoring Books with R Markdown*](https://www.youtube.com/watch?v=dVqVscgwSpw&t=12s), which I found to be a helpful supplement.

The complete version of my [*Statistical Rethinking* with brms, ggplot2, and the tidyverse](https://bookdown.org/content/3890/) project has 15 chapters and several preamble sections. Almost all the chapters files include a lot of computationally-intensive code, with the simulations for chapter 6 taking multiple hours to compute. I do not recommend starting off with a project like that, at least not all at once. If you follow along with Yihui Xie’s guide, you’ll practice stitching together simple files, first. After learning those basics, I then picked up other helpful tricks, like [caching analyses](https://bookdown.org/yihui/bookdown/preview-a-chapter.html#).

Although I didn’t use these resources while I was learning bookdown, you might also benefit from checking out

-   Sean Kross’s [*How to Start a Bookdown Book*](http://seankross.com/2016/11/17/How-to-Start-a-Bookdown-Book.html),
-   Karl Broman’s [*omg, bookdown!*](https://kbroman.org/blog/2017/05/31/omg-bookdown/),
-   Rachael Lappan’s [*Using Bookdown for tidy documentation*](https://rachaellappan.github.io/bookdown/), or
-   Pablo Casas’s [*How to self-publish a book: A handy list of resources*](https://blog.datascienceheroes.com/how-to-self-publish-a-book/).
