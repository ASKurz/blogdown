---
title: 'Switch to Hugo Apéro: These are my notes'
author: A. Solomon Kurz
date: '2022-12-19'
draft: false
excerpt: "The purpose of this post is to highlight some of the steps I took to switch my blogdown website to the Hugo Apéro theme. At a minimum, I'm hoping this post will help me better understand how to set up my website the next time it needs an overhaul. Perhaps it will be of some help to you, too."
layout: single
tags:
  - blogdown
  - hugo
  - Netlify
  - GitHub
  - R
  - tutorial
featured: no
bibliography: /Users/solomonkurz/Dropbox/blogdown/content/blog/my_blog.bib
biblio-style: apalike
csl: /Users/solomonkurz/Dropbox/blogdown/content/blog/apa.csl  
link-citations: yes
---

## Purpose

Once again, it was time to update my website. This time I am switching to the Hugo Apéro (a-pay-ROH) theme, by the great [Alison Hill](https://alison.rbind.io). The purpose of this post is to highlight some of the steps I took to rebuild my website. At a minimum, I’m hoping this post will help me better understand how to set up my website the next time it needs an overhaul. Perhaps it will be of some help to you, too.

## Assumptions

In this post, I’m assuming you are familiar

-   with **R** ([R Core Team, 2022](#ref-R-base)) and the **blogdown** ([Xie et al., 2021](#ref-R-blogdown)) package,
-   with GitHub,
-   and with Netlify.

I will provide some resources for these, but this really isn’t an introductory blog post.

## Resources

### Apéro.

-   Check out the Apéro repoisity on GitHub at https://github.com/hugo-apero/hugo-apero.
-   You can find a website with all the default Apéro settings at https://hugo-apero.netlify.app/.
-   For extensive documentation and user-friendly directions, check out the website at https://hugo-apero-docs.netlify.app/. I found this particularly helpful.
-   For tips on how to switch from the Hugo Academic theme to Apéro, check out the nice blog post by Silvia Canelón at https://silviacanelon.com/blog/2021-hello-hugo-apero/.
-   Alison Hill has posted several recordings on how to use Apéro. I found the following helpful:
    -   https://youtu.be/RksaNh5Ywbo
    -   https://youtu.be/AADnslLpzJ4
    -   https://youtu.be/yXFu_upDL2o
    -   https://youtu.be/oBtDgW9u7Nw

### blogdown.

For an exhaustive introduction to **blogdown**, you have the ebook by Xie, Hill, and Thomas ([2017](#ref-xieBlogdown2017)), [*blogdown: Creating websites with R markdown*](https://bookdown.org/yihui/blogdown/).

### GitHub.

If you’re new to GitHub, let the great Jenny Bryan lead you, [here](http://happygitwithr.com/) ([Bryan et al., 2020](#ref-bryanHappyGitGitHub2020)).

### Netlify.

I’m not sure where to go for the best introduction to Netlify, but one place to start might be [here](https://www.apreshill.com/blog/2020-12-new-year-new-blogdown/#step-5-publish-site). At some point, you’ll need to go to https://www.netlify.com/.

## Foundations

My first few steps are the same as they were in my [original post](https://solomonkurz.netlify.app/blog/2021-05-03-blogdown-updates-prompted-a-website-overhaul-these-are-my-notes/) from a year and a half ago.

### Step 1. GitHub.

Log on to your GitHub account and start a fresh repo. See [here](https://solomonkurz.netlify.app/blog/2021-05-03-blogdown-updates-prompted-a-website-overhaul-these-are-my-notes/#step-1-github).

### Step 2. RStudio projects.

Make a fresh RStudio project to go along with your fresh GitHub repo. For more, see [here](https://solomonkurz.netlify.app/blog/2021-05-03-blogdown-updates-prompted-a-website-overhaul-these-are-my-notes/#step-2-rstudio-projects).

## Hugo Apéro mini launch

### Step 3. Make a default `hugo-apero` site.

If you haven’t already, install the current version of **blogdown** by executing `install.packages("blogdown")`. Restart **R**, if necessary. Now within a fresh session within your RStudio project, execute the following:

``` r
library(blogdown)

new_site(theme = "hugo-apero/hugo-apero", 
         format = "toml",
         sample = FALSE,
         empty_dirs = TRUE)
```

Over the next minute or two, you’ll see a handful of new files pop up in your project root folder. In your console, you’ll probably notice the prompt: “Want to serve and preview the site now? (y/n)”. I recommend executing `y`, which will return a preview of your default `hugo-apero` website in the RStudio Viewer panel.

### Step 4. `.gitignore`.

I don’t know that you have to do this right now, but a good early step is to make a `.gitignore` file. Following Hill ([here](https://alison.rbind.io/post/new-year-new-blogdown/#using-github)), you can do this by executing `file.edit(".gitignore")`. Then go ahead and enter this content to the file:

``` r
.Rproj.user
.Rhistory
.RData
.Ruserdata
.DS_Store
Thumbs.db 
/public/
/resources/
```

Once you save the changes, you might execute `blogdown::check_gitignore()` to confirm you’re okay.

If you haven’t done so, yet, this would be a good time to commit your changes and push them to GitHub. Again, if this is new to you, let Bryan et al. ([2020](#ref-bryanHappyGitGitHub2020)) lead you.

## Netlify

### Step 5. Sign up and deploy.

Hill has recommended you both build and host your **blogdown** website on Neflify (see [here](https://alison.rbind.io/post/new-year-new-blogdown/#step-1-create-repo)). I’m not going to argue. Go to <https://www.netlify.com/> and either log in or sign up using your GitHub account. Even if you already have a Netlify account, I recommend making a new Netlify site with `New site from Git > Continuous Deployment: GitHub`. You’ll then need to select your fresh GitHub repo, from above, to connect it to Netlify. This may require you to follow the prompts to actually navigate to GitHub, enable the connection, there, and then follow back to Netlify. Once you’re back in Netlify, leave settings at their defaults and select `Deploy Site`. You should be directed to a page with a header called *Production deploys* somewhere in the middle of the screen. After a minute of two, Netlify will finish deploying your site.

### Step 6. Customize your Netlify subdomain.

When you create your new site, Netlify will have automatically generated a subdomain name following the form `random-word-12345`. You should be able to see this at the top of your screen. This subdomain name will be part of your web address. You’re at liberty to keep the default name, if you want. But you can customize your subdomain name by navigating to `Site settings > General > Site details`. Then click the gray button named `Change site name`. In the field, I renamed my subdomain to `solomonkurz`. As a result, my website is deployed at <https://solomonkurz.netlify.app/>. Once you save this change, your website should be available at your customized address almost instantly.

## Start customizing

### Step 7. `config.toml`.

You should see a `config.toml` file in the root directory of your RStudio project folder. Hill gave a nice overview of this file [here](https://hugo-apero-docs.netlify.app/start/site-config/). Here we’ll walk through the bulk of my changes.

#### Step 7a. Info at the top.

Here’s what I changed in the top section:

``` r
baseURL = "https://luxury-alfajores-ec0741.netlify.app/"
# baseURL = 'https://solomonkurz.netlify.app/'
title = "A. Solomon Kurz" # Website name, which lives as meta data
author = "A. Solomon Kurz"
copyright = "© A. Solomon Kurz (2022)" # set to override the auto generated copyright using org info and now year
```

#### Step 7b. \[params\].

The first parameters in this section now read:

``` r
  orgName = "A. Solomon Kurz"
  orgLocal = ""  # to make it blank
  description = "Clinical psychology researcher"
```

The `favicon` parameter controls the little icon at the tab at the top of your browser. The default is saved as `favicon.ico`, which is found in the `/themes/hugo-apero/static/img` subfolder. I converted a personal head shot into a `.ico` file, and saved that file in that subfolder as `avatar.ico`. I then updated the line in the `config.toml` file like so:

``` r
  favicon = "/img/avatar.ico"  # default was "/img/favicon.ico"
```

So now when you go to my website, you’ll see my face in your browser tab.

The `logo` parameter controls the little logo you see at the upper left corner of the page, which leads back to the home page when clicked. The default is terrible and I don’t really have a personal logo, so I just put my little head shot pic in there, too.

``` r
  logo = "/img/avatar.ico" # defualt was "/img/blogophonic-mark-dark.png"
```

The `mainSections` parameter controls which sections are highlighted in your About page. I did change mine, but it won’t make sense to talk about those changes at this point. So if you’re following along chronologically, I recommend leaving that parameter alone for now. I changed mine to highlight my blogs and workshops.

The `sharing_image` parameter controls the image included when someone shares your Home page on social media. I have a higher-resolution head shot saved as a `.jpg` file, which I’ve also named `avatar.jpg` and saved into the `img` subfolder. Now there was already a default `avatar.jpg` image in that folder that came along for the ride when we executed `new_site(theme = "hugo-apero/hugo-apero", ...)` above. So when I saved my head shot as `avatar.jpg`, it overwrote the default file. For me, that was a good thing, and I suspect this is what you’ll want to do, too. We’ll bring this up later.

``` r
  sharing_image = "/img/avatar.jpg" # default was "/img/papillons.jpg"
```

If you’re on Twitter, update your handle.

``` r
  twitter = "SolomonKurz"  # default was "apreshill"
```

The `theme` parameter controls the overall color palette in the site. Hill documented it [here](https://hugo-apero-docs.netlify.app/start/site-config/#color-themes). `hugo-apero` comes with several default options and I opted for `grayscale`, which seems to play the best with my blog posts.

``` r
  theme = "grayscale"  # default was "sky"
```

I’m not fully in love with the palette and I might opt for a custom palette sometime in the future.

The `customtextFontFamily` and `customheadingFontFamily` parameters control the fonts on your site. There are several built-in options to choose from and you can even import your own. I left `customtextFontFamily` as the default and made a change to `customheadingFontFamily`.

``` r
  customheadingFontFamily = "Bitter"  # default was "Fraunces"
```

The parameters in the `[params.utterances]` section controls how people can comment on your content, such as on your blog posts. At a minimum, you’ll want to update the `repo_name` parameter to match your GitHub.

``` r
    repo_name = "ASKurz/blogdown"  # default was "apreshill/apero"
```

The various `[[params.social]]` sections below contain the infomraiton for your social media icons/hyperlinks. Here are my changes:

``` r
  [[params.social]]
      icon      = "twitter" # icon name without the 'fa-'
      icon_pack = "fab"
      url       = "https://twitter.com/SolomonKurz"
  [[params.social]]
      icon      = "google-scholar" # icon name without the 'fa-'
      icon_pack = "ai"
      url       = "https://scholar.google.com/citations?user=P8JWywQAAAAJ&hl"
  [[params.social]]
      icon      = "osf" # icon name without the 'fa-'
      icon_pack = "ai"
      url       = "https://osf.io/2t7j2/"
  [[params.social]]
      icon      = "github" # icon name without the 'fa-'
      icon_pack = "fab"
      url       = "https://github.com/ASKurz"
```

#### Step 7c. \[menu\].

The `[menu]` section is divided into the `[[menu.header]]` blocks, which control the menu bar at the top of your website, and the `[[menu.footer]]` blocks, which control the links at the bottom of your page. Hill documented these sections [here](https://hugo-apero-docs.netlify.app/start/site-config/#menus).

I’ll have more to say about the `[[menu.header]]` blocks further down. I wasn’t interested in any of the footer sections, so I commented them all out.

``` r
  # Configure footer  
#  [[menu.footer]]
#    name = "License"
#    title = "License"
#    url = "/license/"
#    weight = 1
#  [[menu.footer]]
#    name = "Contact"
#    title = "Contact form"
#    url = "/contact/"
#    weight = 2
#  [[menu.footer]]
#    name = "Contributors"
#    title = "Contributors"
#    url = "/contributors/"
#    weight = 3
```

#### Step 7d. Save and `check_config()`.

After you’re satisfied playing around with these settings, con ahead and save your `config.toml` file. Then to make sure nothing has gone catastrophically wrong, execute this:

``` r
blogdown::check_config()
```

If you pass all your checks, this might be a good time to commit your files and push to GitHub.

### Step 8. `netlify.toml`.

Next, check for a `netlify.toml` file in your root directory. You can also open or create it by executing `blogdown::config_netlify()`. Once the file’s open, make sure Hugo is installed and matched up by executing `blogdown::check_hugo()`. Mine was all good. However, I did get the following on my to-do list: `● [TODO] Set options(blogdown.hugo.version = "0.107.0") in .Rprofile and restart R.`. We’ll cover that next.

### Step 9. `.Rprofile`.

Hill covered how to change an `.Rprofile` file [here](https://www.apreshill.com/blog/2020-12-new-year-new-blogdown/#step-4-create-content). If all went well, you should have that file in your root directory. If not, you can make an `.Rprofile` file by executing `blogdown::config_Rprofile()`. Then customize some of the settings within `options()`, as desired. Taking cues from Hill, mine now read:

``` r
options(
  # to automatically serve the site on RStudio startup, set this option to TRUE
  blogdown.serve_site.startup = FALSE,
  # to disable knitting Rmd files on save, set this option to FALSE
  blogdown.knit.on_save = TRUE,
  # build .Rmd to .html (via Pandoc); to build to Markdown, set this option to 'markdown'
  blogdown.method = 'markdown',
  # fix Hugo version
  blogdown.hugo.version = "0.107.0",
  # These changes are based on Alison Hill's posts:
  # https://alison.rbind.io/post/2019-02-21-hugo-page-bundles/#project-specific-rprofile
  # and 
  # https://alison.rbind.io/post/new-year-new-blogdown/#step-4-create-content
  blogdown.author = "A. Solomon Kurz",
  blogdown.ext = ".Rmd",
  blogdown.subdir = "post"
)
```

In her post, Hill stressed it’s important to restart your RStudio session after you save any changes to `.Rprofile`. If you haven’t done so, in a while, this is probably a good time to commit and push your files to GitHub.

## Revamp content

With Hill’s Hugo Apéro framework, the bulk of your public-facing content will live in the `content/` folder. This is a big section, so we’ll break it down into smaller bites.

### Step 10. Home and the top-level `_index.md` file.

The `_index.md` file at the top of the `content/` folder controls the content on your homepage. At a minimum, you’ll want to customize parameters like `title`, `subtitle`, and the `description`. Note the `action_label` and `action_link` settings control the little “read more” arrow link that directs viewers to your About page. I left those at their defaults. The only other noteworthy change was I changed the `images` setting like so:

``` r
images:
- img/Solomon_BW_2012.jpeg
```

To make this work, I had previously saved a picture of myself called `Solomon_BW_2012.jpeg` in the same `/themes/hugo-apero/static/img` subfolder from above. I found that to match the layout settings in Apéro, it was important to find a large, wide rectangular picture this. Otherwise the empty space looked off.

I also set `text_align_left: true` to switch the alignment in the text.

### Step 11. About.

The `content/about/` subfolder contains the building blocks for your About section. You’ll want to do a lot of personalizing, here.

The top-level `_index.md` file mainly contains meta data for when someone were to share a link to your About page. Here I only personalized the `description` and `title` parameters and left everything else alone.

The `content/about/header` subfolder controls the header section. In the `index.md` file, I simply customized the `headline` parameter and personalized the prose below the `<!-- this is a subheadline -->` line.

The `content/about/main` subfolder controls the section below the header. In the `index.md` file, I simply personalized the `title` and `intro` parameters. In addition, I set `number_featured: 2` to pull 2 links from my main sections (see `mainSections` in the `config.toml` file), and I set `number_categories: 0` to disable the featured categories option. As I wasn’t interested in the outro feature, I set `show_outro: false` and deleted the content in the `outro` parameter.

The `content/about/sidebar` subfolder controls settings for the sidebar. In the `index.md` file, I personalized the `author` and `role` parameters. To remove their content, I set both `audio_link_label` and `link_list_label` to blank. I also commented out the `link_list` content. Setting `avatar_shape: circle` will show your avatar pic as a circle, which worked best with my pic. Speaking of which, the `avatar.jpg` that comes in this subfolder is the pic that will appear at the top of the sidebar. I replaced the default file with the same head show I’d already saved as `avatar.jpg` in the `/themes/hugo-apero/static/img` subfolder.

### Step 12. Blog.

The `content/blog/` subfolder contains the building blocks for your blog posts. I gave the top-level `_index.md` file all the typical personalized touches, such a inserting my name in the `author` parameter and so on. After playing around with the options a bit, I decided to set `layout: list-grid` to display my blog posts in a nice grid layout, which I’ll say more about in a bit.

I had a lot of blog posts from the previous version of my website. Their file structure was already based on Hugo Page Bundles, and Hill detailed [here](https://www.apreshill.com/blog/2019-02-spoonful-bundles/). I did make several changes, though:

First, I followed Hill’s recommendation (I can’t remember from where) to base my blog posts on the actual markdown files, rather than HTML files. In part, this meant that within the `.Rprofile` file, I changes from `blogdown.method = 'html'` to `blogdown.method = 'markdown'`, which I already displayed above. This also meant deleting all `.html` files from my blog subfolders.

Second, my blogs tend to be heavy on **R** code and the syntax from **R**’s various packages tends to change from time to time. So I took the opportunity to rerun the code in all my blog posts to catch and fix any code breaks. Most of the time, the changes were subtle and not worth drawing attention to. In other cases, I specifically mentioned the blog post had been edited at the top of the file.

Third, I added featured images. The Hugo Apéro framework makes it easy to give each blog post its own featured image, which is something my blogs didn’t have before. Within each blog’s subfolder, just save the desired image in a `.jpg` with the word `featured` somewhere in the name. Since my blogs tend to have lots of **ggplot2**-based figures, I usually added a final code block at the end of the blog where I reproduced one of those figures and saved the image with `ggsave()`. In a typical file, that code block looked something like this:

Since this code wasn’t something I wanted to highlight in the blog post itself, I would always include `eval = F, echo = F` in the code chunk options.

Also, back in the `config.toml` file, I set `paginate = 9` so you could see 9 blogs at a time in my grid-like layout. The default was `paginate = 5`.

### Step 13. Big delete.

Several of the default folders and `.md` files didn’t serve my purposes. After some experimentation, I concluded it was save to delete them. So I deleted the following:

-   the `content/collection/` subfolder
-   the `contributors.md` file
-   the `content/elements/` subfolder
-   the `content/form/` subfolder
-   the `license.md` file

### Step 14. Project to Pubs.

None of the things I currently do fit well under the heading of “Project,” as such. But I do like the overall format of this section and I copied and pasted the subfolder and renamed the copy as `content/pub/`. This will serve as a place to list my academic publications.

In the `_index.md` file of my awesome new `content/pub/` subfolder, I personalized the typical parameters, like `author`, `title`, and so on. I changed the `layout` parameter from the default `list-grid` to `list-sidebar` to include a nice sidebar, and I then personalized the various `sidebar` settings, such as `author` and `description`. To make the picture at the top of the sidebar my own, I replaced the default `sidebar-listing.jpg` file in the subfolder with a cropped head shot.

As Hill explained [here](https://hugo-apero-docs.netlify.app/start/section-config/#renaming-sections), it’s not trivial to make new folders within `content` which contain the layout settings as the default subfolders. To make sure my new `content/pub/` subfolder used the same layout settings as `content/project/`, I had to add the following to its `_index.md` file:

``` r
type: project
cascade:
  type: project
# blah
# blah ...
sidebar:
  type: project
```

As a first step to making new subfolders for my academic publications, I copied and pasted one of the default subfolders from `content/project/` and renamed it `Anderson et al (2022)` after one of my recent publications. I started switching out the usual parameters, such as `title` and `date`, to match the information from the article. The `tags` parameter is where I listed the article’s key words. Using syntax similar to the `index.md` file in the default `bakeoff` subfolder within the `content/project/` subfolder, I used the following to make the buttons with the nice icons and hyperlinks for my article’s PDF, DOI site, and supporting materials on the OSF:

``` r
links:
  - icon: osf
    icon_pack: ai
    name: OSF
    url: https://osf.io/fhuqz/
  - icon: doi
    icon_pack: ai
    name: Publication
    url: https://doi.org/10.1177/13591053211072685  
  - icon: file-pdf
    icon_pack: fa
    name: PDF
    url: pdf/Anderson et al (2022) Exploring the longitudinal clustering of lifestyle behaviors, social determinants of health, and depression.pdf
```

Note for that first link to work for the PDF, I had to save the article’s PDF in a new `pdf` subfolder within the `static` subfolder. This is ultimately where I saved PDF copies of all my articles.

The old `hugo-academic` theme from the last version of my website had a nice support for a link to BibTeX-type citations. Sadly, it wasn’t clear how to replicate that functionality within the `hugo-apero` framework. My quick-and-dirty solution was to just include the BibTeX-type citation in a code block in the body of the `index.md` file, like so:

    @article{andersonExploring2022,
      title = {Exploring the longitudinal clustering of lifestyle behaviors, social determinants of health, and depression},
      author = {Austen R. Anderson and Adam P. McGuire and A. Solomon Kurz and Yvette Z. Szabo and Sheila B. Frankfurt},
      journal = {Journal of Health Psychology},
      year = 2022,
      volume = 27,
      issue = 13,
      page = 2922-2935,
      doi = {https://doi.org/10.1177/13591053211072685}
    }

As far as the thumbnail pics go, I’m not going to make the effort to create a distinct image for each of my publications. The easy fix was to take a screenshot of the first page of each article’s PDF and save the image as `featured-pdf.png` within the relevant subfolder. It worked like a charm.

After my new `content/pub/` subfolder was working well, removed the default `content/project/` subfolder.

### Step 15. Talk

Other than the occasional ‘this is the story of how I went to grad school’ type talk, I don’t give enough public lectures to justify a Talk section. However, I am starting to give workshops and the Talk section format seemed like a great fit for those. However, I want to call the section *Workshops* in the menu bar and I also want to use a `content/workshop/` subfolder, rather than a `content/talk/` subfolder, so my first step was to copy the entire `content/talk/` subfolder and rename the copy as `content/workshop/`.

In the `_index.md` file of my new `content/workshop/` subfolder, I personalized the typical parameters, like `author` and `title`. As I wasn’t interested in a sidebar for this section, I removed the default `sidebar-listing.jpg` file. To make sure my new `content/workshop/` subfolder used the same layout settings as `content/talk/`, I added the following to the `_index.md` file:

``` r
type: talk
cascade:
  type: talk
```

To make new subfolders for my academic publications, I copied and pasted one of the default subfolders from `content/talk/` and renamed it `intro_bayes_march_2023` for my upcoming workshop. I started switching out the usual parameters, such as `title`, `subtitle` and `author`. Note that this format allows for multi-day workshops by way of the `date` and `date_end` parameters. Since I wanted to include a custom pic for the `featured.jpg` file, I made a `content/workshop/intro_bayes_march_2023/make featured` subfolder, which contains a `.Rmd` file containing the code I used to make the `featured.jpg` file.

:warning: If any of your talks/workshops are scheduled for a date in the future, make sure to look at the \[Add `buildFuture`.\]\[Add `buildFuture`.\] section, below, before you commit and push to GitHub. Otherwise you’ll likely return an error.

After my new `content/workshop/` subfolder was working well, removed the default `content/talk/` subfolder.

### Step 16. Books, Courses, and Support.

A few of the sections of my website are just single pages with text, which doesn’t quite fit into the layout framework Hugo Apéro allows for blogs, projects, and talks. So for these, I just made three new subfolders within `/content/`, which were:

-   `/book/`,
-   `/course/`, and
-   `/support/`.

The content of each of these subfolders is just an `indes.Rmd` file, in which I saved my desired prose using simple Markdown. In the documentation site, these sections are called *regular pages*, and you can learn more about them [here](https://hugo-apero-docs.netlify.app/start/page-config/#regular-page). The only other thing to mention is I found the layout looked best with these settings in the YAML at the top of the files:

``` r
layout: standard # standard or wide-body
show_title_as_headline: false
```

### Step 17. Back to `config.toml`.

Now we’ve revamped the `/content` section, we’re ready to make additional changes to the `config.toml` file.

#### Step 17a. Add `buildFuture`.

The week-long workshop I wanted to include in my website was scheduled for a future date, and I listed the start and end dates in the `date` and `date_end` parameters in the workshop’s `index.md` file. As it turns out, you have to explicitly allow for future dates in the `config.toml`, otherwise you might get an error when building your site on Netlify. The fix was to add the following to my `config.toml` file:

``` r
buildFuture = true
```

I got this information from this GitHub issue: https://github.com/hugo-apero/hugo-apero/issues/82.

#### Step 17b. `mainSections`.

If you recall, the `mainSections` parameter controls which sections are highlighted in your About page. To highlight some of my new sections, my `mainSections` now includes the following:

``` r
  mainSections = ["blog", "workshop", "pubs"]  # default was ["blog", "project", "talk"]
```

#### Step 17c. Add a CV section.

In addition to all the cool primary sections in the `/contenet/` folder, I also wanted a prominent way to show my CV in the menu bar. I forget where I picked up this trick, but if you go back to the `[menu]` section of the `config.toml` file, you can just manually add a section like this:

``` r
  [[menu.header]]
    name = "CV"
    title = "CV"
    url = "/cv/Kurz_CV.pdf"
    weight = 8
```

To make this work, I made a new `/static/cv/` subfolder, into which I saved my current CV as `Kurz_CV.pdf`.

#### Step 17d. Speaking of `[menu]`.

To arrange all my content areas in my menu bar, the `[[menu.header]]` blocks in the `[menu]` subsection of my `config.toml` file now look like this:

``` r
[menu]
  # Configure header
  [[menu.header]]
    name = "About"
    title = "About"
    url = "/about/"
    weight = 1
  [[menu.header]]
    name = "Blog"
    title = "Blog"
    url = "/blog/"
    weight = 2
  [[menu.header]]
    name = "Pubs"
    title = "Pubs"
    url = "/pubs/"
    weight = 3
  [[menu.header]]
    name = "Books"
    title = "Books"
    url = "/book/"
    weight = 4
  [[menu.header]]
    name = "Workshops"
    title = "Workshops"
    url = "/workshop/"
    weight = 5
  [[menu.header]]
    name = "Courses"
    title = "Courses"
    url = "/course/"
    weight = 6
  [[menu.header]]
    name = "Support"
    title = "Support"
    url = "/support/"
    weight = 7
  [[menu.header]]
    name = "CV"
    title = "CV"
    url = "/cv/Kurz_CV.pdf"
    weight = 8
```

## Step 18. Allow for comments

I don’t think it’s particularly well documented on the site at the moment[^1], but Hugo Apéro does allow for comments sections. I haven’t allowed for comments on my website before, so naturally I asked the good people of Twitter about their thoughts.

{{% tweet user="SolomonKurz" id="1602690155986427904" %}}

Overall, the response was encouraging. At the time of this writing, I believe the two officially supported ways to allow for comments were through either [Disqus](https://disqus.com/) or [utterances](https://utteranc.es/). However, several of the commenters recommended the newer [giscus](https://giscus.app/) app, which isn’t fully supported by Hugo Apéro, but is clearly in the works (see [this pull request](https://github.com/hugo-apero/hugo-apero/pull/63)). It turns out giscus has some very nice qualities, such as full Markdown and LaTeX support, so I decided to try that option. If you scroll though my post (start [here](https://twitter.com/SolomonKurz/status/1603144297641017349)), you’ll see I had some difficulty setting the whole thing up. Here are the basic steps:

1.  Make sure your GitHub repository is [public](https://docs.github.com/en/repositories/managing-your-repositorys-settings-and-features/managing-repository-settings/setting-repository-visibility#making-a-repository-public).
2.  Enabling the [Discussions feature](https://docs.github.com/en/repositories/managing-your-repositorys-settings-and-features/enabling-features-for-your-repository/enabling-or-disabling-github-discussions-for-a-repository) on your repository.
3.  Add a “Comments” category on your Discussions page. You can do so by:

-   navigating to the Discussions page on your GitHub repo,
-   clicking the pencil icon beside the “Categories” header,
-   clicking on the green “New category” button,
-   typing in “Comments” in the “Title” field in the pop-up box, and
-   clicking the green “Create” button.

4.  Once you’re all set up with your Comments category, *then* install the [giscus app](https://github.com/apps/giscus).
5.  Once giscus is installed, scroll down to the *Repository* section at https://giscus.app/ and enter in the name of your GitHub repo in the text field. If all goes well in this step, you’ll see the words “Success! This repository meets all of the above criteria” pop up in green font just below the text field.
6.  Make your selection in the *Page ↔️ Discussions Mapping* section at https://giscus.app/. I just kept with the default “Discussion title contains page `pathname`” option.
7.  Scroll down to the next section, called *Discussion Category*. Using the drop down, select “Comments.” If you don’t see a “Comments” option there, go back step 3, above.
8.  Scroll down to the *Features* section and make your selection. I went with *Enable reactions for the main post*.
9.  Using the drop down, select your preferred theme. I went with *Preferred color theme*.
10. Now, take note of the information in the *Enable giscus* section. You’re not going to copy and paste that script, exactly. But you’ll need that information in a bit.
11. Within your root directory for your website, you’ll need to make a new `layouts/partials/shared/` subfolder. In sobfolder, save a file called `comments.html`. Into that file, copy and paste the code from [here](https://github.com/hugo-apero/hugo-apero/blob/b173e7e10c6d76331051eaeb5e1420f097f0bd39/layouts/partials/shared/comments.html).
12. Now open your `config.toml` file.

-   Either comment out or delete the `[params.utterances]` subsection.
-   Copy and paste the following code in that area:

``` toml
  # Specify use of utterances, giscus or disqus
  comments = "giscus"

  # Configuration of https://giscus.app/ for comments
  [params.giscus]
    use_giscus = true
    repo = "ASKurz/blogdown5"
    repo_id = "R_kgDOInfFgA"
    category = "Comments"
    category_id = "DIC_kwDOInfFgM4CTFM3"
    mapping = "pathname"
    reactions_enabled = "1"
    emit_metadata = "0"
    theme = "preferred_color_scheme"
    lang = "en"
```

-   At the moment, these are all the settings for my website. When you do this, you’ll need to fill in the corresponding information from the *Enable giscus* section at https://giscus.app/, which we mentioned back in step 10. Pay particular attention to the `repo_id` and `category_id` parameters.

13. Finally, try saving your `comments.html` and `config.toml` files, and then commit and push them to GitHub. If all went well, you should have working comments sections.

I should note I got extensive help from the great [Michael McCarthy](https://michaelmccarthy.tidytales.ca/) on this section.

## Step 19. Font and hyperlink colors

Overall, I like the look and feel of the `theme = "grayscale"` set up, which we discussed \[above\]\[\[params\].\]. However, the grey color of the main text seemed a little too light, and I wanted to highlight when there were hyperlinks. Hill documented a couple ways you can customize color themes [here](https://hugo-apero-docs.netlify.app/learn/color-themes/#bring-your-own-hex-codes). I opted for the [second approach](https://hugo-apero-docs.netlify.app/learn/color-themes/#bring-your-own-hex-codes), where you use your own HEX codes. Here were the steps:

1.  I opened the `hex-colors.scss` file, which had already been created by default in the `assets` folder within the root directory.
2.  I also opened the `grayscale.scss` file, which was in `assets/themes`.
3.  I copied the content in the `grayscale.scss` file and pasted it in the `hex-colors.scss` file, which meant the `grayscale` color theme would be the foundation of my custom theme.
4.  Based on `grayscale`, the setting for the `$textColorCustom` parameter was `#666260`, which controlled the font color for the text you would consider the “body” of most sections. Based on [this web page](https://www.colorhexa.com/666260), I found `#524f4d` was a darker version of that default, and I ended up setting `$textColorCustom: #524f4d`.
5.  A quick web serach led me to [this stackoverflow](https://stackoverflow.com/questions/4774022/whats-default-html-css-link-color) discussion, where I learned `#0000EE` was a typical HEX color for blue hyperlinks. That color was too intense for my liking. However, on [this page](https://www.color-hex.com/color/0000ee), I learned `#6666f4` was a muted alternative on the same color scale. Thus I saved that value in my `$bodyLinkColorCustom` and `$sidebarLinkColorCustom` parameters. Thus, the contents of my `hex-colors.scss` file now reads:

<!-- -->

    // set custom hex colors 
    $siteBgColorCustom: #ffffff;
    $sidebarBgColorCustom: #F8F9FA;
    $textColorCustom: #524f4d;
    $sidebarTextColorCustom: #495057;

    $headlineColorCustom: #343A40;
    $headingColorCustom: #212529;

    $bodyLinkColorCustom: #6666f4;
    $navLinkColorCustom: #495057;
    $sidebarLinkColorCustom: #6666f4;

    $footerTextColorCustom: #6F777F;

    $buttonTextColorCustom: #F8F9FA;
    $buttonHoverTextColorCustom: #ffffff;
    $buttonBgColorCustom: #404040;
    $buttonHoverBgColorCustom: #121417;
    $borderColorCustom: #DEE2E6;

6.  Back in the `config.toml` file, I set `theme = ""` and `custom_theme = "hex-colors"`.

I don’t love everything about these changes, such as how the titles of my individual blog posts are now hyperlink blue when displayed on the top-level blog page (https://solomonkurz.netlify.app/blog/), but overall the changes seem better to my eyes.

## References

<div id="refs" class="references csl-bib-body hanging-indent" line-spacing="2">

<div id="ref-bryanHappyGitGitHub2020" class="csl-entry">

Bryan, J., the STAT 545 TAs, & Hester, J. (2020). *Happy Git and GitHub for the <span class="nocase">useR</span>*. <https://happygitwithr.com>

</div>

<div id="ref-R-base" class="csl-entry">

R Core Team. (2022). *R: A language and environment for statistical computing*. R Foundation for Statistical Computing. <https://www.R-project.org/>

</div>

<div id="ref-R-blogdown" class="csl-entry">

Xie, Y., Dervieux, C., & Presmanes Hill, A. (2021). *<span class="nocase">blogdown</span>: Create blogs and websites with R Markdown* \[Manual\]. <https://CRAN.R-project.org/package=blogdown>

</div>

<div id="ref-xieBlogdown2017" class="csl-entry">

Xie, Y., Hill, A. P., & Thomas, A. (2017). *<span class="nocase">blogdown</span>: Creating websites with R markdown*. Chapman and Hall/CRC. <https://bookdown.org/yihui/blogdown/>

</div>

</div>

[^1]: I may have missed it, but I didn’t find any helpful information about comments when searching around on https://hugo-apero-docs.netlify.app/.
