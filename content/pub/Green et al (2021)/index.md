---
title: 'Predicting healthcare-seeking behavior based on stated readiness to act: Development and validation of a prediction model'
author: Eric P. Green, Shyam Pradheep, Jessica Heinzelman, Anne Nyanchoka, Daphine Achieng, Siddhartha Goyal, Laura Cusson, A. Solomon Kurz, & Benjamin Bellows
date: '2021-07-22'
featured: false
# `tags` are like keywords
tags:
  - chatbot
  - family planning
  - prediction model
  - stage of change
  - digital health

links:
  - icon: file-pdf
    icon_pack: fa
    name: PDF
    url: pdf/Green et al (2021) Predicting healthcare-seeking behavior based on stated readiness to act_ Development and validation of a prediction model.pdf
  - icon: github
    icon_pack: fab
    name: GitHub
    url: https://github.com/ericpgreen/readiness-2020/
  - icon: doi
    icon_pack: ai
    name: Journal
    url: https://doi.org/10.1093/tbm/ibab096
---

## Abstract

A starting point of many digital health interventions informed by the Stages of Change Model of behavior change is assessing a person's readiness to change. In this paper, we use the concept of readiness to develop and validate a prediction model of health-seeking behavior in the context of family planning. We conducted a secondary analysis of routinely collected, anonymized health data submitted by 4,088 female users of a free health chatbot in Kenya. We developed a prediction model of (future) self-reported action by randomly splitting the data into training and test data sets (80/20, stratified by the outcome). We further split the training data into 10 folds for cross-validating the hyperparameter tuning step in model selection. We fit nine different classification models and selected the model that maximized the area under the receiver operator curve. We then fit the selected model to the full training dataset and evaluated the performance of this model on the holdout test data. The model predicted who will visit a family planning provider in the future with high precision (0.93) and moderate recall (0.75). Using the Stages of Change framework, we concluded that 29% of women were in the *Preparation* stage, 21% were in the *Contemplation* stage, and 50% were in the *Pre-Contemplation* stage. We demonstrated that it is possible to accurately predict future healthcare-seeking behavior based on information learned during the initial encounter. Models like this may help intervention developers to tailor strategies and content in real-time.

```{}
@article{greenPredicting2021,
  title = {Predicting healthcare-seeking behavior based on stated readiness to act: Development and validation of a prediction model},
  author = {Eric P. Green and Shyam Pradheep and Jessica Heinzelman and Anne Nyanchoka and Daphine Achieng and Siddhartha Goyal and Laura Cusson and A. Solomon Kurz and Benjamin Bellows},
  journal = {Translational Behavioral Medicine},
  year = 2021,
  volume = 12,
  number = 1,
  doi = {https://doi.org/10.1093/tbm/ibab096}
}
```

