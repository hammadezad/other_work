
---
title: Household Based (weights) Sampling 
author: Hammad Ezad
date: \today
header-includes:
  - \usepackage{placeins}
output: pdf_document

---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = FALSE)
knitr::knit_hooks$set(plot = function (x, options) {
  float_correct <- function(f, y, opts)  {
    if (is.null(opts$regfloat) || opts$regfloat==FALSE)
      paste0(f(y, opts), "\n\n\\FloatBarrier\n")
    else
      f(y, opts)
  }
  if (!is.null(options$out.width) || !is.null(options$out.height) ||
      !is.null(options$out.extra) || options$fig.align != "default" ||
      !is.null(options$fig.subcap)) {
    if (is.null(options$fig.scap))
      options$fig.scap = NA
    return(float_correct(knitr:::hook_plot_tex, x, options))
  }
  return(float_correct(knitr:::hook_plot_md_base, x, options))
})
```



```{r, warning = FALSE, include = FALSE}
library(knitr)
knitr::opts_chunk$set(fig.width=15, fig.height=12, echo = FALSE) 
library(tidyverse)
library(lme4)
library(fixest)
library(broom)
library(furrr)
rm(list=ls())
library(ggplot2)
library(dplyr)
```



# Karachi East District
```{r, echo = FALSE, warning=FALSE, message= FALSE}
urban_df = read_csv("C:/Users/basicuser/Desktop/Sample Calculation/Block Level Data/urban_blocks.csv")

```



```{r}
ke_df <- urban_df %>% 
    group_by(town_name) %>% 
    summarise(sum_hh=sum(num_hh),.groups = 'drop') %>%
    mutate(
        pr = sum_hh/sum(sum_hh), 
        n_blocks = round(130*pr)
        ) 
ke_df

urban_df = urban_df %>%
    mutate(
        block_id_1 = paste0(town_id, "_", charge_id, "_", circle_id, "_", block_id),
        circle_id_1 = paste0(town_id, "_", charge_id, "_", circle_id),
        charge_id_1 = paste0(town_id, "_", charge_id)
    ) %>%
    group_by(charge_id) %>%
    mutate(charge_id_i = cur_group_id()) %>%
    group_by(circle_id) %>%
    mutate(circle_id_i = cur_group_id()) %>%
    group_by(block_id) %>%
    mutate(block_id_i = cur_group_id()) %>%
    group_by(town_id) %>%
    mutate(town_id_i = cur_group_id()) %>%
    ungroup()

```






```