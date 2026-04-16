#### Libraries ---------------------------------------------------------
# General
library(data.table)
library(tidyverse, warn.conflicts = FALSE)
options(readr.show_col_types = FALSE)
library(runner)
library(corrplot)
library(GGally)
library(tidymodels)
library(purrr)
library(piecewiseSEM)
library(FactoMineR)
library(factoextra)
library(googlesheets4)
library(ggthemes)
library(ggpmisc)
library(ggpubr)
library(ggforce)
library(ggrepel)
library(Gmisc)
library(patchwork)
library(here)
library(styler)
library(preMetabolizer)
library(streamMetabolizer)
library(rstan)
options(mc.cores = parallel::detectCores())
library(dataRetrieval)
library(scales)
library(knitr)
library(lubridate)
library(zoo)
library(nasapower)
library(imputeTS)
library(rgbif)
library(dygraphs)
library(neonstore)
library(neonUtilities)
library(vegan)
library(RColorBrewer)
library(furrr)
library(sf)
# Project Specific

#### Options ------------------------------------------------------------
nCores <- parallel::detectCores()-1
options(mc.cores = nCores)
options(readr.show_col_types = FALSE)
# Colors from https://sashamaps.net/docs/resources/20-colors/
distColors <- sample(c('#e6194B', '#3cb44b', '#ffe119', '#4363d8', '#f58231', '#911eb4', '#42d4f4', '#f032e6', '#bfef45', '#fabed4', '#469990', '#dcbeff', '#9A6324', '#fffac8', '#800000', '#aaffc3', '#808000', '#ffd8b1', '#000075', '#a9a9a9', '#ffffff', '#000000'))

okabeIto <- c("#0072B2", "#E69F00", "#CC79A7", "#009E73", "#56B4E9", "#D55E00", "#F0E442", "#999999", "#000000")
#### Paths --------------------------------------------------------------
# Project Specific

#### Functions ----------------------------------------------------------
# function_files <- list.files(c("~/Dropbox/R/Functions"),
#                              full.names = T)
# lapply(function_files, source)

#### Clean Up -----------------------------------------------------------
#rm(function_files)

