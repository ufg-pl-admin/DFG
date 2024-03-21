library(plumber)
library(ggplot2)
library(dplyr)
library(forcats)

library(cowplot)
library(gt)
library(gridExtra) 
library(grid)
library(stringr)
library(gtable)
library(scales)
library(reshape2)
# library(sysfonts)


wojewodztwa <- readRDS(file = paste(getwd(), '/src/wojewodztwa.Rds', sep=""))
wojewodztwa_df <- readRDS(file = paste(getwd(), '/src/wojewodztwa_df.Rds', sep=""))
srodki <- readRDS(file = paste(getwd(), '/src/srodki.Rds', sep=""))

pr(paste(getwd(), '/src/server.R', sep="")) %>% pr_run(host="0.0.0.0", port=8080)

