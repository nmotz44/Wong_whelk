##### mixing models for channeled whelk #####
# first run of mixing models for smooth whelk
# loop through multiple TDFs from 0-3 by 0.5

## load data and packages
# packages
library(tidyverse)
library(simmr)
source('format_simmr_data.R')

# load data
prey = read.csv("Data/kmeans_output.csv") |>
  select(-X.1, -CommonName, -meand13C, -sdd13C, -meand15N, -sdd15N)
whelk_data = read.csv("Data/SIA_Database_2025_12_11.csv")

# rename CommonNameTissue to CommonName
prey = prey %>%
  rename("CommonName" = "CommonNameTissue") %>%
  mutate(kmeans = case_when(kmeans == 1 ~ "one",
                            kmeans == 2 ~ "two",
                            kmeans == 3 ~ "three",
                            kmeans == 4 ~ "four",
                            TRUE ~ "other"))

# subset for only smooth whelk
whelk_data = whelk_data %>%
  filter(ProjID == "Whelk" & CommonName == "Smooth Whelk")

# add in a k means column
whelk_data$kmeans = "Smooth Whelk"

# put Smooth whelk in the dataframe
iso_data = rbind(prey, whelk_data)

# make kmeans categorical
iso_data$kmeans = as.character(iso_data$kmeans)

##### format mixng models ######
# make nitrogen TDF
n_tdf = c(0, 0.5, 1, 1.5, 2, 2.5, 3)

# make blank model fits
model_fits = data_frame(tdf = n_tdf, DIC = NA)

for (i in 1:length(n_tdf)) {
  # print TDF for this iteration
  print("########################################################")
  print(paste0("TDF for this run: ", n_tdf[i]))
  
  # format data for simmr
  simmr_object = format_simmr_data(data = iso_data,
                                   consumer.names = "Smooth Whelk",
                                   prey.names = c("one", "two", "three", "four"),
                                   species.column = "kmeans",
                                   d13C.column = "d13C", 
                                   d15N.column = "d15N",
                                   disc.factors.c = 1.35,
                                   disc.factors.c.sd = 0.3,
                                   disc.factors.n = n_tdf[i],
                                   disc.factors.n.sd = 0.5,
                                   data.type = "raw"
  )
  
  # run simmr model
  simmr.out = simmr_mcmc(simmr_object, mcmc_control = list(iter = 10000, burn = 1000, thin = 10, n.chain = 4))
  
  # check diagnostics
  summary(simmr.out, type = "diagnostics")
  
  # make bi-plot
  plot(simmr_object, title = paste0(n_tdf[i], " Bi-plot"))
  
  # make boxplot
  plot(simmr.out, type = "boxplot", title = paste0(n_tdf[i], " Box Plot"))
  
  # density plot
  plot(simmr.out, type = "density", title = paste0(n_tdf[i], " Box Plot"))
  
  # correlation plot
  plot(simmr.out, type = "matrix", title = paste0(n_tdf[i], " Box Plot"))
  
  
  # add model fit into dataframe
  model_fits$DIC[model_fits$tdf == n_tdf[i]] = simmr.out$output$`Smooth Whelk`$BUGSoutput$DIC
  
  # add in correlation plot
  
  # print
  print("#####################################################################")
}


###### rerun models without group 4 #####
for (i in 1:length(n_tdf)) {
  # print TDF for this iteration
  print("########################################################")
  print(paste0("TDF for this run: ", n_tdf[i]))
  
  # format data for simmr
  simmr_object = format_simmr_data(data = iso_data,
                                   consumer.names = "Smooth Whelk",
                                   prey.names = c("one", "two", "three"),
                                   species.column = "kmeans",
                                   d13C.column = "d13C", 
                                   d15N.column = "d15N",
                                   disc.factors.c = 1.35,
                                   disc.factors.c.sd = 0.3,
                                   disc.factors.n = n_tdf[i],
                                   disc.factors.n.sd = 0.5,
                                   data.type = "raw"
  )
  
  # run simmr model
  simmr.out = simmr_mcmc(simmr_object, mcmc_control = list(iter = 10000, burn = 1000, thin = 10, n.chain = 4))
  
  # check diagnostics
  summary(simmr.out, type = "diagnostics")
  
  # make bi-plot
  plot(simmr_object, title = paste0(n_tdf[i], " Bi-plot"))
  
  # make boxplot
  plot(simmr.out, type = "boxplot", title = paste0(n_tdf[i], " Box Plot"))
  
  # density plot
  plot(simmr.out, type = "density", title = paste0(n_tdf[i], " Box Plot"))
  
  # correlation plot
  plot(simmr.out, type = "matrix", title = paste0(n_tdf[i], " Box Plot"))
  
  
  # add model fit into dataframe
  model_fits$DIC[model_fits$tdf == n_tdf[i]] = simmr.out$output$`Smooth Whelk`$BUGSoutput$DIC
  
  # add in correlation plot
  
  # print
  print("#####################################################################")
}

# models were slightly better with group 4 included
