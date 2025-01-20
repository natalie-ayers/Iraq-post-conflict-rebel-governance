library(ggplot2)
library(gridExtra)
library(grid)
library(broom)
library(sp)
library(dplyr)


## ---------------------- IMPORT DATA -------------------- ##

seasonal_data_productivity <- read.csv("data/combined/plant_season_liveuamap_adm3_data.csv")


## -------------- ANALYSIS DURING OCCUPATION ------------- ##


occlen_extremes_relig <- seasonal_data_productivity |>
    filter(ever_isis_controlled == 1) |>
    mutate(sunni_status=ifelse(sunni_dom==1,'Sunni Dominated',
                               ifelse(sunni_mix==1,'Sunni Mixed',
                                      ifelse(no_sunni==1,'No Sunni Pop',
                                             NA))),
           occ_len_cat = ifelse(full_occ_dur_mon<=12, "Short",
                                ifelse(full_occ_dur_mon>=35,"Long",
                                       "Mid"))) |>
    mutate(occlen_and_relig = paste0(sunni_status," - ",occ_len_cat),
           occlen_and_timing = paste0(occ_len_cat, " - " ,occ_status_lm))

occlen_extremes_relig_filt <- occlen_extremes_relig |>
      filter(occ_len_cat %in% c('Long','Short')) |>
      filter(occ_status_lm %in% c('Pre','Post'))


occ_vs_noocc_relig <- seasonal_data_productivity |>
  mutate(sunni_status=ifelse(sunni_dom==1,'Sunni Dominated',
                             ifelse(sunni_mix==1,'Sunni Mixed',
                                    ifelse(no_sunni==1,'No Sunni Pop',
                                           NA))),,
         occ_len_cat = ifelse(ever_isis_controlled==1,
                              ifelse(full_occ_dur_mon<=12, "Short",
                                     ifelse(full_occ_dur_mon>=35,"Long",
                                            "Mid")),NA),
         occ_status = ifelse(!is.na(occ_status_lm), "Occupied",
                             "Not Occupied")) |>
  mutate(occ_and_relig = paste0(sunni_status," - ",occ_status),
         occ_and_timing = paste0(occ_status, " - " ,occ_status_lm))


occ_vs_noocc_relig_filt <- occ_vs_noocc_relig |>
  filter(occ_status_lm %in% c('Pre','Post'))

plt_occ_len_wheatbarley <- ggplot(filter(occlen_extremes_relig_filt,
                                         plant_season=='WheatBarley'),
                                  aes(x=evi_prod_nat_divergence,color=occlen_and_timing)) +
  geom_density() +
  scale_color_manual(values=c("#F02015","#FA900F","#f5ed02",
                              "#030ffc","#0cbff5","#0cf5a3")) +
  labs(title="Agricultural Productivity (Divergence from National Avg) for ISIS Occupied Areas by Occupation Length and Timing\nduring Wheat and Barley Growing Months",
       x = "Ag Productivity (Diff from National Mean)", y="Density", color="ISIS Occupation Length - Timing") +
  theme_bw()
plt_occ_len_summer <- ggplot(filter(occlen_extremes_relig_filt,
                                    plant_season=='SummerCrops'),
                             aes(x=evi_prod_nat_divergence,color=occlen_and_timing)) +
  geom_density() +
  scale_color_manual(values=c("#F02015","#FA900F","#f5ed02",
                              "#030ffc","#0cbff5","#0cf5a3")) +
  labs(title="Agricultural Productivity (Divergence from National Avg) for ISIS Occupied Areas by Occupation Length and Timing\nduring Summer Crop Growing Months",
       x = "Ag Productivity (Diff from National Mean)", y="Density", color="ISIS Occupation Length - Timing") +
  theme_bw()

fin_plots <- arrangeGrob(plt_occ_len_wheatbarley,plt_occ_len_summer,nrow=1,
                         bottom=textGrob("ISIS Occupation Years Collected Monthly from Liveuamap Occupation Reports; Long Occupation is >= 35 months, Short Occupation is <= 12 months; Wheat and Barley Growing Season is Dec - Apr, Summer is May - Sept.\nAgricultural productivity calculated as the mean EVI for the first 6 weeks of the growing season minus the max EVI for the entire growing season, following Asher 2020.\nNational mean of productivity calculated for each growing season and subtracted from ADM3 productivity to get divergence from national mean."))
fin_plots
plt_occ_len_wheatbarley




