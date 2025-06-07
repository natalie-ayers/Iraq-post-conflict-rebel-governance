library(ggplot2)
library(gridExtra)
library(grid)
library(broom)
library(sp)
library(dplyr)
library(fixest)
library(stringr)

## ---------------------- IMPORT DATA -------------------- ##

seasonal_data_productivity <- read.csv("data/combined/plant_season_liveuamap_adm3_data.csv")


## ---------------------- ANALYSIS DURING OCCUPATION ------------------- ##


occlen_extremes <- seasonal_data_productivity |>
    filter(ever_isis_controlled_lm == 1) |>
    mutate(occ_len_cat = ifelse(full_occ_dur_mon_lm<=12, "Short",
                                ifelse(full_occ_dur_mon_lm>=35,"Long",
                                       "Mid"))) |>
    mutate(occlen_and_timing = paste0(occ_len_cat, " - " ,occ_status_lm))

occlen_extremes_filt <- occlen_extremes |>
      filter(occ_len_cat %in% c('Long','Short')) |>
      filter(occ_status_lm %in% c('Pre','Post', 'During'))


# 217 Long-Post, 557 Long-Pre, 1551 Short-Post, 2377 Short-Pre
# 162 Long - During, 104 Short - During
occlen_extremes_filt |>
  group_by(occlen_and_timing) |>
  summarize(n=n())


occ_vs_noocc <- seasonal_data_productivity |>
  mutate(occ_len_cat = ifelse(ever_isis_controlled_lm==1,
                              ifelse(full_occ_dur_mon_lm<=12, "Short",
                                     ifelse(full_occ_dur_mon_lm>=35,"Long",
                                            "Mid")),NA),
         occ_status = ifelse(!is.na(occ_status_lm), "Occupied",
                             "Not Occupied")) |>
  mutate(occ_and_timing = paste0(occ_status, " - " ,occ_status_lm))


occ_vs_noocc_filt <- occ_vs_noocc |>
  filter(occ_status_lm %in% c('Pre','Post'))

# Not occupied: 14544, Occupied - During: 453, Occupied - Post: 2257, Occupied-Pre: 3914
occ_vs_noocc |>
  group_by(occ_and_timing) |>
  summarize(n=n())


occlen_vs_noocc <- seasonal_data_productivity |>
  mutate(occ_len_cat = ifelse(ever_isis_controlled_lm==1,
                              ifelse(full_occ_dur_mon_lm<=12, "Short",
                                     ifelse(full_occ_dur_mon_lm>=35,"Long",
                                            "Mid")),NA),
         occ_status = ifelse(!is.na(occ_status_lm), "Occupied",
                             "Not Occupied")) |>
  mutate(occ_and_timing = paste0(occ_status, " - " ,occ_len_cat, " - " ,occ_status_lm))


occlen_vs_noocc_filt <- occlen_vs_noocc |>
  filter(occ_status_lm %in% c('Pre','Post'))

# Not occupied: 14544, Occupied - During: 453, Occupied - Post: 2257, Occupied-Pre: 3914
occlen_vs_noocc |>
  group_by(occ_and_timing) |>
  summarize(n=n())



## -------------------- PLOTS WITH REFINED DATA --------------------------- ##

### ------------------------------------- OCCUPIED COMPARING DURATIONS ------------------------------- ###

# Plot EVI
plt_occ_len_wheatbarley <- ggplot(filter(occlen_extremes_filt,
                                         plant_season=='WheatBarley'),
                                  aes(x=evi_prod_scaled,color=occlen_and_timing)) +
  geom_density() +
  scale_color_manual(values=c("#F02015","#FA900F","#f5ed02",
                              "#030ffc","#0cbff5","#0cf5a3")) +
  labs(title="Agricultural Productivity for ISIS Occupied Areas by Occupation Length and Timing\nduring Wheat and Barley Growing Months",
       x = "Ag Productivity", y="Density", color="ISIS Occupation Length - Timing") +
  theme_bw()
plt_occ_len_summer <- ggplot(filter(occlen_extremes_filt,
                                    plant_season=='SummerCrops'),
                             aes(x=evi_prod_scaled,color=occlen_and_timing)) +
  geom_density() +
  scale_color_manual(values=c("#F02015","#FA900F","#f5ed02",
                              "#030ffc","#0cbff5","#0cf5a3")) +
  labs(title="Agricultural Productivity for ISIS Occupied Areas by Occupation Length and Timing\nduring Summer Crop Growing Months",
       x = "Ag Productivity", y="Density", color="ISIS Occupation Length - Timing") +
  theme_bw()

fin_plots <- arrangeGrob(plt_occ_len_wheatbarley,plt_occ_len_summer,nrow=1,
                         bottom=textGrob("ISIS Occupation Years Collected Monthly from Liveuamap Occupation Reports; Long Occupation is >= 35 months, Short Occupation is <= 12 months; Wheat and Barley Growing Season is Dec - Apr, Summer is May - Sept.\nAgricultural productivity calculated as the mean EVI for the first 6 weeks of the growing season minus the max EVI for the entire growing season, following Asher 2020."))
fin_plots
plt_occ_len_wheatbarley



# Plot EVI Difference from National Mean
plt_occ_len_wheatbarley <- ggplot(filter(occlen_extremes_filt,
                                         plant_season=='WheatBarley'),
                                  aes(x=evi_prod_nat_divergence,color=occlen_and_timing)) +
  geom_density() +
  scale_color_manual(values=c("#F02015","#FA900F","#f5ed02",
                              "#030ffc","#0cbff5","#0cf5a3")) +
  labs(title="Agricultural Productivity (Divergence from National Avg) for ISIS Occupied Areas by Occupation Length and Timing\nduring Wheat and Barley Growing Months",
       x = "Ag Productivity (Diff from National Mean)", y="Density", color="ISIS Occupation Length - Timing") +
  theme_bw()
plt_occ_len_summer <- ggplot(filter(occlen_extremes_filt,
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

# Plot EVI Difference from ADM1 Mean
plt_occ_len_wheatbarley <- ggplot(filter(occlen_extremes_filt,
                                         plant_season=='WheatBarley'),
                                  aes(x=evi_prod_adm1_divergence,color=occlen_and_timing)) +
  geom_density() +
  scale_color_manual(values=c("#F02015","#FA900F","#f5ed02",
                              "#030ffc","#0cbff5","#0cf5a3")) +
  labs(title="Agricultural Productivity (Divergence from Adm1 Avg) for ISIS Occupied Areas by Occupation Length and Timing\nduring Wheat and Barley Growing Months",
       x = "Ag Productivity (Diff from Adm1 Mean)", y="Density", color="ISIS Occupation Length - Timing") +
  theme_bw()
plt_occ_len_summer <- ggplot(filter(occlen_extremes_filt,
                                    plant_season=='SummerCrops'),
                             aes(x=evi_prod_adm1_divergence,color=occlen_and_timing)) +
  geom_density() +
  scale_color_manual(values=c("#F02015","#FA900F","#f5ed02",
                              "#030ffc","#0cbff5","#0cf5a3")) +
  labs(title="Agricultural Productivity (Divergence from Adm1 Avg) for ISIS Occupied Areas by Occupation Length and Timing\nduring Summer Crop Growing Months",
       x = "Ag Productivity (Diff from Adm1 Mean)", y="Density", color="ISIS Occupation Length - Timing") +
  theme_bw()

fin_plots <- arrangeGrob(plt_occ_len_wheatbarley,plt_occ_len_summer,nrow=1,
                         bottom=textGrob("ISIS Occupation Years Collected Monthly from Liveuamap Occupation Reports; Long Occupation is >= 35 months, Short Occupation is <= 12 months; Wheat and Barley Growing Season is Dec - Apr, Summer is May - Sept.\nAgricultural productivity calculated as the mean EVI for the first 6 weeks of the growing season minus the max EVI for the entire growing season, following Asher 2020.\nAdm1 mean of productivity calculated for each growing season and subtracted from ADM3 productivity to get divergence from Adm1 mean."))
fin_plots
plt_occ_len_wheatbarley


### ------------------------------------- OCCUPIED VS NOT-OCCUPIED ------------------------------- ###

# Plot EVI
plt_occ_wheatbarley <- ggplot(filter(occ_vs_noocc,
                                         plant_season=='WheatBarley'),
                                  aes(x=evi_prod_scaled,color=occ_and_timing)) +
  geom_density() +
  scale_color_manual(values=c("#F02015","#FA900F","#f5ed02",
                              "#030ffc","#0cbff5","#0cf5a3")) +
  labs(title="Agricultural Productivity for ISIS Occupied Areas by Occupation Length and Timing\nduring Wheat and Barley Growing Months",
       x = "Ag Productivity", y="Density", color="ISIS Occupation Length - Timing") +
  theme_bw()
plt_occ_summer <- ggplot(filter(occ_vs_noocc,
                                    plant_season=='SummerCrops'),
                             aes(x=evi_prod_scaled,color=occ_and_timing)) +
  geom_density() +
  scale_color_manual(values=c("#F02015","#FA900F","#f5ed02",
                              "#030ffc","#0cbff5","#0cf5a3")) +
  labs(title="Agricultural Productivity for ISIS Occupied Areas by Occupation Length and Timing\nduring Summer Crop Growing Months",
       x = "Ag Productivity", y="Density", color="ISIS Occupation Length - Timing") +
  theme_bw()

fin_plots <- arrangeGrob(plt_occ_wheatbarley,plt_occ_summer,nrow=1,
                         bottom=textGrob("ISIS Occupation Years Collected Monthly from Liveuamap Occupation Reports; Long Occupation is >= 35 months, Short Occupation is <= 12 months; Wheat and Barley Growing Season is Dec - Apr, Summer is May - Sept.\nAgricultural productivity calculated as the mean EVI for the first 6 weeks of the growing season minus the max EVI for the entire growing season, following Asher 2020."))
fin_plots
plt_occ_wheatbarley



# Plot EVI Difference from National Mean
plt_occ_wheatbarley <- ggplot(filter(occ_vs_noocc,
                                         plant_season=='WheatBarley'),
                                  aes(x=evi_prod_nat_divergence,color=occ_and_timing)) +
  geom_density() +
  scale_color_manual(values=c("#F02015","#FA900F","#f5ed02",
                              "#030ffc","#0cbff5","#0cf5a3")) +
  labs(title="Agricultural Productivity (Divergence from National Avg) for ISIS Occupied Areas by Occupation Length and Timing\nduring Wheat and Barley Growing Months",
       x = "Ag Productivity (Diff from National Mean)", y="Density", color="ISIS Occupation Length - Timing") +
  theme_bw()
plt_occ_summer <- ggplot(filter(occ_vs_noocc,
                                    plant_season=='SummerCrops'),
                             aes(x=evi_prod_nat_divergence,color=occ_and_timing)) +
  geom_density() +
  scale_color_manual(values=c("#F02015","#FA900F","#f5ed02",
                              "#030ffc","#0cbff5","#0cf5a3")) +
  labs(title="Agricultural Productivity (Divergence from National Avg) for ISIS Occupied Areas by Occupation Length and Timing\nduring Summer Crop Growing Months",
       x = "Ag Productivity (Diff from National Mean)", y="Density", color="ISIS Occupation Length - Timing") +
  theme_bw()

fin_plots <- arrangeGrob(plt_occ_wheatbarley,plt_occ_summer,nrow=1,
                         bottom=textGrob("ISIS Occupation Years Collected Monthly from Liveuamap Occupation Reports; Long Occupation is >= 35 months, Short Occupation is <= 12 months; Wheat and Barley Growing Season is Dec - Apr, Summer is May - Sept.\nAgricultural productivity calculated as the mean EVI for the first 6 weeks of the growing season minus the max EVI for the entire growing season, following Asher 2020.\nNational mean of productivity calculated for each growing season and subtracted from ADM3 productivity to get divergence from national mean."))
fin_plots
plt_occ__wheatbarley

# Plot EVI Difference from ADM1 Mean
plt_occ_wheatbarley <- ggplot(filter(occ_vs_noocc,
                                         plant_season=='WheatBarley'),
                                  aes(x=evi_prod_adm1_divergence,color=occ_and_timing)) +
  geom_density() +
  scale_color_manual(values=c("#F02015","#FA900F",
                              "#030ffc","#0cf5a3")) +
  labs(title="Agricultural Productivity (Divergence from Adm1 Avg) for ISIS Occupied Areas by Occupation Length and Timing\nduring Wheat and Barley Growing Months",
       x = "Ag Productivity (Diff from Adm1 Mean)", y="Density", color="ISIS Occupation Length - Timing") +
  theme_bw()
plt_occ_summer <- ggplot(filter(occ_vs_noocc,
                                    plant_season=='SummerCrops'),
                             aes(x=evi_prod_adm1_divergence,color=occ_and_timing)) +
  geom_density() +
  scale_color_manual(values=c("#F02015","#FA900F",
                              "#030ffc","#0cf5a3")) +
  labs(title="Agricultural Productivity (Divergence from Adm1 Avg) for ISIS Occupied Areas by Occupation Length and Timing\nduring Summer Crop Growing Months",
       x = "Ag Productivity (Diff from Adm1 Mean)", y="Density", color="ISIS Occupation Length - Timing") +
  theme_bw()

fin_plots <- arrangeGrob(plt_occ_wheatbarley,plt_occ_summer,nrow=1,
                         bottom=textGrob("ISIS Occupation Years Collected Monthly from Liveuamap Occupation Reports; Long Occupation is >= 35 months, Short Occupation is <= 12 months; Wheat and Barley Growing Season is Dec - Apr, Summer is May - Sept.\nAgricultural productivity calculated as the mean EVI for the first 6 weeks of the growing season minus the max EVI for the entire growing season, following Asher 2020.\nAdm1 mean of productivity calculated for each growing season and subtracted from ADM3 productivity to get divergence from Adm1 mean."))
fin_plots


## ------------------------- REGRESSIONS ---------------------- ##

seasonal_data_productivity_reg <- seasonal_data_productivity |>
                                    filter(plant_season != "NonGrowing") |>
                                    mutate(year=as.integer(str_sub(plant_season_year,-4,-1)),
                                           occ_len_cat = ifelse(full_occ_dur_mon_lm<=12, "Short",
                                                                ifelse(full_occ_dur_mon_lm>=35,"Long",
                                                                       "Mid")))

seasonal_data_productivity_reg_keepna <- seasonal_data_productivity_reg |>
                                          filter(plant_season != "NonGrowing") |>
                                          mutate(occ_status_lm=ifelse(is.na(occ_status_lm),'None',
                                                                      occ_status_lm),
                                                 full_occ_dur_mon_lm=ifelse(is.na(full_occ_dur_mon_lm),
                                                                            0,full_occ_dur_mon_lm),
                                                 occ_status_dur=paste0(occ_status_lm,"_",occ_len_cat))



seasonal_data_productivity_reg_keepna <- seasonal_data_productivity_reg |>
      mutate(occ_status = ifelse(!is.na(occ_status_lm), "Occupied",
                                 "Not Occupied"),
             occ_status_lm=ifelse(is.na(occ_status_lm),'None',
                              occ_status_lm),
             full_occ_dur_mon_lm=ifelse(is.na(full_occ_dur_mon_lm),
                                        0,full_occ_dur_mon_lm)) |>
  mutate(occ_and_timing = paste0(occ_status, " - " ,occ_len_cat, " - " ,occ_status_lm))




seasonal_data_productivity_reg_keepna |>
  filter(plant_season=="WheatBarley") |>
  ggplot(aes(x=year,y=evi_prod_scaled,group=occ_status_lm,
             colour = occ_status_lm)) +
  geom_smooth(method="loess")



seasonal_data_productivity_reg_keepna |>
  filter(plant_season=="SummerCrops") |>
  ggplot(aes(x=year,y=evi_prod_scaled,group=occ_status_lm,
             colour = occ_status_lm)) +
  geom_smooth(method="loess")



## ONLY COMPARING OCCUPIED AREAS
# when keeping occ dur mon as NA for non-occupied, get significant negative impact from occ dur - but only with
  # standard errors clustered by year. Argument for clusteringn by year is that agricultural production
  # very year-dependent (but already take that into account somewhat with divergence from national)
feols(seasonal_data_productivity_reg,
      evi_prod_nat_divergence ~ full_occ_dur_mon_lm + ucdp_isis_events_adm3prec + pop_density +
        + Shape_Area + plant_season | ADM1_EN + year)

# when keeping occ status as NA for non-occupied, get significant positive impact for Post - gets smaller with clustering
  # on admin1 area
feols(seasonal_data_productivity_reg,
      evi_prod_nat_divergence ~ occ_status_lm + ucdp_isis_events_adm3prec + pop_density + mean_spei +
        + Shape_Area + plant_season | ADM1_EN + year)


## COMPARING OCCUPIED VS NON-OCCUPIED

feols(seasonal_data_productivity_reg_keepna,
      evi_prod_nat_divergence ~ full_occ_dur_mon_lm + ucdp_isis_civ_events_adm3prec + ucdp_isis_nonciv_events_adm3prec + ucdp_nonisis_civ_events_adm3prec +
                                        pop_density + Shape_Area + plant_season | ADM1_EN + year)

summary(feols(seasonal_data_productivity_reg_keepna,
      evi_prod_nat_divergence ~ occ_and_timing + ucdp_isis_civ_events_adm3prec + ucdp_isis_nonciv_events_adm3prec + ucdp_nonisis_civ_events_adm3prec + pop_density + mean_spei +
        + Shape_Area + plant_season | ADM1_EN + year))


feols(seasonal_data_productivity_reg_keepna,
      evi_prod_scaled ~ full_occ_dur_mon_lm + ucdp_isis_civ_events_adm3prec + ucdp_isis_nonciv_events_adm3prec + ucdp_nonisis_civ_events_adm3prec +
        pop_density + Shape_Area + plant_season | ADM1_EN + year)

summary(feols(seasonal_data_productivity_reg_keepna,
              evi_prod_scaled ~ occ_and_timing + ucdp_isis_civ_events_adm3prec + ucdp_isis_nonciv_events_adm3prec + ucdp_nonisis_civ_events_adm3prec + pop_density + mean_spei +
                + Shape_Area + plant_season | ADM1_EN + year))




# significant impact for pre and post, both positive
feols(seasonal_data_productivity_reg_keepna,
      evi_prod_nat_divergence ~ occ_status_lm + ucdp_isis_events_adm3prec + pop_density + mean_spei +
        + Shape_Area + plant_season | ADM1_EN + year)



## ONLY COMPARING OCCUPIED AREAS
# when keeping occ dur mon as NA for non-occupied, get significant negative impact from occ dur - but only with
# standard errors clustered by year. Argument for clustering by year is that agricultural production
# very year-dependent (but already take that into account somewhat with divergence from national)
feols(seasonal_data_productivity_reg,
      evi_prod_scaled ~ full_occ_dur_mon_lm + ucdp_isis_events_adm3prec + pop_density +
        + Shape_Area + plant_season | ADM1_EN + year)

# when keeping occ status as NA for non-occupied, get significant positive impact for Post - gets smaller with clustering
# on admin1 area
feols(seasonal_data_productivity_reg,
      evi_prod_scaled ~ occ_status_lm + ucdp_isis_events_adm3prec + pop_density + mean_spei +
        + Shape_Area + plant_season | ADM1_EN + year)


## COMPARING OCCUPIED VS NON-OCCUPIED
feols(seasonal_data_productivity_reg_keepna,
      evi_prod_scaled ~ full_occ_dur_mon_lm + ucdp_isis_events_adm3prec + pop_density +
        + Shape_Area + plant_season | ADM1_EN + year)

# significant impact for pre and post, both positive
feols(seasonal_data_productivity_reg_keepna,
      evi_prod_scaled ~ occ_status_dur + ucdp_all_events_adm3prec + pop_density + mean_spei +
        + Shape_Area + plant_season | ADM1_EN + year)
