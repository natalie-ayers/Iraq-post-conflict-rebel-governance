library(dplyr)
library(lubridate)

combined_monthly_data <- read.csv('data/combined/monthly_adm3_data.csv')

# remove fields that are currently unnecessary
monthly_data_sel <- combined_monthly_data |>
                      select(-c('ADM2_EN','ADM2_AR','ADM2_PCODE',
                                'ADM1_EN','ADM1_AR','ADM1_PCODE',
                                'ADM3_AR','ADM3_PCODE','isil_ingroup_prior',
                                'isil_ingroup_curr','mean_evi','max_evi',
                                'ACTION_PRT'))

# include spei to allow for residual calculations
spei <- read.csv('data/drought/gebrechorkos_etal_2023_spei/spei_sums/CHIRPS_GLEAM_06_spei_sums.csv')
spei <- spei |>
        select(c('time','mean_spei','max_spei','min_spei',
                 'num_drought','num_all','ADM3_EN')) |>
        mutate(perc_drought_points=num_drought / num_all,
               time=as.Date(time) + months(1) - days(1)) |>
        select(-c(num_drought,num_all))


# create data aggregated by seasons, with Feb - Apr as Spring, May - Jul as Summer
# Aug - Oct as Fall, and Nov - Jan as Winter
seasonal_data <- monthly_data_sel |>
                  mutate(month=as.Date(month)) |>
                  merge(spei,by.x=c('month','ADM3_EN'),
                        by.y=c('time','ADM3_EN'),
                        all.x=TRUE) |>
                  mutate(month_num=month(month),
                         year_num=year(month)) |>
                  mutate(season=ifelse(month_num %in% c(2,3,4),
                                            'Spring',
                                  ifelse(month_num %in% c(5,6,7),
                                         'Summer',
                                  ifelse(month_num %in% c(8,9,10),
                                         'Fall',
                                  ifelse(month_num %in% c(11,12,1),
                                         'Winter',
                                         NA))))) |>
                  mutate(season_year=ifelse(!is.na(season),
                                            paste0(season," ",as.character(year_num)),
                                             NA)) |>
                  select(-c(month,month_num)) |>
                  group_by(season_year,ADM3_EN,pop_count,pop_density,
                           disputed_area,iom_attacked,iom_occupied,
                           retaken_year,retaken_post_2016,iom_no_isil_action,
                           sunni_dom,sunni_mix,no_sunni,Shape_Area,year_num,season) |>
                  summarise(mean_evi_scaled=mean(mean_evi_scaled),
                            max_evi_scaled=max(max_evi_scaled),
                            ACTION_IND=sum(ACTION_IND),
                            ACTION_DIR=sum(ACTION_DIR),
                            mean_spei=mean(mean_spei),
                            max_spei=max(max_spei),
                            min_spei=min(min_spei),
                            perc_drought_points=mean(perc_drought_points)) |>
                  ungroup()

# run regression to predict EVI from SPEI metrics; use residuals
# as one (rough) measure of EVI that diverges from what would be expected
# given drought status alone
spei_mean_evi_lm <- lm(mean_evi_scaled ~ mean_spei +
                          max_spei + min_spei + perc_drought_points + season,
                       data=seasonal_data,
                       na.action=na.exclude)
#summary(spei_mean_evi_lm)
spei_max_evi_lm <- lm(max_evi_scaled ~ mean_spei +
                         max_spei + min_spei + perc_drought_points + season,
                       data=seasonal_data,
                      na.action=na.exclude)

# add the residuals back to the data as fields
# code suggestion: https://stackoverflow.com/questions/76046980/merge-residuals-to-data-with-missing-obs-na-exclude-not-working
seasonal_data[attr(spei_mean_evi_lm$residuals,which="name"),"mean_evi_resids"] <- spei_mean_evi_lm$residuals
seasonal_data[attr(spei_max_evi_lm$residuals,which="name"),"max_evi_resids"] <- spei_max_evi_lm$residuals



conservative_pre_isis <- seasonal_data |>
                          filter(year_num %in% c(2001,2002,2012,2013),
                                 season %in% c('Spring','Summer'))
all_pre_isis <- seasonal_data |>
                filter(year_num <= 2013,
                       season %in% c('Spring','Summer'))

post_isis <- seasonal_data |>
                filter(year_num >= 2018,
                       season %in% c('Spring','Summer'))

