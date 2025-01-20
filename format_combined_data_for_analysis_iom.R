library(dplyr)
library(lubridate)
library(geojsonio)
library(data.table)

combined_monthly_data <- read.csv('data/combined/monthly_adm3_data.csv')
combined_monthly_data_geo <- geojson_read('data/combined/monthly_adm3_data.geojson',
                                          what="sp")


# remove fields that are currently unnecessary
monthly_data_sel <- combined_monthly_data |>
                      select(-c('ADM2_EN','ADM2_AR','ADM2_PCODE',
                                'ADM1_EN','ADM1_AR','ADM1_PCODE',
                                'ADM3_AR','ADM3_PCODE','iom_isil_ingroup_prior',
                                'iom_isil_ingroup_curr','mean_evi_mask',
                                'max_evi_mask','mean_evi_nomask','max_evi_nomask',
                                'ACTION_PRT','ACTION_IND', 'ACTION_DIR'))



# include spei to allow for residual calculations
spei <- read.csv('data/drought/gebrechorkos_etal_2023_spei/spei_sums/CHIRPS_GLEAM_06_spei_sums.csv')
spei <- spei |>
        select(c('time','mean_spei','max_spei','min_spei',
                 'num_drought','num_all','ADM3_EN')) |>
        mutate(perc_drought_points=num_drought / num_all,
               time=as.Date(time) + months(1) - days(1)) |>
        select(-c(num_drought,num_all))


## ------------------------ PLANT SEASON TIMING, ASHER METHOD (UNMASKED INDICES) --------------- ##

# calculate the mean EVI for the first 6 weeks of each planting season to use as
# part of the agricultural productivity measure
seasonstart_evi_nomask <- read.csv('data/satellite_indices/evi/evi_mean_max_nomask/evi_stats_Feb2000-Feb2024.csv')  |>
                        select(c(ADM3_EN,date, mean_evi_scaled,max_evi_scaled)) |>
                        mutate(date_dt=as.Date(date),
                               month_num=month(date_dt),
                               year_num=year(date_dt),
                               day_num=day(date_dt)) |>
                        mutate(plant_season=ifelse(month_num %in% c(12,1,2,3,4),
                                            'WheatBarley',
                                            ifelse(month_num %in% c(5,6,7,8,9),
                                                   'SummerCrops',
                                                   ifelse(month_num %in% c(10,11),
                                                          'NonGrowing',
                                                          NA))))  |>
                        mutate(plant_season_year=case_when(
                          ((plant_season=="WheatBarley")&(month_num==12)) ~ paste0(plant_season,"-",as.character(year_num+1)),
                          ((plant_season=="WheatBarley")&(month_num!=12)) ~ paste0(plant_season,"-",as.character(year_num)),
                          .default = paste0(plant_season,"-",as.character(year_num))
                        )) |>
                        filter(month_num %in% c(12,1,5,6)) |>
                        filter(case_when(month_num %in% c(1,6) ~ day_num <= 14,
                                         T ~ day_num <= 40)) |>
                        select(c(ADM3_EN,plant_season,plant_season_year,
                                 mean_evi_scaled)) |>
                        group_by(ADM3_EN,plant_season,plant_season_year) |>
                        summarize(seasonstart_mean_evi_scaled=mean(mean_evi_scaled,
                                                                   na.rm=TRUE)) |>
                        ungroup()


# define seasonal productivity in the style of Asher et al. 2024, subtracting start-of-season mean EVI from
# seasonal Max EVI
seasonal_data_productivity_init <- monthly_data_sel |>
  select(-c(mean_evi_mask_scaled,max_evi_mask_scaled)) |>
  mutate(month=as.Date(month),
         iom_occ_end_month=as.Date(iom_occ_end_month),
         iom_occ_start_month=as.Date(iom_occ_start_month)) |>
  merge(spei,by.x=c('month','ADM3_EN'),
        by.y=c('time','ADM3_EN'),
        all.x=TRUE) |>
  mutate(isis_occ_monthly=ifelse(iom_occupied==1,
                                 ifelse(month>=iom_occ_start_month&month<=iom_occ_end_month,1,0),NA),
         isis_occ_status=ifelse(iom_occupied==1,
                                ifelse(month<iom_occ_start_month,'Pre',
                                       ifelse(month>=iom_occ_start_month&month<=iom_occ_end_month,'During',
                                              ifelse(month>iom_occ_end_month,'Post',NA))),NA),
         month_num=month(month),
         year_num=year(month),
         retaken_year=year(iom_occ_end_month),
         occ_start_year=year(iom_occ_start_month)) |>

  mutate(retaken_post_2016=ifelse(retaken_year>2016,1,0),
         plant_season=ifelse(month_num %in% c(12,1,2,3,4),
                             'WheatBarley',
                             ifelse(month_num %in% c(5,6,7,8,9),
                                    'SummerCrops',
                                    ifelse(month_num %in% c(10,11),
                                           'NonGrowing',
                                           NA)))) |>
  mutate(plant_season_year=case_when(
    ((plant_season=="WheatBarley")&(month_num==12)) ~ paste0(plant_season,"-",as.character(year_num+1)),
    ((plant_season=="WheatBarley")&(month_num!=12)) ~ paste0(plant_season,"-",as.character(year_num)),
    .default = paste0(plant_season,"-",as.character(year_num))
  ))



seasonal_data_productivity <- seasonal_data_productivity_init |>
  group_by(plant_season_year,ADM3_EN,plant_season,
           iom_disputed_area,iom_attacked,iom_occupied,
           retaken_year,retaken_post_2016,occ_start_year,
           iom_occ_length_mon, iom_no_isil_action,
           sunni_dom,sunni_mix,no_sunni) |>
  summarise(max_evi_nomask_scaled=max(max_evi_nomask_scaled,
                                      na.rm=TRUE),
            ucdp_nonisis_events_adm1prec = sum(ucdp_nonisis_events_adm1prec,na.rm=TRUE),
            ucdp_isis_events_adm1prec = sum(ucdp_isis_events_adm1prec,na.rm=TRUE),
            ucdp_all_events_adm1prec = sum(ucdp_all_events_adm1prec,na.rm=TRUE),
            ucdp_nonisis_events_adm2prec = sum(ucdp_nonisis_events_adm2prec,na.rm=TRUE),
            ucdp_isis_events_adm2prec = sum(ucdp_isis_events_adm2prec,na.rm=TRUE),
            ucdp_all_events_adm2prec = sum(ucdp_all_events_adm2prec,na.rm=TRUE),
            ucdp_nonisis_events_adm3prec = sum(ucdp_nonisis_events_adm3prec,na.rm=TRUE),
            ucdp_isis_events_adm3prec = sum(ucdp_isis_events_adm3prec,na.rm=TRUE),
            ucdp_all_events_adm3prec = sum(ucdp_all_events_adm3prec,na.rm=TRUE),
            mean_spei=mean(mean_spei),
            max_spei=max(max_spei),
            min_spei=min(min_spei),
            # since population data are at 5-year intervals, some plant seasons
            # will encompass 2 different estimates (eg, Dec of 2004 and Jan-Apr of 2005)
            # take an average in these cases
            pop_count=mean(pop_count),
            pop_density=mean(pop_density),
            perc_drought_points=mean(perc_drought_points),
            isis_occ_season=max(isis_occ_monthly),
            isis_occ_status = paste(isis_occ_status, collapse="-")) |>
  ungroup() |>
  merge(seasonstart_evi_nomask,
        by=c('plant_season_year','ADM3_EN','plant_season'),
        all=TRUE) |>
  mutate(evi_prod_scaled = max_evi_nomask_scaled - seasonstart_mean_evi_scaled,
         isis_occ_status = ifelse(grepl("During",isis_occ_status),
                                  'During',
                                  ifelse(grepl('Post',isis_occ_status),
                                         'Post',
                                         ifelse(grepl('Pre',isis_occ_status),
                                                'Pre',NA)))) |>
  # don't have data pre-2000, so can't get full season start mean for the 1999/2000 WheatBarley season (WheatBarley-2000)
  filter(plant_season_year != 'WheatBarley-2000')


# to measure divergence of agricultural productivity from the annual average, removing
# some of the effects of any given year, calculate the country-wide average of ag productivity for each growing season
national_annual_plantseason_productivity <- seasonal_data_productivity |>
                                              filter(plant_season!="NonGrowing") |>
                                              select(c(plant_season_year,ADM3_EN,evi_prod_scaled)) |>
                                              group_by(plant_season_year) |>
                                              summarize(national_evi_prod_scaled=mean(evi_prod_scaled)) |>
                                              ungroup()


seasonal_data_productivity <- seasonal_data_productivity |>
                                  merge(national_annual_plantseason_productivity,
                                        by='plant_season_year',
                                        all=TRUE) |>
                                  mutate(evi_prod_nat_divergence= evi_prod_scaled - national_evi_prod_scaled)



# confirm have 21168 distinct plant-season-year and ADM3 combos
# before removing plant-season-years where we don't have full data
seasonal_data_productivity |>
    select(c(plant_season_year, ADM3_EN)) |>
    distinct() |>
    nrow()

# confirm no duplicates
seasonal_data_productivity[duplicated(seasonal_data_productivity[,c('plant_season_year', 'ADM3_EN')]),]


write.csv(seasonal_data_productivity,
          "data/combined/plant_season_adm3_data.csv",
          row.names=FALSE)


## ---------------------- 3-MO SEASON TIMING, ASHER MEHOD (UNMASKED INDICES) ---------------- ##


# define seasonal productivity in the style of Asher et al. 2024, subtracting start-of-season mean EVI from
# seasonal Max EVI - use first 2 months of mean EVI as start-of-season, since don't have weekly EVI to do first 6 weeks
seasonal_data_productivity_init <-monthly_data_sel |>
                              mutate(month=as.Date(month),
                                     occ_end_month=as.Date(occ_end_month),
                                     occ_start_month=as.Date(occ_start_month)) |>
                              merge(spei,by.x=c('month','ADM3_EN'),
                                    by.y=c('time','ADM3_EN'),
                                    all.x=TRUE) |>
                              mutate(isis_occ_monthly=ifelse(iom_occupied==1,
                                                             ifelse(month>=occ_start_month&month<=occ_end_month,1,0),NA),
                                     isis_occ_status=ifelse(iom_occupied==1,
                                                            ifelse(month<occ_start_month,'Pre',
                                                                   ifelse(month>=occ_start_month&month<=occ_end_month,'During',
                                                                          ifelse(month>occ_end_month,'Post',NA))),NA),
                                     month_num=month(month),
                                     year_num=year(month),
                                     retaken_year=year(occ_end_month),
                                     occ_start_year=year(occ_start_month)) |>
                              mutate(retaken_post_2016=ifelse(retaken_year>2016,1,0),
                                     season=ifelse(month_num %in% c(2,3,4),
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
                                                        NA))


# identify first week of each season
# because data is grouped into Months, by last day of month,
# these week numbers will not start at first week of month of first month in season
first_seasonal_weak_year <- seasonal_data_productivity_init |>
                            group_by(year_num,season) |>
                            summarize(first_season_week=min(week_num),
                                      # winter crosses into Jan, so min week num won't work
                                      mid_season_week=median(week_num)) |>
                            ungroup() |>
                            mutate(first_season_week=ifelse(season=='Winter',
                                                            mid_season_week,
                                                            first_season_week)) |>
                            select(-c(mid_season_week))

season_start_means  <- seasonal_data_productivity_init |>
                                # take first 2 months of each growing season to calculate
                                # mean EVI during start of growing season
                                filter(month_num %in% c(2,3,5,6,8,9,11,12)) |>
                                select(c(season_year,ADM3_EN,year_num,season,
                                         mean_evi_scaled,mean_ndvi_scaled)) |>
                                group_by(season_year,ADM3_EN,year_num,season) |>
                                mutate(season_start_mean_evi_scaled=mean(mean_evi_scaled),
                                       season_start_mean_ndvi_scaled=mean(mean_ndvi_scaled)) |>
                                ungroup() |>
                                select(-c(mean_evi_scaled,mean_ndvi_scaled)) |>
                                distinct()



seasonal_data_productivity <- seasonal_data_productivity_init |>
                              merge(season_start_means,
                                    by=c('season_year','ADM3_EN','year_num','season')) |>
                              select(-c(month,month_num)) |>
                              group_by(season_year,ADM3_EN,pop_count,pop_density,
                                       disputed_area,iom_attacked,iom_occupied,
                                       retaken_year,retaken_post_2016,occ_start_year,
                                       occ_length_mon,
                                       iom_no_isil_action,
                                       sunni_dom,sunni_mix,no_sunni,Shape_Area,year_num,season,
                                       season_start_mean_evi_scaled,season_start_mean_ndvi_scaled) |>
                              summarise(max_evi_scaled=max(max_evi_scaled),
                                       max_ndvi_scaled=max(max_ndvi_scaled),
                                        ucdp_nonisis_events_adm1prec = sum(ucdp_nonisis_events_adm1prec,na.rm=TRUE),
                                        ucdp_isis_events_adm1prec = sum(ucdp_isis_events_adm1prec,na.rm=TRUE),
                                        ucdp_all_events_adm1prec = sum(ucdp_all_events_adm1prec,na.rm=TRUE),
                                        ucdp_nonisis_events_adm2prec = sum(ucdp_nonisis_events_adm2prec,na.rm=TRUE),
                                        ucdp_isis_events_adm2prec = sum(ucdp_isis_events_adm2prec,na.rm=TRUE),
                                        ucdp_all_events_adm2prec = sum(ucdp_all_events_adm2prec,na.rm=TRUE),
                                        ucdp_nonisis_events_adm3prec = sum(ucdp_nonisis_events_adm3prec,na.rm=TRUE),
                                        ucdp_isis_events_adm3prec = sum(ucdp_isis_events_adm3prec,na.rm=TRUE),
                                        ucdp_all_events_adm3prec = sum(ucdp_all_events_adm3prec,na.rm=TRUE),
                                        mean_spei=mean(mean_spei),
                                        max_spei=max(max_spei),
                                        min_spei=min(min_spei),
                                        perc_drought_points=mean(perc_drought_points),
                                        isis_occ_season=max(isis_occ_monthly),
                                        isis_occ_status = paste(isis_occ_status, collapse="-")) |>
                              ungroup() |>
                              mutate(evi_prod_scaled = max_evi_scaled - season_start_mean_evi_scaled,
                                     ndvi_prod_scaled = max_ndvi_scaled - season_start_mean_ndvi_scaled,
                                      isis_occ_status = ifelse(grepl("During",isis_occ_status),
                                                              'During',
                                                              ifelse(grepl('Post',isis_occ_status),
                                                                     'Post',
                                                                     ifelse(grepl('Pre',isis_occ_status),
                                                                            'Pre',NA))))



# create data aggregated by seasons, with Feb - Apr as Spring, May - Jul as Summer
# Aug - Oct as Fall, and Nov - Jan as Winter
seasonal_data <- monthly_data_sel |>
                  mutate(month=as.Date(month),
                         occ_end_month=as.Date(occ_end_month),
                         occ_start_month=as.Date(occ_start_month)) |>
                  merge(spei,by.x=c('month','ADM3_EN'),
                        by.y=c('time','ADM3_EN'),
                        all.x=TRUE) |>
                  mutate(isis_occ_monthly=ifelse(iom_occupied==1,
                                                 ifelse(month>=occ_start_month&month<=occ_end_month,1,0),NA),
                         isis_occ_status=ifelse(iom_occupied==1,
                                                ifelse(month<occ_start_month,'Pre',
                                                       ifelse(month>=occ_start_month&month<=occ_end_month,'During',
                                                              ifelse(month>occ_end_month,'Post',NA))),NA),
                         month_num=month(month),
                         year_num=year(month),
                         retaken_year=year(occ_end_month),
                         occ_start_year=year(occ_start_month)) |>
                  mutate(retaken_post_2016=ifelse(retaken_year>2016,1,0),
                          season=ifelse(month_num %in% c(2,3,4),
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
                           retaken_year,retaken_post_2016,occ_start_year,
                           occ_length_mon,
                           iom_no_isil_action,
                           sunni_dom,sunni_mix,no_sunni,Shape_Area,year_num,season) |>
                  summarise(mean_evi_scaled=mean(mean_evi_scaled),
                            max_evi_scaled=max(max_ndvi_scaled),
                            mean_ndvi_scaled=mean(mean_ndvi_scaled),
                            max_ndvi_scaled=max(max_evi_scaled),
                            ucdp_nonisis_events_adm1prec = sum(ucdp_nonisis_events_adm1prec,na.rm=TRUE),
                            ucdp_isis_events_adm1prec = sum(ucdp_isis_events_adm1prec,na.rm=TRUE),
                            ucdp_all_events_adm1prec = sum(ucdp_all_events_adm1prec,na.rm=TRUE),
                            ucdp_nonisis_events_adm2prec = sum(ucdp_nonisis_events_adm2prec,na.rm=TRUE),
                            ucdp_isis_events_adm2prec = sum(ucdp_isis_events_adm2prec,na.rm=TRUE),
                            ucdp_all_events_adm2prec = sum(ucdp_all_events_adm2prec,na.rm=TRUE),
                            ucdp_nonisis_events_adm3prec = sum(ucdp_nonisis_events_adm3prec,na.rm=TRUE),
                            ucdp_isis_events_adm3prec = sum(ucdp_isis_events_adm3prec,na.rm=TRUE),
                            ucdp_all_events_adm3prec = sum(ucdp_all_events_adm3prec,na.rm=TRUE),
                            mean_spei=mean(mean_spei),
                            max_spei=max(max_spei),
                            min_spei=min(min_spei),
                            perc_drought_points=mean(perc_drought_points),
                            isis_occ_season=max(isis_occ_monthly),
                            isis_occ_status = paste(isis_occ_status, collapse="-")) |>
                  ungroup() |>
                  mutate(isis_occ_status = ifelse(grepl("During",isis_occ_status),
                                                        'During',
                                            ifelse(grepl('Post',isis_occ_status),
                                                   'Post',
                                            ifelse(grepl('Pre',isis_occ_status),
                                                   'Pre',NA))))

# run regression to predict EVI from SPEI metrics; use residuals
# as one (rough) measure of EVI that diverges from what would be expected
# given drought status alone
spei_mean_evi_lm <- lm(mean_evi_scaled ~ mean_spei +
                          max_spei + min_spei + perc_drought_points + season + pop_density,
                       data=seasonal_data,
                       na.action=na.exclude)
#summary(spei_mean_evi_lm)
spei_max_evi_lm <- lm(max_evi_scaled ~ mean_spei +
                         max_spei + min_spei + perc_drought_points + season + pop_density,
                       data=seasonal_data,
                      na.action=na.exclude)

spei_mean_ndvi_lm <- lm(mean_ndvi_scaled ~ mean_spei +
                         max_spei + min_spei + perc_drought_points + season + pop_density,
                       data=seasonal_data,
                       na.action=na.exclude)
#summary(spei_mean_evi_lm)
spei_max_ndvi_lm <- lm(max_ndvi_scaled ~ mean_spei +
                        max_spei + min_spei + perc_drought_points + season + pop_density,
                      data=seasonal_data,
                      na.action=na.exclude)

summary(spei_mean_evi_lm)


# add the residuals back to the data as fields
# code suggestion: https://stackoverflow.com/questions/76046980/merge-residuals-to-data-with-missing-obs-na-exclude-not-working
seasonal_data[attr(spei_mean_evi_lm$residuals,which="name"),"mean_evi_resids"] <- spei_mean_evi_lm$residuals
seasonal_data[attr(spei_max_evi_lm$residuals,which="name"),"max_evi_resids"] <- spei_max_evi_lm$residuals
seasonal_data[attr(spei_mean_ndvi_lm$residuals,which="name"),"mean_ndvi_resids"] <- spei_mean_ndvi_lm$residuals
seasonal_data[attr(spei_max_ndvi_lm$residuals,which="name"),"max_ndvi_resids"] <- spei_max_ndvi_lm$residuals



saveRDS(seasonal_data, 'seasonal_data.Rds')

# get descriptive stats for categories
seasonal_data |>
  select(c(ADM3_EN,ucdp_isis_events_adm1prec,
           isis_occ_season,ucdp_isis_events_adm3prec,
           sunni_dom,sunni_mix,no_sunni)) |>
  mutate(sunni_status=ifelse(sunni_dom==1,'Sunni Dominated',
                             ifelse(sunni_mix==1,'Sunni Mixed',
                                    ifelse(no_sunni==1,'No Sunni Pop',
                                           NA)))) |>
  select(-c(sunni_dom,sunni_mix,no_sunni)) |>
  group_by(ADM3_EN,sunni_status) |>
  summarise(isis_occ_season = max(isis_occ_season,na.rm=TRUE),
            isis_attacks_adm3 = max(ucdp_isis_events_adm3prec,na.rm=TRUE),
            isis_attacks_adm1 = max(ucdp_isis_events_adm1prec,na.rm=TRUE)) |>
  mutate(iom_isis_activity=ifelse(isis_occ_season > 0,
                                  "Occupied",
                                  ifelse(isis_attacks_adm3 > 0,
                                         "Attacked - Adm3 Prec",
                                         ifelse(isis_attacks_adm1 > 0,
                                                "Attacked - Adm1 Prec",
                                                'No ISIS Activity')))) |>
  group_by(iom_isis_activity,sunni_status) |>
  summarise(n_areas = n()) |>
  ungroup() |>
  knitr::kable()



## Filter datasets for specific use cases
conservative_pre_isis <- seasonal_data |>
                          filter(year_num %in% c(2001,2002,2012,2013))
all_pre_isis <- seasonal_data |>
                filter(year_num <= 2013)

post_isis <- seasonal_data |>
                filter(year_num >= 2018)

conservative_pre_post <- rbind(conservative_pre_isis,post_isis)

all_pre_post <- rbind(all_pre_isis,post_isis)



# identify earliest and latest attack dates by admin area
isis_att_dates <- monthly_data_sel |>
  mutate(month=as.Date(month)) |>
  mutate(earliest_isis_att_adm3=min(month[ucdp_isis_events_adm3prec>0],na.rm=TRUE),
         earliest_isis_att_adm1=min(month[ucdp_isis_events_adm1prec>0],na.rm=TRUE),
         latest_isis_att_adm3=max(month[ucdp_isis_events_adm3prec>0],na.rm=TRUE),
         latest_isis_att_adm1=max(month[ucdp_isis_events_adm1prec>0],na.rm=TRUE),
         .by=ADM3_EN) |>
  select(c(ADM3_EN,earliest_isis_att_adm3,earliest_isis_att_adm1,
           latest_isis_att_adm3,latest_isis_att_adm1)) |>
  mutate(earliest_isis_att_adm3=data.table::fifelse(is.infinite(earliest_isis_att_adm3),
                                                    as.Date(NA),
                                                    earliest_isis_att_adm3),
         earliest_isis_att_adm1=data.table::fifelse(is.infinite(earliest_isis_att_adm1),
                                                    as.Date(NA),
                                                    earliest_isis_att_adm1),
         latest_isis_att_adm3=data.table::fifelse(is.infinite(latest_isis_att_adm3),
                                                  as.Date(NA),
                                                  latest_isis_att_adm3),
         latest_isis_att_adm1=data.table::fifelse(is.infinite(latest_isis_att_adm1),
                                                  as.Date(NA),
                                                  latest_isis_att_adm1)) |>
  unique()



## -------------------------------- PLANT SEASON TIMING, MASKED INDICES ------------------ ##

# create data aggregated by plant growing seasons,
plant_timing_data <- monthly_data_sel |>
  mutate(month=as.Date(month),
         occ_end_month=as.Date(occ_end_month),
         occ_start_month=as.Date(occ_start_month)) |>
  merge(spei,by.x=c('month','ADM3_EN'),
        by.y=c('time','ADM3_EN'),
        all.x=TRUE) |>
  mutate(isis_occ_monthly=ifelse(iom_occupied==1,
                                 ifelse(month>=occ_start_month&month<=occ_end_month,1,0),NA),
         isis_occ_status=ifelse(iom_occupied==1,
                                ifelse(month<occ_start_month,'Pre',
                                       ifelse(month>=occ_start_month&month<=occ_end_month,'During',
                                              ifelse(month>occ_end_month,'Post',NA))),NA),
         month_num=month(month),
         year_num=year(month),
         retaken_year=year(occ_end_month),
         occ_start_year=year(occ_start_month)) |>
  mutate(plant_season=ifelse(month_num %in% c(12,1,2,3,4),
                       'WheatBarley',
                       ifelse(month_num %in% c(5,6,7,8,9),
                              'SummerCrops',
                              ifelse(month_num %in% c(10,11),
                                     'NonGrowing',
                                            NA)))) |>
  mutate(season_year=ifelse(!is.na(plant_season),
                            paste0(plant_season," ",as.character(year_num)),
                            NA)) |>
  merge(isis_att_dates, on='ADM3_EN', how='left') |>
  mutate(isis_attack_timing_adm3=ifelse(!is.na(latest_isis_att_adm3),
                                        ifelse(month < earliest_isis_att_adm3,
                                              'Pre',
                                        ifelse(month > latest_isis_att_adm3,
                                               'Post',
                                               'During')),NA),
         isis_attack_timing_adm1=ifelse(!is.na(latest_isis_att_adm1),
                                        ifelse(month < earliest_isis_att_adm1,
                                               'Pre',
                                               ifelse(month > latest_isis_att_adm1,
                                                      'Post',
                                                      'During')),NA)) |>
  # if don't have ucdp conflict or occupation records for admin area,
  # note that there was no isis activity
  mutate(no_isis_action=ifelse(is.na(earliest_isis_att_adm1)&is.na(isis_occ_status),1,0)) |>
  select(-c(month,month_num)) |>
  group_by(season_year,ADM3_EN,pop_count,pop_density,
           disputed_area,iom_attacked,iom_occupied,
           retaken_year,occ_start_year,
           occ_length_mon,isis_attack_timing_adm3,isis_attack_timing_adm1,
           no_isis_action,
           sunni_dom,sunni_mix,no_sunni,Shape_Area,year_num,plant_season) |>
  summarise(mean_evi_scaled=mean(mean_evi_scaled),
            max_evi_scaled=max(max_evi_scaled),
            mean_ndvi_scaled=mean(mean_ndvi_scaled),
            max_ndvi_scaled=max(max_ndvi_scaled),
            ucdp_nonisis_events_adm1prec = sum(ucdp_nonisis_events_adm1prec,na.rm=TRUE),
            ucdp_isis_events_adm1prec = sum(ucdp_isis_events_adm1prec,na.rm=TRUE),
            ucdp_all_events_adm1prec = sum(ucdp_all_events_adm1prec,na.rm=TRUE),
            ucdp_nonisis_events_adm2prec = sum(ucdp_nonisis_events_adm2prec,na.rm=TRUE),
            ucdp_isis_events_adm2prec = sum(ucdp_isis_events_adm2prec,na.rm=TRUE),
            ucdp_all_events_adm2prec = sum(ucdp_all_events_adm2prec,na.rm=TRUE),
            ucdp_nonisis_events_adm3prec = sum(ucdp_nonisis_events_adm3prec,na.rm=TRUE),
            ucdp_isis_events_adm3prec = sum(ucdp_isis_events_adm3prec,na.rm=TRUE),
            ucdp_all_events_adm3prec = sum(ucdp_all_events_adm3prec,na.rm=TRUE),
            mean_spei=mean(mean_spei),
            max_spei=max(max_spei),
            min_spei=min(min_spei),
            perc_drought_points=mean(perc_drought_points),
            isis_occ_season=max(isis_occ_monthly),
            isis_occ_status = paste(isis_occ_status, collapse="-")) |>
  ungroup() |>
  mutate(isis_occ_status = ifelse(grepl("During",isis_occ_status),
                                  'During',
                                  ifelse(grepl('Post',isis_occ_status),
                                         'Post',
                                         ifelse(grepl('Pre',isis_occ_status),
                                                'Pre',NA))),
         isis_attack_timing_adm3 = ifelse(grepl("During",isis_attack_timing_adm3),
                                          'During',
                                          ifelse(grepl('Post',isis_attack_timing_adm3),
                                                 'Post',
                                                 ifelse(grepl('Pre',isis_attack_timing_adm3),
                                                        'Pre',NA))),
         isis_attack_timing_adm1 = ifelse(grepl("During",isis_attack_timing_adm1),
                                          'During',
                                          ifelse(grepl('Post',isis_attack_timing_adm1),
                                                 'Post',
                                                 ifelse(grepl('Pre',isis_attack_timing_adm1),
                                                        'Pre',NA))))



# run regression to predict EVI from SPEI metrics; use residuals
# as one (rough) measure of EVI that diverges from what would be expected
# given drought status alone
spei_mean_evi_lm_plant <- lm(mean_evi_scaled ~ mean_spei +
                         max_spei + min_spei + perc_drought_points + plant_season,
                       data=plant_timing_data,
                       na.action=na.exclude)
#summary(spei_mean_evi_lm)
spei_max_evi_lm_plant <- lm(max_evi_scaled ~ mean_spei +
                        max_spei + min_spei + perc_drought_points + plant_season,
                      data=plant_timing_data,
                      na.action=na.exclude)

spei_mean_ndvi_lm_plant <- lm(mean_ndvi_scaled ~ mean_spei +
                               max_spei + min_spei + perc_drought_points + plant_season,
                             data=plant_timing_data,
                             na.action=na.exclude)
#summary(spei_mean_evi_lm)
spei_max_ndvi_lm_plant <- lm(max_ndvi_scaled ~ mean_spei +
                              max_spei + min_spei + perc_drought_points + plant_season,
                            data=plant_timing_data,
                            na.action=na.exclude)


# add the residuals back to the data as fields
# code suggestion: https://stackoverflow.com/questions/76046980/merge-residuals-to-data-with-missing-obs-na-exclude-not-working
plant_timing_data[attr(spei_mean_evi_lm_plant$residuals,which="name"),"mean_evi_resids"] <- spei_mean_evi_lm_plant$residuals
plant_timing_data[attr(spei_max_evi_lm_plant$residuals,which="name"),"max_evi_resids"] <- spei_max_evi_lm_plant$residuals
plant_timing_data[attr(spei_mean_ndvi_lm_plant$residuals,which="name"),"mean_ndvi_resids"] <- spei_mean_ndvi_lm_plant$residuals
plant_timing_data[attr(spei_max_ndvi_lm_plant$residuals,which="name"),"max_ndvi_resids"] <- spei_max_ndvi_lm_plant$residuals



saveRDS(plant_timing_data, 'plant_timing_data.Rds')

# get descriptive stats for categories
plant_timing_data |>
  select(c(ADM3_EN,ucdp_isis_events_adm1prec,
           isis_occ_season,ucdp_isis_events_adm3prec,
           sunni_dom,sunni_mix,no_sunni)) |>
  mutate(sunni_status=ifelse(sunni_dom==1,'Sunni Dominated',
                             ifelse(sunni_mix==1,'Sunni Mixed',
                                    ifelse(no_sunni==1,'No Sunni Pop',
                                           NA)))) |>
  select(-c(sunni_dom,sunni_mix,no_sunni)) |>
  group_by(ADM3_EN,sunni_status) |>
  summarise(isis_occ_season = max(isis_occ_season,na.rm=TRUE),
            isis_attacks_adm3 = max(ucdp_isis_events_adm3prec,na.rm=TRUE),
            isis_attacks_adm1 = max(ucdp_isis_events_adm1prec,na.rm=TRUE)) |>
  mutate(iom_isis_activity=ifelse(isis_occ_season > 0,
                                  "Occupied",
                                  ifelse(isis_attacks_adm3 > 0,
                                         "Attacked - Adm3 Prec",
                                         ifelse(isis_attacks_adm1 > 0,
                                                "Attacked - Adm1 Prec",
                                                'No ISIS Activity')))) |>
  group_by(iom_isis_activity,sunni_status) |>
  summarise(n_areas = n()) |>
  ungroup() |>
  knitr::kable()



## Filter datasets for specific use cases
conservative_pre_isis <- plant_timing_data |>
  filter(year_num %in% c(2001,2002,2012,2013))
all_pre_isis <- plant_timing_data |>
  filter(year_num <= 2013)

post_isis <- plant_timing_data |>
  filter(year_num >= 2018)

conservative_pre_post <- rbind(conservative_pre_isis,post_isis)

all_pre_post <- rbind(all_pre_isis,post_isis)



## --------------------------- 3 MO SEASONS -------------------------------------- ##

# create data aggregated by seasons, with Feb - Apr as Spring, May - Jul as Summer
# Aug - Oct as Fall, and Nov - Jan as Winter
seasonal_data <- monthly_data_sel |>
  mutate(month=as.Date(month),
         occ_end_month=as.Date(occ_end_month),
         occ_start_month=as.Date(occ_start_month)) |>
  merge(spei,by.x=c('month','ADM3_EN'),
        by.y=c('time','ADM3_EN'),
        all.x=TRUE) |>
  mutate(isis_occ_monthly=ifelse(iom_occupied==1,
                                 ifelse(month>=occ_start_month&month<=occ_end_month,1,0),NA),
         isis_occ_status=ifelse(iom_occupied==1,
                                ifelse(month<occ_start_month,'Pre',
                                       ifelse(month>=occ_start_month&month<=occ_end_month,'During',
                                              ifelse(month>occ_end_month,'Post',NA))),NA),
         month_num=month(month),
         year_num=year(month),
         retaken_year=year(occ_end_month),
         occ_start_year=year(occ_start_month)) |>
  mutate(retaken_post_2016=ifelse(retaken_year>2016,1,0),
         season=ifelse(month_num %in% c(2,3,4),
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
           retaken_year,retaken_post_2016,occ_start_year,
           occ_length_mon,
           iom_no_isil_action,
           sunni_dom,sunni_mix,no_sunni,Shape_Area,year_num,season) |>
  summarise(mean_evi_scaled=mean(mean_evi_scaled),
            max_evi_scaled=max(max_ndvi_scaled),
            mean_ndvi_scaled=mean(mean_ndvi_scaled),
            max_ndvi_scaled=max(max_evi_scaled),
            ucdp_nonisis_events_adm1prec = sum(ucdp_nonisis_events_adm1prec,na.rm=TRUE),
            ucdp_isis_events_adm1prec = sum(ucdp_isis_events_adm1prec,na.rm=TRUE),
            ucdp_all_events_adm1prec = sum(ucdp_all_events_adm1prec,na.rm=TRUE),
            ucdp_nonisis_events_adm2prec = sum(ucdp_nonisis_events_adm2prec,na.rm=TRUE),
            ucdp_isis_events_adm2prec = sum(ucdp_isis_events_adm2prec,na.rm=TRUE),
            ucdp_all_events_adm2prec = sum(ucdp_all_events_adm2prec,na.rm=TRUE),
            ucdp_nonisis_events_adm3prec = sum(ucdp_nonisis_events_adm3prec,na.rm=TRUE),
            ucdp_isis_events_adm3prec = sum(ucdp_isis_events_adm3prec,na.rm=TRUE),
            ucdp_all_events_adm3prec = sum(ucdp_all_events_adm3prec,na.rm=TRUE),
            mean_spei=mean(mean_spei),
            max_spei=max(max_spei),
            min_spei=min(min_spei),
            perc_drought_points=mean(perc_drought_points),
            isis_occ_season=max(isis_occ_monthly),
            isis_occ_status = paste(isis_occ_status, collapse="-")) |>
  ungroup() |>
  mutate(isis_occ_status = ifelse(grepl("During",isis_occ_status),
                                  'During',
                                  ifelse(grepl('Post',isis_occ_status),
                                         'Post',
                                         ifelse(grepl('Pre',isis_occ_status),
                                                'Pre',NA))))

# run regression to predict EVI from SPEI metrics; use residuals
# as one (rough) measure of EVI that diverges from what would be expected
# given drought status alone
spei_mean_evi_lm <- lm(mean_evi_scaled ~ mean_spei +
                         max_spei + min_spei + perc_drought_points + season + pop_density,
                       data=seasonal_data,
                       na.action=na.exclude)
#summary(spei_mean_evi_lm)
spei_max_evi_lm <- lm(max_evi_scaled ~ mean_spei +
                        max_spei + min_spei + perc_drought_points + season + pop_density,
                      data=seasonal_data,
                      na.action=na.exclude)

spei_mean_ndvi_lm <- lm(mean_ndvi_scaled ~ mean_spei +
                          max_spei + min_spei + perc_drought_points + season + pop_density,
                        data=seasonal_data,
                        na.action=na.exclude)
#summary(spei_mean_evi_lm)
spei_max_ndvi_lm <- lm(max_ndvi_scaled ~ mean_spei +
                         max_spei + min_spei + perc_drought_points + season + pop_density,
                       data=seasonal_data,
                       na.action=na.exclude)

summary(spei_mean_evi_lm)


# add the residuals back to the data as fields
# code suggestion: https://stackoverflow.com/questions/76046980/merge-residuals-to-data-with-missing-obs-na-exclude-not-working
seasonal_data[attr(spei_mean_evi_lm$residuals,which="name"),"mean_evi_resids"] <- spei_mean_evi_lm$residuals
seasonal_data[attr(spei_max_evi_lm$residuals,which="name"),"max_evi_resids"] <- spei_max_evi_lm$residuals
seasonal_data[attr(spei_mean_ndvi_lm$residuals,which="name"),"mean_ndvi_resids"] <- spei_mean_ndvi_lm$residuals
seasonal_data[attr(spei_max_ndvi_lm$residuals,which="name"),"max_ndvi_resids"] <- spei_max_ndvi_lm$residuals



saveRDS(seasonal_data, 'seasonal_data.Rds')

# get descriptive stats for categories
seasonal_data |>
  select(c(ADM3_EN,ucdp_isis_events_adm1prec,
           isis_occ_season,ucdp_isis_events_adm3prec,
           sunni_dom,sunni_mix,no_sunni)) |>
  mutate(sunni_status=ifelse(sunni_dom==1,'Sunni Dominated',
                             ifelse(sunni_mix==1,'Sunni Mixed',
                                    ifelse(no_sunni==1,'No Sunni Pop',
                                           NA)))) |>
  select(-c(sunni_dom,sunni_mix,no_sunni)) |>
  group_by(ADM3_EN,sunni_status) |>
  summarise(isis_occ_season = max(isis_occ_season,na.rm=TRUE),
            isis_attacks_adm3 = max(ucdp_isis_events_adm3prec,na.rm=TRUE),
            isis_attacks_adm1 = max(ucdp_isis_events_adm1prec,na.rm=TRUE)) |>
  mutate(iom_isis_activity=ifelse(isis_occ_season > 0,
                                  "Occupied",
                                  ifelse(isis_attacks_adm3 > 0,
                                         "Attacked - Adm3 Prec",
                                         ifelse(isis_attacks_adm1 > 0,
                                                "Attacked - Adm1 Prec",
                                                'No ISIS Activity')))) |>
  group_by(iom_isis_activity,sunni_status) |>
  summarise(n_areas = n()) |>
  ungroup() |>
  knitr::kable()



## Filter datasets for specific use cases
conservative_pre_isis <- seasonal_data |>
  filter(year_num %in% c(2001,2002,2012,2013))
all_pre_isis <- seasonal_data |>
  filter(year_num <= 2013)

post_isis <- seasonal_data |>
  filter(year_num >= 2018)

conservative_pre_post <- rbind(conservative_pre_isis,post_isis)

all_pre_post <- rbind(all_pre_isis,post_isis)



# identify earliest and latest attack dates by admin area
isis_att_dates <- monthly_data_sel |>
  mutate(month=as.Date(month)) |>
  mutate(earliest_isis_att_adm3=min(month[ucdp_isis_events_adm3prec>0],na.rm=TRUE),
         earliest_isis_att_adm1=min(month[ucdp_isis_events_adm1prec>0],na.rm=TRUE),
         latest_isis_att_adm3=max(month[ucdp_isis_events_adm3prec>0],na.rm=TRUE),
         latest_isis_att_adm1=max(month[ucdp_isis_events_adm1prec>0],na.rm=TRUE),
         .by=ADM3_EN) |>
  select(c(ADM3_EN,earliest_isis_att_adm3,earliest_isis_att_adm1,
           latest_isis_att_adm3,latest_isis_att_adm1)) |>
  mutate(earliest_isis_att_adm3=data.table::fifelse(is.infinite(earliest_isis_att_adm3),
                                                    as.Date(NA),
                                                    earliest_isis_att_adm3),
         earliest_isis_att_adm1=data.table::fifelse(is.infinite(earliest_isis_att_adm1),
                                                    as.Date(NA),
                                                    earliest_isis_att_adm1),
         latest_isis_att_adm3=data.table::fifelse(is.infinite(latest_isis_att_adm3),
                                                  as.Date(NA),
                                                  latest_isis_att_adm3),
         latest_isis_att_adm1=data.table::fifelse(is.infinite(latest_isis_att_adm1),
                                                  as.Date(NA),
                                                  latest_isis_att_adm1)) |>
  unique()




