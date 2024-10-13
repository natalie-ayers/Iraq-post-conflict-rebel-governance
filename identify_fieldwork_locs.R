library(ggplot2)
library(gridExtra)
library(grid)
library(broom)
library(sp)
library(dplyr)
library(lubridate)
library(geojsonio)
library(data.table)



setwd("~/Research/Iraq-post-conflict-rebel-governance")

month_adm3_data = geojson_read('data/combined/monthly_adm3_data.geojson',
                               what="sp")
combined_monthly_data <- read.csv('data/combined/monthly_adm3_data.csv')


combined_monthly_data |>
    select(c(ADM1_EN,occ_length_mon)) |>
    distinct() |>
    filter(!is.na(occ_length_mon))


no_ag_areas <- combined_monthly_data |>
                    filter(sunni_dom == 1) |>
                    select(c('ADM3_EN','max_evi_scaled')) |>
                    group_by(ADM3_EN) |>
                    summarize(max_evi_scaled=max(max_evi_scaled,na.rm=TRUE)) |>
                    ungroup() |>
                    filter(max_evi_scaled == -Inf) |>
                    select(c('ADM3_EN'))

potential_fieldwork_areas <- combined_monthly_data |>
                                filter(
                                        #sunni_dom == 1,
                                       !(ADM3_EN %in% no_ag_areas$ADM3_EN)) |>
                                select(c('ADM1_EN','ADM2_EN','ADM3_EN','month',
                                         'occ_length_mon','sunni_dom',
                                         'max_evi_scaled')) |>
                                mutate(year=year(month)) |>
                                group_by(year,ADM1_EN,ADM2_EN,ADM3_EN,sunni_dom,occ_length_mon) |>
                                summarize(max_evi_scaled=max(max_evi_scaled)) |>
                                ungroup() |>
                                mutate(pre_conflict=ifelse(year <= 2013,1,0)) |>
                                group_by(ADM1_EN,ADM2_EN,ADM3_EN,occ_length_mon,sunni_dom,pre_conflict) |>
                                summarize(mean_max_evi_pre=mean(max_evi_scaled,na.rm=TRUE)) |>
                                ungroup() |>
                                filter(pre_conflict == 1) |>
                                select(-c(pre_conflict)) |>
                                rename(avg_max_evi_pre_conflict=mean_max_evi_pre)
























