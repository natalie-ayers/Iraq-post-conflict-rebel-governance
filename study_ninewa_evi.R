library(dplyr)

combined_monthly_data <- read.csv('data/combined/monthly_adm3_data.csv')


# Examine Nineva as example
nineva_df <- combined_monthly_data |>
  filter(ADM1_EN=='Ninewa') |>
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
         occ_start_year=year(occ_start_month))
ninev_gb_df <- nineva_df |>
  group_by(ADM3_EN,isis_occ_status,month_num) |>
  summarise(mean_evi_scaled = mean(mean_evi_scaled),
            max_evi_scaled = max(max_evi_scaled))


ggplot(filter(ninev_gb_df),
       aes(x=month_num,y=mean_evi_scaled,color=isis_occ_status)) +
  geom_smooth()+
  labs(title="Ninewa Mean EVI by Month by ISIS Occupation Timing")

ggplot(filter(ninev_gb_df),
       aes(x=month_num,y=max_evi_scaled,color=isis_occ_status)) +
  geom_smooth() +
  labs(title="Ninewa Max EVI by Month by ISIS Occupation Timing")
