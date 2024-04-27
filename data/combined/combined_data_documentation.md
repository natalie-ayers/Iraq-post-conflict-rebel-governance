## Data Documentation

### General Fields
*month*: final day of month (data provided is for the entire month)

### Iraq Geographic Boundaries
https://data.humdata.org/dataset/cod-ab-irq
Collected by OCHA, provides Iraq subnational administrative boundaries at the 1, 2, and 3 levels
*ADM3_PCODE*, *ADM3_EN*, *ADM3_AR*: Admin 3 area names and code
*ADM2_PCODE*, *ADM2_EN*, *ADM2_AR*: Admin 2 area names and code
*ADM1_PCODE*, *ADM1_EN*, *ADM1_AR*: Admin 1 area names and code
*Shape_Area*: Admin 3 area  


### Agricultural Index: MODIS EVI 
From MODIS EVI (MODIS/061/MOD13Q1 in Google Earth Engine), masked to include only Cropland as identified by MODIS Land Cover product (MODIS/061/MCD12Q1, field LC_Type1==12)
Date range: 2001-01-01 to 2022-12-31

*max_evi*: the maximum EVI recorded over given Adm3 area
*max_evi_scaled*: the maximum evi recorded over given Adm3 area, scaled by the scale factor 0.0001 provided by MODIS to obtain EVI in range of -1 to 1
*mean_evi*: the mean EVI recorded over given Adm3 area
*mean_evi_scaled*: the mean evi recorded over given Adm3 area, scaled by the scale factor 0.0001 provided by MODIS to obtain EVI in range of -1 to 1


### UCDP Conflict Events
https://ucdp.uu.se/downloads/ged/ged231.pdf
UCDP Conflict Events, aggregated to the month-admin 3 level by ISIS affiliation and location precision

*ucdp_nonisis_events_adm3prec* : UCDP conflict events with where_prec of 1 or 2, meaning precise to the admin 3 level, and where 'IS' was not the actor on either side
*ucdp_isis_events_adm3prec* : UCDP conflict events with where_prec of 1 or 2, meaning precise to the admin 3 level, and where 'IS' was the actor on one side of the conflict
*ucdp_all_events_adm3prec* : UCDP conflict events with where_prec of 1 or 2, meaning precise to the admin 3 level, including events by all actors
*ucdp_nonisis_events_adm2prec* : UCDP conflict events with where_prec of 1, 2, or 3 meaning precise to the admin 2 level, and where 'IS' was not the actor on either side
*ucdp_isis_events_adm2prec* : UCDP conflict events with where_prec of 1, 2, or 3, meaning precise to the admin 2 level, and where 'IS' was the actor on one side of the conflict
*ucdp_all_events_adm2prec* : UCDP conflict events with where_prec of 1, 2, or 3, meaning precise to the admin 2 level, including events by all actors
*ucdp_nonisis_events_adm1prec* : UCDP conflict events with where_prec of 1, 2, 3, or 4, meaning precise to the admin 1 level, and where 'IS' was not the actor on either side
*ucdp_isis_events_adm1prec* : UCDP conflict events with where_prec of 1, 2, 3, or 4, meaning precise to the admin 1 level, and where 'IS' was the actor on one side of the conflict
*ucdp_all_events_adm1prec* : UCDP conflict events with where_prec of 1, 2, 3, or 4, meaning precise to the admin 1 level, including events by all actors



### xSub Conflict Events
https://cross-sub.org/about/variables-included
xSub data combined by de-duplicating events within 1km and 2 day windows
Date range: 2001-01-01 to 2019-12-31

*ACTION_IND*: indiscriminate force (eg, indirect fire, shelling, air strikes, chemical weapons)
*ACTION_SEL*: selective force (eg, direct fire, arrest, assassination)
*ACTION_PRT*: protest

### GPW Population Data
https://sedac.ciesin.columbia.edu/data/set/gpw-v4-population-count-adjusted-to-2015-unwpp-country-totals-rev11
UN WPP-Adjusted Population Count, v4.11
Dates available: 2000, 2005, 2010, 2015, 2020

*pop_count*: the sum of the population count in the given adm3 area
*pop_density*: pop_count / Shape_Area, with Shape_Area field from the Iraq administrative divisions shapefiles

### IOM Iraq Survey Responses
2022 IOM Survey of Iraq populations as part of IOM DTM Iraq project.
NOTE: this dataset does NOT vary by month, it is static across time and varies only by Adm3 area.

*disputed_area*: binary, based on Q1.10 IsLocationInDisputedArea. 
	1: yes, 0: no
*iom_attacked*: binary, based on Q1.11 WasLocationOccupied. 
	1: Yes, attacked, 0: otherwise
*iom_occupied*: binary, based on Q1.11 WasLocationOccupied. 
	1: Yes, occupied, 0: otherwise
*isil_ingroup_prior*: binary, based on Q1.12 FirstMajorEthno-religiousPrior.
	1: Arab Sunni Muslim, 0: otherwise
*isil_ingroup_curr*: binary, Q1.13 FirstMajorEthno-religiousCurrent.
	1: Arab Sunni Muslim, 0: otherwise
*retaken_year*: From Q1.11.1 RetakenPeriod: the year mentioned in the retaken period.
*retaken_post_2016*: binary, based on retaken_year
	1: if retaken_year >= 2017, 0: retaken_year not na and < 2017
*iom_no_isil_action*: binary, based on Q1.11 WasLocationOccupied. 
	1: No, 0: otherwise


### Blair and Nihad Occupation Dates
*occ_start_month*: month in which ISIS occupation began, coded to last day of month to match other dates in dataset
*occ_end_month* : month in which ISIS occupation ended, coded to last day of month to match other dates in dataset
*occ_length_mon* : number of months of ISIS occupation

### GeoEPR Ethnicity Fields
From GeoEPR 2021: https://icr.ethz.ch/data/epr/geoepr/

*sunni_dom* : binary, if GeoEPR lists the area as containing Sunni Arabs and there are no other ethnic groups listed in the same area
*sunni_mix* : binary, if GeoEPR lists the area as containing Sunni Arabs and another ethnic group
*no_sunni* : binary, if GeoEPR does not list the area as containing Sunni Arabs
