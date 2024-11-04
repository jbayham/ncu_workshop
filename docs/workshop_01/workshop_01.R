#This is an introduction to working with spatial data in R

library(pacman) #if you have never used this package, you need to install it: install.packages("pacman)
pacman::p_load(tidyverse,sf,tmap)

setwd("~/Documents/git_projects/ncu_workshop/docs/workshop_01")

#############################

jp_boundaries <- st_read("inputs/gm-jpn-all_u_2_2/polbnda_jpn.shp")

mapview(jp_boundaries)

tmap_mode("view")

tm_basemap("CartoDB.Positron") +
  tm_shape(jp_boundaries) +
  tm_polygons(alpha = .4)


###############################

library(estatapi)
library(janitor)

api_key=Sys.getenv("STATISTICS_JAPAN_API_KEY")


stat_tab <- estat_getStatsList(appId = api_key, lang = "E", searchWord = "business conditions")

test_dat <- estat_getStatsData(
  appId = api_key,
  lang = "E",
  statsDataId = "0004008528"
  #cdCat01 = c("002")
)
#api_call <- "http://api.e-stat.go.jp/rest/3.0/app/getSimpleStatsData?cdArea=23000&cdCat01=002&appId=&lang=E&statsDataId=0003404084&metaGetFlg=Y&cntGetFlg=N&explanationGetFlg=Y&annotationGetFlg=Y&sectionHeaderFlg=1&replaceSpChars=0"

raw_dat <- test_dat %>%
  clean_names()

test_dat2 <- raw_dat %>%
  filter(cat04_code==13,#str_detect(labour_force_status_working,"Wishing to switch to another job"),
         cat03_code==0, #,age=="Total",
         cat02_code==0, #sex=="Both sexes")
         cat01_code==0)

with(test_dat2,table(age,sex))

jp_labor <- inner_join(jp_boundaries,test_dat2,by=c("adm_code"="area_code")) %>%
  mutate(frac = value/pop)

mapview(jp_labor,zcol="frac")


