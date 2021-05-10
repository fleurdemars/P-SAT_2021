rm(list=ls())
setwd('C:/Users/fleur/2021 P-SAT/주제분석/주제분석 1주차/데이터')

library(data.table)
library(tidyverse)
library(ggplot2)
library(raster)
library(rgeos)
library(maptools)
library(rgdal)

# 대한민국 전도(시군구)
map = readOGR('TL_SCCO_SIG.shp')
korea = fortify(map, region = 'SIG_CD')

# 시각화할 데이터
prov = fread('province_info.csv', data.table=FALSE)

# 법정동코드 데이터
adm = fread('adm_codeee.csv', data.table=FALSE, encoding='UTF-8')[,c(4,3)]

# 법정동코드 데이터와 맞춰주기 위해 임시로 데이터셋 수정
prov[c(230:233),] = prov[17,] #충북 청주시
prov[c(234,235),] = prov[28,] #충남 천안시
prov[c(236,237),] = prov[79,] #경북 포항시
prov[c(238:241),] = prov[102,] #경기도 수원시
prov[c(242:244),] = prov[103,] #경기도 성남시
prov[c(245,246),] = prov[105,] #경기도 안양시
prov[c(247,248),] = prov[110,] #경기도 안산시
prov[c(249:251),] = prov[111,] #경기도 고양시
prov[c(252:254),] = prov[120,] #경기도 용인시
prov[c(255:259),] = prov[133,] #경남 창원시
prov[c(260,261),] = prov[163,] #전북 전주시

prov[230,1] = '충청북도 청주시 상당구'
prov[231,1] = '충청북도 청주시 서원구'
prov[232,1] = '충청북도 청주시 청원구'
prov[233,1] = '충청북도 청주시 흥덕구'

prov[234,1] = '충청남도 천안시 동남구'
prov[235,1] = '충청남도 천안시 서북구'

prov[236,1] = '경상북도 포항시 남구'
prov[237,1] = '경상북도 포항시 북구'

prov[238,1] = '경기도 수원시 권선구'
prov[239,1] = '경기도 수원시 영통구'
prov[240,1] = '경기도 수원시 장안구'
prov[241,1] = '경기도 수원시 팔달구'

prov[242,1] = '경기도 성남시 분당구'
prov[243,1] = '경기도 성남시 수정구'
prov[244,1] = '경기도 성남시 중원구'

prov[245,1] = '경기도 안양시 동안구'
prov[246,1] = '경기도 안양시 만안구'

prov[247,1] = '경기도 안산시 단원구'
prov[248,1] = '경기도 안산시 상록구'

prov[249,1] = '경기도 고양시 덕양구'
prov[250,1] = '경기도 고양시 일산동구'
prov[251,1] = '경기도 고양시 일산서구'

prov[252,1] = '경기도 용인시 기흥구'
prov[253,1] = '경기도 용인시 수지구'
prov[254,1] = '경기도 용인시 처인구'

prov[255,1] = '경상남도 창원시 마산합포구'
prov[256,1] = '경상남도 창원시 마산회원구'
prov[257,1] = '경상남도 창원시 성산구'
prov[258,1] = '경상남도 창원시 의창구'
prov[259,1] = '경상남도 창원시 진해구'

prov[260,1] = '전라북도 전주시 덕진구'
prov[261,1] = '전라북도 전주시 완산구'

prov = prov[-c(17,28,79,102,103,105,110,111,120,133,163),]
rownames(prov) = NULL

prov %>% dim()

# 법정동 코드 데이터와 join해서 id(법정동 코드) 생성
prov_adm = left_join(prov, adm, key = 'sigungu')

prov_adm %>% is.na %>% sum #인천 미추홀구 제외하고선 NA 없음

# 지도 데이터의 id와 동일하게 character 형식으로
prov_adm$id = as.character(prov_adm$id)

# 시각화에 사용할 최종 데이터!
map_data = left_join(korea, prov_adm, by='id')


# EDA ####################################################
# (1) 1인당 GRDP EDA
ggplot() + geom_polygon(data=map_data, aes(x=long, y=lat, group=group, fill=grdp)) + 
  scale_fill_gradient(low='#f0fff5', high='#054d1d') +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank())

# (2) 1인당 지역총소득 EDA
ggplot() + geom_polygon(data=map_data, aes(x=long, y=lat, group=group, fill=income_ct)) + 
  scale_fill_gradient(low='#f5e4f2', high='#a36098') +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank())

# (3) 1인당 개인소득 EDA
ggplot() + geom_polygon(data=map_data, aes(x=long, y=lat, group=group, fill=income_personal)) + 
  scale_fill_gradient(low='#faf8eb', high='#b09a09') +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank())

# (4) 1인당 민간소비 EDA
ggplot() + geom_polygon(data=map_data, aes(x=long, y=lat, group=group, fill=consumption)) + 
  scale_fill_gradient(low='#dadae6', high='#1e216b') +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank())

# (5) 주민등록세대수 EDA
ggplot() + geom_polygon(data=map_data, aes(x=long, y=lat, group=group, fill=family)) + 
  scale_fill_gradient(low='white', high='#F2762E') +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank())

# (6) 동물병원 개수 EDA
ggplot() + geom_polygon(data=map_data, aes(x=long, y=lat, group=group, fill=hospital_num)) + 
  scale_fill_gradient(low='white', high='#09b0e3') +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank())
