####
library(plyr)
library(ggplot2)
library(tidyverse)
library(data.table)
library(jsonlite)
library(stringr)
library(KoNLP)
library(wordcloud2)
library(progress)

useSejongDic()

setwd("C:/Users/hjh05/Downloads/ugi")
data = fread("data/data_2020_dog.csv",header = TRUE,data.table = FALSE)
data$specialMark[data$specialMark=="특이사항없음"] <- "없음"

#disease = fread("한국과학기술정보연구원_동물질병데이터_20201203.csv",header = TRUE,data.table = FALSE)
################################
##단어 쪼개는 함수 직접 만들기
divid_chr = function(sentence){
  txt = gsub("/"," ",sentence)
  txt = gsub("\\.",",",txt)
  txt = gsub(","," ",txt)
  txt = str_split(txt, '\\s+')
  txt = unlist(txt)
  
  txt = txt[-grepl('[1-9]',txt)==F]
  txt = txt[-grepl('색',txt)==F] 
  txt = txt[-grepl('믹스',txt)==F]
  txt = txt[-grepl('이름',txt)==F]
  txt = txt[-grepl('줄',txt)==F]
  txt = txt[-grepl('털',txt)==F]
  txt = txt[-grepl('에서',txt)==F]
  txt = txt[-grepl('하네스',txt)==F]
  txt = txt[-grepl('중성화',txt)==F]
  return(txt)
}


divid_chr2 = function(sentence){
  txt = gsub("/"," ",sentence)
  txt = gsub("\\.",",",txt)
  txt = gsub(","," ",txt)
  txt = str_split(txt, '\\s+')
  txt = unlist(txt)
  
  txt = txt[-grepl('[1-9]',txt)==F]
  txt = txt[-grepl('색',txt)==F] 
  txt = txt[-grepl('믹스',txt)==F]
  txt = txt[-grepl('이름',txt)==F]
  txt = txt[-grepl('줄',txt)==F]
  txt = txt[-grepl('털',txt)==F]
  txt = txt[-grepl('에서',txt)==F]
  txt = txt[-grepl('하네스',txt)==F]
  txt = txt[-grepl('중성화',txt)==F]
  txt = txt[-grepl('[A-Z]',txt)==F]
  txt = txt[-grepl('[a-z]',txt)==F]
  return(txt)
}


#############
##################
#질병 사전 만들기
#all_dis <- data.frame(do.call('rbind', strsplit(as.character(disease$질병명),
#                                                split = '(',fixed = TRUE)))[,1]

#nouns = lapply(all_dis,extractNoun)
#nouns2 <- unlist(nouns)
#nouns <- Filter(function(x) {nchar(x) >= 2}, nouns2)
#영어는 빼고
#nouns = nouns[-grepl('[A-Z]',nouns)==F]
#nouns = nouns[-grepl('[a-z]',nouns)==F]
#nouns <- unlist(nouns) %>% unique %>% sort()
#write.table(nouns,file='질병단어사전.txt')

#dis_dic <- nouns

#############################
#data$diseaseYN

####################################################
####################################################
#특징 전체 단어 워드클라우드
yung <- sapply(data$specialMark,extractNoun,USE.NAMES = F)
yung <- unlist(yung)
yung <- Filter(function(x) {nchar(x) >= 2}, yung)

yung <- gsub('\\d+','',yung)
yung <- gsub('-','',yung)
yung <- gsub('""','',yung)
write(unlist(yung),'all_details1_2.txt') #filter전이 1
yung <- read.table('all_details1_2.txt')
head(yung,200)

wordcount <- table(yung)
wordcount <- head(sort(wordcount,decreasing=T),200)
wordcount

wordcloud2(data=wordcount,fontFamily = '나눔스퀘어_ac')

#####details 정리후 워드클라우드
details <- data
yung <- unlist(details)
yung <- Filter(function(x) {nchar(x) >= 2}, yung) #추가
yung <- gsub('\\d+','',yung)
yung <- gsub('-','',yung)
yung <- gsub('""','',yung)
write(unlist(yung),'all_details2_2.txt')
yung <- read.table('all_details2_2.txt')
head(yung,200)

wordcount <- table(yung)
wordcount <- head(sort(wordcount,decreasing=T),200)
wordcount

wordcloud2(data=wordcount,fontFamily = '나눔스퀘어_ac')

#####################################
## plot here!!

####################################
####질병변수만들기
#####################################
###질병사전 주요 서술어 col뽑기

dis_dictionary = read.table('질병단어사전.txt')
dis_dictionary = unlist(dis_dictionary)

details = lapply(data$specialMark,divid_chr)

top_dic = list(NULL)

pb <- progress_bar$new(total = nrow(data))

for (k in 1:nrow(data)){
  pb$tick()

  sen = details[k]
  
  cks=NULL
  for (i in 1:length(sen)){
    ck = dis_dictionary[str_detect(sen[i], dis_dictionary)] 
    cks=c(cks,ck)
  }
  if (length(cks)!=0) {
    top_dic[k] <- as.list(cks)
  }
}

top_dic %>% head

#어떤 워딩때문에 걸렸을까?

##details가 질병사전(dis_dic)에 있는지 확인 후 질병변수만들기
data$diseaseYN = NA

data$diseaseYN <- sapply(top_dic, length)
data$diseaseYN <- as.factor(data$diseaseYN)
data %>% colnames
data[1:10,c(12,19)]

##시각화
## disease2 워드크래프트
yung <- unlist(top_dic)
yung <- gsub('\\d+','',yung)
yung <- gsub('-','',yung)
yung <- gsub('""','',yung)
write(unlist(yung),'top_disdic.txt')
yung <- read.table('top_disdic.txt')
head(yung,200)

wordcount <- table(yung)
wordcount <- head(sort(wordcount,decreasing=T),200)
wordcount

wordcloud2(data=wordcount,fontFamily = '나눔스퀘어_ac')


#######질병유무가 표시된 막대그래프
######입양된 애들의 질병비율 파이차트
######


######################################







####################################
#####################################
##성격 사전 만들기
#군산대 한국어 감성사전의 명사로 새로 만듦
#positive <- readLines("pos_pol_word.txt", encoding = "UTF-8")
#negative <- readLines("neg_pol_word.txt", encoding = "UTF-8")

#positive %>% head
#nouns<-extractNoun(positive)
#nouns <- unlist(nouns)
#nouns <- Filter(function(x) {nchar(x) >= 2}, nouns)
#nouns <- unique(nouns) %>% sort()
#pos_dic <- nouns
#write(unlist(pos_dic),'pos_dic.txt')
pos_dic <- unlist(read.table('pos_dic.txt'))

#nouns<-extractNoun(negative)
#nouns <- unlist(nouns)
#nouns <- Filter(function(x) {nchar(x) >= 2}, nouns)
#nouns <- unique(nouns) %>% sort()
#neg_dic <- nouns
#write(unlist(neg_dic),'neg_dic.txt')
neg_dic <- unlist(read.table('neg_dic.txt'))



## 성격처리 divid_chr2
# 질병에 겹치는게 없었던 col
details = data$specialMark
detail2 = divid_chr2(details)
detail3 <- detail2 %>% unlist() %>% unique()

detail4 <- Filter(function(x) {nchar(x) >= 2}, detail3)

#질병사전에 없는 단어만 추출
index=NULL
for (i in 1:length(detail4)){
  if (sum(str_detect(detail4[i],dis_dictionary))==0){
    index = c(index, i)}
}
detail_dic <- detail4[index]

#단어 긍정 부정으로 나눠
pindex=NULL
nindex=NULL
for (i in 1:length(detail_dic)){
  words <- detail_dic[i]
  if (sum(str_detect(words,pos_dic))>= 1){
    pindex = c(pindex, i)}
  else if (sum(str_detect(words,neg_dic))>= 1){
    nindex = c(nindex, i)}
}

pos_dic <- detail_dic[pindex]
neg_dic <- detail_dic[nindex]

#write(unlist(pos_dic) %>% sort,'positives_in_detail.txt')
#write(unlist(neg_dic) %>% sort,'negatives_in_detail.txt')


##########################################
########################################
########################################
########################################

###############################################
## deatail2 기준으로 감성분석해 #match말고!
#1eh dlTdma
pos_dic <- unlist(read.table('positives_in_detail.txt'))
neg_dic <- unlist(read.table('negatives_in_detail.txt'))

#1:length(detail2)
sentiment=NULL
for (k in 1:length(detail2)){
  print(paste(k, "/ 94534"))
  pos=0
  neg=0
  score=0
  words = detail2[[k]]
  if(length(words)!=0){
    for (i in 1:length(words)){
      try({
        if (sum(str_count(pos_dic,words[i]))>=1){
          pos = pos+1}
        if (sum(str_count(neg_dic,words[i]))>=1){
          neg=neg+1}
        },silent=TRUE)}
    score = pos-neg}
  sentiment = c(sentiment,score)
}

sentiment %>% unique

sentiment[sentiment >=1] = 1
sentiment[sentiment <=-1] = -1

sentiment %>% unique

data$mood <- sentiment
data %>% colnames
