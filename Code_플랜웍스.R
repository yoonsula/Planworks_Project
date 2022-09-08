# NETWORK analysis

library(pacman)
library(network)
library(sna)
library(ggplot2)
library(GGally)
pacman::p_load('dplyr','tidymodels','tidytext','NLP4kec' ,'stringr','magrittr','tm')

data <- read.csv('/Users/parkjungeun/Downloads/topic6_data_last.csv', header = T)

#unlist
tdata <- rep(0, dim(data)[1])
for (i in 1:dim(data)[1]) {
  end <- nchar(data$new_tokens[i])
  str <- substr(data$new_tokens[i], 2, end-1)
  tdata[i] <- gsub(str, pattern = "'", replacement = "")
  tdata[i] <- gsub(tdata[i], pattern = ",", replacement = "")
}

cps <- VCorpus(VectorSource(tdata))
dtmTfIdf <- DocumentTermMatrix(x=cps, control = list(removeNumbers = TRUE, 
                                                     wordLenghts = c(2, Inf), 
                                                     weigthting = function(x) weightTfIdf(x, normalize = TRUE) ))


#차원 축소
dtmTfIdf <- removeSparseTerms(x = dtmTfIdf, sparse = as.numeric(x = 0.99))


#상관행렬 만들기
dtmTfIdf %>% as.matrix() %>% cor() -> corTerms


#텍스트 데이터-단어 네트워크맵

netTerms <- network(x = corTerms, directed = FALSE)


#상관행렬 크기 조정
corTerms[corTerms <= 0.15] <- 0    
netTerms <- network(x = corTerms, directed = FALSE)

#매개 중심성 계산
btnTerms <- betweenness(netTerms)

#네트워크 맵 그리기
netTerms %v% 'mode' <-
  ifelse(
    test = btnTerms >= quantile(x = btnTerms, probs = 0.90, na.rm = TRUE),
    yes = 'Top', 
    no = 'Rest')


nodeColors <- c('Top' = 'gold', 'Rest' = 'lightgrey')

# edge의 크기를 상관계수의 3배로 설정
set.edge.value(netTerms, attrname = 'edgeSize', value = corTerms * 3)

#12.3.4 네트워크 맵 그리기(2)

dev.new(width = 1000, height = 1000, unit="px")

ggnet2(
  net = netTerms,
  mode = 'fruchtermanreingold',
  layout.par = list(cell.jitter = 0.001),
  size.min = 3,
  label = TRUE,
  label.size = 3,
  node.color = 'mode',
  palette = nodeColors,
  node.size = sna::degree(dat = netTerms),
  edge.size = 'edgeSize',
  family = 'AppleGothic')+
  labs(title = "매개중심성 반영한 단어-네트워크맵")+  
  theme_gray(base_family = 'NanumGothic')

dev.off()

#===============================================================================
# 상관 단어 추출
# 키워드 유무 확인
checkCorTerms <- function(x,n = 10, keyword) {
  
  x %>%
    colnames() %>%
    str_subset(pattern = keyword) %>%
    print()
  
  corRef <- data.frame()
  
  corRef <- x[ , keyword] %>%
    sort(decreasing = TRUE) %>%
    data.frame() %>%
    set_colnames(c('corr'))
  
  head(x = corRef, n = n + 1)
}


# 상관행렬 만들기
dtmTfIdf %>% as.matrix() %>% cor() -> corTerms
checkCorTerms(corTerms, 5 ,'beach')





#=========================================================================
# Comparison Word Cloud

library(qdap)
library(tidyverse)
library(dplyr)
library(tidyr)
library(stringr)
library(tidytext)
library(ggplot2) 
library(ggthemes)
library(wordcloud)


topic6 = read.csv("C:\\R_dir\\newtopic_data.csv",stringsAsFactors = F, header = TRUE,
                  encoding = "UTF-8")

#-----------------bing 감성사전 수정-----------------
#부정 단어 -> 긍정으로 수정
pos_neg_df = as.data.frame(get_sentiments('bing'))
pos_neg_df[pos_neg_df$word %in% c('cheap','funny','chill','stun'),]$sentiment <- 'positive'
pos_neg_tibble <- as_tibble(pos_neg_df)


#bing 사전안에 단어 제거
pos_neg_df = as.data.frame(pos_neg_tibble)
pos_neg_df <- pos_neg_df[!(pos_neg_df$word %in% c('tank','lemon','rail','die','fried','stalls', 'stall', 'fall', 'cave','overlook','complex','miss')),]
pos_neg_tibble <- as_tibble(pos_neg_df)

#-----------------불용어사전 custom--------------------
custom_stop_words <- bind_rows(
  stop_words,
  tibble(word = c("nice","beautiful"), lexicon = c("custom")))

custom_stop_words %>% 
  tail()

#------------Comparison Word Cloud by topic--------------
#filter를 통해 topic별 comparison word cloud 확인 가능
wordcloudData2 = 
  topic6 %>%
  filter(new_topic==6)%>%
  select(new_tokens,new_topic)%>%
  unnest_tokens(output=word,input=new_tokens)%>%
  anti_join(custom_stop_words)%>%
  inner_join(pos_neg_tibble)%>%
  count(sentiment,word,sort=T)%>%
  spread(key=sentiment,value = n,fill=0)%>%
  data.frame()
rownames(wordcloudData2) = wordcloudData2[,'word']
wordcloudData2 = wordcloudData2[,c('positive','negative')]

set.seed(621)
comparison.cloud(term.matrix = wordcloudData2,scale = c(2,0.5),max.words = 150, rot.per=0,colors = c("blue","red"))
