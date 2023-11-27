
rm(list=ls())
#Load libraries
library(scholar)
library(tidyverse)
library(wordcloud)
library(wordcloud2)
library(tm)

# Scrape publications from google scholar
mgr<-"L-TjbwcAAAAJ" #Manu
jv<-"T-nbIuUAAAAJ" #Julie
# me<-"mlDS95sAAAAJ&hl"
jo<-"1qY46vkAAAAJ" # Juan
# lcr<-"ktoySDMAAAAJ"
kf<-"AqHxsRMAAAAJ" #Katharina
ac<-"HiZ4jTIAAAAJ"
# mj<-"AVEfN3AAAAAJ&hl"
nc<- "ZdkEQCIAAAAJ"
# at<-"j1WI0HcAAAAJ"
rf<-"zJC8trkAAAAJ" #Rena
sc<- "0wc_ap4AAAAJ"# Samuel
#yn<-   #Yash
ag<- "mpFnDZ8AAAAJ"   #Ash
ek<- "RHpGbo8AAAAJ"#Emma
ml<-  "w8xVTj8AAAAJ" #Murray
vh<-  "MSmlY90AAAAJ" #Vanessa
#kc<-   #Kerryn
ma<-  "7Cc2H8QAAAAJ" #Mariana
sg<-  "bPHl_6AAAAAJ" #Sophie



ids<-c(mgr,jv,jo,kf,ac,nc,rf, sc, ag, ek, ml,vh,ma,sg)

pubs<-lapply(ids, get_publications)
pubs<-do.call(rbind.data.frame, pubs)

# my_id<-"L-TjbwcAAAAJ&hl" # from google scholar URL
# pubs<-get_publications(my_id)

#Change to Corpus
titles<-pubs$title

docs<-Corpus(VectorSource(titles))

#Format text
docs<-docs %>%
  tm_map(removeNumbers)%>%
  tm_map(removePunctuation)%>%
  tm_map(stripWhitespace)
docs<-tm_map(docs, content_transformer(tolower))
docs<- tm_map(docs, removeWords, stopwords(kind="english"))


common.words<-c( "coral","corals", "reef", 
                 "great", "barrier", "reefs", "marine", 
                 "–","water","report","along","")
#Make matrix
dtm<- TermDocumentMatrix(docs)
matrix <-  as.matrix(dtm)
words <- sort(rowSums(matrix), decreasing = T)
df <-  data.frame(word=names(words), freq=words)

#plot
set.seed(1234)
df<-df%>%
  filter(!(word %in% common.words))
wordcloud(words=df$word, freq=df$freq,
          min.freq=5, max.words=100, random.order=F, 
          rot.per=0.35, colors=brewer.pal(8, "Dark2"), scale=c(3, 0.2))
wordcloud2(data=df,size=1,minSize=0,gridSize=0, shuffle = F)



period_1_start = 2010
period_2_start = 2016
cites<-lapply(ids, get_citation_history)
cites<-do.call(rbind.data.frame, cites)
cites<-cites%>%group_by(year)%>%
  summarise(cites=sum(cites))
cites_1 <- cites %>% dplyr::filter((year>=period_1_start & year<period_2_start ))
#remove last year since it's not a full year
cites_2 <- cites %>% dplyr::filter((year>=period_2_start & year<2021 )) 


fit1=lm(cites ~ year, data = cites_1)
fit2=lm(cites ~ year, data = cites_2)
inc1 = fit1$coefficients["year"]
inc2 = fit2$coefficients["year"] 
print(sprintf('Annual increase for periods 1 and 2 are %f, %f',inc1,inc2))


cites_1$group = "1"
cites_2$group = "2"
cites_df = rbind(cites_1,cites_2)
xlabel = cites_df$year[seq(1,nrow(cites_df),by=2)]
#make the plot and show linear fit lines
p1 <- ggplot(data = cites_df, aes(year, cites, colour=group, shape=group)) + 
  geom_point(size = I(4)) + 
  geom_smooth(method="lm",aes(group = group), se = F, size=1.5) + 
  scale_x_continuous(name = "Year", breaks = xlabel, labels = xlabel) +     scale_y_continuous("Citations according to Google Scholar") +
  theme_bw(base_size=14) + theme(legend.position="none") + 
  geom_text(aes(x=2012,y=3000,label=sprintf("Average annual \n increase %i", as.integer(inc1))),color="black",size=5.5) +
  geom_text(aes(x=2017,y=5000,label=sprintf("Average annual \n increase %i", as.integer(inc2))),color="black",size=5.5) 

#open a new graphics window
#note that this is Windows specific. Use quartz() for MacOS
ww=5; wh=5; 
windows(width=ww, height=wh)					
print(p1)

