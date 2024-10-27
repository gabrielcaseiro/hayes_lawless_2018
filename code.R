library(tidyverse)
library(fixest)
library(haven)

# 1. Media coverage data ----

dt1<-read_dta('2010_news_data_JOP_dataverse.dta')
  dt1$year<-2010

dt2<-read_dta('2014_news_data_JOP_dataverse.dta')
  dt2$year<-2014

colnames(dt1)<-str_remove(colnames(dt1),'10')
colnames(dt2)<-str_remove(colnames(dt2),'14')

dt<-bind_rows(dt1,dt2)

rm(dt1)
rm(dt2)

# 2. CCES data ----

cces<-read_dta('CCES_Panel_Full3waves_VV_V4.dta')

cols<-c('caseid',
        'weight',
        'cdid112_10','countyfips_10','cdid114_14','countyfips_14', #Districts IDS
        'CC10_315a','CC14_315a', # Incumbent evaluation
        'CC10_390','CC14_390','CC14_390b' # Vote intention
        )

cces<-cces %>% 
  select(any_of(cols)) %>% 
  mutate(`CC14_390`=ifelse(is.na(`CC14_390`),`CC14_390b`,`CC14_390`), # Adjust variable
         `cdid114_14`=str_pad(`cdid114_14`,2,'left',0)) %>% 
  select(-`CC14_390b`)

# Consolidate panel:

cces<-cces %>% 
  mutate(across(3:10,as.character)) %>% 
  pivot_longer(3:10)

cces<-cces %>% 
  mutate(year=ifelse(grepl('14',name),2014,2010),
         name=gsub(paste(c('CC10_','CC14_','114_14','112_10','_14','_10'),collapse = '|'),'',name))
  
cces<-cces %>% 
  pivot_wider(names_from = name,values_from = value)

cces<-cces %>% 
  mutate(ccesdistrict=paste0(str_sub(countyfips,1,2),cdid)) %>% # District id
  mutate(across(4:8,as.numeric))

# Merge with media coverage data:

cces<-cces %>% 
  left_join(dt) %>% 
  filter(!is.na(newspaper))

# CCES variables coding:

cces<-cces %>% 
  mutate(vote_intention=1*(as.numeric(`390`)<=7),
         rate_incumbent=1*(as.numeric(`315a`)<5),
         storycount=scale(storycount)) %>% 
  group_by(caseid) %>% 
  mutate(nomove=1*(length(unique(ccesdistrict))==1)) %>% 
  ungroup()

# First-Diffrence dataset:

cces_fd<-cces %>% 
  arrange(year) %>% 
  group_by(caseid) %>% 
  mutate(vote_intention=vote_intention-lag(vote_intention),
         rate_incumbent=rate_incumbent-lag(rate_incumbent),
         storycount=storycount-lag(storycount)) %>% 
  ungroup() %>% 
  filter(year==2014)


# 3. Analysis ----

reg1fd<-feols(rate_incumbent ~ storycount  ,
            cluster='ccesdistrict',
            data = cces_fd )

summary(reg1fd)


reg1<-feols(rate_incumbent ~ storycount | caseid + year ,
           cluster='ccesdistrict',
           data = cces )

summary(reg1)

reg2fd<-feols(vote_intention ~ storycount  ,
              cluster='ccesdistrict',
              data = cces_fd )

summary(reg2fd)

reg2<-feols(vote_intention ~ storycount | caseid + year ,
            cluster='ccesdistrict',
            data = cces)

summary(reg2)

# Main table:

etable(reg1fd,reg1,reg2fd,reg2,drop = 'Constant', tex = TRUE)


