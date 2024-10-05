library(defactor)
data=read_dta('data/etholing.DTA') %>% setDT()
data=data.table(data)
defactor::getLbl(data,'ethnicid')
listVars(data)

data=data %>% left_join(getLbl(data,'I_RELIGION'),by='I_RELIGION') %>% left_join(getLbl(data,'ethnicid'),by='ethnicid') %>% left_join(getLbl(data,'orthchur'),by='orthchur') %>% setDT()
engc=data[,.(engc=1-mean(liturgy,na.rm = TRUE)/100,.N),.(church)][N>20]%>%filter(church %in% c('gre','ser','rus','ukr','rom')) %>%mutate(church=c('greek','serb','rus','ukr','rom')) %>%mutate(ethn=church) %>%select(ethn,engc)
dt %>% left_join(engc,ethn) %>% fwrite('english_by_ethnicity.csv')
data=data %>% left_join(data.table(
    orthchur = c(1:12),
    church=c('alb','car','ant','bul','gre','orth','geo','rom','rus','rus','ser','ukr'))
    ,by='orthchur') %>% select(
        church,name=orthchur_labels,relig=I_RELIGION_labels,ethnicity=ethnicid_labels,liturgy,ethnicityid=ethnicid,sermon,choir
    ) 

juridiction= %>%group_by(church,relig)%>% summarise(N=n())%>% pivot_wider(names_from=relig,values_from=N) 

kable(juridiction, format = "latex", booktabs = T, caption = "Orthodox church by Juridiction") %>% cat(file = "tables/tab.2_juridiction.tex")
data=data %>% left_join(data.table(
    orthchur = c(1:12),
    church=c('alb','car','ant','bul','gre','orth','geo','rom','rus','rus','ser','ukr'))
    ,by='orthchur') %>% select(
        church,name=orthchur_labels,relig=I_RELIGION_labels,ethnicity=ethnicid_labels,liturgy,ethnicityid=ethnicid,sermon,choir
    ) 


data %>% select(ethnicity,church) %>%table()
data %>% select(church,ethnicity)  %>% group_by(ethnicity) %>% summarise(N=n()) %>% table()
data[,.N,.(church,ethnicity)] %>% pivot_wider(names_from = church, values_from = N) %>%   summarise(across(colnames(.)[-1], ~ 100*(.) / sum(.,na.rm = TRUE), .names = "{col}"))

# This study was conducted under auspices of the Assembly of Canonical Orthodox Bishops of the United States--a religious organization that unites the bishops representing the following Orthodox Church jurisdictions (denominations): Albanian Orthodox Diocese of America; American Carpatho-Russian Orthodox Diocese; Antiochian Orthodox Christian Archdiocese of North America; Bulgarian Eastern Orthodox Diocese of the USA and Canada; Greek Orthodox Archdiocese of America; Orthodox Church in America; Parishes of the Georgian Orthodox Church; Romanian Orthodox Archdiocese in the Americas; Russian Orthodox Church Outside of Russia; Russian Orthodox Church in the USA; Serbian Orthodox Church in North America; Ukrainian Orthodox Church of the USA.
#  The goal of this study and survey was to assess the usage of various languages and the strength of the ethnic culture in U.S. Orthodox parishes. This survey includes parishes from different parts of the United States and from various Orthodox jurisdictions.
# 3) LITURGY Please, estimate the percentage of the English language used in your parish on a typical Sunday as the language of the Divine Liturgy (from 0% - 'no English used' to 100% - 'exclusively English used'). Approximately ___%
# 4) SERMON Please, estimate the percentage of the English language used in your parish on a typical Sunday as the language of sermon(s) (from 0% - 'no English used' to 100% - 'exclusively English used'). Approximately ___%
# 5) CHOIR Please, estimate the percentage of the English language used in your parish on a typical Sunday as the language in which the church choir or cantors sing (from 0% - 'no English used' to 100% - 'exclusively English used'). Approximately ___%
# 6) ETHNICID Do you agree or disagree with the statement: 'Our parish has a strong ethnic culture and identity that we are trying to preserve'? (Please, select one answer.)
# 7) I-RELIGION Orthodox Church to which the parish belongs (Recoded for use with online analysis)

# ethnicid   ethnicid_labels
# 1        1    Strongly agree
# 2        2      Rather agree
# 3        3    Neutral/Unsure
# 4        4   Rather disagree
# 5        5 Strongly disagree


# Krindatch, A. (2020, April 25). The Ethno-Linguistic Situation in the Parishes of U.S. Orthodox Christian Churches.
# Alexei Krindatch, Research Coordinator (akrindatch@aol.com)
# Assembly of Canonical Orthodox Bishops of North and Central America


# The question to what extent the various American Orthodox Churches can still be seen as “ethnically based”
# religious communities remains open. Further, this subject continues to be hotly debated by Orthodox church
# leadership and by the “rank and file” clergy and laity. That is for good reason. Indeed, the inquiry in this
# question leads to many issues which have significant implications for church life such as the usage of English
# versus “ethnic” languages in church, the presence and role of converts, the openness of Orthodox parishes to
# the ethnically and culturally “others,” the preference of younger generation of faithful for either keeping their
# ethnic heritage and identity or for “blending” with mainstream America – the list of these “big” questions is
# endless.  

# The membership of the present‐day Orthodox Christian Churches in the United States consists of four very
# distinct demographic groups:
#  US‐born descendants (second, third, fourth, fifth generations) of the original Greek, Slavic or Arab
# immigrants;
#  Newly arrived immigrants who emigrated to United States from Eastern Europe or Middle East in
# recent decades;
#  American converts to Orthodox Christianity – the former Protestants or Roman Catholics;
#  The children of American converts: the persons who were born and raised in the Orthodox Church, but
# have no Orthodox “ethnic” heritage themselves.  
# The presence of these four groups varies significantly from jurisdiction to jurisdiction and – within each
# jurisdiction ‐ from parish to parish. As a result, there exists great diversity among local Orthodox communities
# in terms of how strong various ethnic elements in their religious and social lives are expressed.    
# This report provides several insights into the subject of strength of ethnic identity and ethnic culture in US
# Orthodox Christian Churches at the beginning of the third millennium. Our goal was to examine this issue
# both jurisdiction by jurisdiction (i.e. comparing various national Orthodox Church bodies) and state by state
# (i.e. comparing various geographic areas on US territory).


english_Use=data %>% reframe(agree = as.numeric(ethnicityid<=3),church,ethnicity) %>% group_by(church) %>% summarise(StrongEthnic=round(100*mean(agree),2)) %>% 
left_join(data[,.(noEnglish=100-round(mean(liturgy,na.rm = TRUE),2)),.(church)]) %>%
left_join(data[,.(choir=round(mean(sermon,na.rm = TRUE),2)),.(church)]) %>%
left_join(data[,.(sermon=round(mean(choir,na.rm = TRUE),2)),.(church)]) %>%
left_join(data %>% group_by(church) %>% summarise(N=n())) %>%
filter(N>20) %>% arrange(-noEnglish)

kable(english_Use, format = "latex", booktabs = T, caption = "The Ethno-Linguistic Situation in the Parishes of U.S. Orthodox Christian Churches") %>% cat(file = "tables/tab.2.1_ethnic_background.tex")
english_Use %>% select(church,noEnglish,StrongEthnic)