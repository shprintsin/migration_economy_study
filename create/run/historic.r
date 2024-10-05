library(stringr)

codes=lapply(c(1906,1916,1926,1936),function(x){
    path=paste0('data/historic/relig_bodies_',x,'.DTA')
    data=read_dta(path)%>% setDT()
    labels=data.table(year=x,defactor::listVars(data))
}) %>% rbindlist() %>% mutate(labels=str_to_lower(labels),var = str_to_lower(var)) %>% filter( grepl('ortho',labels)) %>% filter(!var %in% c('frienort','frndorth','syrorth','frorth') ) %>%
select(var) %>%unique()


lapply(c(1906,1916,1926,1936),function(x){
    path=paste0('data/historic/relig_bodies_',x,'.DTA')
    data=read_dta(path)%>% setDT()
    labels=data.table(year=x,defactor::listVars(data))
}) %>% rbindlist() %>% mutate(labels=str_to_lower(labels),var = str_to_lower(var)) %>% filter( grepl('ortho',labels)) %>% filter(!var %in% c('frienort','frndorth','syrorth','frorth') ) 

dfs=lapply(c(1906,1916,1926,1936),function(x){
    path=paste0('data/historic/relig_bodies_',x,'.DTA')
    data=read_dta(path)%>% setDT()
    data=data.table(year=x,data)
    data=data%>% pivot_longer(colnames(.)[-c(1:5)]) %>% filter(name %in% codes$var,!name %in% c('frienort','frndorth','syrorth','frorth'),value!=0)
}) %>% rbindlist() %>% mutate(
    name=case_when(
    name %in% c('grkruss','russorth','erusorth','russorth')~'rus',
    name %in% c('greekort','grkeorth','egrkorth','grkorthh')~'greek' ,
    name %in% c('serborth','servorth')~'serb',
    name %in% c('eastorth','eastorth')~'eastern',
    TRUE~ name
    )
) %>% group_by(fipst,fipcnt,name,year) %>% summarise(value=mean(value)) %>% ungroup() %>% filter(!is.na(fipcnt)) %>%
mutate(fips=paste0(sprintf('%02d',as.numeric(fipcnt)),sprintf('%03d',as.numeric(fipcnt))))  %>% setDT()


dfs %>% group_by(year,fips) %>% summarise(max(value))
dfs %>% group_by(name) %>% summarise(n=n()) %>% arrange(desc(n))
dfs %>% pivot_wider(names_from = name,values_from = value)

church_exist_1900=dfs %>% mutate(value = as.numeric(value>0)) %>% filter(value==1) %>% group_by(fips) %>% summarise(parish=as.numeric(n()>0)) 
russian_churches=data %>% filter(year==2020,cng>0) %>% select(fips,cng,ethn) %>% filter(duplicated(fips))  %>%filter(ethn=='rus') 

ad19=dfs %>% filter(name=='rus',year==1926) %>% select(fips,value) %>% mutate(ad19=value)
rc=counties %>% filter(year==2020) %>% select(fips) %>% left_join(russian_churches) %>% left_join(church_exist_1900) %>% mutate(r19=parish,r20=cng) %>% left_join(ad19)
setnafill(rc,cols=c('r19','r20','ad19'),fill=0)

summary(lm(r20~ad19,data=rc))