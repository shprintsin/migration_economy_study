mean = function(x) base::mean(x,na.rm=TRUE)
#### Relig Function

reader=function(x){
    year=str_extract(x,'[0-9]{4}')
    dt=read_dta(x) %>% mutate(year=year)
    colnames(dt)[grep('fip',colnames(dt))]='fips'
    print(paste('Load',x))
    return(dt)
}
main_vars <- c('year','YEAR','fips','totcg','stateab','POP2000','statnam','counam','stabbrev','stabbr','county','ctycod','stcod','stname','state','fip','POP2020','POP2010','contry','cntyname','POP1990','POP1980','name')
clean_and_pivot = function(dt) pivot_longer(dt,colnames(dt)[!colnames(dt) %in% main_vars],names_to='var',values_to='value',values_drop_na=TRUE) %>% mutate(var=str_to_lower(var))
createType=function(x) x %>% mutate(var=str_remove_all(var,'_2020'),
type = case_when(
        str_ends(var, 'cg$|cng$|_c$') ~ 'cng',
        str_ends(var, 'ad$|adh$|_a$') ~ 'adh',
        str_ends(var, 'rate$|rt$') ~ 'rate',
        TRUE ~ NA_character_  # Add a default case
        ))

remove_and_filter = function(dt){
  dt%>% filter(type!='rate') %>% mutate(
            var=stringr::str_remove_all(var,'cng$|adh$|cg$|ad$|_.*')
            )%>%filter(var %in% array(code_list$var)
            ) %>%select(fips,var,value,type,year) 
}

createLong= function(dt){dt%>%left_join(code_list,by=c('var','year'))  %>% 
            filter(group!='NONE')  %>% group_by(group,type,fips,year) %>% summarise(sum=sum(value)) %>% ungroup() %>% spread(type,sum) %>% mutate(church=1) %>% 
            pivot_wider(names_from=group,values_from=c('cng','adh','church'),values_fill = 0)
            }

attach_year=function(dt) {attach_year=paste0(colnames(dt)[!colnames(dt) %in% main_vars],'_',max(dt$year))
            colnames(dt)=c('fips','year',attach_year)
            
return(dt %>% select(-year))
}
fixfips=function(dt){
dt%>%mutate(fips=sprintf("%05d",as.numeric(fips)))
}

group_and_map=function(dt){
dt %>%
  pivot_wider(names_from = 'type', values_from = 'value') %>%
  mutate(var = case_when(
    var %in% names(term_mapping) ~ term_mapping[var],
    TRUE ~ var
  )) %>% group_by(fips,var,year) %>% summarise(cng=sum(cng),adh=sum(adh))

}