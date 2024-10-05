data=lapply(datasrc,function(dt) pivot_longer(dt,colnames(dt)[!colnames(dt) %in% main_vars],names_to='var',values_to='value',values_drop_na=TRUE) %>% mutate(var=str_to_lower(var)))
data=lapply(data,createType)
data=lapply(data,remove_and_filter)
data=lapply(data,fixfips)

term_mapping <- c(
  'alora' = 'alb',
  'alort' = 'alb',
  'eaor'='acro',
  'srbus' = 'serb',
  'srbgm' = 'serb',
  'eapr' = 'acro',
  'alort' = 'alora',
  'antam' = 'aoca',
  'ocabd' = 'bulg',
  'bulor' = 'bulg',
  'ocbulor' = 'bulg',
  'copor' = 'copt',
  'grora' = 'grk',
  'grorv' = 'grk',
  'hlyor' = 'hoc',
  'macor' = 'moc',
  'ocatd' = 'oca',
  'rmoea' = 'roaa',
  # 'rmoac' = 'roaa',
  'rousa' = 'roc',
  'ukort' = 'uoc',
  'mlkad'='mala',
  'mlksy'='mala',
  'mosc'='mala',
'srocc'='syran',
'soca'='syran',

'orprs'='opc'
)

# Apply the mapping to the dataset
group_and_map=function(dt){
dt %>%
  pivot_wider(names_from = 'type', values_from = 'value') %>%
  mutate(var = case_when(
    var %in% names(term_mapping) ~ term_mapping[var],
    TRUE ~ var
  )) %>% group_by(fips,var,year) %>% summarise(cng=sum(cng),adh=sum(adh))

}

data_mapped=lapply(data,group_and_map)

codes=rbindlist(lapply(datasrc,function(dt) defactor::listVars(dt))) %>% mutate(var=str_to_lower(var),labels=str_to_lower(labels) ) %>% filter(grepl('orthodox',labels)) %>% mutate(var=str_remove_all(var,'cng_.*|adh_.*|rate_.*|cng$|adh$|cg$|ad$|rt$|rate')) %>% group_by(var) %>%setDT()

codes=codes %>% mutate(var = case_when(
    var %in% names(term_mapping) ~ term_mapping[var],
    TRUE ~ var
  )) %>% setDT()

codes=codes[,.SD[1],var] %>% mutate(labels=str_remove_all(labels,'--.*|—number*|^.*_2020|\\(2000\\)'))

fwrite(codes,'data/relig_codebook_fullrelig.csv')

data_mapped=rbindlist(data_mapped) %>% setnafill(.,cols=c('cng','adh'),fill = 0)
res=data_mapped %>% filter(cng+adh>0)  %>% group_by(var,year) %>% summarise(cng=sum(cng)) %>% pivot_wider(values_from =cng,names_from = year)
setnafill(res,fill=0,cols=c(2,3,4))
res=res %>% left_join(codes,by='var') %>% filter(var!='gcnj') %>% filter(!(`2000`==0 | `2010`==0 | `2020`==0)) %>%arrange(-desc(var))

res <- res %>%
  mutate(
    labels = case_when(
      var == 'orth' ~ 'Orthodox Church Total',
      var == 'opc' ~ 'Orthodox Presbyterian Church',
      var == 'acro' ~ 'American Carpatho-Russian Orthodox Diocese',
      var == 'alb' ~ 'Albanian Orthodox Diocese of America',
      var == 'aoca' ~ 'Antiochian Orthodox Christian Archdiocese of North America',
      var == 'bulg' ~ 'Bulgarian Eastern Orthodox Diocese of USA',
      var == 'copt' ~ 'Coptic Orthodox Church',
      var == 'grk' ~ 'Greek Orthodox Archdiocese of America',
      var == 'hoc' ~ 'Holy Orthodox Church in North America',
      var == 'mala' ~ 'Malankara Archdiocese of the Syrian Orthodox Church',
      var == 'moc' ~ 'Macedonian Orthodox Church: American Diocese',
      var == 'oca' ~ 'Orthodox Church in America',
      var == 'roaa' ~ 'Romanian Orthodox Archdiocese in Americas',
      var == 'roc' ~ 'Patriarchal Parishes of the Russian Orthodox Church',
      var == 'roor' ~ 'Russian Orthodox Church Outside of Russia',
      var == 'serb' ~ 'Serbian Orthodox Church in North America',
      var == 'syran' ~ 'Syriac Orthodox Church of Antioch',
      var == 'uoc' ~ 'Ukrainian Orthodox Church of the USA',
      TRUE ~ labels
    ),
    
        ethn = case_when(
      var == 'orth' ~ 'none',
      var == 'uoc' ~ 'ukr',
      var == 'roor' ~ 'rus',
      var == 'roc' ~ 'rus',
      var == 'alb' ~ 'alb',
      var == 'moc' ~ 'mac',
      var == 'bulg' ~ 'bulg',
      var == 'grk' ~ 'greek',
      var == 'aoca' ~ 'ant',
      var == 'acro' ~ 'carp',
      var == 'roaa' ~ 'rom',
      var == 'copt' ~ 'copt',
      var == 'serb' ~ 'serb',
      TRUE ~ 'other'
    ))


res=res %>%select(1,2,6,3,4,5) 
res %>% select(var,ethn,labels) %>% fwrite('orthodox_churches_var_ethn_codebook.csv')
res%>% fwrite('orthodox_churches_ordered_codebook.csv')
res=res %>% mutate(ethn=case_when(ethn %in% c('other','none')~'X',TRUE~ethn))

kable(res,caption = 'Number of congregetions')

res=res %>% mutate(
  int=case_when(
    var %in% c('roor','moc','aoca','grk','acro') ~ 'med',
    ethn %in% c('copt','alb','ukr','serb','rus','rom','bulg')~'high',
    ethn=='X'~'low'
)
) %>% group_by(int) %>% arrange(order(int))
result=rbind(
  res  %>% filter(int=='high'),
  res  %>% filter(int=='med'),
  res  %>% filter(int=='low'))
kable(result,col.names = c('Church','Ethnicity','Church Name','2000','2010','2020','Intense'),
caption='Numer of congregertions in Orthodox churches in the US'
,format = 'latex') %>% cat(file='tables/3_church_by_ethn.tex')

kable(result,col.names = c('Church','Ethnicity','Church Name','2000','2010','2020','Intense'),
caption='Numer of congregertions in Orthodox churches in the US'
,format = 'markdown') %>% cat(sep='\n',file='tables/markdown/3_church_by_ethn.md')
fwrite(result,'tables/csv/3_church_by_ethn.tex.csv')
setDT(result)
res.2=result[,.N,.(var,ethn,int)][,-"N"]
res.2=res.2 %>% pivot_wider(names_from='int',values_from='ethn',values_fill ='-')
clipr::write_clip(kable(res.2))
rbind(result[,.N,int],result[,.(int='Total Churces',N=.N)]) %>% fwrite('tables/csv/3.1_total_churches.csv')
defactor::getLbl(acs,'SPEAKENG')

colnames(acs)