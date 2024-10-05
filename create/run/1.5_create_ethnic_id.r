rel=acs[isImmigrant==1|isImmigrant_FA==1|isImmigrant_MA==1,.(fips,SERIAL,RELATE,ethn,gender,BPLD,BPLD_MOM,BPLD_POP)] %>%  mutate( 
    # Match Etnicity
    ethn=case_when(
    BPLD == 46500 ~ 'rus',
    BPLD == 43000 ~ 'alb',
    BPLD == 45700 ~ 'serb',
    BPLD == 45730 ~ 'serb',
    BPLD == 43330 ~ 'mac',
    BPLD == 45100 ~ 'bulg',
    BPLD == 46530 ~ 'ukr',
    BPLD == 43300 ~ 'greek',
    BPLD == 45600 ~ 'rom',
    BPLD %in% c(1:10000) ~ 'usa',
    TRUE ~ 'other'
    ))
rel=rel %>% filter(ethn!='other') %>%
  mutate(
    ethn=case_when(
    ethn=='usa' & BPLD_POP == 46500 ~ 'rus',
    ethn=='usa' & BPLD_POP == 43000 ~ 'alb',
    ethn=='usa' & BPLD_POP == 45700 ~ 'serb',
    ethn=='usa' & BPLD_POP == 45730 ~ 'serb',
    ethn=='usa' & BPLD_POP == 43330 ~ 'mac',
    ethn=='usa' & BPLD_POP == 45100 ~ 'bulg',
    ethn=='usa' & BPLD_POP == 46530 ~ 'ukr',
    ethn=='usa' & BPLD_POP == 43300 ~ 'greek',
    ethn=='usa' & BPLD_POP == 45600 ~ 'rom',
    TRUE ~ ethn
    )
  )

rel=rel %>% filter(ethn!='other') %>%
  mutate(
    ethn=case_when(
    ethn=='usa' & BPLD_MOM == 46500 ~ 'rus',
    ethn=='usa' & BPLD_MOM == 43000 ~ 'alb',
    ethn=='usa' & BPLD_MOM == 45700 ~ 'serb',
    ethn=='usa' & BPLD_MOM == 45730 ~ 'serb',
    ethn=='usa' & BPLD_MOM == 43330 ~ 'mac',
    ethn=='usa' & BPLD_MOM == 45100 ~ 'bulg',
    ethn=='usa' & BPLD_MOM == 46530 ~ 'ukr',
    ethn=='usa' & BPLD_MOM == 43300 ~ 'greek',
    ethn=='usa' & BPLD_MOM == 45600 ~ 'rom',
    TRUE ~ ethn
    )
  )
rel=rel %>% filter(!ethn %in% c('other','usa')) %>% select(SERIAL,ethn) %>% group_by(SERIAL,ethn) %>% summarise(n()) %>% select(SERIAL,ethn)

%>% mutate(realation=case_when(
  RELATE==1 ~ 'head',
  RELATE==2 ~ 'spouse',
  RELATE==5&gender==1 ~ 'father',
  RELATE==5&gender==0 ~ 'mother',
)) %>% pivot_wider(names_from=realation,values_from='ethn') %>% data.table() %>% arrange(order(SERIAL))

