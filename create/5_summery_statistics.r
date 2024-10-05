dt=dt %>% group_by(household) %>% mutate(min_mig=min(imm_year),max_mig=max(imm_year))
dt=dt %>%filter(max_mig<2003,min_mig>1980,age>18,age<64)

stargazer::stargazer(dt,out="tables/summery_statistics_df.tex")
stargazer::stargazer(dt,type="text",out='tables/markdown/summery_statistics_df.md')

table_m=dt[,sum(cng),.(ethn,year)][!is.na(ethn)] %>% pivot_wider(names_from='ethn',values_from="V1")

line=dt%>% select(ethn,year,county) %>%group_by(ethn,year,county) %>% summarise(n=n()) %>% group_by(ethn,year,county) %>% summarise(n=n()) %>% pivot_wider(names_from = year, values_from = n) %>% filter(`2010`+`2000`+`2020`==3) %>% group_by(ethn) %>% summarise(n())
# Create the table
table_m <- table_m %>%select(-other)
line=data.table(year='counties avalliable For 3 Periods',t(line))[2]
colnames(line) <- colnames(table_m)
table_m=rbind(table_m,line)
tab.1=kable(table_m, format = "latex", booktabs = T, caption = "Cencus matched with ethnic orthodox church")
# Save the LaTeX code to a file
cat(tab.1, file = "tables/tab.5.1.tex")

kable(table_m, format = "markdown", booktabs = T, caption = "Cencus matched with ethnic orthodox church") %>% cat(.,'tables/markdown/tab.5.1.md',sep = '\n')