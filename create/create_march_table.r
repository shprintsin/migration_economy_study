library(kableExtra)
table_m=df[,.N,.(ethn,year)] %>% pivot_wider(names_from = ethn, values_from = N)
df[,.N,.(ethn,year,fips)][,.(N),.(ethn,year)]
line=dt%>% select(ethn,year,county) %>%group_by(ethn,year,county) %>% summarise(n=n()) %>% group_by(ethn,year,county) %>% summarise(n=n()) %>% pivot_wider(names_from = year, values_from = n) %>% filter(`2010`+`2000`+`2020`==3) %>% group_by(ethn) %>% summarise(n())
# Create the table
table_m <- table_m %>%select(-other)
line=data.table(year='counties avalliable For 3 Periods',t(line))[2]
colnames(line) <- colnames(table_m)
table_m=rbind(table_m,line)
tab.1=kable(table_m, format = "latex", booktabs = T, caption = "Cencus matched with ethnic orthodox church")
# Save the LaTeX code to a file
cat(tab.1, file = "tables/tab.1.tex")

