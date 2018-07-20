pacman::p_load(data.table, dplyr, ggplot2, reshape, extrafont)

folder = "C:/projects/visitors/muenchen/"
file = "monatszahlenmonatszahlen1803tourismus.csv"
path = paste(folder, file, sep = "")

visitors_by_month = read.csv(path,encoding = "UTF-8" )




summary = visitors_by_month %>% group_by(MONAT, ZAHL, AUSPRAEGUNG) %>% summarize(value = sum(WERT))

summary = cast(data = summary, formula =  MONAT ~ ...)


summary = summary %>% mutate(year = substring(as.character(MONAT), first = 1, last = 4))
summary = summary %>% mutate(month = substring(as.character(MONAT), first = 5, last = 6))

summary$nights_domestic = summary$Übernachtungen_Inland
summary$visitors_domestic = summary$Gäste_Inland

domestic_arrivals = summary %>% dplyr::select(year, month, nights_domestic, visitors_domestic)

#if only for 2018
domestic_arrivals_2008 = domestic_arrivals %>% filter(year == 2008)

ggplot(domestic_arrivals_2008, aes(x=month, y=visitors_domestic/30)) + geom_bar(stat = "identity")

ggplot(domestic_arrivals_2008, aes(x=as.factor(as.numeric(month)), y=visitors_domestic/30)) +
  geom_bar(stat = "identity", fill = "#7e7e7e") + 
  xlab("Month") + ylab("Average dayly visitors") +
  theme_light() + 
  theme(text=element_text(size=16, family="Times New Roman")) + 
  theme(legend.position = "bottom")

#export metafile as 725x600
