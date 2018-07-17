pacman::p_load(data.table, dplyr, ggplot2, reshape)

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

domestic_arrivals = summary %>% select(year, month, nights_domestic, visitors_domestic)

#if only for 2018
domestic_arrivals_2008 = domestic_arrivals %>% filter(year == 2008)
ggplot(domestic_arrivals_2008, aes(x=month, y=nights_domestic/30)) + geom_bar(stat = "identity")
ggplot(domestic_arrivals_2008, aes(x=month, y=visitors_domestic/30)) + geom_bar(stat = "identity")




