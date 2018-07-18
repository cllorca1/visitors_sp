library(extrafont)

#trips by main destination
data$destination = factor(x=data$p1012, levels = c(1,2,3,7,8), 
                          labels = c("domestic", "european", "out_europe", "denied", "unaware"))

data %>% group_by(destination) %>% summarize(sum(r_gew))

data_domestic = data %>% filter(destination == "domestic")


#expanded --> not expandable, weights are scaled to average 1
sum(data_domestic$r_gew)

#trips by purpose
data_domestic$purpose = factor(x = data_domestic$p101, 
                      levels = c(1,2,3,4,5,6,97,98), 
                      labels = c("leisure", "visit", "other_private", "business", "weekend_commuter", "other", "denied", "unaware"))
data_domestic %>% group_by(purpose) %>% summarize(sum(r_gew))

#analyze purpose by month
ggplot(data_domestic %>% filter(purpose!="unaware", purpose !="denied"),
       aes(x=as.factor(stich_m), fill = factor(purpose, labels = c("Leisure", "Visit", "Other Private", "Business", "(Weekend) Commuter", "Other")))) +
  geom_bar(position = "fill") +
  xlab("Month") + ylab("Share of trips by purpose") +
  theme_light() + 
  theme(text=element_text(size=16, family="Times New Roman")) + 
  labs(fill = "Purpose") + 
  theme(legend.position = "bottom")

  

trips_purpose_month = data_domestic %>% group_by(purpose, stich_m) %>% summarize(sum(r_gew))
write.csv(trips_purpose_month, "c:/projects/visitors/analysis/mid_purpose_month.csv", row.names = F)

#trips_by_mode
data_domestic$main_mode = factor(x=data_domestic$hvm_r, levels = c(1,2,3,4,5,6,7,8,9), 
                        labels = c("car", "train", "coach", "air", "bicycle", "ship", "other", "denied", "unaware"))
data_domestic %>% group_by(main_mode) %>% summarise(sum(r_gew))

#analyze mode by month
ggplot(data_domestic, aes(x=as.factor(stich_m), fill = main_mode)) + geom_bar(position = "fill")

#number of nights
data_domestic = data_domestic %>% rowwise() %>%
  mutate(nights = if_else(p1014 >= 5  && p1014 < 96, 11L, p1014))
  
data_domestic %>% group_by(nights) %>% 
  summarize(sum(r_gew))

#analyze nights by month
ggplot(data_domestic, aes(x=as.factor(stich_m), fill = as.factor(nights))) + geom_bar(position = "fill")

trips_nights_month = data_domestic %>% group_by(nights, stich_m) %>% summarize(sum(r_gew))
write.csv(trips_nights_month, "c:/projects/visitors/analysis/mid_month_nights.csv", row.names = F)


#party size whithin hh

data_domestic = data_domestic %>% rowwise() %>% 
  mutate(hh_party = if_else(p1015 < 90, p1015 + 1, if_else(p1015 > 100, 1, NULL))) 

data_domestic %>% group_by(hh_party) %>% summarize(sum(r_gew))

#analyze party by month
ggplot(data_domestic, aes(x=as.factor(stich_m), fill = as.factor(hh_party))) + geom_bar(position = "fill")


#distance distribution
ggplot(data, aes(x=p1016, weight = r_gew, color = destination)) + stat_ecdf() + xlim(0,10000)

#number of overnihts trips in the last 3 months
data %>% group_by(p10) %>% summarize(sum(r_gew))
