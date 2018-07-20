travelers_sp = travelers_sp %>% 
  mutate(child = if_else(hp_alter < 18,1,0)) %>%
  mutate(adult = if_else(hp_alter >= 18 & hp_alter < 65 ,1,0)) %>%
  mutate(retired = if_else(hp_alter >= 65,1,0))

#assign compositions to parties
party_composition = travelers_sp %>% group_by(unique_id) %>%
  summarize(n_child = sum(child), n_adult=sum(adult), n_retired = sum(retired))

parties_sp = merge(x=parties_sp, y = party_composition, by = "unique_id")

#filter na values and create dummies
cluster_data_parties = parties_sp %>% rowwise() %>% 
  filter(main_mode != "denied", main_mode != "unaware", purpose != "denied", purpose !="unaware") %>%
  filter(p1014 < 95, hheink < 15) %>%
  mutate(is_car = if_else(main_mode == "car",1,0)) %>% 
  mutate(is_train =  if_else(main_mode == "train",1,0)) %>% 
  mutate(is_coach =  if_else(main_mode == "coach",1,0)) %>% 
  mutate(is_airplane =  if_else(main_mode == "air",1,0)) %>% 
  mutate(is_bicycle =  if_else(main_mode == "bicycle",1,0)) %>% 
  mutate(is_ship = if_else(main_mode == "ship",1,0)) %>% 
  mutate(is_leisure = if_else(purpose == "leisure",1,0)) %>% 
  mutate(is_visit = if_else(purpose == "visit",1,0)) %>%
  mutate(is_other_private = if_else(purpose == "other_private",1,0)) %>%
  mutate(is_business = if_else(purpose == "business",1,0)) %>% 
  mutate(is_commuter = if_else(purpose == "weekend_commuter",1,0)) %>% 
  mutate(is_other = if_else(purpose == "other",1,0)) %>%
  mutate(number_of_nights = p1014)
  







plot_data = parties_sp %>% rowwise() %>% 
  filter(main_mode != "denied", main_mode != "unaware", purpose != "denied", purpose !="unaware") %>%
  filter(p1014 < 95, hheink < 15)

plot_data$cluster = fit$cluster

#analyze purpose by month
ggplot(plot_data,
       aes(x=as.factor(month), fill = as.factor(cluster))) +
  geom_bar() +
  xlab("Month") + ylab("Share of trips by cluster") +
  theme_light() + 
  theme(text=element_text(size=16, family="Times New Roman")) + 
  labs(fill = "Purpose") + 
  theme(legend.position = "bottom")
