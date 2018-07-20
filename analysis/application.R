#input travelers_sp and parties_sp

#find typical travel parties by purpose
parties_sp = parties_sp %>% 
  mutate(party_type = paste(n_child, n_adult, n_retired, sep = "/"))

party_types = parties_sp %>%
  group_by(purpose, party_type) %>%
  summarize(trips = n()) %>% 
  tidyr::spread(purpose, trips)


party_types[is.na(party_types)] = 0

party_types = party_types %>% 
  rowwise() %>%
  mutate(all = sum(leisure, visit, business, weekend_commuter, other, other_private, denied,unaware ))

party_types = party_types[order( -party_types$all),]


party_types_frequent = party_types %>% top_n(10)


party_types_frequent = melt(as.data.frame(party_types_frequent), id.vars = c("party_type","all"))

party_types_frequent = party_types_frequent %>% filter(variable != "denied", variable != "unaware")

##rename parties
parties = unique(party_types_frequent$party_type)
###reorder from small to large parties and relabel
parties = c("0/1/0", "0/0/1", "1/0/0", "0/2/0", "0/0/2", "1/1/0", "0/1/1",  "1/2/0", "0/3/0", "2/2/0")
party_labels = c("1 A","1 C", "1 R" , "2 A", "2 R",   "1 C + 1 A",  "1 A + 1 R",
                 "1 C + 2 A", "3 A", "2 C + 2 A")

party_types_frequent$party_type = factor(x = party_types_frequent$party_type,
                                         levels = parties, labels = party_labels)

party_types_frequent$variable = factor(x= party_types_frequent$variable,
                                       levels = levels(party_types_frequent$variable),
                                       labels = c("Leisure", "Visit", 
                                                  "Other Private", "Business",
                                                  "Commuter", "Other", "Denied", "Unaware"))

ggplot(party_types_frequent, aes(x= variable, y = value, fill = party_type)) +
  geom_bar(position = "fill", stat = "identity") + 
  xlab("Trip purpose") + ylab("Share of trips by party type") + 
  labs(fill = "Travel party type") + 
  theme_light() +
  scale_fill_manual(values = c("#fe8181", "#fe2e2e", "#b62020", "#a3c1ad", "#5f9ea0", "#317873",
                               "#2b4840", "#77aaff", "#3366ff", "#8755ba")) + 
  theme(text=element_text(size=14, family="Times New Roman")) + 
  theme(legend.position = "bottom", legend.title = element_text(size = 12))

ggplot(party_types_frequent, aes(x= variable, y = value, fill = as.factor(party_type))) +
  geom_bar(stat = "identity") + 
  xlab("Trip purpose") + ylab("Number of trips by party type") +
  labs(fill = "Travel party type") + 
  scale_fill_manual(values = c("#fe8181", "#fe2e2e", "#b62020", "#a3c1ad", "#5f9ea0", "#317873",
                               "#2b4840", "#77aaff", "#3366ff", "#8755ba")) + 
  theme_light() + 
  theme(text=element_text(size=14, family="Times New Roman")) + 
  theme(legend.position = "bottom", legend.title = element_text(size = 12))

#share of trip parties over the total
sum(party_types_frequent$all) / 6 / sum(party_types$all)

income_distributions = parties_sp %>%
  group_by(purpose, oek_stat) %>%
  summarize(trips = n())

income_distributions = income_distributions %>% filter(oek_stat < 90, 
                                                       purpose != "denied", purpose != "unaware")

income_distributions$oek_stat = factor(income_distributions$oek_stat, 
                                       levels = 5:1, 
                                       labels = c("Very high", "High", "Medium", "Low", "Very low"))
                                              
income_distributions$purpose = factor(x= income_distributions$purpose,
                                       levels = levels(income_distributions$purpose),
                                       labels = c("Leisure", "Visit", 
                                                  "Other Private", "Business",
                                                  "Commuter", "Other", "Denied", "Unaware"))
#calculate the distributions of income groups in the overall population

oek_stat_composition = households %>% group_by(oek_stat) %>% summarize(cases = sum(hh_gew))
oek_stat_composition$share = oek_stat_composition$cases / sum(oek_stat_composition$cases)

#this dataset (oek_stat_composition) was processed in excel to paint the horizontal lines with geom_hline
       
ggplot(income_distributions, aes(x= purpose, y = trips, fill = oek_stat)) +
  geom_bar(position = "fill", stat = "identity") + 
  xlab("Trip purpose") + ylab("Share of trips by economic status") + 
  labs(fill = "Economic status") + 
  theme_light() + 
  scale_fill_brewer(palette =  "Blues", direction = -1) +
  theme(text=element_text(size=14, family="Times New Roman")) + 
  theme(legend.position = "bottom", legend.title = element_text(size = 12)) + 
  geom_hline(yintercept = 0.09605383, linetype = "dashed") + 
  geom_hline(yintercept = 0.389306985, linetype = "dashed") +
  geom_hline(yintercept = 0.797843692, linetype = "dashed") +  
  geom_hline(yintercept = 0.917647136, linetype = "dashed")   

ggplot(income_distributions, aes(x= purpose, y = trips, fill = oek_stat)) +
  geom_bar( stat = "identity") + 
  xlab("Trip purpose") + ylab("Number of trips by economic status") + 
  labs(fill = "Economic status") + 
  theme_light() + 
  scale_fill_brewer(palette =  "Blues", direction = -1) +
  theme(text=element_text(size=14, family="Times New Roman")) + 
  theme(legend.position = "bottom", legend.title = element_text(size = 12))



