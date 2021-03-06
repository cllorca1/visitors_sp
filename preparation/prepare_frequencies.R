


dataset = data_domestic %>% select(index, nights, purpose, main_mode, hh_party, real_nights = p1014)

dataset = dataset %>% rowwise() %>% 
  filter(!is.na(hh_party),!is.na(real_nights), nights < 96) %>%
  mutate(visitors_leisure = if_else(purpose == "leisure", hh_party, 0)) %>% 
  mutate(visitors_visit = if_else(purpose == "visit", hh_party, 0)) %>% 
  mutate(visitors_other_private = if_else(purpose == "other_private", hh_party, 0)) %>% 
  mutate(visitors_business = if_else(purpose == "business", hh_party, 0)) %>% 
  mutate(visitors_weekend_commuter = if_else(purpose == "weekend_commuter", hh_party, 0)) %>% 
  mutate(visitors_other = if_else(purpose == "other", hh_party, 0)) %>% 
  mutate(visitors_denied = if_else(purpose == "denied", hh_party, 0)) %>% 
  mutate(visitors_unaware = if_else(purpose == "unaware", hh_party, 0)) %>% 
  mutate(visitors_nights = real_nights * hh_party) %>% 
  mutate(visitors_car = if_else(main_mode == "car", hh_party, 0)) %>% 
  mutate(visitors_train = if_else(main_mode == "train", hh_party, 0)) %>% 
  mutate(visitors_coach = if_else(main_mode == "coach", hh_party, 0)) %>% 
  mutate(visitors_air = if_else(main_mode == "air", hh_party, 0)) %>% 
  mutate(visitors_bicycle = if_else(main_mode == "bicycle", hh_party, 0)) %>% 
  mutate(visitors_ship = if_else(main_mode == "ship", hh_party, 0)) %>% 
  mutate(visitors_other_mode = if_else(main_mode == "other", hh_party, 0)) %>%
  mutate(visitors_unaware_mode = if_else(main_mode == "unaware", hh_party, 0)) %>%
  mutate(hh_party1 = if_else(hh_party == 1, hh_party, 0)) %>%
  mutate(hh_party2 = if_else(hh_party == 2, hh_party, 0)) %>%
  mutate(hh_party3 = if_else(hh_party == 3, hh_party, 0)) %>%
  mutate(hh_party4 = if_else(hh_party == 4, hh_party, 0)) %>%
  mutate('hh_party5+' = if_else(hh_party  > 4 ,hh_party, 0)) %>%
  mutate(nights1 = if_else(nights == 1, hh_party, 0)) %>%
  mutate(nights2 = if_else(nights == 2, hh_party, 0)) %>%
  mutate(nights3 = if_else(nights == 3, hh_party, 0)) %>%
  mutate(nights4 = if_else(nights == 4, hh_party, 0)) %>%
  mutate('nights5+' = if_else(nights  > 4, hh_party, 0))

summary(dataset)



write.csv(dataset %>% select(ID = index,7:33), "C:/projects/visitors/input_sp/frequencyMatrix.csv", row.names = F)
