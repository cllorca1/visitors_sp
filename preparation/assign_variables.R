parties_sp = merge(x= micro_data, y = data_domestic, by.x = "id", by.y = "index")

#assign an id for the trip party microdata
parties_sp = parties_sp %>% 
  rowwise() %>% 
  mutate(trip_party_microdata = hhid * 10000 + pid * 100 + rid) 

#number of microdata records used for the sp generation
length(unique(parties_sp$trip_party_microdata))

#assign unique id
parties_sp$unique_id = seq.int(nrow(parties_sp))

write.csv(parties_sp, "C:/projects/visitors/result/trip_parties.csv", row.names = F)

#parties_sp = read.csv("C:/projects/visitors/result/trip_parties.csv")

#assign persons in the household travelling

travelers_sp = data.frame()
missing_persons = 0
count_persons = 0
for (id in 1:nrow(parties_sp)){
  this_household_id = parties_sp$hhid[id]
  this_person_id = parties_sp$pid[id]
  #household of the traveler
  this_household = subset(persons, hhid  == this_household_id)
  #main traveler
  this_person = subset(this_household, pid == this_person_id)
  this_person$unique_id = id
  this_person$month = parties_sp$month[id]
  this_person$nights = parties_sp$nights[id]
  this_person$purpose = parties_sp$purpose[id]
  this_person$main_mode = parties_sp$main_mode[id]
  count_persons = count_persons + 1
  travelers_sp = rbind(travelers_sp, this_person)
  this_household = subset(this_household, pid != this_person_id)
  #rest of household members in the party
  this_hh_party = parties_sp$hh_party[id] - 1
  if (this_hh_party > 0 & this_hh_party <= nrow(this_household)){
    count_persons = count_persons + this_hh_party 
    if (this_hh_party < nrow(this_household)){
      additional_person_ids = sample(this_household$pid, size = this_hh_party, replace = F)
    } else if (this_hh_party == nrow(this_household)){
      additional_persons_ids = this_household$pid
    }
    additional_persons = subset(this_household, pid %in% additional_persons_ids)
    additional_persons$unique_id = id
    additional_persons$month = parties_sp$month[id]
    additional_persons$nights = parties_sp$nights[id]
    additional_persons$purpose = parties_sp$purpose[id]
    additional_persons$main_mode = parties_sp$main_mode[id]
    travelers_sp = rbind(travelers_sp, additional_persons)
  } else if (this_hh_party > nrow(this_household)){
    #there are not sufficient persons in the household to fill this travel party (why?)
    missing_persons = missing_persons + this_hh_party - nrow(this_household)
  }
  print(paste(id,count_persons,missing_persons, sep = "/"))
}

write.csv(travelers_sp, "C:/projects/visitors/result/persons.csv", row.names = F)


