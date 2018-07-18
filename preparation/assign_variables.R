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

#assign persons in the household travelling

travelers_sp = data.frame()
for (id in parties$unique_id){
  this_household_id = parties_sp$hhid[id]
  this_person_id = parties_sp$pid[id]
  
  this_household = subset(persons, hhid  == this_household_id)
  
  this_person = subset(this_household, pid = this_household_id)
  
  
  
}


