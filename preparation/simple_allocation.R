pacman::p_load(data.table, dplyr, ggplot2, reshape, extrafont)

folder = "c:/projects/visitors/silo_folder/"
file_weights = paste(folder,"microData/interimFiles/weigthsMatrix.csv", sep = "")

weights = fread(file_weights)

file_months = paste(folder,"marginalsMunicipality.csv" , sep = "")

monthly_totals = fread(file_months)
monthly_totals = monthly_totals %>% 
  mutate(visitors_total = visitors_leisure + 
           visitors_visit + 
           visitors_other_private + 
           visitors_business + 
           visitors_weekend_commuter + 
           visitors_other + 
           visitors_unaware + 
           visitors_denied)



#months = c(1)
#or
months = seq(1:12)

micro_data = data.frame()
for (month in months){
  visitors_total = subset(monthly_totals, Month == month)$visitors_total
  visitors = 0
  while (visitors < visitors_total){
    column = 1 + month
    weights_this_month = weights[[column]]
    id = sample(x = weights$ID, size = 1, prob = weights_this_month, replace = T )
    row = as.list(subset(dataset,index == id))
    visitors = visitors + row$hh_party
    micro_data = rbind(micro_data, list(month = month, id = row$index))
    progress = visitors/visitors_total * 100
    print(paste(month,progress, sep = "---"))
  }
}

write.csv(micro_data, "C:/projects/visitors/result/ids.csv", row.names = F)

micro_data_validate = merge(x= micro_data, y = dataset, by.x = "id", by.y = "index")

by_mode = micro_data_validate %>% group_by(main_mode) %>% summarize(persons = sum(hh_party))
by_purpose = micro_data_validate %>% group_by(purpose) %>% summarize(persons = sum(hh_party))
by_party_size = micro_data_validate %>% group_by(hh_party) %>% summarize(persons = sum(hh_party))
by_nights = micro_data_validate %>% group_by(nights) %>% summarize(persons = sum(hh_party)) 
