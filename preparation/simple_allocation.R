
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
    id = sample(x = weights$ID, size = 1, prob =  weights[[column]] )
    row = as.list(dataset %>% filter(index == id)) 
    visitors = visitors + row$hh_party
    micro_data = rbind(micro_data, list(month = month, id = row$index))
    progress = visitors/visitors_total * 100
    print(paste(month,progress, sep = "---"))
  }
}

micro_data_validate = merge(x= micro_data, y = dataset, by.x = "id", by.y = "index")

micro_data_validate %>% group_by(main_mode) %>% summarize(trips = sum(hh_party))
micro_data_validate %>% group_by(purpose) %>% summarize(trips = sum(hh_party))
