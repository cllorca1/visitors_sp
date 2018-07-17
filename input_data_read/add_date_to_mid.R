#merge data among MiD data sources

persons_with_date = persons %>% select (pid, hhid, stich_j, stich_m )


data = merge(x = data, y = persons_with_date, by = c("pid", "hhid"))

rm(persons_with_date)

data %>% group_by(stich_m) %>% summarize(n())
