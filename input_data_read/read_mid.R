pacman::p_load(data.table, dplyr, ggplot2)

folder = "C:/projects/visitors/MiD2008/CSV/SAS_PUF/"
file = "MiD2008_PUF_Reisen.sas7bdat.csv"
path = paste(folder, file, sep = "")

data = fread(path)

file = "MiD2008_PUF_Haushalte.sas7bdat.csv"
path = paste(folder, file, sep = "")
households = fread(path)

file = "MiD2008_PUF_Personen.sas7bdat.csv"
path = paste(folder, file, sep = "")
persons = fread(path)

file = "MiD2008_PUF_Wege.sas7bdat.csv"
path = paste(folder, file, sep = "")
trips = fread(path)

