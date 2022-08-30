csv_files <- list.files(recursive = TRUE, pattern ="*.csv", full.names = TRUE)  #lists all .csv files in the in the vector "csv_files"

length(csv_files) #Outputs number of .csv files

csv_files # displays to console vector csv_files

read.csv("Data/wingspan_vs_mass.csv") #Reads "Data/wingspan_vs_mass.csv" 

df <- read.csv("Data/wingspan_vs_mass.csv") #stores file "Data/wingspan_vs_mass.csv" as df

head(df, n=5L) #displays first five lines of "Data/wingspan_vs_mass.csv"

list.files(recursive = TRUE, pattern = "^b.*",full.names = FALSE ) #shows files starting with "b"
b <- list.files(recursive = TRUE, pattern = "^b.*",full.names = TRUE ) #stores files starting with "b" as "b"

for(i in 1:8) {
  head(b, n=5L)
}

head(b[1], n=5L)
  