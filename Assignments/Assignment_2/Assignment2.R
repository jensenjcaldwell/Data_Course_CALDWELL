csv_files <- list.files(path="Data",
                        recursive = TRUE,
                        pattern ="*.csv",
                        full.names = TRUE)  #lists all .csv files in the in the vector "csv_files"

length(csv_files) #Outputs number of .csv files

csv_files # displays to console vector csv_files

df <- read.csv("Data/wingspan_vs_mass.csv") #stores file "Data/wingspan_vs_mass.csv" as df

head(df, n=5L) #displays first five lines of "Data/wingspan_vs_mass.csv"

list.files(recursive = TRUE, pattern = "^b",full.names = FALSE ) #shows files starting with "b"

#^ <- starts with
#$ <- ends with
#* <- 0-inf of whatever comes before
#* . <- any character

b <- list.files(path="Data",recursive = TRUE, pattern = "^b",full.names = TRUE ) #stores files starting with "b" as "b"

for(i in b) {
  print(readLines(i, n=5L))
} #prints first 5 lines of each of the three files in vector "b"

for(i in csv_files) {
  print(readLines(i, n=5L))
} #prints first 5 lines of each .csv file as stored in vector "df"

