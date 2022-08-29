 list.files(pattern="grade", recursive = TRUE, full.names = TRUE, ignore.case = TRUE) #csv files assigned to x
y <- x[1] #Assigns first .csv file to y
readLines(y)[1:3]

z <- read.csv(y)

n=z$IATA_CODE


grades <- read.csv("./Data/Fake_grade_data.csv")

grades[3,c(1,3,5)]


grades$Student[grades$Assignment_1 > 15] #all students with a score >15 on assignment 1


mean(grades[3,2:15])

# ?list.files
# ?readLines
# ?read.csv
# ?c
