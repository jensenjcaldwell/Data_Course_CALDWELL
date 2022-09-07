x <- read.csv("./Data/lw.csv")
x$area <- x$length*x$width

library(tidyverse)

?tidyverse

?plot(x$width,x$area,color=x$name)

cor(x$length*x$width,x$area)


#$names <- NULL #makes column go bye-bye

