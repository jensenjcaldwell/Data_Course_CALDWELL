library(tidyverse)
library(ShortRead)
library(Biostrings)
library(dada2)


BiocManager::install("dada2")


list.files("./Data/Fastq_16S", full.names = TRUE)

fq <- readFastq("./Data/Fastq_16S/F3D2_S190_L001_R1_001.fastq")
fq
sread(fq) %>% translate()
reverseComplement(sread(fq))

sread(fq)[[8]]


trimEnds(sread(fq), a = "#") %>% translate()


sread(fq)[[7]]

filterAndTrim(fwd = "./Data/Fastq_16S/F3D2_S190_L001_R1_001.fastq",
              filt = "./Data/Fastq_16S/F3D2_S190_L001_R1_001_trimmed.fastq",
              maxN = 0)

fq <- readFastq("./Data/Fastq_16S/F3D2_S190_L001_R1_001_trimmed.fastq")

sread(fq) %>% translate()


p1 <- plotQualityProfile("./Data/Fastq_16S/F3D2_S190_L001_R1_001_trimmed.fastq")
p2 <- plotQualityProfile("./Data/Fastq_16S/F3D2_S190_L001_R1_001_extratrimmed.fastq")


filterAndTrim("./Data/Fastq_16S/F3D2_S190_L001_R1_001_trimmed.fastq",
              filt = "./Data/Fastq_16S/F3D2_S190_L001_R1_001_extratrimmed.fastq",
              truncQ = 30,
              trimLeft = 50)

library(patchwork)
p1/p2

install.packages("remotes")

# Install using the remotes package
remotes::install_github("rstudio/shinyuieditor")

library(shinyuieditor)

launch_editor("./newapp")
install.packages("DT")



df <- data.frame(ob1 = (), ob2 =())

library(rgdal)
