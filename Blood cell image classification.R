install.packages("drat", repos="https://cran.rstudio.com")
drat:::addRepo("dmlc")
#install.packages("mxnet")
#image_dir <- "C:/Nithi/Github/MyProgramming/images"
library(EBImage)
library(keras)
install.packages("keras")
#Read images
image_dir <- "C:/Nithi/Github/MyProgramming/TEST"
bloodcell <- c('E1.jpeg','E2.jpeg','E3.jpeg','E4.jpeg','E5.jpeg',
               'L1.jpeg','L2.jpeg','L3.jpeg','L4.jpeg','L5.jpeg',
               'M1.jpeg','M2.jpeg','M3.jpeg','M4.jpeg','M5.jpeg',
               'N1.jpeg','N2.jpeg','N3.jpeg','N4.jpeg','N5.jpeg')
train <- list()
for (i in 1:20) {train[[i]] <- readImage(bloodcell[i])}
bloodcell1 <- c('E6.jpeg','L6.jpeg','M6.jpeg','N6.jpeg')
test <- list()
for (i in 1:20) {train[[i]] <- readImage(bloodcell[i])} {
  
}