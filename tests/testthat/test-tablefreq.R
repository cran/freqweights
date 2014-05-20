## Emilio Torres Manzanera
## University of Oviedo
## Time-stamp: <2014-04-25 07:21 emilio on emilio-Satellite-P100>
## ============================================================

context("Table of frequencies")

test_that("Table of frequencies. Updating",{
  
tfq <- tablefreq(iris[,c(1:2,5)],freq="Sepal.Width")
chunk1 <- iris[1:10,c(1:2,5)]
chunk2 <- iris[c(11:20),]
chunk3 <- iris[-c(1:20),]
a <- tablefreq(chunk1,freq="Sepal.Width")
a <- update(a,chunk2)
a <- update(a,chunk3)
expect_that(a, equals(tfq))

})
