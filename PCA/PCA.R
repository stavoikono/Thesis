library(AMR)
data("mtcars")
head(mtcars)
mtcars <- mtcars[,c(1:7,10,11)]
mtcars.pca <- prcomp(mtcars, center = TRUE,scale. = TRUE)
summary(mtcars.pca)
ggplot_pca(mtcars.pca)