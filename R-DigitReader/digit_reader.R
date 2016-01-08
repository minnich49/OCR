library(bpca)
library(rgl)
library(ggfortify)
library(ggplot2)
library(RColorBrewer)

setwd("/Users/bobminnich/Documents/Columbia/Courses/Data_Mining/Examples/DigitReader")
data = as.data.frame(read.csv("train.csv", header = TRUE, sep = ","))
labels2 = data[,1]
labels_frame <- as.data.frame(data[,1])
labels_frame <- setNames(labels_frame, c("l"))

data_r = data[,-1]
pca = prcomp(data_r)
cov_col = apply(pca$x,2,var)
cov_col_rat = cov_col/sum(cov_col)
cum_sum_rat = cumsum(cov_col)/max(cumsum(cov_col))
plot(cum_sum_rat,type = "lines")
screeplot(pca)

dev.off()
labels_frame$color[data$label == 0] = brewer.pal(10, "RdYlGn")[1]
for(i in 1:9){
  labels_frame$color[data$label == i] = brewer.pal(10, "Spectral")[i+1]
}

#ßlabels_frame$color[data$label == '0'] <- 'red'
#labels_frame$color[data$label != c('1','0')] <- 'black'
write.table(pca$x[,1:3], "pca_digitreader")
plot3d(pca$x[,1:3],col = labels_frame$color, size = 1)
legend3d("topright", legend = paste('Num', c(0:9)), pch = 16, col = labels_frame$color, cex=1, inset=c(0.15))
rgl.viewpoint(theta = -35, phi = 30, fov = 45, zoom = 1)


writeWebGL(width = 500, height = 500)


