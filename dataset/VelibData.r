install.packages("miceadds")

library(miceadds)

#LOADING OF THE DATA
load.Rdata( filename="velib.Rdata", "velib" )
head(velib)

#PRETREATMENT AND DESCRIPTIVE STATISTIC
#selection of the most useful data (data and position)
summary(velib)

#visualise the most useful data
data.frame(velib$data , velib$position)

#DATA VISUALIZATION
#Diamension reduction using PCA
install.packages("FactoMineR")
library(FactoMineR)

RealVelib=data.frame(velib$data , velib$position)
#If we would like to perfom PCA, we have the `princomp` function in R:
pc = princomp(RealVelib)
#The main task to do now is to select the number of components to retain:
summary(pc)
#If we apply the rule of the 90%, 22 components are enough on this case (91.23% > 90%).

screeplot(pc)
#The rule of the break in the eigenvalue scree would probably prefer d = 10 ish.

#And finally, if we try to apply the scree-test of Cattell:
diff = abs(diff(pc$sdev))
plot(diff,type='b')
abline(h = 0.1*max(diff),lty=2,col='blue')
#This test also recommends to pick d=10.

#The correlation circle may be obtainned thnaks to the `biplot` fucntion:
biplot(pc)

#In the FactoMineR package, there is a more clear visualization:
pc = PCA(RealVelib,scale.unit = TRUE)
plot(pc)

#CLUSTERING
#Hierarchical clustering
#The hierarchical clustering is available in R within the `class` package:
install.packages("class")
library(class)

#Notice that the input data are not the actual data but a distance matrix between all obseravtions
D = dist(RealVelib) # Compute the distance matrix between all observations
out = hclust(D,method='complete')
#To look at the result, we have to plot the dendrogram:
plot(out)

#When looking at this dendrogram, we may choose to cut the tree at K=6. In order to obtain the final clustering partition, we have to "cut the tree" at the level K=6:
cl = cutree(out,k=6)
cl

plot(cl)

#It is also possible to exploit the numerical values that are stored in the `out` object to draw a curve to choose K:
plot(out$height,type='b')

#A map of the results may be obtained using the GPS coordinates of the stations
install.packages("leaflet")

library(leaflet)

palette = colorFactor("RdYlBu", domain = NULL)
leaflet(RealVelib) %>% addTiles() %>%
addCircleMarkers(radius = 3,
color = palette(cl),
stroke = FALSE, fillOpacity = 0.9)

#k-means
#

out2 = kmeans(RealVelib,centers = 3)

out2

#it usually first necessary to find the right value for K:
J = c()
for (k in 1:15){
  out2 = kmeans(RealVelib,centers = k,nstart = 10)
  J[k] = out2$betweenss / out2$totss
}
plot(J,type='b')

Kstar = 8 
# my choice!
out2 = kmeans(RealVelib,centers = Kstar,nstart = 10)
out2

dev.new()
# plot()
# save your plot
pairs(RealVelib,col=out2$cluster,pch=19)
dev.off()

#A simple way to look at the clustering:
pairs(RealVelib,col=out2$cluster,pch=19)

?par
