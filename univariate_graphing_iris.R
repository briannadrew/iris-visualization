# Name: Univariate Graphing Using Iris Dataset (Assignment #1)
# Author: Brianna Drew                                              
# Date Created: October 3rd, 2020
# Last Modified: October 12th, 2020  

library(datasets) # Import base R datasets
library(viridis) # Import viridis color palette package
library(wesanderson) # Import wesanderson color palette package
library(beeswarm) # Import beeswarm package
library(vioplot) # Import vioplot package
View(iris) # View the Iris dataset #GrandBudapest2

# Separating species into subsets
vers <- subset(iris, Species == "versicolor")# viridis
seto <- subset(iris, Species == "setosa") # plasma
virg <- subset(iris, Species == "virginica") # Moonrise3

# Separating each measurement into subsets
sLength <- iris$Sepal.Length
sWidth <- iris$Sepal.Width
pLength <- iris$Petal.Length
pWidth <- iris$Petal.Width

# Decrease axis text size
par(cex.axis = 0.75)

# Boxplot for all species
boxplot(iris[,1:4], main="All Species", col = wes_palette("GrandBudapest2", 4, type = "continuous"), notch = TRUE)

# Boxplots for each species
boxplot(vers[,1:4], main = "Versicolor", col = viridis(4), notch = TRUE)
boxplot(seto[,1:4], main = "Setosa", col = plasma(4), notch = TRUE)
boxplot(virg[,1:4], main = "Virginica", col = wes_palette("Moonrise3", 4, type = "continuous"), notch = TRUE)

# Reset axis text size to default
par(cex.axis = 1.0)

# Histograms for all species
hist(iris$Sepal.Length, main = "Sepal Length", xlab = "Sepal Length", col = wes_palette("GrandBudapest2", 8, type = "continuous"))
hist(iris$Sepal.Width, main = "Sepal Width", xlab = "Sepal Width", col = wes_palette("GrandBudapest2", 12, type = "continuous"))
hist(iris$Petal.Length, main = "Petal Length", xlab = "Petal Length", col = wes_palette("GrandBudapest2", 12, type = "continuous"))
hist(iris$Petal.Width, main = "Petal Width", xlab = "Petal Width", col = wes_palette("GrandBudapest2", 13, type = "continuous"))

# Histograms for Versicolor species
hist(vers$Sepal.Length, main = "Versicolor Sepal Length", xlab = "Sepal Length", col = viridis(5))
hist(vers$Sepal.Width, main = "Versicolor Sepal Width", xlab = "Sepal Width", col = viridis(7))
hist(vers$Petal.Length, main = "Versicolor Petal Length", xlab = "Petal Length", col = viridis(5))
hist(vers$Petal.Width, main = "Versicolor Petal Width", xlab = "Petal Width", col = viridis(8))

# Histograms for Setosa species
hist(seto$Sepal.Length, main = "Setosa Sepal Length", xlab = "Sepal Length", col = plasma(8))
hist(seto$Sepal.Width, main = "Setosa Sepal Width", xlab = "Sepal Width", col = plasma(5))
hist(seto$Petal.Length, main = "Setosa Petal Length", xlab = "Petal Length", col = plasma(9))
hist(seto$Petal.Width, main = "Setosa Petal Width", xlab = "Petal Width", col = plasma(5))

# Histograms for Virginica species
hist(virg$Sepal.Length, main = "Virginica Sepal Length", xlab = "Sepal Length", col = wes_palette("Moonrise3", 7, type = "continuous"))
hist(virg$Sepal.Width, main = "Virginica Sepal Width", xlab = "Sepal Width", col = wes_palette("Moonrise3", 8, type = "continuous"))
hist(virg$Petal.Length, main = "Virginica Petal Length", xlab = "Petal Length", col = wes_palette("Moonrise3", 5, type = "continuous"))
hist(virg$Petal.Width, main = "Virginica Petal Width", xlab = "Petal Width", col = wes_palette("Moonrise3", 6, type = "continuous"))

# Decrease axis text size
par(cex.axis = 0.75)

# Pie Chart (showing % of each species of Iris dataset)
slices <- c(length(vers), length(seto), length(virg))
lbls <- c("Versicolor", "Setosa", "Virginica")
pct <- round(slices/sum(slices)*100) # get percentages
lbls <- paste(lbls, pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # add % to labels
pie(slices, labels = lbls, main = "Iris Species", col = wes_palette("GrandBudapest2", 3, type = "continuous"))

# Beeswarm for all species
beeswarm(iris[,1:4], main="All Species", col = wes_palette("GrandBudapest2", 4, type = "continuous"))

# Beeswarms for each species
beeswarm(vers[,1:4], main = "Versicolor", col = viridis(4))
beeswarm(seto[,1:4], main = "Setosa", col = plasma(4))
beeswarm(virg[,1:4], main = "Virginica", col = wes_palette("Moonrise3", 4, type = "continuous"))

# Violin plot for all species
vioplot(iris[,1:4], main="All Species", col = wes_palette("GrandBudapest2", 4, type = "continuous"))

# Violin plots for each species
vioplot(vers[,1:4], main = "Versicolor", col = viridis(4))
vioplot(seto[,1:4], main = "Setosa", col = plasma(4))
vioplot(virg[,1:4], main = "Virginica", col = wes_palette("Moonrise3", 4, type = "continuous"))

# Reset axis text size to default
par(cex.axis = 1.0)

# Density plots for all species
plot(density(iris$Sepal.Length, bw = 0.05), main = "Sepal Length", xlab = "Sepal Length", col = wes_palette("GrandBudapest2", 1, type = "continuous"))
plot(density(iris$Sepal.Width, bw = 0.05), main = "Sepal Width", xlab = "Sepal Width", col = wes_palette("GrandBudapest2", 1, type = "continuous"))
plot(density(iris$Petal.Length, bw = 0.05), main = "Petal Length", xlab = "Petal Length", col = wes_palette("GrandBudapest2", 1, type = "continuous"))
plot(density(iris$Petal.Width, bw = 0.05), main = "Petal Width", xlab = "Petal Width", col = wes_palette("GrandBudapest2", 1, type = "continuous"))

# Density plots for Versicolor species
plot(density(vers$Sepal.Length, bw = 0.05), main = "Versicolor Sepal Length", xlab = "Sepal Length", col = viridis(1))
plot(density(vers$Sepal.Width, bw = 0.05), main = "Versicolor Sepal Width", xlab = "Sepal Width", col = viridis(1))
plot(density(vers$Petal.Length, bw = 0.05), main = "Versicolor Petal Length", xlab = "Petal Length", col = viridis(1))
plot(density(vers$Petal.Width, bw = 0.05), main = "Versicolor Petal Width", xlab = "Petal Width", col = viridis(1))

# Density plots for Setosa species
plot(density(seto$Sepal.Length, bw = 0.05), main = "Setosa Sepal Length", xlab = "Sepal Length", col = plasma(1))
plot(density(seto$Sepal.Width, bw = 0.05), main = "Setosa Sepal Width", xlab = "Sepal Width", col = plasma(1))
plot(density(seto$Petal.Length, bw = 0.05), main = "Setosa Petal Length", xlab = "Petal Length", col = plasma(1))
plot(density(seto$Petal.Width, bw = 0.05), main = "Setosa Petal Width", xlab = "Petal Width", col = plasma(1))

# Density plots for Virginica species
plot(density(virg$Sepal.Length, bw = 0.05), main = "Virginica Sepal Length", xlab = "Sepal Length", col = wes_palette("Moonrise3", 1, type = "continuous"))
plot(density(virg$Sepal.Width, bw = 0.05), main = "Virginica Sepal Width", xlab = "Sepal Width", col = wes_palette("Moonrise3", 1, type = "continuous"))
plot(density(virg$Petal.Length, bw = 0.05), main = "Virginica Petal Length", xlab = "Petal Length", col = wes_palette("Moonrise3", 1, type = "continuous"))
plot(density(virg$Petal.Width, bw = 0.05), main = "Virginica Petal Width", xlab = "Petal Width", col = wes_palette("Moonrise3", 1, type = "continuous"))

# Bar plots for Versicolor species (first 10 samples)
barplot(vers[1:10, "Sepal.Length"], main = "Versicolor Sepal Length", horiz = TRUE, xlab = "Sepal Length", ylab = "Samples", col = viridis(10))
barplot(vers[1:10, "Sepal.Width"], main = "Versicolor Sepal Width", horiz = TRUE, xlab = "Sepal Width", ylab = "Samples", col = viridis(10))
barplot(vers[1:10, "Petal.Length"], main = "Versicolor Petal Length", horiz = TRUE, xlab = "Petal Length", ylab = "Samples", col = viridis(10))
barplot(vers[1:10, "Petal.Width"], main = "Versicolor Petal Width", horiz = TRUE, xlab = "Petal Width", ylab = "Samples", col = viridis(10))

# Bar plots for Setosa species (first 10 samples)
barplot(seto[1:10, "Sepal.Length"], main = "Setosa Sepal Length", horiz = TRUE, xlab = "Sepal Length", ylab = "Samples", col = plasma(10))
barplot(seto[1:10, "Sepal.Width"], main = "Setosa Sepal Width", horiz = TRUE, xlab = "Sepal Width", ylab = "Samples", col = plasma(10))
barplot(seto[1:10, "Petal.Length"], main = "Setosa Petal Length", horiz = TRUE, xlab = "Petal Length", ylab = "Samples", col = plasma(10))
barplot(seto[1:10, "Petal.Width"], main = "Setosa Petal Width", horiz = TRUE, xlab = "Petal Width", ylab = "Samples", col = plasma(10))

# Bar plots for Virginica species (first 10 samples)
barplot(virg[1:10, "Sepal.Length"], main = "Virginica Sepal Length", horiz = TRUE, xlab = "Sepal Length", ylab = "Samples", col = wes_palette("Moonrise3", 10, type = "continuous"))
barplot(virg[1:10, "Sepal.Width"], main = "Virginica Sepal Width", horiz = TRUE, xlab = "Sepal Width", ylab = "Samples", col = wes_palette("Moonrise3", 10, type = "continuous"))
barplot(virg[1:10, "Petal.Length"], main = "Virginica Petal Length", horiz = TRUE, xlab = "Petal Length", ylab = "Samples", col = wes_palette("Moonrise3", 10, type = "continuous"))
barplot(virg[1:10, "Petal.Width"], main = "Virginica Petal Width", horiz = TRUE, xlab = "Petal Width", ylab = "Samples", col = wes_palette("Moonrise3", 10, type = "continuous"))

# Decrease axis text size
par(cex.axis = 0.75)

# Side-by-side column charts for each species (first 5 samples)
samples <- c("Sample #1", "Sample #2", "Sample #3", "Sample #4", "Sample #5")
vers5 = data.matrix(vers[1:5, 1:4])
seto5 = data.matrix(seto[1:5, 1:4])
virg5 = data.matrix(virg[1:5, 1:4])

barplot(vers5, main = "Versicolor", xlab = "Measures", ylab = "Length/Width", col = viridis(5), beside = TRUE)
legend("topright", samples, fill = viridis(5), cex = 0.75)
barplot(seto5, main = "Setosa", xlab = "Measures", ylab = "Length/Width", col = plasma(5), beside = TRUE)
legend("topright", samples, fill = plasma(5), cex = 0.75)
barplot(virg5, main = "Virginica", xlab = "Measures", ylab = "Length/Width", col = wes_palette("Moonrise3", 5, type = "continuous"), beside = TRUE)
legend("topright", samples, fill = wes_palette("Moonrise3", 5, type = "continuous"), cex = 0.75)

# Stacked column charts for each species (first 5 samples)
barplot(vers5, main = "Versicolor", xlab = "Measures", ylab = "Length/Width", col = viridis(5), beside = FALSE)
legend("topright", samples, fill = viridis(5), cex = 0.75)
barplot(seto5, main = "Setosa", xlab = "Measures", ylab = "Length/Width", col = plasma(5), beside = FALSE)
legend("topright", samples, fill = plasma(5), cex = 0.75)
barplot(virg5, main = "Virginica", xlab = "Measures", ylab = "Length/Width", col = wes_palette("Moonrise3", 5, type = "continuous"), beside = FALSE)
legend("topright", samples, fill = wes_palette("Moonrise3", 5, type = "continuous"), cex = 0.75)

# Boxplot + beeswarm for all species
boxplot(iris[,1:4], main="All Species", col = wes_palette("GrandBudapest2", 4, type = "continuous"))
beeswarm(iris[,1:4], main="All Species", col = wes_palette("BottleRocket2", 4, type = "continuous"), add = TRUE)

# Boxplots + beeswarms for each species
boxplot(vers[,1:4], main = "Versicolor", col = viridis(4))
beeswarm(vers[,1:4], main = "Versicolor", col = wes_palette("GrandBudapest1", 4, type = "continuous"), add = TRUE)
boxplot(seto[,1:4], main = "Setosa", col = plasma(4))
beeswarm(seto[,1:4], main = "Setosa", col = wes_palette("Darjeeling1", 4, type = "continuous"), add = TRUE)
boxplot(virg[,1:4], main = "Virginica", col = wes_palette("Moonrise3", 4, type = "continuous"))
beeswarm(virg[,1:4], main = "Virginica", col = wes_palette("FantasticFox1", 4, type = "continuous"), add = TRUE)