############################################################################
############PCA###########################################################
###########################################################################
solar_pca<-prcomp(num_solar_sample,scale. = TRUE)
solar_cs_pca<-prcomp(num_solar_cs_sample,scale. = TRUE)
solar_pca$center #Mean of Variables
########################################################
#             G0       GHI       DHI       BHI       BNI 
#         509.73846 228.96556 131.05573  98.13186 189.79168 
################################################################################
solar_cs_pca$center #Mean of Variables
########################################################
#            G0    CS_GHI    CS_DHI    CS_BHI    CS_BNI 
#       503.20098 351.77146  96.88005 254.89140 514.79925 
################################################################################

#######################################################
solar_pca$scale #STD DEVIATION OF VARIABLES
#######################################################
#       G0       GHI       DHI       BHI       BNI 
#   335.20316 205.63508  98.48663 138.93768 219.30655
#######################################################
solar_cs_pca$scale#STD DEVIATION OF VARIABLES
#######################################################
#       G0    CS_GHI    CS_DHI    CS_BHI    CS_BNI 
#     333.48340 266.96759  57.07413 222.89294 276.56801
#######################################################
solar_pca$rotation

######################################################
#       PC1         PC2        PC3        PC4           PC5
#G0  -0.4484560  0.42940687 -0.1662082  0.7660724 -0.0022112278
#GHI -0.5057704 -0.01017908 -0.1981968 -0.3355927 -0.7695377004
#DHI -0.4127788  0.56477711  0.3860230 -0.4733914  0.3708467891
#BHI -0.4553542 -0.41505490 -0.5706973 -0.1562305  0.5198832238
#BNI -0.4066323 -0.56944040  0.6770404  0.2280421  0.0009647443
#############################################################################
solar_cs_pca$rotation

######################################################
#           PC1         PC2        PC3         PC4           PC5
#G0     -0.4708386  0.02257026 -0.1888005 -0.86148473 -1.053083e-08
#CS_GHI -0.4684956 -0.06465643 -0.3133441  0.32303071  7.574946e-01
#CS_DHI -0.4034142  0.84831584  0.2339795  0.19143024 -1.619423e-01
#CS_BHI -0.4578369 -0.29466192 -0.4352175  0.33788876 -6.324371e-01
#CS_BNI -0.4318162 -0.43456267  0.7886756  0.05177711  1.379267e-09
#############################################################################

dim(solar_pca$x)#[1]  9959    5
biplot(solar_pca,scale = 0)
biplot(solar_cs_pca,scale = 0)
solar_pca_sd<-solar_pca$sdev

solar_pca_var<-solar_pca_sd^2

solar_cs_pca_sd<-solar_cs_pca$sdev

solar_cs_pca_var<-solar_cs_pca_sd^2

#proportion of solar variance explained
prop_Var_Ex <- solar_pca_var/sum(solar_pca_var)
prop_Var_Ex[1:5]
#scree plot
plot(prop_Var_Ex, xlab = "Principal Component",
     ylab = "Proportion of Variance Explained",
     type = "b")
#cumulative scree plot
plot(cumsum(prop_Var_Ex), xlab = "Principal Component",
     ylab = "Cumulative Proportion of Variance Explained",
     type = "b")
#Component 5 tends to extreme 0 for solar
pca_cluster<-solar_pca$x
pca_cluster<-pca_cluster[,1:4]

##########################################################
#proportion of clear sky solar variance explained
prop_cs_Var_Ex <- solar_cs_pca_var/sum(solar_cs_pca_var)
prop_cs_Var_Ex[1:5]
#scree plot
plot(prop_cs_Var_Ex, xlab = "Principal Component",
     ylab = "Proportion of Variance Explained",
     type = "b")
#cumulative scree plot
plot(cumsum(prop_cs_Var_Ex), xlab = "Principal Component",
     ylab = "Cumulative Proportion of Variance Explained",
     type = "b")
#Component 5 tends to extreme 0 for solar
pca_cs_cluster<-solar_cs_pca$x
pca_cs_cluster<-pca_cs_cluster[,1:4]
hist(Temp_Solar_Sample$G0)
hist(Temp_Solar_Sample$GHI)
hist(Temp_Solar_Sample$DHI)
hist(Temp_Solar_Sample$BHI)
hist(Temp_Solar_Sample$BNI)

hist(Temp_Solar_CS_Sample$CS_GHI)
hist(Temp_Solar_CS_Sample$CS_DHI)
hist(Temp_Solar_CS_Sample$CS_BHI)
hist(Temp_Solar_CS_Sample$CS_BNI)

library(FactoMineR)
install.packages("factoextra")
library(factoextra)
pca<-PCA(TempData1Sample[, c(-1,-2)], scale.unit = T, ncp=3)
fviz_pca_ind(pca, habillage = TempData1Sample$County, label="none", addEllipses = T)

table(TempData1Sample$County)
set.seed(2000)
mySolar <- TempData1Sample[, c(3:11)] #removing the dependent
mySolar <- sapply(mySolar, FUN=function(x) { scale(x, scale = T, center=T)})
res.1 <- kmeans(mySolar, 3)
str(res.1)
df <- data.frame(cluster = res.1$cluster, county = TempData1Sample$County)
table (factor(df$cluster), df$county)