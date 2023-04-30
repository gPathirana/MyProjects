library(tidyverse)
library(wesanderson)
library(factoextra)
library(ggplot2)
library(dplyr)
library(ggcorrplot)
library(GGally)
library(GoodmanKruskal)
library(ggExtra)
library(FactoMineR)
library(gplots)
library(hrbrthemes)
library(viridis)

#Importing data  set
setwd("E:/UOC NOTES/3rd year/sem 2/ST 3082/data analys/archive")
Data=read.csv("train.csv")
#Checking for the missing values
anyNA(Data)
TestData=read.csv("test.csv")
anyNA(TestData)
#no missing values in data set

dim(Data)
str(Data)
nrow(unique(Data))==nrow(Data)
N=nrow(Data)

#Data cleaning
#setting "south" country to NA and omit the rows with missing values
unique(Data$native.country)
for(i in 1:N){
  if(Data$native.country[i]==" South"){
    Data$native.country[i] = NA
  }else{
    next
  }
}
anyNA(Data)

Data1=na.omit(Data)
anyNA(Data1)
n=nrow(Data1)

#boxplot of response variable

boxplot(Data1$hours.per.week,col="darkgoldenrod2" ,ylab = "Hours per week")

# Layout to split the screen
layout(mat = matrix(c(1,2),2,1, byrow=TRUE),  height = c(1,8))

# Draw the boxplot and the histogram 
par(mar=c(0, 3.1, 1.1, 2.1))
boxplot(Data1$hours.per.week, horizontal=TRUE , col="darkolivegreen3" , frame=F)
par(mar=c(4, 3.1, 1.1, 2.1))
hist(Data1$hours.per.week , col="darkgoldenrod2" , border=F , main="" , xlab="Hours per week")

#checking normality of the response
qqnorm(Data1$hours.per.week)
qqline(Data1$hours.per.week)

#checking on native country variable
variable_table <- table(Data1$native.country)

# Calculate the proportions of levels
variable_prop <- (variable_table / sum(variable_table))*100
variable_prop

#before compare means check the normality of distributions
unique(Data1$education)
GA=Data1$hours.per.week[Data1$education==" Bachelors"]
shapiro.test(GA)
#distribution is not normal.cant use ANOVA

#Checking the association between hours per week and education with medians
kruskal.test(hours.per.week~education, data = Data1)
#Null hypothesis is rejected.therefore medians are different

#before compare means check the normality of distributions
unique(Data1$education)
Edu=Data1$hours.per.week[Data1$education==" Bachelors"]
shapiro.test(Edu)
#distribution is not normal.cant use ANOVA

#Checking the association between hours per week and education with medians
kruskal.test(hours.per.week~education, data = Data1)
#Null hypothesis is rejected.therefore medians are different

#before compare means check the normality of distributions
unique(Data1$sex)
gender=Data1$hours.per.week[Data1$sex==" Male"]
ks.test(gender,"pnorm")
#distribution is not normal

#Checking the association between hours per week and sex with medians
kruskal.test(hours.per.week~sex, data = Data1)
#Null hypothesis is rejected.therefore medians are different

#before compare means check the normality of distributions
unique(Data1$race)
RACE=Data1$hours.per.week[Data1$race==" White"]
ks.test(RACE,"pnorm")
#distribution is not normal

#Checking the association between hours per week and race with medians
kruskal.test(hours.per.week~race, data = Data1)
#Null hypothesis is rejected.therefore medians are different

#before compare means check the normality of distributions
unique(Data1$occupation)
Occu=Data1$hours.per.week[Data1$occupation==" Armed-Forces"]
ks.test(Occu,"pnorm")
#distribution is not normal

#Checking the association between hours per week and occupation with medians
kruskal.test(hours.per.week~occupation, data = Data1)
#Null hypothesis is rejected.therefore medians are different

#before compare means check the normality of distributions
unique(Data1$native.country)
Country=Data1$hours.per.week[Data1$native.country==" United-States"]
ks.test(Country,"pnorm")
#distribution is not normal

#Checking the association between hours per week and native country with medians
kruskal.test(hours.per.week~native.country, data = Data1)
#Null hypothesis is rejected.therefore medians are different


######Data binning##########

unique(Data1$marital.status)
Data1$marital.statusN=rep("a",n)
for( i in 1:n){
  if (Data1$marital.status[i]==" Never-married"){
    Data1$marital.statusN[i]=" Unmaried"
  }else if (Data1$marital.status[i]==" Married-civ-spouse"){
    Data1$marital.statusN[i]=" Maried"
  }else if (Data1$marital.status[i]==" Married-AF-spouse"){
    Data1$marital.statusN[i]=" Maried"
  }else if (Data1$marital.status[i]==" Married-spouse-absent"){
    Data1$marital.statusN[i]=" Maried"
  }else if (Data1$marital.status[i]==" Divorced"){
    Data1$marital.statusN[i]=" Seperated"
  }else if (Data1$marital.status[i]==" Separated"){
    Data1$marital.statusN[i]=" Seperated"
  }else if (Data1$marital.status[i]==" Widowed"){
    Data1$marital.statusN[i]=" Widowed"
  }
}

unique(Data1$education)
Data1$educationN=rep("a",n)
for( i in 1:n){
  if (Data1$education[i]==" HS-grad"){
    Data1$educationN[i]=" School"
  }else if (Data1$education[i]==" Some-college"){
    Data1$educationN[i]=" School"
  }else if (Data1$education[i]==" 7th-8th"){
    Data1$educationN[i]=" School"
  }else if (Data1$education[i]==" 5th-6th"){
    Data1$educationN[i]=" School"
  }else if (Data1$education[i]==" 11th"){
    Data1$educationN[i]=" School"
  }else if (Data1$education[i]==" 9th"){
    Data1$educationN[i]=" School"
  }else if (Data1$education[i]==" 1st-4th"){
    Data1$educationN[i]=" School"
  }else if (Data1$education[i]==" 12th"){
    Data1$educationN[i]=" School"
  }else if (Data1$education[i]==" 10th"){
    Data1$educationN[i]=" School"
  }else if (Data1$education[i]==" Preschool"){
    Data1$educationN[i]=" School"
  }else if (Data1$education[i]==" Bachelors"){
    Data1$educationN[i]=" Bachelors"
  }else if (Data1$education[i]==" Prof-school"){
    Data1$educationN[i]=" Bachelors"
  }else if (Data1$education[i]==" Masters"){
    Data1$educationN[i]=" Post-Graduate"
  }else if (Data1$education[i]==" Doctorate"){
    Data1$educationN[i]=" Post-Graduate"
  }else if (Data1$education[i]==" Assoc-voc"){
    Data1$educationN[i]=" Assoc_Degree"
  }else if (Data1$education[i]==" Assoc-acdm"){
    Data1$educationN[i]=" Assoc_Degree"
  }
}

unique(Data1$workclass)
Data1$workclassN=rep("a",n)
for( i in 1:n){
  if (Data1$workclass[i]==" Private"){
    Data1$workclassN[i]=" Private"
  }else if (Data1$workclass[i]==" Self-emp-not-inc"){
    Data1$workclassN[i]=" Self employed"
  }else if (Data1$workclass[i]==" Self-emp-inc"){
    Data1$workclassN[i]=" Self employed"
  }else if (Data1$workclass[i]==" State-gov"){
    Data1$workclassN[i]=" Gov"
  }else if (Data1$workclass[i]==" Local-gov"){
    Data1$workclassN[i]=" Gov"
  }else if (Data1$workclass[i]==" Federal-gov"){
    Data1$workclassN[i]=" Gov"
  }else if (Data1$workclass[i]==" Never-worked"){
    Data1$workclassN[i]=" Unemployed"
  }else if (Data1$workclass[i]==" Without-pay"){
    Data1$workclassN[i]=" Without-pay"
  }
}


unique(Data1$native.country)
Data1$native.countryN=rep(" ",n)
for(i in 1:n){
   if (Data1$native.country[i]==" United-States"){
     Data1$native.countryN[i] = " United-States"
  }else{
    Data1$native.countryN[i]=" Other"
  }
} 

#spliting the data set
set.seed(1234)
dt=sort(sample(nrow(Data1),nrow(Data1)*.8))
train=Data1[dt,]
test=Data1[-dt,]

attach(train)

# figure of working hours with Education

ggplot(train, aes(x = factor(1), y = (hours.per.week),fill=factor(educationN))) +
  geom_boxplot() +
  scale_fill_manual(values = wes_palette(c("Moonrise1"), n = 5,type="continuous"))+
  ylab("Working hours per week") +
  xlab("Education")


# figure of working hours with education level
ggplot(train, aes(x = factor(1), y = (hours.per.week),fill=factor(educationN))) +
  geom_boxplot() +
  scale_fill_manual(values=c("bisque","burlywood","darkgoldenrod1","darkgoldenrod3","darkgrey"))+
  theme(legend.position = "right")+
  ylab("Working hours per week") +
  xlab("education level") 
 

# figure of working hours with education level
ggplot(train, aes(x = factor(1), y = (hours.per.week),fill=factor(workclassN))) +
  geom_boxplot() +
  scale_fill_manual(values=c("bisque","burlywood","darkgoldenrod1","darkgoldenrod3","darkgrey"))+
  theme(legend.position = "right")+
  ylab("Working hours per week") +
  xlab("Workclass")

# figure of working hours with marital status
ggplot(train, aes(x = factor(1), y = hours.per.week,fill=factor(marital.statusN))) +
  geom_boxplot() +
  scale_fill_manual(values = wes_palette(c("Moonrise1"), n = 5,type="continuous"))+
  theme_minimal() +
  ylab("Working hours per week") +
  xlab("Marital status") 

# figure of working hours with gender
ggplot(train, aes(x = factor(1), y = hours.per.week,fill=factor(sex))) +
  geom_boxplot() +
  scale_fill_manual(values=c("darkgoldenrod1","darkgrey"))+
  theme_minimal() +
  ylab("Working hours per week") +
  xlab("Gender") 

#figure of working hours with income

ggplot(train, aes(x = factor(1), y = hours.per.week,fill=factor(income))) +
  geom_boxplot() +
  scale_fill_manual(values=c("darkgoldenrod1","darkgrey"))+
  theme_minimal() +
  ylab("Working hours per week") +
  xlab("Income") 


ggplot(Data1, aes(x = factor(1), y = hours.per.week)) +
  geom_boxplot() +
  scale_fill_manual(values="darkgoldenrod1")+
  theme_minimal() +
  ylab("Working hours per week") +
  xlab("Income") 

ggplot(train, aes(x = factor(1), y = hours.per.week,fill=factor(native.countryN))) +
  geom_boxplot() +
  scale_fill_manual(values=c("darkgoldenrod1","darkgrey"))+
  theme_minimal() +
  ylab("Working hours per week") +
  xlab("Native country") 

#Hours per week with gender
ggplot(train, aes(fill=sex,x = hours.per.week))+ 
  geom_histogram()+
  scale_fill_manual(values=c("darkgoldenrod1","darkgrey"))+
  theme_minimal()

#checking the association between categorical variables
tab1 = table(train$income, train$workclassN)
chisq.test(train$income, train$workclassN)
barplot(tab1,
        legend = TRUE,
        beside = TRUE,
        xlab = "Gender",
        ylab = "Frequency",
        main = "Association between Gender and Marital Status")
max(train$hours.per.week)
min(train$hours.per.week)


n2=length(unique(train$occupation))
median_occupation=numeric(length = n2)
trainnew = median(train$hours.per.week[train$occupation==" Other-service"])

train_df=data.frame(train)


median_occupation[1]=median(subset(train_df,occupation==" Other-service" )$hours.per.week)
median_occupation[2]=median(subset(train_df,occupation==" Sales"   )$hours.per.week)
median_occupation[3]=median(subset(train_df,occupation==" Prof-specialty")$hours.per.week)
median_occupation[4]=median(subset(train_df,occupation==" Adm-clerical")$hours.per.week)
median_occupation[5]=median(subset(train_df,occupation==" Exec-managerial")$hours.per.week)
median_occupation[6]=median(subset(train_df,occupation==" Farming-fishing" )$hours.per.week)
median_occupation[7]=median(subset(train_df,occupation==" Craft-repair" )$hours.per.week)
median_occupation[8]=median(subset(train_df,occupation==" Tech-support")$hours.per.week)
median_occupation[9]=median(subset(train_df,occupation==" Protective-serv")$hours.per.week)
median_occupation[10]=median(subset(train_df,occupation==" Transport-moving")$hours.per.week)
median_occupation[11]=median(subset(train_df,occupation==" Machine-op-inspct")$hours.per.week)
median_occupation[12]=median(subset(train_df,occupation==" Handlers-cleaners")$hours.per.week)
median_occupation[13]=median(subset(train_df,occupation==" Priv-house-serv")$hours.per.week)
median_occupation[14]=median(subset(train_df,occupation==" Armed-Forces" )$hours.per.week)

df_occupation=data.frame(names=unique(train$occupation),median=median_occupation)
df_occupation

ggplot(df_occupation, aes(x=names, y=median)) +
  geom_segment( aes(x=names, xend=names, y=0, yend=median), color="black") +
  geom_point( color="darkgoldenrod2", size=4, alpha=0.6) +
  theme_light() +
  coord_flip() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank()
  )

ggplot(train, aes(x =train$age,y=train$hours.per.week, fill = native.country)) +
  geom_bar(stat = "identity") +
  coord_polar("y", start = 0)+
  scale_color_viridis(discrete = TRUE, option = "D")+
  scale_fill_viridis(discrete = TRUE) +
  theme_minimal()

#Correlation matrix
library(corrplot)
train_num <- train %>% select_if(is.numeric)
cor_matrix <- cor(train_num)
ggheatmap <- ggcorrplot(cor_matrix, hc.order = TRUE, type = "lower", lab = TRUE, lab_size = 3)
# Show the plot
print(ggheatmap)

#Goodman kruskal plot
g_f=data.frame(train$educationN,train$occupation,train$race,train$sex,train$workclassN,train$marital.statusN,train$native.countryN)
names(g_f)=c("Education","ocucupation","Race","Gender","Workclass","Marital status","Native country")
GKmatrix=GKtauDataframe(g_f)
plot(GKmatrix)



#working hours with age

mean_hours=data.frame(aggregate(train$hours.per.week~train$age,FUN=mean))
mean_hours

#working hours with age

a=mean_hours[,1]
b=mean_hours[,2]
meanmat=data.frame(a,b)

plot(mean_hours)
fit=lm(train$education.num~train$hours.per.week,data=mean_hours2)
abline(fit)

colnames(train)
train_new=train[ ,c("occupation","race","sex","income","marital.statusN","educationN",
                    "workclassN","native.countryN")]

cats = apply(train_new, 2, function(x) nlevels(as.factor(x)))
cats
mca1=MCA(train_new,graph=TRUE)
mca1$eig
summary(mca1)
mca1_vars_df = data.frame(mca1$var$coord, Variable = rep(names(cats), cats))
mca1_vars_df
mca1_obs_df = data.frame(mca1$ind$coord)
mca1_obs_df

# plot of variable categories
ggplot(data = mca1_vars_df, aes(x = Dim.1, y = Dim.2, label = rownames(mca1_vars_df))) + 
  geom_hline(yintercept = 0, colour = "gray70") +
  geom_vline(xintercept = 0,colour = "gray70") + 
  geom_text(aes(colour = Variable))
  
# MCA plot of observations and categories
ggplot(data = mca1_obs_df, aes(x = Dim.1, y = Dim.2)) +
  geom_hline(yintercept = 0,  colour = "gray70") + 
  geom_vline(xintercept = 0, colour = "gray70") + 
  geom_point(colour = "gray50",  alpha = 0.7) +
  ggtitle("MCA plot of vobservations using R package FactoMineR") + scale_colour_discrete(name = "Variable")
  

# Create a score plot
plot.MCA(mca1, invisible = "quali", col.quali.sup = "blue")

# default biplot in FactoMineR
plot(mca1,cex=0.5)

library(vctrs)
colnames(train)

traindf2=data.frame(train[,c(1,3,5,7,9,10,11,12,13,15,16,17,18,19)])
res.famd <- FAMD(traindf2,ncp=10, graph = FALSE)
eig.val <- get_eigenvalue(res.famd)
head(eig.val)
fviz_screeplot(res.famd)
var <- get_famd_var(res.famd)

# Plot of variables
fviz_famd_var(res.famd, repel = TRUE)
# Contribution to the first dimension
fviz_contrib(res.famd, "var", axes = 1)
# Contribution to the second dimension
fviz_contrib(res.famd, "var", axes = 2)

# Plot the score plot of the observations in the FAMD space
plot(res.famd, select="ind", habillage = "none", cex = 0.8, 
     xlim = c(-5, 5), 
     ylim = c(-5, 5), xlab = "Dim1", ylab = "Dim2")

#score plot
plot(res.famd$ind$coord,col="azure4")


quanti.var <- get_famd_var(res.famd, "quanti.var")
quanti.var 

fviz_famd_var(res.famd, "quanti.var", col.var = "contrib", 
              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
              repel = TRUE)

quali.var <- get_famd_var(res.famd, "quali.var")
quali.var 

fviz_famd_var(res.famd, "quali.var", col.var = "contrib", 
              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"))

library(car)

