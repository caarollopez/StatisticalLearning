 # DATA PREPROCESSING AND VISUALIZATION

# loading packages
if (!require("tidyverse")){
  install.packages("tidyverse")
}
library(tidyverse)

if (!require("leaflet")){
  install.packages("leaflet")
}
library(leaflet)

if (!require("rgdal")){
  install.packages("rgdal")
}
library(rgdal)

if (!require("stringr")){
  install.packages("stringr")
}
library(stringr)

if (!require("htmltab")){
  install.packages("htmltab")
}
library(htmltab)

# set workind directory
setwd("~/Desktop/Statistical Learning/Data Preprocessing & Visualization")

# downloading first data fronm Ministerio del Interior
url = "https://infoelectoral.interior.gob.es/estaticos/docxl/apliextr/02201911_MESA.zip"

temp <- tempfile()
download.file(url,dest="data/MESA.zip")
unzip("data/MESA.zip", exdir = "data/")  

# reading data file and assigning names
participacion <- read.fwf("data/09021911.DAT",  
                          widths=c(2,4,2,1, 
                                   2,2,3,2,4,1, 
                                   7,7,7,7,7,7,7,7,7,7,7,   
                                   1),  
                          colClasses=c(rep("numeric",4),    
                                       rep("character",6),  
                                       rep("numeric",11),   
                                       "character"))

colnames(participacion) <- c("tipo","year","month","vuelta","ccaa","provincia","municipio","distrito","seccion","mesa","censo","censo_escrutinio","censo_cere","total_cere","votantes_primer","votantes_segundo","blanco","nulos","votos","afirmativos","negativos","datos_oficiales")

# taking a look
head(participacion)

# this is high granularity data: information up to mesa electoral
# we will base our analysis to: ccaa, provincia, censo, votos
participacion = participacion %>% select(ccaa, provincia, municipio, censo, votos)
# select function is used to choose whether a column of the data frame is selected or not


##########################FEATURE ENGINEERING####################################################################

# FILTER OUT some noisy information
# we focus on national elections
participacion = participacion %>% filter(ccaa<99, provincia<99, municipio<999)
# filter function is used to subset a data frame based on a provided condition
# if a row satisfies the condition, it must produce TRUE
# otherwise, non-satisfying rows will return NA values -> the row will be dropped

# FEATURE EXTRACTION
# build participation level (from votos and censo)
# build the CODIGOINE identification for municipality, which will be used later as the variable
# to merge some datasets
participacion = participacion %>% mutate(part=votos/censo,
                                         CODIGOINE = str_trim(paste0(participacion$provincia,participacion$municipio)))
# mutate function is used to create a new variable from a data set                             
# str_trim removes whitespace from start and end of string
# paste0 function is used to concatenate vectors after converting to character vectors

str(participacion)

head(participacion)

# convert char variables into factor ones
# this conversion will be used in R models as encoding (creation of dummies)
participacion$ccaa = as.factor(participacion$ccaa)
participacion$provincia = as.factor(participacion$provincia)
participacion$CODIGOINE = as.factor(participacion$CODIGOINE)

# Censo by CCAA: high variability
participacion %>% ggplot(aes(x=ccaa,y=censo)) + geom_boxplot(fill="lightblue")


# AGGREGATION
# we will use information at municipality level, hencew we are going to aggregate results to that level
# we will reduce noise (variability) at the price of losing also some information
part.aggr <- participacion %>%  
  group_by(ccaa,provincia,municipio,CODIGOINE) %>%  
  summarize(total_votos=sum(votos),total_censo=sum(censo)) %>%  
  mutate(total_part = total_votos/total_censo)

# group_by function groups a selected set of rows in a summary set of rows
# taking into account the values of one or more columns

# summarize function creates a new data frame
# it will have one (or more) rows for each combination of grouping variables
# mutate function is used to create a new variable from a data set 

head(part.aggr)

# participacion by provincia
part.aggr %>% ggplot(aes(x=provincia,y=total_part)) + geom_boxplot(fill="lightblue")

# boxplots are very useful to identify differences in provincias

# OUTLIERS
# outliers are atypical/extreme values that are far from the rest of the values
# one of the most useful tools to identify outliers is the boxplot  -> univariate identification

# identification by 3-sigma rule
mu <- mean(part.aggr$total_part)
sigma <- sd(part.aggr$total_part)

sum(part.aggr$total_part < mu - 3*sigma | part.aggr$total_part > mu + 3*sigma)


# identification by IQR
QI <- quantile(part.aggr$total_part, 0.25)
QS <- quantile(part.aggr$total_part, 0.75)
IQR = QS-QI

sum(part.aggr$total_part < QI - 1.5*IQR | part.aggr$total_part > QS + 1.5*IQR)

# depending on the context, we must decide what to do 
# 1 remove them: can a municipality have a participation greater than 100% or smaller than 0%?
# 2 leave them: can a municipality have a participation smaller than 40%?

# CAN WE CREATE MORE VARIABLES?
# municipalities by ccaa
part.aggr %>% ggplot(aes(x=reorder(ccaa, ccaa, length))) +geom_bar(aes(fill=ccaa)) + 
  labs(caption="Municipios por CCAA",
       x = "", y = "")+ theme(legend.position="none")

#####################################################################################
#####################################################################################
# to explain participation level, besides provincia or municipality, we need more variables
# like the unemployment rate, population age, income for each municipality

# downloading the unemployment rate in Spain by municipality
url = "https://sede.sepe.gob.es/es/portaltrabaja/resources/sede/datos_abiertos/datos/Paro_por_municipios_2019_csv.csv"
paro = read.csv2(url, skip=1, header=T, sep=";", encoding="latin1")
#paro = read.csv2("data/Paro_por_municipios_2019_csv.csv", skip=1, header=T, sep=";", encoding="latin1")

str(paro)

head(paro)

# select relevant variables and filter out the month 
# we are just interested in 2019

# AGGREGATION
paro = paro %>% select(c(1,4,6,7,8,9))
colnames(paro) = c("Mes", "CCAA", "Provincia", "CODIGOINE", "Municipio", "Paro")

paro$Mes = as.factor(paro$Mes)
paro.aggr <- paro %>%   
  group_by(CCAA, Provincia, Municipio, CODIGOINE) %>%   
  summarize(total_paro=mean(Paro)) 

# we have just the same for CODIGOINE -> will allows us to join the data sets

# MERGE
# CODIGOINE must be in the same format (char) and with same numbers
paro.aggr$CODIGOINE=factor(str_pad(paro.aggr$CODIGOINE, 5, pad = "0"))
total.data=merge(part.aggr, paro.aggr, by="CODIGOINE", all.x=T)


# MISSING VALUES
# non-present value in a variable
# most of the real datasets have missing values
# we need to identify them and decide what to do 

# they are usually represented by NULL or NA
# sometimes they are given by specific codes (for instance 99999)
sum(is.na(total.data))
# 32 missing values!

# distribution of NAs using mice package
library(mice)
md.pattern(total.data)


# there are 8 municipalities with missing values (in the 4 paro variables)
# what to do??
# 1 if they are just a few and not relevant, we can delete them (say less than 55 of the sample)
# 2 otherwise:
## we can remove rows when most of their corresponding variables are empty
## we can remove columns when most of their corresponding rows are empty
## we can impute NAS
### simple imputation (use the mean for non-NAs, or the media, mode, etc)
### regressions: train with non-NAs to predict NAs
### k-nearest neighbour (kNN) imputation
### multiple imputation: for each NA, multiple imputations are found to capture better sample variability
### and then a pooling approach is performed to have the final imputation


# we only have a few NAs, let's remove them (8 rows)
total.data <- na.omit(total.data)
# na.omit function removes any incomplete cases in a vector, matrix or data frame


# regression to explain participation by provincia and paro
lm.fit = lm(total_part ~ provincia, total.data) 
resid = residuals(lm.fit)
qplot(total.data$total_paro, resid)

summary(lm(total_part ~ provincia + total_paro, total.data))
# paro is not informative: it is in absolute values (not relative), hence it is related
# to population mainly

# we need total population by municipality to create Paro in percentage


#######################################################################################
#######################################################################################
municipios.moredata = read.csv(url("http://www.est.uc3m.es/nogales/municipios2017.csv"), header=T, sep=",")
# municipios.moredata = read.csv("data/municipios2017.csv", header=T, sep=",")
head(municipios.moredata)

# MERGE
# CODIGOINE must be in the same format (char) and with same numbers
municipios.moredata$CODIGOINE=factor(str_pad(municipios.moredata$CODIGOINE, 5, pad = "0"))
total.data=merge(total.data, municipios.moredata, by="CODIGOINE")

# FEATURE EXTRACTION
total.data$habitantes = total.data$HOMBRES+total.data$MUJERES
total.data$densidad = total.data$habitantes/sqrt(total.data$SUPERFICIE)
total.data$sexratio = total.data$HOMBRES/total.data$MUJERES
total.data$jovenes = total.data$de0.18/total.data$habitantes
total.data$seniors = total.data$masde68/total.data$habitantes

# more feature extraction: now we can create the unemployment rate
total.data$paro = total.data$total_paro/total.data$habitantes

# explaint participation by unemploymen rate
lm.fit = lm(total_part ~ provincia, total.data) 
resid = residuals(lm.fit)
qplot(total.data$paro, resid)

summary(lm(total_part ~ provincia + paro, total.data))

# beta coefficient for paro is the slope of the scatterplot
# we can aslo reduce the noise by discretizing variables, at the cost of reducing also the information


# OUTLIERS
resid %>% as.data.frame() %>% ggplot(aes(x=resid)) + geom_boxplot(fill="lightblue") 

# 3-sigma, IQR were based on univariate information
# multivariate information is better to detect outliers, but more difficult
## based on regression (target-based outliers)
## based on multivariate tools (clustering, Mahalanobis distance,etc)
## based on dimensionality reduction (PCA)

# use the package outliers to detect municipalities with participation anomalies (respect to unemployment)
if (!require("outliers")){
  install.packages("outliers")
}
library(outliers)

idx = outlier(resid, logical = T)
# outlier
total.data[idx, ]
# no unemployment but 37% election participation, it is a very small town

# DISCRETIZE AGE
# Create the column young, and indicate whether town is young or not
total.data$EdadG[total.data$jovenes > .17] <- 'Young'
total.data$EdadG[total.data$jovenes <= .17] <- 'Senior'
total.data$EdadG = factor(total.data$EdadG)

# Show counts
prop.table(table(total.data$EdadG))

# DISCRETIZE UNEMPLOYMENT
total.data$paroD[total.data$paro <= 0.05] <- 'bajo'
total.data$paroD[total.data$paro <= 0.10 & total.data$paro > .05] <- 'medio'
total.data$paroD[total.data$paro > 0.10] <- 'alto'

total.data$paroD = factor(total.data$paroD)

prop.table(table(total.data$paroD))
ggplot(filter(total.data,paro>0), aes(paro, total_part, group=Provincia, size=habitantes, color=EDAD_MEDIA)) + scale_x_sqrt(breaks=c(0.05,0.1), label=c("5%","10%"))+
  geom_point(alpha=0.5) + geom_smooth(method=lm,se=F) +
  facet_wrap(~ Provincia) +
  scale_color_gradient(low="green", high="red") +
  theme_minimal()+ theme(legend.position="none") + 
  labs(title = "Participación elecciones vs Paro", subtitle="(color denota edad media municipio)",caption="uc3m - statistical learning",
       x = "", y = "")

########################################################################################
#########################################################################################
# let's try to get now the income (per capita)
# only for municipalities with more than 1000 inhabitants
renta=htmltab("https://www.agenciatributaria.es/AEAT/Contenidos_Comunes/La_Agencia_Tributaria/Estadisticas/Publicaciones/sites/irpfmunicipios/2018/jrubik7fe28e5d4daeab97eaf47efe29f0716914ab405e.html")

renta = renta %>% drop_na()
# drop_na drops rows containing missing values

# FEATURE EXTRACTION
renta = renta %>% select(c(1,4,7))
renta$V1 = str_extract(renta$V1, "[0-9]+")
colnames(renta)=c("CODIGOINE", "habitantes2", "renta")
renta$habitantes2 = as.numeric(gsub(".","", renta$habitantes2, fixed=T))
renta$renta = as.numeric(gsub(".","", renta$renta, fixed=T))

# MERGE
# CODIGOINE must be in the same format (char) and with same numbers
renta$CODIGOINE=factor(str_pad(renta$CODIGOINE, 5, pad = "0"))
total.data=merge(total.data, renta, by="CODIGOINE", all.x=T)
sum(is.na(total.data$renta))

# MISSING VALUES: advanced approaches
#- Simple imputation (use the mean for non-NAs, or the median, mode, etc.)
#- Regressions: train with non-NAs to predict NAs
#- k-nearest neighbour (kNN) imputation
#- Multiple imputation: for each NA, multiple imputations are found to capture better sample variability, and then a pooling approach is performed to have the final imputation 


## IMPUTATION by the median
total.data$renta_imp_median = total.data$renta
total.data$renta_imp_median[is.na(total.data$renta)] = median(total.data$renta, na.rm=T)

# same but grouping by provincia
total.data = total.data %>% group_by(provincia) %>%
  mutate(renta_imp_provincia=ifelse(is.na(renta),median(renta,na.rm=TRUE),renta))

# still NAs in Pais Vasco and NAvarra

## IMPUTATION by regression
# 1 train a model with non-NAs
i.na = is.na(total.data$renta)
renta.model <- lm(renta ~ paro + EDAD_MEDIA + densidad + total_part, data = total.data[!i.na,])
summary(renta.model)

# 2 predict the NAs
total.data$renta_imp_reg = total.data$renta
total.data$renta_imp_reg[i.na]=predict(renta.model, newdata = total.data[i.na,])

# valid if R^2 is relatively high, not the case

## MULTIPLE IMPUTATION
# for each NA, multiple imputations are found to capture better the sample variability
# and then a pooling approach is performed to have the final imputaiton

# MICE (Multivariate Imputation via Chained Equations) is widely used

# with mice, each variable has its own imputation method (for continuous variables, binary, categorical, etc)

set.seed(42)
mice.obj=mice(total.data[,c(2,3,7,16,24,25,26,27,28,33)], method = 'rf')

mice.obj.imp=mice::complete(mice.obj)
total.data$renta_imp_mice = mice.obj.imp$renta

# sort income
total.data %>% arrange(desc(renta)) %>% select(Municipio, Provincia, renta, EdadG, paroD) %>% head(10)
# arrange orders the rows of a data fram by the values of selected columns

total.data %>% arrange(desc(renta_imp_mice)) %>% select(Municipio, Provincia, renta, renta_imp_mice, EdadG, paroD, habitantes) %>% head(10)


###############################################################################
# to map some variables, we need some boundaries (polygons) for the municipalities
municipios <- readOGR("data/Municipios_IGN/Municipios_IGN.shp")
WGS84 <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")	
municipios <- spTransform(municipios,CRSobj=WGS84)

datos.mapa <- merge(municipios,total.data,by="CODIGOINE")	

# now we can plot, for instance, our main variable participation

pal <- colorQuantile("Blues", datos.mapa$total_part, na.color="white")

datos.mapa %>% leaflet() %>% 
  addProviderTiles(providers$CartoDB.Positron) %>% 
  # addTiles("MapBox") %>%
  setView(lng=-3.69, lat=40.42, zoom = 9) %>%
  addPolygons(fillColor = ~ pal(total_part),fillOpacity = 0.6,color = "white",weight = .5, label = ~ paste0(datos.mapa$Municipio," ", round(total_part*100,2),"%")) 

# or we can plot the unemployment rate in the same way

pal <- colorQuantile("Greens", datos.mapa$paro, na.color="white")
# pal <- colorFactor(palette = 'Greens', domain = datos.mapa$paroD, na.color="white")

datos.mapa %>% leaflet() %>% 
  addProviderTiles(providers$CartoDB.Positron) %>% 
  # addTiles("MapBox") %>%
  setView(lng=-3.69, lat=40.42, zoom = 9) %>%
  addPolygons(fillColor = ~ pal(paro),fillOpacity = 0.6,color = "white",weight = .5, label = ~ paste0(datos.mapa$Municipio," ", round(paro*100,2),"%")) 

# España vacia

total.data$habitantesD[total.data$habitantes <= 5000] <- 'pocos'
total.data$habitantesD[total.data$habitantes > 5000] <- 'muchos'
total.data$habitantesD = as.factor(total.data$habitantesD)

datos.mapa.empty <- merge(municipios,filter(total.data, habitantes<5000),by="CODIGOINE")	

pal <- colorFactor(
  palette = c('white', 'skyblue2'),
  domain = datos.mapa.empty$habitantesD,na.color="white"
)

datos.mapa.empty %>% leaflet() %>% 
  addProviderTiles(providers$CartoDB.Positron) %>% 
  # addTiles("MapBox") %>%
  setView(lng=-3.69, lat=40.42, zoom = 6) %>%
  addPolygons(fillColor = ~ pal(habitantesD),weight = .5,fillOpacity = 0.8,color = "white", label = ~ paste0(datos.mapa.empty$Municipio," ", habitantes," hab")) 



####################################################################
# ANALYZE INCOME IN SPAIN

# with incomplete information about income
data.map = merge(municipios,total.data,by="CODIGOINE")

pal <- colorQuantile("Blues", data.map$renta, na.color="white")

data.map	 %>% leaflet() %>% 
  addProviderTiles(providers$CartoDB.Positron) %>% 
  setView(lng=-3.69, lat=40.42, zoom = 9) %>%
  addPolygons(fillColor = ~ pal(renta),opacity = 0.2,fillOpacity = 0.6,color = "white",weight = .5, label = ~ paste0(Municipio,": ", renta))


# with complete infotmation
pal <- colorQuantile("Blues", data.map$renta_imp_mice, na.color="white")

data.map	 %>% leaflet() %>% 
  addProviderTiles(providers$CartoDB.Positron) %>% 
  setView(lng=-3.69, lat=40.42, zoom = 9) %>%
  addPolygons(fillColor = ~ pal(renta_imp_mice),opacity = 0.2,fillOpacity = 0.6,color = "white",weight = .5, label = ~ paste0(Municipio,": ", renta_imp_mice))

# ANALYZE PARTICIPATION
# plot the relation between Participation and Income (at province level)
total.data %>% filter(renta_imp_mice>10000) %>%
  ggplot(aes(x=renta_imp_mice, y=total_part, group=Provincia, size=habitantes, color=EDAD_MEDIA)) + 
  scale_x_log10(breaks=c(20000,60000), label=c("20K","60K"))+
  geom_point(alpha=0.5) + geom_smooth(method=lm,se=F) +
  facet_wrap(~ Provincia) +
  scale_color_gradient(low="green", high="red") +
  theme_minimal()+ theme(legend.position="none") + 
  labs(title = "Participación elecciones vs Renta", subtitle="(color denota edad media municipio)", caption="uc3m - statistical learning",
       x = "", y = "")

#  make some regression models to understand better the relations
lm(total_part ~ Provincia, total.data) %>% summary()

lm(total_part ~ Provincia + log(paro+1)*log(renta_imp_mice)+poly(EDAD_MEDIA,2), total.data) %>% summary()

cor(log(total.data$renta_imp_mice),log(total.data$paro+1))

lm.fit = lm(total_part ~ Provincia, total.data) 
resid = residuals(lm.fit)
qplot(log(total.data$paro+1), resid)

qplot(log(total.data$renta_imp_mice), resid)

qplot(total.data$EDAD_MEDIA, resid)

# relations, beside Provincia, are weak


# SCALE
# for some models, it is required to have different ranges
# the models can then equally weight each feature
# otherwise, a feature measured in meters will have more weight than the same feature in km

# the disadvantage is that we lose variability information

# we can either STANDARDIZE (remove the mean and divide by the standard deviation)
total.data$rentaNormalized <- scale(total.data$renta_imp_mice)
summary(total.data$rentaNormalized)

# scale effect
boxplot(total.data[,c("total_part", "EDAD_MEDIA", "densidad", "paro", "renta_imp_mice")], las = 2, cex.max = 0.8, col = "darkslateblue")
boxplot(scale(total.data[,c("total_part", "EDAD_MEDIA", "densidad", "paro", "renta_imp_mice")]), las = 2, cex.max = 0.8, col = "darkslateblue")

# variances are the same = 1


# or NORMALIZE
total.data$rentaNormalized <- (total.data$renta_imp_mice - min(total.data$renta_imp_mice))/(max(total.data$renta_imp_mice) - min(total.data$renta_imp_mice))
summary(total.data$rentaNormalized)

# ENCODING
# most statistical and machine-learning models require the predictors to be in 
# some sort of numeric encoding to be used
# for example, linear regression required numbers so that it can assign slopes to each of the predictors


# the most common encoding is to make simply dummy variables
# if we have a predictor with c levels, c- 1 dummies are needed
# in this way, the X matrix is full rank
lm(total_part ~ Provincia + log(paro+1) + log(renta_imp_mice)+poly(EDAD_MEDIA,2), total.data) %>% summary()

# you need to encode (or use a factor variable) for instance: month of the year, day of the week, etc.


# CONCLUSIONS

# feature engineering is the process of getting features (variables) useful as input
# for machine-learning tools
# better input = better prediction

# feature extraction creates new features from original raw variables

# do not confuse with feature selection: how to get a subset of the features

# feature selection or variable selection is better understood in regression context

# PCA can also be used for feature extraction

# you need to deal always with outliers and missing values







