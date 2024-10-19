library(ggplot2)
library(dplyr)
library(readxl)
library(tidyverse)
library(R2jags)

#----
#A) Fertilizer use intensity
fert.df <- read.csv("C:\\Users\\sofyc\\OneDrive\\Desktop\\Int. Org. in OS\\project final\\colombia_fertilizer_use_intensity.csv", 
                    header = TRUE)
str(fert.df)
fert.df$Year #1961-2021

#Plot
ggplot(fert.df, aes(x = Year, y = Value)) +
  geom_line(color = "lightblue", size = 1) +
  geom_point(color = "blue", size = 2) +
  labs(title = "Colombia's Fertilizer Use Intensity",
       x = "Year",
       y = "Fertilizer Use Intensity (kg per hectare)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 12),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 10))

summary(fert.df$Value)
# Min. 1st Qu.  Median  Mean  3rd Qu.    Max. 
#4.63   25.92   52.12   54.97   73.55  130.81
fert.df[fert.df$Value==130.81, ] #the maximum in 2014
fert.df[fert.df$Value==4.63, ] #the minimum in 1962
fert.df[fert.df$Year==2021, ] #52.86 --> the last one registered

#B) Pesticides use intensity
pest.df <- read.csv("C:\\Users\\sofyc\\OneDrive\\Desktop\\Int. Org. in OS\\project final\\colombia_pesticides_use_intensity.csv",
                    header=TRUE)
str(pest.df)
pest.df$Year #1990-2021

#Plot
ggplot(pest.df, aes(x = Year, y = Value)) +
  geom_line(color = "lightpink", size = 1) +
  geom_point(color = "darkviolet", size = 2) +
  labs(title = "Colombia's Pesticides Use Intensity",
       x = "Year",
       y = "Pesticides Use Intensity (kg per hectare)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 12),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 10))

summary(pest.df$Value)
# Min. 1st Qu.  Median  Mean  3rd Qu.    Max. 
#3.10    7.19   14.06   13.56   17.69   32.63
pest.df[pest.df$Value==32.63, ] #the maximum in 2005
pest.df[pest.df$Value==3.10, ] #the minimum in 1993
pest.df[pest.df$Year==2021, ] #8.74

#C) Agricultural component of water stress
#Level of water stress
water.df <- read.csv("C:\\Users\\sofyc\\OneDrive\\Desktop\\Int. Org. in OS\\project final\\colombia_water_stress.csv",
                     header=TRUE)
str(water.df)
water.df$Year #2000-2021 for each sector
#Sectors: Agriculture, Industries, No breakdown, Services

#I select just the level of water stress for Agriculture (the first 22 elements)
water.df <- water.df[1:22,]

#Plot
ggplot(water.df, aes(x = Year, y = Value)) +
  geom_line(color = "orange", size = 1) +
  geom_point(color = "darkred", size = 2) +
  labs(title = "Colombia's Agricultural component of Water Stress",
       x = "Year",
       y = "Agriculture component of water stress") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 12),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 10))

summary(water.df$Value)
# Min. 1st Qu.  Median  Mean  3rd Qu.    Max. 
#0.7400  0.8775  1.6100  1.9386  3.0525  3.8100
water.df[water.df$Value==3.8100, ] #the maximum in 2021
water.df[water.df$Value==0.7400, ] #the minimum in 2000

#D) GHG Emissions intensity
#- Farm gate emissions
emission.df <- read.csv("C:\\Users\\sofyc\\OneDrive\\Desktop\\Int. Org. in OS\\project final\\colombia_emissions_total_CO2.csv",
                        header=TRUE)
str(emission.df)

#Aggregation by year
em.year <- emission.df %>%
  group_by(Year) %>%
  summarise(Total_Emissions = sum(Value, na.rm = TRUE)) %>%
  as.data.frame()

em.year #1961-2021, unit=kt
em.year$Total_Emissions <- em.year$Total_Emissions * 1000000 #unit=kg
plot(em.year$Year, em.year$Total_Emissions, type="l", col="darkgray", lwd=2,
     xlab="Year", ylab="Farm gate emissions")

#- Value of Agricultural Production
agr.prod.df <- read.csv("C:\\Users\\sofyc\\OneDrive\\Desktop\\Int. Org. in OS\\project final\\colombia_value_prod_agriculture.csv",
                        header=TRUE)
str(agr.prod.df)

#Aggregation by year
agr.prod.year <- agr.prod.df %>%
  group_by(Year) %>%
  summarise(Total_Agr_Prod = sum(Value, na.rm = TRUE)) %>%
  as.data.frame()

agr.prod.year #1961-2022, unit="1000 USD"
#Taking the range 1961-2021
agr.prod.year <- agr.prod.year[1:61,]
agr.prod.year$Total_Agr_Prod <- agr.prod.year$Total_Agr_Prod * 1000 #unit=USD

#Merging the two dataframe
GHGE.df <- merge(em.year, agr.prod.year, by = "Year")
#Calculating the proxy sub-indicator: Greenhouse Gas Emissions Intensity
GHGE.df$Value <- (GHGE.df$Total_Emissions/GHGE.df$Total_Agr_Prod)
GHGE.df$Unit <- rep("kgCO2eq per USD", nrow(GHGE.df)) #kgCO2eq per constant 2014-2016 USD

#Plot for GHG Emissions intensity
ggplot(GHGE.df, aes(x = Year, y = Value)) +
  geom_line(color = "lightgreen", size = 1) +
  geom_point(color = "darkgreen", size = 2) +
  labs(title = "Colombia's Greenhouse Gas Emissions Intensity",
       x = "Year",
       y = "GHG Emissions Intensity (kgCO2eq per USD)") + 
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 12),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 10))

summary(GHGE.df$Value)
# Min. 1st Qu.  Median  Mean  3rd Qu.    Max. 
#3.462   4.693   9.770   8.320  11.816  14.389
GHGE.df[GHGE.df$Value==14.389, ] #the maximum in 1994
GHGE.df[GHGE.df$Value==3.462, ] #the minimum in 1989
GHGE.df[GHGE.df$Year==2021, ] #9.906238

###############################################################################

#To calculate the Status Assessment and Trend Assessment, first I do some 
#preliminary controls:
#Checking of NA values
sum(is.na(fert.df$Value)) #0
sum(is.na(pest.df$Value)) #0
sum(is.na(water.df$Value)) #0
sum(is.na(fert.df$Value)) #0
sum(is.na(GHGE.df$Value)) #0

#Status assessment:
#Quantile distro.
#Function to assign the category
assign.category <- function(x, q20, q40, q60, q80) {
  ifelse(x <= q20, 1,
         ifelse(x <= q40, 2,
                ifelse(x <= q60, 3,
                       ifelse(x <= q80, 4,
                              5))))
}

#A)
quantiles.FUI <- quantile(fert.df$Value, probs = seq(0, 1, by = 0.2))
categories.FUI <- assign.category(fert.df$Value, quantiles.FUI[2],
                                  quantiles.FUI[3], quantiles.FUI[4],
                                  quantiles.FUI[5])
categories.FUI <- cut(fert.df$Value, 
                      breaks = quantiles.FUI, 
                      include.lowest = TRUE, 
                      labels = 1:(length(quantiles.FUI) - 1))
categories.FUI <- as.integer(as.character(categories.FUI))
categories.FUI <- 6 - categories.FUI
print(categories.FUI[length(categories.FUI)]) #3

#B)
quantiles.PUI <- quantile(pest.df$Value, probs = seq(0, 1, by = 0.2))
categories.PUI <- assign.category(pest.df$Value, quantiles.PUI[2],
                                  quantiles.PUI[3], quantiles.PUI[4],
                                  quantiles.PUI[5])
categories.PUI <- cut(pest.df$Value, 
                      breaks = quantiles.PUI, 
                      include.lowest = TRUE, 
                      labels = 1:(length(quantiles.PUI) - 1))
categories.PUI <- as.integer(as.character(categories.PUI))
categories.PUI <- 6 - categories.PUI
print(categories.PUI[length(categories.PUI)]) #4

#C)
#Since the function of agr. water stress is increasing, the allocation of the
#categories should reflect this trend. Higher values should reflect a less
#desirable category.
rel.water <- water.df$Value/100 #Normalizing values
quantiles.agr.WS <- quantile(rel.water, probs = seq(0, 1, by = 0.2))
categories.agr.WS <- assign.category(rel.water, quantiles.agr.WS[2],
                                     quantiles.agr.WS[3], quantiles.agr.WS[4],
                                     quantiles.agr.WS[5])
categories.agr.WS <- cut(rel.water, 
                      breaks = quantiles.agr.WS, 
                      include.lowest = TRUE, 
                      labels = 1:(length(quantiles.agr.WS) - 1))
categories.agr.WS <- as.integer(as.character(categories.agr.WS))
categories.agr.WS <- 6 - categories.agr.WS
print(categories.agr.WS[length(categories.agr.WS)]) #1

#D)
quantiles.GHGE <- quantile(GHGE.df$Value, probs = seq(0, 1, by = 0.2))
categories.GHGE <- assign.category(GHGE.df$Value, quantiles.GHGE[2],
                                   quantiles.GHGE[3], quantiles.GHGE[4],
                                   quantiles.GHGE[5])
categories.GHGE <- cut(GHGE.df$Value, 
                      breaks = quantiles.GHGE, 
                      include.lowest = TRUE, 
                      labels = 1:(length(quantiles.GHGE) - 1))
categories.GHGE <- as.integer(as.character(categories.GHGE))
categories.GHGE <- 6 - categories.GHGE
print(categories.GHGE[length(categories.GHGE)]) #3

#Barplot Summary
categories <- data.frame(
  Indicator = c("FUI", "PUI", "Agr.WS", "GHGE"),
  Category = c(categories.FUI[length(categories.FUI)], 
               categories.PUI[length(categories.PUI)], 
               categories.agr.WS[length(categories.agr.WS)], 
               categories.GHGE[length(categories.GHGE)])
)

category.colors <- c("1" = "red", 
                     "2" = "orange", 
                     "3" = "yellow", 
                     "4" = "lightgreen", 
                     "5" = "darkgreen")

bar.colors <- category.colors[as.character(categories$Category)]

barplot(height = categories$Category,
        names.arg = categories$Indicator,
        col = bar.colors,
        main = "Categories Status Assessment",
        xlab = "Proxy sub-indicator",
        ylab = "Category",
        border = "black",
        cex.names = 0.8,
        width = 0.05)
legend("topright",
       legend = c("Undesirable", "Undesirable", "Acceptable", "Desirable"),
       fill = c("red", "orange", "yellow", "lightgreen", "darkgreen"),
       border = "white",
       box.lty = 0, 
       cex = 0.5)

##--
#Trend assessment:
#Function to calculate CAGR (to measure Trend Assessment at target level)
CAGR.fun <- function(df, base.year, ck.year, target, target.year=2030, logit=FALSE){
  start <- min(df$Year)
  
  first.val <- df$Value[df$Year == base.year]
  final.val <- df$Value[df$Year == ck.year]
  
  if(df$Unit[1]=="%"){
    first.val <- df$Value[df$Year == base.year]
    final.val <- df$Value[df$Year == ck.year]
  }
  
  nY.curr <- ck.year - base.year
  nY.target <- target.year - base.year
  
  #Actual CAGR
  if(logit) {
    y <- car::logit(p = final.val, adjust = 0.005)
    y0 <- car::logit(p = first.val, adjust = 0.005)
    if(!is.null(target)){
      ytgt <- car::logit(p = target, adjust = 0.005)
    }
  }else{
    y <- log(abs(final.val))
    y0 <- log(abs(first.val))
    if(!is.null(target)){
      ytgt <- log(abs(target))
    }
  }
  
  actual.growth <- (1/nY.curr)*(y - y0)
  if(logit) actual.CAGR <- actual.growth
  else actual.CAGR <- exp(actual.growth) - 1
  
  #CAGR to achieve the target (if required)
  if(!is.null(target)){
    req.growth <- (1/nY.target)*(ytgt - y0)
    if(logit) req.CAGR <- req.growth
    else  req.CAGR <- exp(req.growth) - 1
    ratio <- actual.CAGR/req.CAGR
  }
  else{
    req.CAGR <- NA
    ratio <- NA 
  }
  
  #Managing the results
  names(actual.CAGR) <- NULL
  names(req.CAGR) <- NULL
  names(ratio) <- NULL
  res.out <- list(start.Year=start, last.Year=ck.year, 
                  n=nrow(df), nNA=sum(is.na(df$Value)),
                  baseline.Year=base.year, target.Year=target.year,
                  target.Value=ifelse(is.na(target), NA, target), 
                  actual.CAGR=actual.CAGR, required.CAGR=req.CAGR, 
                  actualVSrequired=ratio, 
                  base=first.val,last=final.val)
  
  return(res.out)
}

#Function for the symbol category
symbol.name.cat <- function(cagr, df_cagr){
  if(cagr < -0.001){
    df_cagr$Category <- "Deterioration"
    df_cagr$Symbol <- "<<"
    df_cagr$cat_color <- "red"
  }
  if(-0.001 <= cagr & cagr < -0.0005){
    df_cagr$Category <- "Slight Deterioration"
    df_cagr$Symbol <- "<"
    df_cagr$cat_color <- "orange"
  }
  if(-0.0005 <= cagr & cagr < 0.001){
    df_cagr$Category <- "Slight or no improvement"
    df_cagr$Symbol <- ">="
    df_cagr$cat_color <- "lightgreen"
  }
  if(cagr > 0.001){
    df_cagr$Category <- "Improvement"
    df_cagr$Symbol <- ">>"
    df_cagr$cat_color <- "darkgreen"
  }
  return(df_cagr)
}

#Now I apply the function to the sub-indicators
fert_cagr <- CAGR.fun(fert.df, 2015, 2021, target=NULL)
fert_cagr$actual.CAGR <- -fert_cagr$actual.CAGR
fert_cagr$actual.CAGR #0.1381117
fert_cagr <- symbol.name.cat(fert_cagr$actual.CAGR, fert_cagr)


pest_cagr <- CAGR.fun(df=pest.df, base.year=2015, ck.year=2021, target=NULL)
pest_cagr$actual.CAGR <- -pest_cagr$actual.CAGR
pest_cagr$actual.CAGR #0.008332713
pest_cagr <- symbol.name.cat(pest_cagr$actual.CAGR, pest_cagr)

water_cagr <- CAGR.fun(df=water.df, base.year=2015, ck.year=2021, target=NULL)
water_cagr$actual.CAGR <- -water_cagr$actual.CAGR
water_cagr$actual.CAGR #-0.05330334
water_cagr <- symbol.name.cat(water_cagr$actual.CAGR, water_cagr)

GHGE_cagr <- CAGR.fun(df=GHGE.df, base.year=2015, ck.year=2021, target=NULL)
GHGE_cagr$actual.CAGR <- -GHGE_cagr$actual.CAGR
GHGE_cagr$actual.CAGR #-0.01580278
GHGE_cagr <- symbol.name.cat(GHGE_cagr$actual.CAGR, GHGE_cagr)

#Plot summary
categories2 <- data.frame(
  Indicator = c("FUI", "PUI", "Agr.WS", "GHGE"),
  Category = c(fert_cagr$actual.CAGR, 
               pest_cagr$actual.CAGR, 
               water_cagr$actual.CAGR, 
               GHGE_cagr$actual.CAGR),
  cat_color = c(fert_cagr$cat_color, 
                pest_cagr$cat_color, 
                water_cagr$cat_color, 
                GHGE_cagr$cat_color)
)

barplot(height = categories2$Category,
        names.arg = categories2$Indicator,
        col = categories2$cat_color,
        main = "Categories Trend Assessment",
        xlab = "Proxy sub-indicator",
        ylab = "Category",
        border = "black",
        cex.names = 0.8,
        width = 0.05)
legend("topright",
       legend = c("Undesirable", "Acceptable", "Desirable", "Desirable"),
       fill = c("red", "orange", "lightgreen", "darkgreen"),
       border = "white",
       box.lty = 0, 
       cex = 0.7)


################################################################################
#To obtain a composite index of agriculture sustainability, I use the CAGR
#calculated before as the weights of the index.
#I take the range of years 2000-2021 since it is the only one in common
#between the sub-indicators.

#First step: Creating a dataframe with the sub-indicators.
composite.df <- data.frame(
  Year = seq(2000, 2021, by=1),
  Fertilizer.Intensity = fert.df$Value[fert.df$Year >= 2000 & fert.df$Year <= 2021],
  Pesticides.Intensity = pest.df$Value[pest.df$Year >= 2000 & pest.df$Year <= 2021],
  Level.Water.Stress = water.df$Value[water.df$Year >= 2000 & water.df$Year <= 2021],
  GHGE.Intensity = GHGE.df$Value[GHGE.df$Year >= 2000 & GHGE.df$Year <= 2021]
)
composite.df

#Second step: Normalizing data
norm.fun <- function(x){
  return( (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)) )
}

composite.df$Fertilizer.Intensity <- norm.fun(composite.df$Fertilizer.Intensity)
composite.df$Pesticides.Intensity <- norm.fun(composite.df$Pesticides.Intensity)
composite.df$Level.Water.Stress <- norm.fun(composite.df$Level.Water.Stress)
composite.df$GHGE.Intensity <- norm.fun(composite.df$GHGE.Intensity)

#Third step: Assigning weights
weights <- c(
  fert_cagr$actual.CAGR,
  pest_cagr$actual.CAGR,
  water_cagr$actual.CAGR,
  GHGE_cagr$actual.CAGR
)

composite.df$Idx.Agr.Sustainability <- (
    weights[1] * composite.df$Fertilizer.Intensity +
    weights[2] * composite.df$Pesticides.Intensity +
    weights[3] * composite.df$Level.Water.Stress +
    weights[4] * composite.df$GHGE.Intensity
)

#Look at
composite.df[,c(1,6)] #year and corresponding value
min(composite.df[,6]) #-0.05260097 --> 2018
max(composite.df[,6]) #0.1159529 --> 2011

################################################################################
################################################################################
#WATER STRESS SECTION
#Avocado data EUROSTAT
imp.to.EU <- read_xlsx("C:\\Users\\sofyc\\OneDrive\\Desktop\\Int. Org. in OS\\project final\\avocado\\avocado_eurostat.xlsx",
                       sheet="Sheet 2")
imp.to.EU <- as.data.frame(imp.to.EU)
imp.to.EU <- imp.to.EU[, c(1,4)]
colnames(imp.to.EU) <- c("Year", "EU")

#Selecting the period with data 2001-2023
imp.to.EU <- imp.to.EU[14:36, ]
imp.to.EU$EU <- as.numeric(imp.to.EU$EU)
#Re-indexing
rownames(imp.to.EU) <- 1:nrow(imp.to.EU)

barplot(height = imp.to.EU$EU,
        names.arg = imp.to.EU$Year,
        col = "orchid",
        border = "white",
        xlab = "Years",
        ylab = "Imports to EU (100KG)",
        main = "Annual Imports to EU from Colombia",
        las = 2,
        cex.names = 0.8,
        cex.axis = 0.7)

#Avocado data FAOSTAT
imp.ex.avocado <- read.csv("C:\\Users\\sofyc\\OneDrive\\Desktop\\Int. Org. in OS\\project final\\avocado\\avocado_colombia_import_export.csv",
                           header=TRUE)
str(imp.ex.avocado)

#Filtering the dataframe
df_filtered <- imp.ex.avocado %>%
  filter(Element %in% c("Import Quantity", "Import Value", "Export Quantity", "Export Value")) %>%
  select(Year, Element, Value)

#pivot_wider() to transform data in the desired columns
value.df <- df_filtered %>%
  pivot_wider(names_from = Element, values_from = Value) %>%
  select(Year, `Import Value`, `Export Value`) %>%
  as.data.frame()
str(value.df)

quantity.df <- df_filtered %>%
  pivot_wider(names_from = Element, values_from = Value) %>%
  select(Year, `Import Quantity`, `Export Quantity`) %>%
  as.data.frame()
str(quantity.df)

#Selecting the range of years 2000-2021 since for the level of water stress
#I have just this one:
quantity.df <- quantity.df[quantity.df$Year >=2000 & quantity.df$Year <= 2021, ]

#Plot with normalized data
plot(quantity.df$Year, norm.fun(quantity.df$`Export Quantity`), col="orange", lwd=2,
     ylab="f(Year)", xlab="Year", type="l", cex.axis=0.9,
     main="Colombia's data on Avocado Production")
lines(quantity.df$Year, norm.fun(quantity.df$`Import Quantity`), col="lightgreen",
      lwd=2)
lines(water.df$Year, norm.fun(water.df$Value), col="red", lwd=2)
legend("topleft", lwd=2, col=c("orange", "lightgreen", "red"),
       c("Export quantity", "Import quantity", "Water stress level"),
       cex = 0.5, bty="n")

#Looking at the relation between water stress and import-export variables.
#Linear regression model
mod <- lm(water.df$Value ~ quantity.df$`Import Quantity` + quantity.df$`Export Quantity`)
summary(mod)
#Positive relation between Water stress level and Export Quantity.
#Negative relation between Water stress level and Import Quantity.

#Plot of the linear regression lines
model.df <- merge(water.df, quantity.df, by = "Year")
model.df <- model.df[,c(1,12,16,17)]
ggplot(model.df, aes(x = `Import Quantity`, y = Value)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Relation between Water Stress and Import Quantity",
       x = "Import Quantity (t)",
       y = "Water Stress Level") +
  theme_minimal()

ggplot(model.df, aes(x = `Export Quantity`, y = Value)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Relation between Water Stress and Export Quantity",
       x = "Export Quantity (t)",
       y = "Water Stress Level") +
  theme_minimal()

################################################################################
#After these justifications, I want to focus more on water stress level and try
#to make predictions on it.

#Reading the data:
data <- read_excel("C:\\Users\\sofyc\\OneDrive\\Desktop\\Int. Org. in OS\\project final\\water stress - World Bank\\water stress and variables - WB.xlsx",
                   sheet = "Dataset")
data <- as.data.frame(data)
str(data)
data$Year <- as.numeric(data$Year)

#I take the range of years 2000-2020 since in the World Bank dataset the 
#information for the Water stress level goes from 2000 till 2020.
data <- data[data$Year>=2000 & data$Year<=2020,]

#Actual Columns names:
#"Year"                                                                                          
#"Annual freshwater withdrawals, agriculture (% of total freshwater withdrawal)"                 
#"Annual freshwater withdrawals, domestic (% of total freshwater withdrawal)"                    
#"Annual freshwater withdrawals, industry (% of total freshwater withdrawal)"                    
#"Annual freshwater withdrawals, total (billion cubic meters)"                                   
#"Level of water stress: freshwater withdrawal as a proportion of available freshwater resources"
#"Renewable internal freshwater resources, total (billion cubic meters)"                         
#"Average precipitation in depth (mm per year)"                                                  
#"Population, total"                                                                             
# "GDP per capita (constant 2015 US$)"                                                            
# "Population density (people per sq. km of land area)"

#Renaming columns because they are too long:
colnames(data) <- c("Year",
                    "Freshwater withdrawals Agriculture",
                    "Freshwater withdrawals Domestic",
                    "Freshwater withdrawals Industry",
                    "Freshwater withdrawals Total",
                    "Level of water stress",
                    "Renewable freshwater Total",
                    "Average precipitation",
                    "Population total",
                    "GDP per capita",
                    "Population density")
str(data)

#Converting in number the observations
data$`Freshwater withdrawals Agriculture` <- as.numeric(data$`Freshwater withdrawals Agriculture`)
data$`Freshwater withdrawals Domestic` <- as.numeric(data$`Freshwater withdrawals Domestic`)
data$`Freshwater withdrawals Industry` <- as.numeric(data$`Freshwater withdrawals Industry`)
data$`Freshwater withdrawals Total` <- as.numeric(data$`Freshwater withdrawals Total`)
data$`Level of water stress` <- as.numeric(data$`Level of water stress`)
data$`Renewable freshwater Total` <- as.numeric(gsub(",", "", data$`Renewable freshwater Total`))
data$`Average precipitation` <- as.numeric(gsub(",", "", data$`Average precipitation`))
data$`Population total` <- as.numeric(gsub(",", "", data$`Population total`))
data$`GDP per capita` <- as.numeric(gsub(",", "", data$`GDP per capita`))
data$`Population density` <- as.numeric(data$`Population density`)

str(data)
#Re-indexing rows
row.names(data) <- 1:nrow(data)

#Saving the clean dataset for the prevision in Python
#write.csv(data, file = "C:\\Users\\sofyc\\OneDrive\\Desktop\\Int. Org. in OS\\project final\\python part\\data_cleaned.csv", row.names = FALSE)

#Finally I tranform the variable Level of water stress in normalized data
#since they are in percentage.
data$`Level of water stress` <- data$`Level of water stress`/100

#Applying JAGS to make predictions on water stress level:
df.subset <- data[,2:11]
dim(df.subset)

#Data preparation for JAGS
X <- as.matrix(df.subset[,c(1:4,6:10)])
y <- as.vector(df.subset[,5])

dt <- list(
  Y = df.subset$`Level of water stress`,
  Agriculture = df.subset$`Freshwater withdrawals Agriculture`,
  Domestic = df.subset$`Freshwater withdrawals Domestic`,
  Industry = df.subset$`Freshwater withdrawals Industry`,
  Total = df.subset$`Freshwater withdrawals Total`,  
  Renewable = df.subset$`Renewable freshwater Total`,
  Precipitation = df.subset$`Average precipitation`,
  Population = df.subset$`Population total`,
  GDP = df.subset$`GDP per capita`,
  Density = df.subset$`Population density`,
  N = nrow(df.subset)
)

#MCMC 
S <- 20000
burn_in <- 2000

params <- c('beta')
b.0 <- rep(1,dim(X)[2])
inits <- list(inits1=list('beta' = b.0))

jags.water <- jags(data=dt,
                   inits=inits,
                   parameters.to.save=params,
                   model.file="C:\\Users/sofyc\\OneDrive\\Desktop\\Int. Org. in OS\\project final\\model_jags.txt",
                   n.chains=1,
                   n.iter=S,
                   n.burnin=burn_in)
jags.water$BUGSoutput$DIC

#Diagnostic and mean estimates of coefficients
#Beta 1: Agriculture
MCMC.b1 <- mean(jags.water$BUGSoutput$sims.array[,1,1])
plot(jags.water$BUGSoutput$sims.array[,1,1], type="l", col="orange",
     ylab="jags.Agriculture", xlab="Iterations")
abline(h=MCMC.b1, lwd=2, lty=3, col="darkgrey")
plot(acf(jags.water$BUGSoutput$sims.array[,1,1]))

#Beta 2: Domestic
MCMC.b2 <- mean(jags.water$BUGSoutput$sims.array[,1,2])
plot(jags.water$BUGSoutput$sims.array[,1,2], type="l", col="orange",
     ylab="jags.Domestic", xlab="Iterations")
abline(h=MCMC.b2, lwd=2, lty=3, col="darkgrey")
plot(acf(jags.water$BUGSoutput$sims.array[,1,1]))

#Beta 3: Industry
MCMC.b3 <- mean(jags.water$BUGSoutput$sims.array[,1,3])
plot(jags.water$BUGSoutput$sims.array[,1,3], type="l", col="orange",
     ylab="jags.Industry", xlab="Iterations")
abline(h=MCMC.b3, lwd=2, lty=3, col="darkgrey")
plot(acf(jags.water$BUGSoutput$sims.array[,1,3]))

#Beta 4: Total
MCMC.b4 <- mean(jags.water$BUGSoutput$sims.array[,1,4])
plot(jags.water$BUGSoutput$sims.array[,1,4], type="l", col="orange",
     ylab="jags.Total", xlab="Iterations")
abline(h=MCMC.b4, lwd=2, lty=3, col="darkgrey")
plot(acf(jags.water$BUGSoutput$sims.array[,1,4]))

#Beta 5: Renewable
MCMC.b5 <- mean(jags.water$BUGSoutput$sims.array[,1,5])
plot(jags.water$BUGSoutput$sims.array[,1,5], type="l", col="orange",
     ylab="jags.Renewable", xlab="Iterations")
abline(h=MCMC.b5, lwd=2, lty=3, col="darkgrey")
plot(acf(jags.water$BUGSoutput$sims.array[,1,5]))

#Beta 6: Precipitation
MCMC.b6 <- mean(jags.water$BUGSoutput$sims.array[,1,6])
plot(jags.water$BUGSoutput$sims.array[,1,6], type="l", col="orange",
     ylab="jags.Precipitation", xlab="Iterations")
abline(h=MCMC.b6, lwd=2, lty=3, col="darkgrey")
plot(acf(jags.water$BUGSoutput$sims.array[,1,6]))

#Beta 7: Population
MCMC.b7 <- mean(jags.water$BUGSoutput$sims.array[,1,7])
plot(jags.water$BUGSoutput$sims.array[,1,7], type="l", col="orange",
     ylab="jags.Population", xlab="Iterations")
abline(h=MCMC.b7, lwd=2, lty=3, col="darkgrey")
plot(acf(jags.water$BUGSoutput$sims.array[,1,7]))

#Beta 8: GDP
MCMC.b8 <- mean(jags.water$BUGSoutput$sims.array[,1,8])
plot(jags.water$BUGSoutput$sims.array[,1,8], type="l", col="orange",
     ylab="jags.GDP", xlab="Iterations")
abline(h=MCMC.b8, lwd=2, lty=3, col="darkgrey")
plot(acf(jags.water$BUGSoutput$sims.array[,1,8]))

#Beta 9: Density
MCMC.b9 <- mean(jags.water$BUGSoutput$sims.array[,1,9])
plot(jags.water$BUGSoutput$sims.array[,1,9], type="l", col="orange",
     ylab="jags.Density", xlab="Iterations")
abline(h=MCMC.b9, lwd=2, lty=3, col="darkgrey")
plot(acf(jags.water$BUGSoutput$sims.array[,1,9]))

Bayesian.regression <- c(
  'Agriculture' = MCMC.b1,
  'Domestic' = MCMC.b2,
  'Industry' = MCMC.b3,
  'Total' = MCMC.b4,  
  'Renewable' = MCMC.b5,
  'Precipitation' = MCMC.b6,
  'Population' = MCMC.b7,
  'GDP' = MCMC.b8,
  'Density' = MCMC.b9
)

Bayesian.regression

#Simulation based on JAGS model to predict water stress level
#assuming Normality of data:
set.seed(134)
M <- 10000
sim.df <- data.frame(
  Agriculture=rnorm(M, mean=df.subset$`Freshwater withdrawals Agriculture`, sd=sd(df.subset$`Freshwater withdrawals Agriculture`)),
  Domestic=rnorm(M, mean=df.subset$`Freshwater withdrawals Domestic`, sd=sd(df.subset$`Freshwater withdrawals Domestic`)),
  Industry=rnorm(M, mean=df.subset$`Freshwater withdrawals Industry`, sd=sd(df.subset$`Freshwater withdrawals Industry`)),
  Total=rnorm(M, mean=df.subset$`Freshwater withdrawals Total`, sd=sd(df.subset$`Freshwater withdrawals Total`)),
  Renewable=rnorm(M, mean=df.subset$`Renewable freshwater Total`, sd=sd(df.subset$`Renewable freshwater Total`)),
  Precipitation=rnorm(M, mean=df.subset$`Average precipitation`, sd=sd(df.subset$`Average precipitation`)),
  Population=rnorm(M, mean=df.subset$`Population total`, sd=sd(df.subset$`Population total`)),
  GDP=rnorm(M, mean=df.subset$`GDP per capita`, sd=sd(df.subset$`GDP per capita`)),
  Density=rnorm(M, mean=df.subset$`Population density`, sd=sd(df.subset$`Population density`))
)

head(sim.df)
dim(sim.df)

Y.pred <- rep(NA, M)

for(i in 1:M){
  Y.pred[i] <- MCMC.b1*sim.df$Agriculture[i]+
               MCMC.b2*sim.df$Domestic[i]+
               MCMC.b3*sim.df$Industry[i]+
               MCMC.b4*sim.df$Total[i]+
               MCMC.b5*sim.df$Renewable[i]+
               MCMC.b6*sim.df$Precipitation[i]+
               MCMC.b7*sim.df$Population[i]+
               MCMC.b8*sim.df$GDP[i]+
               MCMC.b9*sim.df$Density[i]
}

#Quick analysis
summary(Y.pred)
#Median: 0.02799
#Mean: 0.02847
#It is the central tendency of water stress level. Confirmed by:
hist(Y.pred, col="lightblue", xlab="Water stress level predicted",
     ylab="Frequency", main="Distribution of the predictions", cex.main=1,
     cex.axis=0.8)

#Confidence interval
c(lower=quantile(Y.pred, 0.025), upper=quantile(Y.pred, 0.975))
#-0.2014694   0.2593994
