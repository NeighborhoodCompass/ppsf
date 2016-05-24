##PPSF_Tax
setwd("C:/Users/johnki/Desktop/Compass_Development/PPSF")
PPSF <- read.csv("AMDTSL15_2.csv", header = TRUE, stringsAsFactors=FALSE, sep = ",")
bg <- read.csv("bg.csv", header = TRUE, stringsAsFactors=FALSE, sep = ",")
colnames(bg) <- c("id")

require(lubridate)
require(reshape)
require(dplyr)
require(plyr)

# format year and id fields, calculate per square foot values, make field for generating counts
P <- mutate(PPSF, YR = substr(PPSF$AMDTSL,1,4), GEOID = as.character(GEOID10), YEAR = as.integer(YR), BUILT = as.integer(AHACYR), PPSF_H = PPSF$AMSLAM/PPSF$XXHAUN,PPSF_F = PPSF$AMSLAM/PPSF$AHFNAR, CT = 1)
PP <- subset(P, select = -c(YR), YEAR > 2004 & BUILT > 0 & AMVICD == 'I' & AHBED_ > 0)


#creat adj factor using averaged annual housing cpi from here: http://data.bls.gov/cgi-bin/surveymost?cu
housingcpi <- read.csv("HousingCPI_05_15.csv", header = TRUE, sep = ",")
View(housingcpi)
housingcpi$adj_factor <- housingcpi$cpi/housingcpi$cpi[housingcpi$YEAR == 2015]
##join mls and annual cpi tables
Pcpi <- merge(PP, housingcpi, by = "YEAR")

##adjust list and sale values, heated sq ft and finished sq ft alternatives
Padj <- mutate(Pcpi, SOLDADJ = (AMSLAM/adj_factor), ADJPPSF_H = (SOLDADJ/XXHAUN),ADJPPSF_F = (SOLDADJ/AHFNAR))

#write raw table
write.table(Padj, file = "tax_sales_20052015_2.csv", sep = ",", row.names = FALSE)

## create table of sale counts for BGs for each year
bgct <- cast(GEOID ~ YEAR, data = Padj, fun = sum, value = "CT", na.rm = TRUE)

## create table of sale counts for neighborhoods for each year
nhct <- cast(name_1 ~ YEAR, data = Padj, fun = sum, value = "CT", na.rm = TRUE)


# create table of median values for blockgroups
bgmed <- cast(GEOID ~ YEAR, data = Padj, fun = median, value = "ADJPPSF_H", na.rm = TRUE)
colnames(bgmed) <- c("id","y_2005","y_2006","y_2007","y_2008","y_2009","y_2010","y_2011","y_2012","y_2013","y_2014","y_2015")
# create table of median values for neighborhoods
nhmed <- cast(name_1 ~ YEAR, data = Padj, fun = median, value = "ADJPPSF_H", na.rm = TRUE)
# create csvs
  write.table(nhmed, file = "nhmed_pricepersquareft_tax20052015.csv", sep = ",", col.names = NA)
  write.table(bgmed, file = "bgmed_pricepersquareft_tax20052015.csv", sep = ",", row.names = FALSE)

  #combine counts and medians
  combo_bg <- merge(bgmed, bgct, by = "GEOID")
  combo_nh <- merge(nhmed, nhct, by = "name_1")
  
# create table of median sales values for blockgroups
bgmedsale <- cast(GEOID ~ YEAR, data = Padj, fun = median, value = "SOLDADJ", na.rm = TRUE)
colnames(bgmedsale) <- c("id","y_2005","y_2006","y_2007","y_2008","y_2009","y_2010","y_2011","y_2012","y_2013","y_2014","y_2015")
medsale <- merge(bg, bgmedsale, by = "id", all=TRUE)  
write.table(medsale, file = "bgmed_sale_tax20052015.csv", sep = ",", row.names = FALSE)  
  
##County medians
P2 <- select(Padj, YEAR, AMSLAM, PPSF_H, SOLDADJ, cpi, adj_factor, ADJPPSF_H)
head(P2)

comed_1 <- ddply(P2,~YEAR,summarise,adj_med_ppsf=median(ADJPPSF_H))

comed_2 <- ddply(P2,~YEAR,summarise,median=median(AMSLAM)) 

comed_3 <- ddply(P2,~YEAR,summarise,med_ppsf=median(PPSF_H)) 

# table of combined price per square foot in Durham County 2005-2015 with cpi-adjusted same 
county_medians <- merge(comed_3, comed_1, by="YEAR")
county_medians_wcpi <- merge(county_medians, housingcpi, by="YEAR")
write.table(county_medians_wcpi, file = "DurhamCounty_MedianPricePerSqFt_AllResidential_2005_2015.csv", sep =",", row.names= FALSE)
