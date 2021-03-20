library(lubridate)
library(ggplot2)

#clear environment at the start of the run
rm(list = ls(all.names = TRUE))

# Datasets creation and removal of files
download.file(url = 'https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip', destfile = 'exdata_data_NEI_data.zip', method = 'curl')
unzip(zipfile = "exdata_data_NEI_data.zip", exdir = getwd())
file.remove("exdata_data_NEI_data.zip")
NEI <- readRDS("summarySCC_PM25.rds")
file.remove("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")
file.remove("Source_Classification_Code.rds")

# creating data subset for vehicles. Unlike NEI, the SCC dataframe does not have and id number for data classification, so using grep should be better than subsetting
sub_SCC <- SCC[grep("[Vv]eh",SCC$Short.Name),]

#this line creates extracts ONLY 2116
sub_NEI <- subset(NEI,SCC %in% sub_SCC$SCC & (fips == "24510" | fips == "06037"), select = c(fips, year, Emissions))
sub_NEI$year <- year(as.Date(as.character(sub_NEI$year), format = "%Y"))

#whereas the following extracts 3.231.399 vehicle observations
#veh_NEI <- subset(NEI, SCC %in% sub_SCC$SCC, select = c(fips, year, Emissions))

# and of the vehicle observations, 11.416 seem to be about Baltimore and Los Angeles
#sub_NEI <- subset(NEI, fips == "24510" | fips == "06037", select = c(fips, year, Emissions))
#sub_NEI$year <- year(as.Date(as.character(sub_NEI$year), format = "%Y"))
#sub_NEI$Emissions <- as.numeric(as.character(sub_NEI$Emissions))

sub_NEI$fips[sub_NEI$fips == "24510"] <- "Baltimore"
sub_NEI$fips[sub_NEI$fips == "06037"] <- "Los Angeles"

rm(SCC)
rm(NEI)
rm(sub_SCC)
#rm(veh_SCC)

Vehicle_Emissions_comparison <- aggregate(list(Vehicle_Emissions = sub_NEI$Emissions), by = list(year = sub_NEI$year, City = sub_NEI$fips), na.rm = TRUE, FUN = sum)

png("Plot6.png", width = 630, height = 390)

options(scipen=999)

plotting <- ggplot(Vehicle_Emissions_comparison, aes(factor(year), Vehicle_Emissions, color = City))
plotting <- plotting + geom_bar(stat = "identity") + ylab("Vehicle Emissions [Tons]") + xlab("Year") + facet_wrap(.~City, scales = "free",) + ggtitle("Baltimore vs Los Angeles Vehicle Emissions by Year")

print(plotting)

while (!is.null(dev.list())) dev.off()

rm(list = ls(all.names = TRUE))