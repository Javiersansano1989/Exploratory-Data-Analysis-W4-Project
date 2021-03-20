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
#NEI$fips <- as.numeric(as.character(NEI$fips))
#sub_NEI <- subset(NEI, fips == 24510 & SCC %in% sub_SCC$SCC, select = c(year, Emissions))
sub_NEI <- subset(NEI, fips == "24510" & SCC %in% sub_SCC$SCC, select = c(year, Emissions))
sub_NEI$year <- year(as.Date(as.character(sub_NEI$year), format = "%Y"))

rm(SCC)
rm(NEI)
rm(sub_SCC)

BALT_Vehicle_Emissions_vs_Year <- aggregate(list(BALT_Vehicle_Emissions = sub_NEI$Emissions), by = list(year = sub_NEI$year), na.rm = TRUE, FUN = sum)

reg <- lm(BALT_Vehicle_Emissions_vs_Year$BALT_Vehicle_Emissions~BALT_Vehicle_Emissions_vs_Year$year)

trend <- paste0("PM25 = ", round(reg$coefficients[2]), " * year + ", round(reg$coefficients[1]))

png("Plot5.png", width = 630, height = 390)

options(scipen=999)

plotting <- ggplot(BALT_Vehicle_Emissions_vs_Year, aes(year, BALT_Vehicle_Emissions))
plotting <- plotting + geom_line() + geom_point() + ylab("Baltimore's Vehicle Emissions [Tons]") + xlab("Year") + ggtitle("Baltimore's Vehicle Emissions by Year") + geom_smooth(method = "lm", linetype="dotted", formula = y~x) + annotate("text", x = 2005, y = 450, label = trend)

print(plotting)

while (!is.null(dev.list())) dev.off()

rm(list = ls(all.names = TRUE))