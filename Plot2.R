library(lubridate)
#library(ggplot2)

#clear environment at the start of the run
rm(list = ls(all.names = TRUE))

# Datasets creation and removal of files
download.file(url = 'https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip', destfile = 'exdata_data_NEI_data.zip', method = 'curl')
unzip(zipfile = "exdata_data_NEI_data.zip", exdir = getwd())
file.remove("exdata_data_NEI_data.zip")
NEI <- readRDS("summarySCC_PM25.rds")
file.remove("summarySCC_PM25.rds")
#SCC <- readRDS("Source_Classification_Code.rds")
#file.remove("Source_Classification_Code.rds")

sub_NEI <- subset(NEI, fips == "24510", select = c(year, Emissions))
sub_NEI$year <- year(as.Date(as.character(sub_NEI$year), format = "%Y"))

BALT_Emissions_vs_Year <- aggregate(list(Emissions = sub_NEI$Emissions), by = list(year = sub_NEI$year), FUN = sum)

#Emissions_vs_Year <- aggregate(list(Emissions = sub_NEI$Emissions), by = list(fips =  sub_NEI$fips, year = sub_NEI$year), FUN = sum)
# check fips are unique after aggregating
# (1) fip_list <- subset(Emissions_vs_Year, select = c(fips))
# (2) fip_list[fip_list$Freq > 1,]

rm(sub_NEI)

png("Plot2.png", width = 630, height = 390)

options(scipen=999)

barplot(height = BALT_Emissions_vs_Year$Emissions, names = BALT_Emissions_vs_Year$year, main = "Baltimore's Emissions of Particulate Material by Year", ylab = expression("PM "[2.5]*" Emissions [Tons]"), xlab = "year")
reg <- lm(BALT_Emissions_vs_Year$Emissions~BALT_Emissions_vs_Year$year)
lines(predict(reg), type = 'o', lwd=4)
mtext(paste0("PM 2.5 = ", round(reg$coefficients[2]), " * year + ", round(reg$coefficients[1])), side = 3, line = 0, cex = 0.8)

dev.off()

rm(list = ls(all.names = TRUE))