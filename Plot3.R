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
#SCC <- readRDS("Source_Classification_Code.rds")
#file.remove("Source_Classification_Code.rds")

sub_NEI <- subset(NEI, fips == "24510", select = c(year, Emissions, type))
sub_NEI$year <- year(as.Date(as.character(sub_NEI$year), format = "%Y"))

BALT_Emissions_vs_Year_by_type <- aggregate(list(Emissions = sub_NEI$Emissions), by = list(type = sub_NEI$type, year = sub_NEI$year), na.rm = TRUE, FUN = sum)

png("Plot3.png", width = 630, height = 390)

options(scipen=999)

plotting <- ggplot(BALT_Emissions_vs_Year_by_type, aes(year, Emissions, color = type))
plotting <- plotting + geom_line() + geom_point() + facet_wrap(.~type, scales = "free",) + ylab(expression("PM "[2.5]*" Emissions [Tons]")) + xlab("year") + ggtitle("Baltimore's Emissions of Particulate Material by Year")

print(plotting)

while (!is.null(dev.list())) dev.off()

rm(list = ls(all.names = TRUE))