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

sub_NEI <- subset(NEI, select = c(year, Emissions))
  sub_NEI$year <- year(as.Date(as.character(sub_NEI$year), format = "%Y"))

Emissions_vs_Year <- aggregate(list(Emissions = sub_NEI$Emissions), by = list(year = sub_NEI$year), FUN = sum)

rm(sub_NEI)

png("Plot1.png", width = 630, height = 390)

options(scipen=999)

barplot(height = Emissions_vs_Year$Emissions, names = Emissions_vs_Year$year, main = "Emissions of Particulate Material by Year", ylab = expression("PM "[2.5]*" Emissions [Tons]"), xlab = "Year")
reg <- lm(Emissions_vs_Year$Emissions~Emissions_vs_Year$year)
lines(predict(reg), type = 'o', lwd=4)
mtext(paste0("PM 2.5 = ", round(reg$coefficients[2]), " * year + ", round(reg$coefficients[1])), side = 3, line = 0, cex = 0.8)

dev.off()

rm(list = ls(all.names = TRUE))