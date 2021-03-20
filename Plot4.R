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

# creating data subset of coal emisions. Unlike NEI, the SCC dataframe does not have and id number for data classification, so using grep should be better than subsetting
sub_SCC <- SCC[grep("[Cc]oal",SCC$Short.Name),] #this will leave the SCC numbers related only to coal
#BECAUSE SCC is just a classification list, not an actual data set, each SCC value in the dataframe is unique.

sub_NEI <- subset(NEI, SCC %in% sub_SCC$SCC, select = c(year, Emissions))# now, the SCC numbers within the sub_SCC frame will be used to select the subset of NEI
# I noticed that using that SCC == sub_SCC$SCC returned only 266 observations, whereas SCC %in% sub_SCC$SCC returns 28480
## To me, using subset is more comfortable that [] because I can drop columns.

sub_NEI$year <- year(as.Date(as.character(sub_NEI$year), format = "%Y"))

rm(SCC)
rm(NEI)
rm(sub_SCC)

Coal_Emissions_vs_Year <- aggregate(list(Coal_Emissions = sub_NEI$Emissions), by = list(year = sub_NEI$year), na.rm = TRUE, FUN = sum)

reg <- lm(Coal_Emissions_vs_Year$Coal_Emissions~Coal_Emissions_vs_Year$year)

trend <- paste0("PM 2.5 = ", round(reg$coefficients[2]), " * year + ", round(reg$coefficients[1]))

png("Plot4.png", width = 630, height = 390)

options(scipen=999)

plotting <- ggplot(Coal_Emissions_vs_Year, aes(year, Coal_Emissions))
plotting <- plotting + geom_line() + geom_point() + ylab("Coal Emissions [Tons]") + xlab("Year") + ggtitle("Coal Emissions by Year") + geom_smooth(method = "lm", linetype="dotted", formula = y~x) + annotate("text", x = 2005, y = 800000, label = trend)

print(plotting)

while (!is.null(dev.list())) dev.off()

rm(list = ls(all.names = TRUE))