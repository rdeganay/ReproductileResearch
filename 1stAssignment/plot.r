# Setup
library(ggplot2)
library(dplyr)



#fileUrl <- "https://d3c33hcgiwev3.cloudfront.net/_e143dff6e844c7af8da2a4e71d7c054d_payments.csv?Expires=1471996800&Signature=kFfSXKWMFIEoAo6ThsGgGdv9~HiXeLFdd-395oRYlWw8YtKKQuTBFMBAwre6GGiLYGj-~zjFhtiuytvPJeT2pceZWmpVrbRPqgvStiJiS8-m65enZ2q0idEaLYosCnozYUyeXADsa-3OzSnS8AQFc5BGz394f~DJ05gWUji6dcY_&Key-Pair-Id=APKAJLTNE6QMUY6HBC5A"
#download.file(fileUrl, destfile = "data/payments.csv", method = "curl")
#datedownloaded <- date()
payments <- read.csv("payments.csv", header = TRUE, sep = ",")


###subsetting only NYC data
nycity <- subset(payments, payments$Provider.City == "NEW YORK")
nycitylight <- select(.data = nycity, Average.Covered.Charges, Average.Total.Payments)


#after quick exploration it appears the data tends to split up into two parts
#depending on total payments (this is actually one desease that can be isolated based
#only on total payments). It's decided to show this separation on the first plot. 

  
###Plot 1
pdf("plot1.pdf", paper = "a4r")
g1 <- ggplot(data = nycitylight, aes(x=Average.Total.Payments, y=Average.Covered.Charges, 
                                     colour = Average.Total.Payments > 20000)) + 
  geom_point() +
  geom_smooth(data = subset(nycitylight, nycitylight$Average.Total.Payments>20000), method = 'lm') + 
  geom_smooth(data = subset(nycitylight, nycitylight$Average.Total.Payments<20000)) + 
  theme(legend.position = "none") + 
  ggtitle("Covered Charges vs Total Payments in New York City Hospitals") + 
  xlab("Average Total Payments in US$") + 
  ylab("Average Covered Charges in US$")
print(g1)
dev.off()


###Plot 2
pdf("plot2.pdf", paper="a4r")
g2 <- ggplot(data = payments, aes(x=Average.Total.Payments, y=Average.Covered.Charges, colour = Provider.State)) + 
  geom_point() + 
  geom_smooth(method = 'lm', se = FALSE) + 
  facet_grid(Provider.State~., scales = "free") +
  theme(legend.position = "none") + 
  ggtitle("Covered Charges vs Total Payments, split by States") + 
  xlab("Average Total Payments in US$") + 
  ylab("Average Covered Charges in US$")
print(g2)

labels = substring(levels(payments$DRG.Definition),1,3)
names(labels) = levels(payments$DRG.Definition)
g3 <- ggplot(data = payments, aes(x=Average.Total.Payments, y=Average.Covered.Charges, colour = DRG.Definition)) + 
  geom_point() + 
  geom_smooth(method = 'lm', se = FALSE) + 
  facet_grid(DRG.Definition~., labeller = as_labeller(labels))+
  theme(legend.position = "bottom", legend.text=element_text(size=4), legend.title = element_blank()) + 
  ggtitle("Covered Charges vs Total Payments, split by Disease") + 
  xlab("Average Total Payments in US$") + 
  ylab("Average Covered Charges in US$")
print(g3)
dev.off()



