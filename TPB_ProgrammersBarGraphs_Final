###Bar Graphs for AP v. Year, AP per Art v. Year, and Art v. Year###

#Read in Data
TPBPercent <- read.csv('UpdatedProgNumbers.csv')

#Install and add in packages
library(ggplot2)
library(gridExtra)
library(reshape2)

##Acknowledged Programmers (AP) v. Year##
#Binding data as a data frame for Year, Female Programmers, Male Programmers, and NA Programmers
APs = cbind(TPBPercent$Year[1:21], TPBPercent$FemaleProg[1:21], TPBPercent$MaleProg[1:21], TPBPercent$NAProg[1:21])
APs = as.data.frame(APs)

APs1 <- melt(APs, id.var="V1")

#Creating stacked bar plot of Programmers per Year
APBar <- ggplot(APs1, aes(x = V1, y = value, fill = variable)) +
  geom_bar(stat = "identity") + labs(x = ("Year"), y = ("Number of Programmers"), 
                                     fill = ("Gender")) +
  theme_classic() + theme(text = element_text(size=19), legend.position = c(0.93, 0.85),
                          legend.text = element_text(size = 14)) + 
  scale_x_discrete(limits = c(1970:1990)) + 
  scale_fill_manual(values = c("purple3", "gold1","aquamarine3"), 
                    labels=c("Women", "Men", "Ambiguous"))

##AP per Articles v. Year##
#Binding data as a data frame for Year, Female Programmers per Article, Male Programmers per Article, and NA Programmers per Article
APs2 = cbind(TPBPercent$Year[1:21], TPBPercent$FProgArt[1:21], 
             TPBPercent$MProgArt[1:21], TPBPercent$NAProgArt[1:21])
APs2 = as.data.frame(APs2)

APs3 <- melt(APs2, id.var="V1")

#Creating stacked bar plot of Programmers per Article per Year
APpArBar <- ggplot(APs3, aes(x = V1, y = value, fill = variable)) +
  geom_bar(stat = "identity") + labs(x = ("Year"), y = ("No. Programmers per Tot. Articles"), 
                                     fill = ("Gender")) + theme_classic() +
  theme(text = element_text(size=18), legend.position = c(0.9, 0.85),
        legend.text = element_text(size = 14)) +  
  scale_fill_manual(values = c("purple3", "gold1","aquamarine3"), 
                    labels=c("Women", "Men", "Ambiguous")) +
  scale_x_discrete(limits = c(1970:1990))

##Articles v. Year##
#Creating bar plot of the Number of Articles per Year
ArBar <- ggplot(data = TPBPercent, aes(x = Year, y = TotalArt, fill = TotalArt)) + geom_col() + 
  scale_x_discrete(limit = c(1970,1971, 1972, 1973, 1974, 1975, 1976, 1977, 1978, 1979,
                             1980, 1981, 1982, 1983, 1984, 1985, 1986, 1987, 1988,
                             1989, 1990), breaks = seq(1970,1990,1)) +
  ylim(0, 50) +
  labs(aes(y="Number of Articles")) + theme_classic() + theme(legend.position = "None", 
                                                              text = element_text(size = 17)) +
  geom_text(aes(label=TotalArt),hjust=0.5, vjust=2, color = "White")


##Arranging 2 Bar Plots into 1 Plot##
grid.arrange(APBar, APpArBar, ArBar, nrow=3)
