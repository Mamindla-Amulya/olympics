medals <- read.csv(file.choose(), header = T, stringsAsFactors = FALSE)
medals
savePlot <- TRUE
# remove rows that do not contain data
medals$Year <- as.numeric(medals$Year)

#medals$Year
medals <- medals[!is.na(medals$Year), ]
medals

sapply(medals, function(x) cbind(sort(table(x), decreasing = TRUE)))

#which city won more medals
city <- sort(table(medals$City), dec = TRUE)
city
city[city == max(city)]

#which NOC won more medals
committe <- sort(table(medals$NOC), dec = TRUE)
committe
committe[committe == max(committe)]


#no of mens and womens particpating in the event
Gender <- aggregate(medals$Event, list(Gender = medals$Event_gender), length)
Gender


# How many medals have been awarded in each year
medalsByYear <- aggregate(medals$Year, list(Year = medals$Year), length)
medalsByYear
plot(x ~ Year, medalsByYear, ylim = c(0,max(x)), ylab = "Total Medals Awarded", bty="l",main = "Total Medals Awarded in Olympics by Year")



#how many played different kind of sports
sport <- aggregate(list(no_of_participants = medals$Sport), list(sport = medals$Sport), length)
sport


# How has the amount of medals awarded to males and females changed over the years?
medalsByYearByGender <- aggregate(medals$Year, list(Year = medals$Year, Event_gender = medals$Event_gender), length)
medalsByYearByGender
medalsByYearByGender <- medalsByYearByGender[medalsByYearByGender$Event_gender != "X", ]
medalsByYearByGender
#male
if(savePlot == TRUE)
  plot(x ~ Year, medalsByYearByGender[medalsByYearByGender$Event_gender == "M", ], ylim = c(0,max(x)), pch = "m", col = "blue", ylab = "Total Medals Awarded", bty="l",main = "Total Medals Awarded in Olympics\n by Gender and by Year")
points(medalsByYearByGender[medalsByYearByGender$Event_gender == "W", "Year"],medalsByYearByGender[medalsByYearByGender$Event_gender == "W", "x"],col = "red", pch = "f")



#number of countries participated
cities <- aggregate(medals$City ,list(city = medals$City), length)
cities

#proportion of gold medals by each country
NOC50Plus <- names(table(medals$NOC)[table(medals$NOC) > 50])
NOC50Plus
medalsSubset <- medals[medals$NOC %in% NOC50Plus, ]
medalsSubset
medalsByMedalByNOC <- prop.table(table(medalsSubset$NOC, medalsSubset$Medal), margin = 1)
medalsByMedalByNOC
medalsByMedalByNOC <- medalsByMedalByNOC[order(medalsByMedalByNOC[, "Gold"],decreasing = TRUE), c("Gold", "Silver", "Bronze")]
medalsByMedalByNOC
round(medalsByMedalByNOC, 2)

barplot(round(t(medalsByMedalByNOC), 2), horiz = TRUE, las = 1, col=c("gold", "grey71", "chocolate4"), xlab = "Proportion of Medals",main="Proportion of medals won that were gold, silver or bronze.")

counts <- table(medals$Discipline)
counts
barplot(counts, main="Discipline", horiz=TRUE,col="darkgreen")


gender<-table(medals$Event_gender)
gender
barplot(gender)

# How many different countries have won medals by year
listOfYears <- unique(medals$Year)
listOfYears
names(listOfYears) <- unique(medals$Year)
names(listOfYears)
totalNocByYear <- sapply(listOfYears,  function(X) length(table(medals[medals$Year == X, "NOC"])))
totalNocByYear
plot(x= names(totalNocByYear), totalNocByYear, ylim = c(0, max(totalNocByYear)),xlab = "Year",ylab = "Total Number of Countries",bty = "l", main = "Total Number of Countries\n Winning Medals By Year")





# Which Countries have won a medal at every Olympics
propYearsOnePlusMedals <- apply(table(medals$NOC, medals$Year) > 0, 1, mean)
propYearsOnePlusMedals
name <- names(propYearsOnePlusMedals[propYearsOnePlusMedals == 1.0])
name


y <- data.frame(medals$Year)
y
gold <- aggregate(list(GoldMedals = y[medals$Medal == 'Gold',]),list(Year = y[medals$Medal == 'Gold',]),length)
gold
silver <- aggregate(list(SilverMedals = y[medals$Medal == 'Silver',]),list(Year = y[medals$Medal == 'Silver',]),length)
silver
bronze <- aggregate(list(BronzeMedals = y[medals$Medal == 'Bronze',]),list(Year = y[medals$Medal == 'Bronze',]),length)
bronze




skat <-y[medals$Discipline == 'Skating',]
skat
skating <- aggregate(list(Swimming = y[medals$Discipline == 'Speed skating',]),list(Year = y[medals$Discipline == 'Speed skating',]),length)
skating



#plot data 
install.packages("tabplot")
library(tabplot)
tableplot(medals)




library(ggplot2)
geocode("FIN")
geocode("AUT")
geocode("CAN")
geocode("NOR")
geocode("SWE")
geocode("USA")


#using ggplot
#plot the world map
mapWorld <- borders("world", colour="grey", fill="lightblue")
mapWorld
#Plot of cities which won atleast one medal in every olympics
ggplot() + mapWorld + geom_point(aes(x=25.74815, y=61.92411) ,color="red", size=3) + geom_text(data=medals, aes(x=25.74815, y=61.92411, label = "Finland"), size=5) + geom_point(aes(x = 1.468091, y = 44.18961) ,color="red", size=3) + geom_text(data=medals, aes(x = 1.468091, y = 44.18961, label = "Austria"), size=5) + geom_point(aes(x = 27.04663, y = 40.02817) ,color="red", size=3) + geom_text(data=medals, aes(x = 27.04663, y= 40.02817, label = "Turkey"), size=5) + geom_point(aes(x = 8.468946, y = 60.47202) ,color = "red", size = 3) + geom_text(data = medals, aes(x = 8.468946, y = 60.47202, label = "Norway"), size=5) + geom_point(aes(x = -77.98724, y = 43.14404) ,color="red", size = 3) + geom_text(data = medals, aes(x = -77.98724, y = 43.14404, label = "New York"), size=5)    +  geom_point(aes(x = -95.71289, y = 37.09024) ,color = "red", size = 3)  + geom_text(data = medals, aes(x = -95.71289, y = 37.09024, label = "USA"), size = 5)


d <- dist(as.matrix(medals))   # find distance matrix 
hc <- hclust(d)                # apply hirarchical clustering 
plot(hc)

#ggplot() + mapWorld + geom_point(data=medals, mapping=aes(x=medals$City, y=medals$Year, color="green", size=3)) 









