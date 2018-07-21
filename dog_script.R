#Dogs of Allegheny County
library(zipcode)
library(readr)
library(dplyr)
library(ggplot2)

dogs_2017 <- read_csv("./Allegheny_dogs_2017.csv")

#load US zipcodes
data(zipcode)

#combine datasets with an inner join
locs <- merge(dogs_2017,zipcode,by.x="OwnerZip",by.y="zip")
locs <- unique(locs)

cities <- as.data.frame(sort(table(locs$city), decreasing=T))
zips <- as.data.frame(sort(table(locs$OwnerZip), decreasing=T))
colnames(cities) <- c("city","count")

pitt <- locs[locs$city=="Pittsburgh",]


sum(cities$count[cities$city != "Pittsburgh"])
cities$count[cities$city == "Pittsburgh"]/sum(cities$count)

#which breeds are most common?
labs <- locs[grepl("LAB",locs$Breed),]
retrs <- locs[grepl("RETRIEVER",locs$Breed),]
shepherds <- locs[grepl("SHEP",locs$Breed),]
poodles <- locs[grepl("OODLE",locs$Breed),]
terriers <- locs[grepl("TERRIER",locs$Breed),]
pitbulls <- locs[grepl("PIT BULL",locs$Breed),]
mixed <- locs[grepl("MIXED",locs$Breed,fixed=TRUE),]

others <- locs[!(locs$Breed %in% c(labs$Breed,shepherds$Breed,poodles$Breed,terriers$Breed,pitbulls$Breed,retrs$Breed,mixed$Breed)),]

#are there common names?
names <- as.data.frame(sort(table(locs$DogName), decreasing=T))

topdogs <- names[1:10,]
colnames(topdogs) = c("Name","count")

topgraph <- ggplot(topdogs, aes(x=Name,y=count)) + geom_bar(stat="identity",fill="orchid4") + theme_minimal() + theme(axis.text.x = element_text(angle=45)) +
  ggtitle("Dog names of Allegheny County (2017)")

#how many names are unusual? where count = 1
unusual <- names[names$Freq == 1,]

#how many male/female?
sum(grepl("Female",locs$LicenseType),na.rm=TRUE)/nrow(locs)
sum(grepl("Male",locs$LicenseType),na.rm=TRUE)/nrow(locs)

sum(grepl("Female",pitt$LicenseType),na.rm=TRUE)/nrow(pitt)
sum(grepl("Male",pitt$LicenseType),na.rm=TRUE)/nrow(pitt)
