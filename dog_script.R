#Dogs of Allegheny County
library(zipcode)
library(readr)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(grid)
library(scales)

dogs_2017 <- read_csv("../AlleghenyData/Allegheny_dogs_2017.csv")

#load US zipcodes
data(zipcode)

#combine datasets with an inner join
locs <- merge(dogs_2017,zipcode,by.x="OwnerZip",by.y="zip")
locs <- unique(locs) #remove duplicated entries

cities <- as.data.frame(sort(table(locs$city), decreasing=T))
zips <- as.data.frame(sort(table(locs$OwnerZip), decreasing=T))
colnames(cities) <- c("city","count")

pitt <- locs[locs$city=="Pittsburgh",]


sum(cities$count[cities$city != "Pittsburgh"])
cities$count[cities$city == "Pittsburgh"]/sum(cities$count)

#which breeds are most common? In county
labs <- locs[grepl("LAB",locs$Breed),] #2
retrs <- locs[grepl("RETRIEV",locs$Breed),] #4
shepherds <- locs[grepl("SHEP",locs$Breed),] #5
poodles <- locs[grepl("OODLE",locs$Breed),] 
pitbulls <- locs[grepl("PIT BULL",locs$Breed),]
mixed <- locs[grepl("MIXED",locs$Breed,fixed=TRUE),] #3
terriers <- locs[grepl("TERR",locs$Breed)&!(grepl("PIT",locs$Breed)),] #1


others <- locs[!(locs$Breed %in% c(labs$Breed,shepherds$Breed,poodles$Breed,terriers$Breed,pitbulls$Breed,retrs$Breed,mixed$Breed)),]
otherssort <- as.data.frame(sort(table(others$Breed), decreasing=T))
others <- others[others$city!="Pittsburgh",]

#breeds for pittsburgh only
pittlab <- labs[labs$city=="Pittsburgh",] #1
pittretrs <- retrs[retrs$city=="Pittsburgh",] #3
pittshep <- shepherds[shepherds$city=="Pittsburgh",] #5
pittpoodles <- poodles[poodles$city=="Pittsburgh",]
pittterr <- terriers[terriers$city=="Pittsburgh",] #4
pittbulls<- pitbulls[pitbulls$city=="Pittsburgh",]
pittmixed <- mixed[mixed$city=="Pittsburgh",] #2

pittothers <- others[others$city=="Pittsburgh",]
pittotherssort <- as.data.frame(sort(table(pittothers$Breed), decreasing=T))

#put together top 5 from Pitt & county for graph
top5 <- data.frame(row.names = NULL, c("Labrador","Mixed breed","Golden retriever","Terrier (not pit bull)","Shepherd"),
                      c(nrow(labs[labs$city=="Pittsburgh",]),nrow(mixed[mixed$city=="Pittsburgh",]),nrow(retrs[retrs$city=="Pittsburgh",]),
                        nrow(terriers[terriers$city=="Pittsburgh",]),nrow(shepherds[shepherds$city=="Pittsburgh",])))
colnames(top5) <- c("Breed","Count")
top5$Location <- "Pittsburgh"

alleghenytop5 <- data.frame(row.names=NULL, c("Labrador","Mixed breed","Golden retriever","Terrier (not pit bull)","Shepherd"),
                            c(nrow(labs[labs$city!="Pittsburgh",]),nrow(mixed[mixed$city!="Pittsburgh",]),nrow(retrs[retrs$city!="Pittsburgh",]),
                              nrow(terriers[terriers$city!="Pittsburgh",]),nrow(shepherds[shepherds$city!="Pittsburgh",])))
colnames(alleghenytop5) <- c("Breed","Count")
alleghenytop5$Location <- "Rest of Allegheny Co."

bothtop5 <- rbind(top5,alleghenytop5)
bothtop5$Breed <- factor(bothtop5$Breed,levels=bothtop5$Breed)

ggplot(bothtop5,aes(fill=Location,color=Location,x=Breed,y=Count)) + geom_bar(position="dodge",stat="identity")+ theme_minimal() + ggtitle("Dog breeds of Allegheny County (2017)") +
  scale_fill_manual(values=c("gold1","slateblue3")) + scale_color_manual(values=c("black","slateblue4"))


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
sum(grepl("Male",pitt$LicenseType),na.rm=TRUE)/nrow(pitt) #sexes are roughly even in county and Pittsburgh




#import combined dataset to find if dog/human ratio is increasing
alldogs <- read_csv("../AlleghenyData/alldogs.csv")
pittpop <- read_csv("../AlleghenyData/pittpop.csv")

locs <- merge(alldogs,zipcode,by.x="OwnerZip",by.y="zip")
locs <- unique(locs)

#Pittsburgh only
allpitt <- locs[locs$city=="Pittsburgh",]

byyear <- allpitt %>% count(ExpYear)

byyear <- merge(byyear,pittpop,by.x="ExpYear",by.y="year")
byyear$prop <- byyear$n/byyear$population

plot1 <- ggplot(byyear,aes(x=ExpYear,y=n)) + theme(plot.margin = unit(c(1,5,-30,6),units="points")) + 
  geom_line() + scale_x_continuous(limits=c(2006,2018),breaks=pretty_breaks()) +
  labs(y="Number of dog licenses")
plot2 <- ggplot(byyear,aes(x=ExpYear,y=population)) + theme(plot.margin = unit(c(0,5,1,1),units="points")) + geom_line() + scale_x_continuous(limits=c(2006,2018),breaks=pretty_breaks()) +
  labs(x="Year",y="Population")

plot3 <- ggplot(byyear,aes(x=ExpYear,y=prop)) + geom_line() + scale_x_continuous(limits=c(2006,2018),breaks=pretty_breaks()) +
  labs(y="Proportion dogs to humans",x="Year")

lay <- rbind(c(1,1,3,3),c(2,2,3,3))

grid.arrange(plot1,plot2,plot3,layout_matrix=lay,top="Dogs and humans in Pittsburgh")

