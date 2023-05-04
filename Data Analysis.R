library(dplyr)
library(ggplot2)
library(geosphere)

#### Data Analysis####

# Import the dataset
df_cs<-read.csv("All_Chargers.csv")

### Data cleaning
# Select only those stations with review and inside the Netherlands
df_cs<- df_cs[df_cs$Ratings.Total>0,]
df_cs<- df_cs[df_cs$Country=="Netherlands" ,]

summary(df_cs)
unique(df_cs$Name) #74 different types of stations
# Provinces:
unique(df_cs$Admin.Area.Level.1) # It says there are 15, however in the Netherlands there are only 12 provinces

# replace "North" with "Noord" and " " with "-" in the Admin.Area.Level.1 column
df_cs$Admin.Area.Level.1 <- gsub("North", "Noord", df_cs$Admin.Area.Level.1)
df_cs$Admin.Area.Level.1 <- gsub("South", "Zuid", df_cs$Admin.Area.Level.1)
df_cs$Admin.Area.Level.1 <- gsub(" ", "-", df_cs$Admin.Area.Level.1)

unique(df_cs$Admin.Area.Level.1) # 12 provinces in Netherlands

df_cs_loc<-df_cs%>%
  group_by(Admin.Area.Level.1)%>%
  summarise(obs = n(), max = max(Rating), mean = mean(Rating) ,min = min(Rating))


# plot the data with rotated x-axis labels
ggplot(df_cs, aes(x = Admin.Area.Level.1, y = Rating)) +
  geom_boxplot(fill = grey(0.8), color = "black", alpha = 0.7) +
  labs(title = "Distribution of Reviews by Province",
       x = "Location",
       y = "Review") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# function to calculate distance between two coordinates using geosphere package
calc_distance <- function(lat1, lon1, lat2, lon2) {
  dist <- distHaversine(c(lon1, lat1), c(lon2, lat2))
  return(dist)
}

# function to find the nearest charging stations based on distance
find_nearest <- function(lat, lon, df_cs, closest = 1) {
  df_cs$Distance <- sapply(1:nrow(df_cs), function(i) {
    if (df_cs$Latitude[i] == lat && df_cs$Longitude[i] == lon) {
      return(Inf) # set distance to infinity for charging station at given location
    } else {
      return(calc_distance(lat, lon, df_cs$Latitude[i], df_cs$Longitude[i]))
    }
  })
  df_sorted <- df_cs[order(df_cs$Distance),]
  return(head(df_sorted, n=closest))
}

# function to compute average rating of the nearest charging stations
rating_nearest <- function(lat, lon, df, closest_cs = 1) {
  nearest_stations <- find_nearest(lat, lon, df,closest_cs)
  avg_rating <- mean(nearest_stations$Rating)
  return(avg_rating)
}

n_rating <- c()
avg3 <- c()
avg5 <- c()
avg10 <- c()
nrow(df_cs)
for (i in 1:nrow(df_cs)){
  # Find the nearest rating per CS
  n_rating[i]<-rating_nearest(df_cs$Latitude[i], df_cs$Longitude[i], df_cs,1)
  # Find the avg 3 nearest rating per CS
  avg3[i]<-rating_nearest(df_cs$Latitude[i], df_cs$Longitude[i], df_cs, 3)
  # Find the avg 5 nearest rating per CS
  avg5[i]<-rating_nearest(df_cs$Latitude[i], df_cs$Longitude[i], df_cs, 5) 
  # Find the avg 10 nearest rating per CS
  avg10[i]<-rating_nearest(df_cs$Latitude[i], df_cs$Longitude[i], df_cs, 10)
}


#### Find the closest, save distance####
closest <- function(lat, lon, df_cs) {
  df_cs$Distance <- sapply(1:nrow(df_cs), function(i) calc_distance(lat, lon, df_cs$Latitude[i], df_cs$Longitude[i]))
  df_sorted <- df_cs[order(df_cs$Distance),]
  return(head(df_sorted$Distance, n=1))
}

cls<-c()
for (i in 1:nrow(df_cs)){
  # avg_rating_3_nearest function for Station1
  cls[i]<-closest(df_cs$Latitude[i], df_cs$Longitude[i], df_cs)
}

#### Calculate how many stations are under 1000m, 500m and 200m

# Import the dataset ---- We work with the whole dataset since we do not care whether they have review or not in order to take into account 
df_all<-read.csv("All_Chargers.csv")
df_all<-df_all[df_all$Country=="Netherlands" ,]

# initialize empty vector to store counts
counts_1000 <- c()
counts_500 <- c()
counts_250 <- c()
counts_100<-c()
# loop over each row in the dataframe

for (i in 1:nrow(df_all)) {
  # check if the row has a rating
  if ((df_all$Ratings.Total[i] > 0)) {
  # check if the row has a rating
    distances<-c()
  # calculate distance between this row and all other rows
    for (j in 1:nrow(df_all)){
      distances[j]<- distHaversine(c(df_all$Longitude[i], df_all$Latitude[i]), c(df_all$Longitude[j], df_all$Latitude[j]))
    }
    # count how many distances are less than 1000m
    count_1000 <- nrow(df_all[distances < 1000,]) - 1 #Subtract 1 because otherwise we count the station itself
    # count how many distances are less than 500m
    count_500 <- nrow(df_all[distances < 500,])  - 1 #Subtract 1 because otherwise we count the station itself
    # count how many distances are less than 250m
    count_250 <- nrow(df_all[distances < 250,])  - 1 #Subtract 1 because otherwise we count the station itself
    # count how many distances are less than 100m
    count_100 <- nrow(df_all[distances < 100,])  - 1 #Subtract 1 because otherwise we count the station itself
    # add count to vector
    counts_1000<- c(counts_1000, count_1000)
    # add count to vector
    counts_500<- c(counts_500, count_500)
    # add count to vector
    counts_250<- c(counts_250, count_250)
    # add count to vector
    counts_100<- c(counts_100, count_100)
  }
}


# Merging with previous information
df_cs<-cbind(df_cs, n_rating, avg3,avg5, avg10, counts_100, counts_250, counts_500, counts_1000)

##### Relation 1 closest locations with the star rating of the charging station #####
ggplot(df_cs, aes(x = Rating, y = n_rating)) +
  geom_point(color = "black") +
  geom_smooth(method = "lm", color = "red") +
  xlim(1, 5) + ylim(1, 5) +
  labs(x = "Star Rating of Charging Station", 
       y = "Star Rating of the Closest Locations", 
       title = "Relationship Between Charging Station Rating and the Rating of the Closest Charging Station")

##### Relation 3 closest locations with the star rating of the charging station #####
ggplot(df_cs, aes(x = Rating, y = avg3)) +
  geom_point(color = "black") +
  geom_smooth(method = "lm", color = "red") +
  xlim(1, 5) + ylim(1, 5) +
  labs(x = "Star Rating of Charging Station", 
       y = "Average Star Rating of 3 Closest Locations", 
       title = "Relationship Between Charging Station Rating and Average Rating of 3 Closest Charging Stations")

##### Relation 5 closest locations with the star rating of the charging station #####
ggplot(df_cs, aes(x = Rating, y = avg5)) +
  geom_point(color = "black") +
  geom_smooth(method = "lm", color = "red") +
  xlim(1, 5) + ylim(1, 5) +
  labs(x = "Star Rating of Charging Station", 
       y = "Average Star Rating of 5 Closest Locations", 
       title = "Relationship Between Charging Station Rating and Average Rating of 5 Closest Charging Stations")

##### Relation 10 closest locations with the star rating of the charging station #####
ggplot(df_cs, aes(x = Rating, y = avg10)) +
  geom_point(color = "black") +
  geom_smooth(method = "lm", color = "red") +
  xlim(1, 5) + ylim(1, 5) +
  labs(x = "Star Rating of Charging Station", 
       y = "Average Star Rating of 10 Closest Locations", 
       title = "Relationship Between Charging Station Rating and Average Rating of 10 Closest Charging Stations")

##### Charging Stations in the sorrounding area 100 #####
# plot the data with rotated x-axis labels
ggplot(df_cs, aes(x = Admin.Area.Level.1, y = counts_100)) +
  geom_boxplot(fill = grey(0.8), color = "black", alpha = 0.7) +
  labs(title = "Charging Station within 100m by Province",
       x = "Province",
       y = "Charging Stations") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

##### Charging Stations in the sorrounding area 250 #####
# plot the data with rotated x-axis labels
ggplot(df_cs, aes(x = Admin.Area.Level.1, y = counts_250)) +
  geom_boxplot(fill = grey(0.8), color = "black", alpha = 0.7) +
  labs(title = "Charging Station within 250m by Province",
       x = "Province",
       y = "Charging Stations") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


##### Charging Stations in the sorrounding area 500 #####
# plot the data with rotated x-axis labels
ggplot(df_cs, aes(x = Admin.Area.Level.1, y = counts_500)) +
  geom_boxplot(fill = grey(0.8), color = "black", alpha = 0.7) +
  labs(title = "Charging Station within 500m by Province",
       x = "Province",
       y = "Charging Stations") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

##### Charging Stations in the sorrounding area 1000 #####
# plot the data with rotated x-axis labels
ggplot(df_cs, aes(x = Admin.Area.Level.1, y = counts_1000)) +
  geom_boxplot(fill = grey(0.8), color = "black", alpha = 0.7) +
  labs(title = "Charging Station within 1000m by Province",
       x = "Province",
       y = "Charging Stations") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

##### Charging Stations in the sorrounding area 100  - Rating#####
# plot the data with rotated x-axis labels
ggplot(df_cs, aes(x = as.factor(round(Rating,0)), y = counts_100)) +
  geom_boxplot(fill = grey(0.8), color = "black", alpha = 0.7) +
  labs(title = "Charging Station within 100m by Rating",
       x = "Review",
       y = "Charging Stations") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

##### Charging Stations in the sorrounding area 250  - Rating#####
# plot the data with rotated x-axis labels
ggplot(df_cs, aes(x = as.factor(round(Rating,0)), y = counts_250)) +
  geom_boxplot(fill = grey(0.8), color = "black", alpha = 0.7) +
  labs(title = "Charging Station within 250m by Rating",
       x = "Review",
       y = "Charging Stations") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


##### Charging Stations in the sorrounding area 500  - Rating#####
# plot the data with rotated x-axis labels
ggplot(df_cs, aes(x = as.factor(round(Rating,0)), y = counts_500)) +
  geom_boxplot(fill = grey(0.8), color = "black", alpha = 0.7) +
  labs(title = "Charging Station within 250m by Rating",
       x = "Review",
       y = "Charging Stations") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

##### Charging Stations in the sorrounding area 1000  - Rating#####
# plot the data with rotated x-axis labels
ggplot(df_cs, aes(x = as.factor(round(Rating,0)), y = counts_1000)) +
  geom_boxplot(fill = grey(0.8), color = "black", alpha = 0.7) +
  labs(title = "Charging Station within 250m by Rating",
       x = "Review",
       y = "Charging Stations") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



