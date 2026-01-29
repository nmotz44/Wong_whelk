### Code to filter prey species for mixing models ## 

### Code to filter prey species for mixing models ## 

library(tidyverse)
library(sf)


prey_raw <- read.csv("Data/SIA_Database_2025_12_11.csv")
prey_unique <- (unique(prey_raw$CommonName))

#write.csv(prey_unique, "Data/prey_subset_list.csv")
prey_raw_subset <- read.csv("Data/prey_subset_list.csv")

prey_sp <- subset(prey_raw_subset, WhelkDiet == "Y")

prey_sp_list <- prey_sp$CommonName

prey_selected <- prey_raw %>% 
  filter(CommonName %in% prey_sp_list)

prey_selected$Lat <- as.numeric(prey_selected$Lat)
prey_selected$Long <- as.numeric(prey_selected$Long)

kwhelk <- subset(prey_selected, CommonName == "Knobbed Whelk" | CommonName == "Knobbed whelk")
swhelk <- subset(prey_selected, CommonName == "Smooth Whelk" | CommonName == "Smooth whelk")

prey_selected2 <- prey_selected %>%
  filter(!CommonName %in% c("Knobbed whelk", "Knobbed Whelk", "Smooth Whelk", "Smooth whelk"))

prey_selected2 <- subset(prey_selected2, ProjID != "DEEDs")

#remove summer flounder and brief squid 
prey_selected2 <- prey_selected2 %>% 
  filter(!CommonName %in% c("Summer Flounder", "Brief Squid", "Sea Grapes"))

#plot 
ggplot(data = prey_selected2, aes(x = (d13C), y=(d15N), color = CommonName)) +
  geom_point()+
  geom_point()+
  geom_point(data = kwhelk, aes(x = d13C, y = d15N), color = "darkblue", size = 1, inherit.aes = FALSE)+
  geom_point(data = swhelk, aes(x = d13C, y = d15N), color = "darkred", size = 1, inherit.aes = FALSE)+
  theme_minimal()


#make summary 
prey_selected_summary <- prey_selected %>% 
  group_by(CommonName) %>% 
  summarise(
    meand13C = mean(d13C), 
    sdd13C = sd(d13C), 
    meand15N = mean(d15N), 
    sdd15N = sd(d15N)
  )

#remove whelk 
prey_selected_summary2 <- prey_selected_summary %>% 
  filter(!CommonName %in% c("Knobbed whelk", "Knobbed Whelk", "Smooth Whelk", "Smooth whelk"))

#TDFS from Rich's experiment
N_TDF <- 2.93
C_TDF <- 1.35


#plot with means 
ggplot(data = prey_selected_summary2, aes(x = (meand13C+1.35), y=(meand15N+2.93), color = CommonName)) +
  geom_point()+
  geom_text(aes(label = CommonName), color = "black", size = 2)+
  geom_errorbar(aes(xmin = (meand13C+1.35)-sdd13C, xmax = (meand13C+1.35)+sdd13C))+
  geom_errorbar(aes(ymin = (meand15N+2.93) -sdd15N, ymax = (meand15N+2.93)+sdd15N))+
  geom_point(data = kwhelk, aes(x = d13C, y = d15N), color = "darkblue", size = 1, inherit.aes = FALSE)+
  geom_point(data = swhelk, aes(x = d13C, y = d15N), color = "darkred", size = 1, inherit.aes = FALSE)+
  theme_minimal()




## it looks like TDFs might be off here -- the diet values used for the TDF lab study were 6-9 permil, which is lower to the DE bay signature 



## Subset the raw data for location 
prey_selected2$Lat <- as.numeric(prey_selected2$Lat)
prey_selected2$Long <- -abs(as.numeric(prey_selected2$Long))

#make a plot to look at sampling locations 
states <- map_data("state")

mid_atl <- states %>%
  filter(region %in% c("delaware", "new jersey", "maryland"))

ggplot(data = mid_atl) +
  geom_polygon(aes(x = long, y = lat, group = group), fill = NA, color = "black") +
  geom_point(data = prey_selected2, aes(x = Long, y = Lat, color = CommonName), size = 1)+
  geom_point(data = kwhelk, aes(x = Long, y = Lat), color = "darkblue") + 
  geom_point(data = swhelk, aes(x = Long, y = Lat), color = "darkred") +
  geom_hline(yintercept=39.3)+
  coord_sf(xlim = c(-76, -74), ylim = c(38, 40), expand = FALSE) +
  labs(x = "Longitude", y = "Latitude", title = "Sampling Locations") + 
  theme_minimal() + 
  theme(legend.position = "none")


ggplot(data = mid_atl) +
  geom_polygon(aes(x = long, y = lat, group = group), fill = NA, color = "black") +
  geom_point(data = subset(prey_selected2, CommonName == "Eastern Oyster"), aes(x = Long, y = Lat, color = CommonName), size = 1)+
  geom_point(data = kwhelk, aes(x = Long, y = Lat), color = "darkblue") + 
  geom_point(data = swhelk, aes(x = Long, y = Lat), color = "darkred") +
  geom_hline(yintercept=39.3)+
  coord_sf(xlim = c(-76, -74), ylim = c(38, 40), expand = FALSE) +
  labs(x = "Longitude", y = "Latitude", title = "Sampling Locations") + 
  theme_minimal() + 
  theme(legend.position = "none")


#Subset database for prey less than 39.3

prey_selected3 <- prey_selected2 %>% 
  filter(Lat < 39.3) %>% 
  filter(!is.na(Lat))


ggplot(data = mid_atl) +
  geom_polygon(aes(x = long, y = lat, group = group), fill = NA, color = "black") +
  geom_point(data = prey_selected3, aes(x = Long, y = Lat, color = CommonName), size = 1)+
  #geom_point(data = kwhelk, aes(x = Long, y = Lat), color = "darkblue") + 
  #geom_point(data = swhelk, aes(x = Long, y = Lat), color = "darkred") +
  coord_sf(xlim = c(-76, -74), ylim = c(38, 40), expand = FALSE) +
  labs(x = "Longitude", y = "Latitude", title = "Sampling Locations") + 
  theme_minimal() + 
  theme(legend.position = "none")




#remove specific tissues -- Gill from noah 
prey_selected4 <- prey_selected3 %>% 
  filter(Tissue != "gill") 

prey_selected5 <- prey_selected4 %>% 
  mutate(CommonNameTissue = case_when(
    CommonName == "Horseshoe Crab" & Tissue == "eggs" ~ "Horseshoe Crab Eggs",
    TRUE ~ CommonName
  ))


prey_selected_summary_subsetted <- prey_selected5 %>% 
  group_by(CommonNameTissue) %>% 
  summarise(
    meand13C = mean(d13C, na.rm = TRUE), 
    sdd13C = sd(d13C, na.rm = TRUE), 
    meand15N = mean(d15N, na.rm = TRUE), 
    sdd15N = sd(d15N, na.rm = TRUE)
  )


ggplot(data = prey_selected_summary_subsetted, aes(x = (meand13C+1.35), y=(meand15N+2.93), color = CommonNameTissue)) +
  geom_point()+
  geom_text(aes(label = CommonNameTissue), color = "black", size = 2)+
  geom_errorbar(aes(xmin = (meand13C+1.35)-sdd13C, xmax = (meand13C+1.35)+sdd13C))+
  geom_errorbar(aes(ymin = (meand15N+2.93) -sdd15N, ymax = (meand15N+2.93)+sdd15N))+
  #geom_point(data = kwhelk, aes(x = d13C, y = d15N), color = "darkblue", size = 1, inherit.aes = FALSE)+
  #geom_point(data = swhelk, aes(x = d13C, y = d15N), color = "darkred", size = 1, alpha = 0.5, inherit.aes = FALSE)+
  theme_minimal()




#Create Functional Groups 

#do a hierarchical clustering 

data <- prey_selected_summary_subsetted[,c("meand13C", "meand15N")]
rownames(data) <- prey_selected_summary_subsetted$CommonNameTissue

# Calculate the dissimilarity matrix
diss_matrix <- dist(data)

# Perform hierarchical clustering using complete linkage
hc <- hclust(diss_matrix, method = "complete")
max_diff <- max(diff(hc$height))
plot(hc)
abline(h = max_diff, col = "red") 



#kmeans 
# Create an empty vector to store the WCSS values
wcss <- vector()

# Set the maximum number of clusters to try
max_clusters <- 10

# Perform k-means clustering for different numbers of clusters
for (k in 1:max_clusters) {
  kmeans_output <- kmeans(data, k)
  wcss[k] <- kmeans_output$tot.withinss
}

# Plot the WCSS against the number of clusters
plot_data <- data.frame(Clusters = 1:max_clusters, WCSS = wcss)

ggplot(plot_data, aes(x = Clusters, y = WCSS)) +
  geom_line() +
  geom_point() +
  labs(x = "Number of Clusters", y = "WCSS") +
  ggtitle("Elbow Method Data Clustering") + 
  scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)) +
  theme_minimal()

#Try 4,5,6 
kmeans_result <- kmeans(data, centers = 4, nstart = 25)

#add to data 
prey_selected_summary_subsetted$kmeans <- kmeans_result$cluster


#plot by group 
ggplot(data = prey_selected_summary_subsetted, aes(x = (meand13C+1.35), y=(meand15N+2.93), color = as.factor(kmeans))) +
  geom_point()+
  geom_text(aes(label = CommonNameTissue), color = "black", size = 3)+
  geom_errorbar(aes(xmin = (meand13C+1.35)-sdd13C, xmax = (meand13C+1.35)+sdd13C))+
  geom_errorbar(aes(ymin = (meand15N+2.93) -sdd15N, ymax = (meand15N+2.93)+sdd15N))+
  geom_point(data = kwhelk, aes(x = d13C, y = d15N), color = "darkblue", size = 1, inherit.aes = FALSE)+
  geom_point(data = swhelk, aes(x = d13C, y = d15N), color = "darkred", size = 1, alpha = 0.5, inherit.aes = FALSE)+
  theme_minimal()


#add kmeans group to raw data

prey_selected5 <- left_join(prey_selected5, prey_selected_summary_subsetted, by = "CommonNameTissue")


ggplot(data = prey_selected5, aes(x = (d13C+1.35), y=(d15N+2.93), color = as.factor(kmeans))) +
  geom_point()+
  geom_point(data = kwhelk, aes(x = d13C, y = d15N), color = "darkblue", size = 1, inherit.aes = FALSE)+
  geom_point(data = swhelk, aes(x = d13C, y = d15N), color = "darkred", size = 1, alpha = 0.5, inherit.aes = FALSE)+
  theme_minimal()

#mean and SD for each kmeans group 

kmeans_meansd <- prey_selected5 %>% 
  group_by(kmeans) %>% 
  summarise(
    meand13C = mean(d13C, na.rm = TRUE), 
    sdd13C = sd(d13C, na.rm = TRUE), 
    meand15N = mean(d15N, na.rm = TRUE), 
    sdd15N = sd(d15N, na.rm = TRUE)
  )

#TDFS from Rich's experiment
N_TDF <- 0
C_TDF <- 1.35

#plot by functional group 
ggplot(data = kmeans_meansd, aes(x = (meand13C + C_TDF), y=(meand15N+N_TDF), color = as.factor(kmeans))) +
  geom_point()+
  geom_errorbar(aes(xmin = (meand13C + C_TDF)-sdd13C, xmax = (meand13C + C_TDF)+sdd13C), width = 0.25)+
  geom_errorbar(aes(ymin = (meand15N + N_TDF) -sdd15N, ymax = (meand15N + N_TDF)+sdd15N), width = 0.25)+
  geom_point(data = kwhelk, aes(x = d13C, y = d15N), color = "darkblue", size = 1, inherit.aes = FALSE)+
  geom_point(data = swhelk, aes(x = d13C, y = d15N), color = "darkred", size = 1, alpha = 0.5, inherit.aes = FALSE)+
  theme_minimal()


write.csv(prey_selected5, "Data/kmeans_output.csv")

