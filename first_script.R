library(readr)
library(dplyr)
nba_savant202695 <- read_csv("~/Downloads/nba_savant202695.csv")


#Remove no data columns
kawhi_shots <- subset(nba_savant202695[,-(18:22)])
str(kawhi_shots)
names(kawhi_shots)
View(kawhi_shots)

# create a summary stats for shots
shot.distance.stats <- kawhi_shots%>%
        summarize(Dist.Min = min(shot_distance),
                  Dist.Max = max(shot_distance),
                  Dist.Mean = mean(shot_distance),
                  Dist.Var = var(shot_distance),
                  Dist.SD = sd(shot_distance),
                  Dist.IQR = IQR(shot_distance))

View(shot.distance.stats)

#View only shots that were misses
kawhi_missed_shots <- kawhi_shots %>%
        filter(shot_made_flag == 0)
View(kawhi_missed_shots)

#View only shots that were made 
kawhi_made_shots <- kawhi_shots %>%
        filter(shot_made_flag == 1)
View(kawhi_made_shots)

#Let's get the total made by action_type
kawhi_made_shots.action <- kawhi_made_shots %>%
        group_by(action_type) %>%
        summarize(Total = n()) %>%
        arrange(desc(Total))
View(kawhi_made_shots.action)

#Let's get the total missed by action_type
kawhi_missed_shots.action <- kawhi_missed_shots %>%
        group_by(action_type) %>%
        summarize(Total = n()) %>%
        arrange(desc(Total))
View(kawhi_missed_shots.action)

#Make a table of total made shots by action_type 
table(kawhi_made_shots$action_type)
#============================================================================
#List of unique action_types
unique(kawhi_shots$action_type)

#Add another column that duplicates action_type
kawhi_shots <- kawhi_shots%>%
        mutate(Specific_type = rep(action_type))

#Check to see its been added
names(kawhi_shots)

#=============================================================================
#Create a lookup table for Specific types of shots (general)
shot.lookup <-data.frame(Specific_type = c("Alley Oop Dunk Shot", "Alley Oop Layup shot", "Cutting Dunk Shot", "Driving Dunk Shot", "Dunk Shot", 
                                           "Putback Dunk Shot", "Running Dunk Shot", "Tip Dunk Shot", "Driving Finger Roll Layup Shot", "Finger Roll Layup Shot", "Driving Reverse Layup Shot",
                                           "Layup Shot", "Putback Layup Shot", "Reverse Layup Shot", "Cutting Layup Shot", "Running Finger Roll Layup Shot", "Running Layup Shot", 
                                           "Driving Layup Shot", "Tip Layup Shot", "Driving Floating Bank Jump Shot", "Driving Floating Jump Shot", "Jump Bank Shot", "Driving Bank shot", 
                                           "Fadeaway Jump Shot", "Floating Jump shot", "Jump Shot", "Pullup Bank shot", "Pullup Jump shot", "Running Jump Shot", "Running Pull-Up Jump Shot", 
                                           "Step Back Jump shot", "Turnaround Bank shot", "Turnaround Fadeaway shot", "Turnaround Jump Shot", "Hook Bank Shot", "Hook Shot", "Driving Bank Hook Shot", 
                                           "Driving Hook Shot", "Turnaround Hook Shot"), 
                         New.Specific_type = c(rep("Alley Oop", 2), 
                                               rep("Dunk", 6), rep("Layup", 11), 
                                               rep("Jump Shot", 15), 
                                               rep("Hook Shot",5)), 
                         stringsAsFactors = FALSE)
View(shot.lookup)
View(kawhi_shots)

#Replace Titles using lookup table
kawhi_shots <- kawhi_shots %>%
        left_join(shot.lookup, by = "Specific_type")
View(kawhi_shots)

#Remove old column of New.Specific_type
kawhi_shots <- kawhi_shots%>%        
        mutate(Specific_type = New.Specific_type)%>% #Overwrite Title with New.Title 
        select(-New.Specific_type) #Removes New.Title
View(kawhi_shots)

#=============================================================================
#Add another column that duplicates the action type
kawhi_shots <-kawhi_shots %>%
        mutate(incMoving_types = rep(action_type))

View(kawhi_shots)

#=============================================================================
#Create a lookup table for Specific types of shots (incMoving)

shot.lookup2 <- data.frame(incMoving_types = c("Alley Oop Dunk Shot", "Alley Oop Layup shot", 
                                               "Cutting Dunk Shot", "Cutting Layup Shot",
                                               "Dunk Shot", 
                                               "Putback Dunk Shot", "Tip Dunk Shot", "Putback Layup Shot","Tip Layup Shot",
                                               "Driving Finger Roll Layup Shot","Driving Dunk Shot","Driving Bank shot", "Driving Layup Shot","Driving Reverse Layup Shot", "Driving Floating Bank Jump Shot", "Driving Floating Jump Shot", 
                                               "Running Finger Roll Layup Shot","Running Layup Shot", "Running Jump Shot", "Running Pull-Up Jump Shot","Driving Bank Hook Shot","Driving Hook Shot", "Running Dunk Shot",
                                               "Layup Shot", "Reverse Layup Shot","Finger Roll Layup Shot", 
                                               "Jump Bank Shot", "Fadeaway Jump Shot", "Floating Jump shot", "Jump Shot", "Pullup Bank shot", "Pullup Jump shot", "Step Back Jump shot", "Turnaround Bank shot", "Turnaround Fadeaway shot", "Turnaround Jump Shot",
                                               "Hook Bank Shot", "Hook Shot","Turnaround Hook Shot"), 
                           New.incMoving_types = c(rep("Alley Oop", 2),
                                                   rep("Cutting", 2),
                                                   rep("Dunk", 1),
                                                   rep("Putback", 4),
                                                   rep("Driving", 14),
                                                   rep("Layup", 3),
                                                   rep("Jump Shot", 10),
                                                   rep("Hook Shot", 3),
                                                   stringsAsFactors = FALSE)
                           
                           
                           
                           View(shot.lookup2)
                           
                           #Replace incMoving_type using lookup table
                           kawhi_shots <- kawhi_shots %>%
                                   left_join(shot.lookup2, by = "incMoving_types")
                           View(kawhi_shots)
                           
                           #Remove old column of New.Specific_type
                           kawhi_shots <- kawhi_shots%>%        
                                   mutate(incMoving_types = New.incMoving_types)%>% #Overwrite Title with New.Title 
                                   select(-New.incMoving_types) #Removes New.Title
                           View(kawhi_shots)
                           
                           
                           
                           
                           
                           #==================================================================================================
                           #Create a summary stats for Kawhi stats that were Jumpers 
                           
                           kawhi_stats <- kawhi_shots %>%
                                   filter(action_type == "Jump Shot") %>%
                                   group_by(opponent) %>%
                                   summarize(Distance.Min = min(shot_distance),
                                             Distance.Max = max(shot_distance),
                                             Distance.Mean = mean(shot_distance),
                                             Distance.SD = sd(shot_distance),
                                             Distance.sum = sum(shot_distance),
                                             No.of.shots = n())
                           View(kawhi_stats)
                           
                           summary(kawhi_shots$action_type == "Jump Shot")
                           summary(kawhi_shots$action_type == "Hook Shot")
                           summary(kawhi_shots$action_type == "Driving Hook Shot")
                           summary(kawhi_shots$action_type == "Turnaround Hook Shot")
                           summary(kawhi_shots$action_type == "Alley Oop")
                           
                           Shots_per_opp <- kawhi_shots %>%
                                   group_by(action_type)
                           
                           
                           
                           
                           
                           