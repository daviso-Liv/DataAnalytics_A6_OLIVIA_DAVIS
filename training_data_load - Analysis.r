#load the expss library that will be used for variable labeling
library(expss)

#find your working directory for R
getwd()

#change your working directory to the location where your files are
#  (make sure you put the folder path for your new directory between the 
#   quotation marks)
setwd("/Users/macbookpro/Dropbox/dataAnalysis/project")

#load data
a<-readRDS("training_final.RDS")

#label initial_call_type
val_lab(a$initial_call_type)=num_lab("
1 Abdominal Pain-Fever & Cough
2 Abdominal Pain-Fever & Cough
3 Abdominal Pain Fever-Travel
4 Abdominal Pain
5 ACC
6 Active Shooter
7 ADM
8 Altered Mental Status
9 Alt Mental Status-Fever&Cough
10 Amputation, Arm, Hand,Leg,Foot
11 Amputation, Fingers Or Toes
12 Anaphylactic Shock-Fever&Cough
13 Anaphylaxis
14 Card Or Resp Arrest-Fevercough
15 Cardiac Arrest
16 Asthma Attack - Fever&Cough
17 Asthma Patient Fever-Travel
18 Asthma Attack
19 BBP
20 Major Burns 18% Adlt 10% Child
21 Minor Burns <18% Adlt Or <10%
22 Cardiac Condition
23 CARDBR
24 Cardiac Condition-Fever&Cough
25 CDBRFC
26 Child Abuse
27 Choking
28 Choking Fever&Cough
29 Hypothermia
30 COVINF
31 Stroke
32 CVA (Stroke)
33 Stroke Critical - Fever&Cough
34 Stroke - Fever & Cough
35 Death Confirm By Medical Auth
36 Difficult Breather
37 Diff Breathing - Fever&Cough
38 Difficult Breathing Fever-Travel
39 Difficult Breather Rf
40 Death Confirm By Medical Auth
41 DRILL
42 Drowning
43 Hx Drug Or Alcohol Abuse
44 Hx Drug Or Alchl Abuse-Fev&Cou
45 Psychiatric Patient
46 EDPC
47 EDPW
48 Electrocution
49 Evac 
50 Fire75 Working Fire
51 Fire76 High Rise Commercial
52 Fire77 High Rise Residential
53 Gyn Bleeding-Pt Not Pregnant
54 Gyn-Severe Pain-Bleeding
55 Heat Exhaustion
56 Hypertension
57 Internal Bleeding
58 Internal Bleeding-Fever&Cough
59 Inhalation Of Smoke
60 Injury Lower Ext In Elderly
61 Major Injury
62 Minor Injury
63 Non-Critical Injury
64 Jumper Down
65 Jumper Up
66 One Alarm Fire
67 One Alarm Fire
68 Two Alarm Fire
69 Two Alarm Fire
70 Three Alarm Fire
71 Four Alarm Fire
72 MCI25
73 Five Alarm Fire Or Greater
74 Occupied High-Rise Building
75 Occupied High-Rise Building
76 Criminal Detection Facil Incid
77 Report Of Explosives
78 Report Of Explosives
79 Explosion
80 Rapid Transit-Rail Incident
81 Ground Transport Incident
82 Ground Transport Incident
83 Structural Collaspe [Specify]
84 Construction-Demolition Incid
85 Construction-Demolition Incid
86 Confined Space Incident
87 MCI37
88 MCI40
89 Aircraft Incident - Crash
90 Civil Distrubance
91 Hostage Situation - Barricaded
92 Hostage Situation - Barricaded
93 Power Failure - Blackout
94 Active Shooter
95 Active Shooter
96 All Other MCIs
97 All Other MCIs
98 MCI76
99 MCI77
100 Hazardous Materials Incident
101 Hazardous Materials Incident
102 MECHE
103 MECHV
104 Reaction To Med - Fever&Cough
105 Reaction To Medication
106 Medevac, T-C Authority Only
107 MOSILL
108 MOSINJ
109 Auto Accident, No Confirmd Inj
110 Auto Acc W-Injuries
111 MVAINM
112 NOVEH
113 Obstetric Complications
114 Female In Labor
115 Major Obstetrical Complaint
116 Miscarriage
117 Baby Out Or Imminent Birth
118 Unknown Condition
119 Police 10-13, Unconfirmed
120 Police 10-13, Confirmed
121 Sick Ped<5 Yrs-Fever & Cough
122 Sick Ped<5 Yrs-Rash & Fever
123 Pedestrian Struck
124 PEDSTS
125 RADIO
126 Rape
127 Resp Distress - Fever&Cough
128 Respiratory Distress Fever-Travel
129 Respiratory Distress
130 RESPRF
131 SAFE
132 Seizures - Fever & Cough
133 Seizures
134 Gun Shot Wound
135 Sick
136 Sick - Cough & Fever
137 Sick Patient Fever-Travel
138 Sick - Rash And Fever
139 Sick - Minor - Fever & Cough
140 Minor Illness
141 Sick Pediatric, <5 Year Old
142 Special Event
143 Stabbing
144 Status Epilepticus
145 Mult Or Prolong Seizur-Fev&Cou
146 Status Epilepticus Fever-Travel
147 STNDBM
148 Request For Stand-By
149 Stat Transfer Request
150 STUCK
151 T-ARST
152 T-DFBR
153 T-EDP
154 T-INJ
155 T-SICK
156 T-TEXT
157 T-TRMA
158 T-UNC
159 T-UNKN
160 Test Kdt-Modat
161 Multiple Trauma Patient
162 TRAUMS
163 Unconscious Patient
164 Unc Patient - Fever & Cough
165 Unconscious Fever-Travel Patient
166 Unconscious Patient-Rash&Fever
167 Caller Has No Pt Medical Info
168 Venom (Snake Bites)
")

#check that initial call type labels loaded correctly
data.frame(table(a$initial_call_type))

#label final_call_type
val_lab(a$final_call_type)=num_lab("
1 Abdominal Pain-Fever & Cough
2 Abdominal Pain-Fever & Cough
3 Abdominal Pain Fever-Travel
4 Abdominal Pain
5 Active Shooter
6 ALMNFC
7 Altered Mental Status
8 Alt Mental Status-Fever&Cough
9 Amputation, Arm, Hand,Leg,Foot
10 Amputation, Fingers Or Toes
11 Anaphylactic Shock-Fever&Cough
12 Anaphylaxis
13 Card Or Resp Arrest-Fevercough
14 Cardiac Arrest
15 ARSTFC
16 Asthma Attack - Fever&Cough
17 Asthma Attack
18 Major Burns 18% Adlt 10% Child
19 Minor Burns <18% Adlt Or <10%
20 Cardiac Condition
21 CARDBR
22 Cardiac Condition-Fever&Cough
23 CDBRFC
24 Child Abuse
25 Choking
26 Choking Fever&Cough
27 Hypothermia
28 COVINF
29 Stroke
30 CVA (Stroke)
31 Stroke Critical - Fever&Cough
32 Stroke - Fever & Cough
33 Difficult Breather
34 Diff Breathing - Fever&Cough
35 Difficult Breathing Fever-Travel
36 Difficult Breather Rf
37 Death Confirm By Medical Auth
38 Drowning
39 Hx Drug Or Alcohol Abuse
40 Hx Drug Or Alchl Abuse-Fev&Cou
41 Psychiatric Patient
42 EDPC
43 EDPW
44 Electrocution
45 Evac 
46 Fire75 Working Fire
47 Fire77 High Rise Residential
48 Gyn Bleeding-Pt Not Pregnant
49 Gyn-Severe Pain-Bleeding
50 Heat Exhaustion
51 Hypertension
52 Internal Bleeding
53 Internal Bleeding-Fever&Cough
54 Inhalation Of Smoke
55 Injury Lower Ext In Elderly
56 Major Injury
57 Minor Injury
58 Non-Critical Injury
59 Jumper Down
60 Jumper Up
61 One Alarm Fire
62 One Alarm Fire
63 Two Alarm Fire
64 Two Alarm Fire
65 Three Alarm Fire
66 Three Alarm Fire
67 Four Alarm Fire
68 Four Alarm Fire
69 MCI25
70 Five Alarm Fire Or Greater
71 Occupied High-Rise Building
72 Occupied High-Rise Building
73 MCI27
74 Medical Facility Evacuation
75 Criminal Detection Facil Incid
76 Criminal Detection Facil Incid
77 Report Of Explosives
78 Report Of Explosives
79 Explosion
80 Rapid Transit-Rail Incident
81 Rapid Transit-Rail Incident
82 Ground Transport Incident
83 Ground Transport Incident
84 Structural Collaspe [Specify]
85 Structural Collaspe [Specify]
86 Construction-Demolition Incid
87 Construction-Demolition Incid
88 MCI35
89 Confined Space Incident
90 MCI37
91 Marine - Harbor Incident
92 Marine - Harbor Incident
93 MCI40
94 Aircraft Incident - Crash
95 MCI42
96 Civil Distrubance
97 Hostage Situation - Barricaded
98 Hostage Situation - Barricaded
99 Power Failure - Blackout
100 Power Failure - Blackout
101 Active Shooter
102 Active Shooter
103 All Other MCIs
104 All Other MCIs
105 MCI76
106 MCI77
107 Hazardous Materials Incident
108 Hazardous Materials Incident
109 Reaction To Med - Fever&Cough
110 Reaction To Medication
111 Medevac, T-C Authority Only
112 Auto Accident, No Confirmd Inj
113 Auto Acc W-Injuries
114 MVAINM
115 Obstetric Complications
116 Female In Labor
117 Major Obstetrical Complaint
118 Miscarriage
119 Baby Out Or Imminent Birth
120 Unknown Condition
121 Police 10-13, Unconfirmed
122 Police 10-13, Confirmed
123 Sick Ped<5 Yrs-Fever & Cough
124 Sick Ped<5 Yrs-Rash & Fever
125 Pedestrian Struck
126 Rape
127 Resp Distress - Fever&Cough
128 Respiratory Distress Fever-Travel
129 Respiratory Distress
130 RESPRF
131 SAFE
132 Seizures - Fever & Cough
133 Seizures
134 Gun Shot Wound
135 Sick
136 Sick - Cough & Fever
137 Sick Patient Fever-Travel
138 Sick - Rash And Fever
139 SICMFC
140 Minor Illness
141 Sick Pediatric, <5 Year Old
142 Special Event
143 Stabbing
144 Status Epilepticus
145 Mult Or Prolong Seizur-Fev&Cou
146 Status Epilepticus Fever-Travel
147 STNDBM
148 Request For Stand-By
149 Stat Transfer Request
150 T-ABDP
151 T-ARST
152 T-CARD
153 T-CDBR
154 T-DFBR
155 T-EDP
156 T-INBL
157 T-INJ
158 T-OBST
159 T-OTHR
160 T-SICK
161 T-TEXT
162 T-TRMA
163 T-UNC
164 T-UNKN
165 Multiple Trauma Patient
166 Unconscious Patient
167 Unc Patient - Fever & Cough
168 Unconscious Fever-Travel Patient
169 Unconscious Patient-Rash&Fever
170 Caller Has No Pt Medical Info
171 Venom (Snake Bites)
")

#check that final call type labels loaded correctly
data.frame(table(a$final_call_type))

#create month variable
a$month <- format(as.Date(a$incident_dt), "%m")

#create day of week variable
a$dow <- weekdays(as.Date(a$incident_dt))

#label incident disposition code
val_lab(a$incident_disposition_code)=num_lab("
82	transporting patient
83	patient pronounced dead
87	cancelled
90	unfounded
91	condition corrected
92	treated not transported
93	refused medical aid
94	treated and transported
95	triaged at scene no transport
96	patient gone on arrival
")

#check that final call type labels loaded correctly
data.frame(table(a$incident_disposition_code))

#run table of incident disposition code by year
table(a$incident_disposition_code, a$incident_year)

#bar chart of incidents by year
counts_total <- table(a$incident_year) #creates a count of incidents by year and stores it in a new value frame
  barplot(counts_total, xlab="Year", ylab="# of Incidents", col=c("red"), main = "EMS Incidents by Year")

  #line chart of incidents by year
  plot(counts_total, type="o", xlab="Year", ylab="# of Incidents", col=c("red"), main = "EMS Incidents by Year")
  
#multiple line chart of incidents by month and year
  counts_month <- as.data.frame.matrix(table(a$month,a$incident_year)) #creates a count of incidents by year and stores it in a new value frame
  
  library(data.table)
  setDT(counts_month, keep.rownames = TRUE)[] #to create the first row as the row names, i.e years
  colnames(counts_month)[1]<-"month" #rename the first column heading as Month
  
  library(ggplot2) #powerful graphics generator for R
  library(reshape) #to transform a table of rows and columns into individual entries for each row-column combo - think 2008-March and 2009-Feb
  Molten <- melt(counts_month, id.vars = "month")
  colnames(Molten)[2]<-"year" #rename the second column heading as year
  str(Molten) #checks to see whether we have a variable stored as a factor rather than numeric
  Molten$year<-as.numeric(as.character(Molten$year)) #change year from factor to numeric
  Molten$month<-as.numeric(as.character(Molten$month)) #change month from character to numeric
  ggplot(Molten, aes(x = month, y=value, group=year)) + geom_line(aes(color=year)) + xlim(1,12) #generate multiple line plots by year
  ggplot(Molten, aes(x = year, y=value)) + geom_line() + xlim(2008,2016)
  
#multiple charts with incidents by month for a single year
countsmo<-ggplot(Molten, aes(year,value)) +       # we are creating a single year chart
    geom_line() +
    ggtitle("EMS Incident Calls by Month and Year") +
    xlab("year") + ylab("# of Calls") +
    xlim(2008,2016) +
    theme(plot.title = element_text(lineheight=.8, face="bold",
                                    size = 20)) +
    theme(text = element_text(size=18))
countsmo + facet_wrap(~ month, ncol=3) #the use of facet_wrap helps to repeat the single year chart across years

#stacked bar chart  of incident dispositions by year
  counts <- table(a$incident_disposition_code, a$incident_year) #creates a count of incidents by year and stores it in a new value frame
  #plot one - the bar chart with no legend
  opar = par(oma = c(0,0,0,14)) # creates large right margin for plot
  barplot(counts, xlab="Year", col=rainbow(10))
  par(opar) # Reset par
  #plot two - the legend on top of the bar chart
  opar = par(oma = c(0,0,0,0), mar = c(0,0,0,0), new = TRUE) #creates a new margin set up for the legend
  legend(x = "right", legend = rownames(counts), fill = rainbow(10), bty = "n", y.intersp = 2) #creates a legend
  par(opar) # Reset par
  
  
  
  
  
  
  
#------------------------------------------DATA CLEANING begin----------------------------------------------
  
dataFrame <- as.data.frame(a)  
print(sapply(dataFrame, class))
#dataFrame$incident_response_seconds_qy = as.numeric(as.character(dataFrame$incident_response_seconds_qy)) 
dataFrame$incident_response_seconds_qy[is.na(dataFrame$incident_response_seconds_qy)] <- 0  
dataFrame$incident_travel_tm_seconds_qy[is.na(dataFrame$incident_travel_tm_seconds_qy)] <- 0  
dataFrame$incident_disposition_code[is.na(dataFrame$incident_disposition_code)] <- 0  
dataFrame$policeprecinct[is.na(dataFrame$policeprecinct)] <- 0  
dataFrame$citycouncildistrict[is.na(dataFrame$citycouncildistrict)] <- 0  
dataFrame$communitydistrict[is.na(dataFrame$communitydistrict)] <- 0  
dataFrame$communityschooldistrict[is.na(dataFrame$communityschooldistrict)] <- 0  
dataFrame$congressionaldistrict[is.na(dataFrame$congressionaldistrict)] <- 0 
dataFrame$final_call_type[is.na(dataFrame$final_call_type)] <- 0 

dataFrame$cad_incident_id[is.na(dataFrame$cad_incident_id)] <- 0 
dataFrame$initial_severity_level_code[is.na(dataFrame$initial_severity_level_code)] <- 0 
dataFrame$final_severity_level_code[is.na(dataFrame$final_severity_level_code)] <- 0 
dataFrame$valid_dispatch_rspns_time_indc[is.na(dataFrame$valid_dispatch_rspns_time_indc)] <- 0
dataFrame$dispatch_response_seconds_qy[is.na(dataFrame$dispatch_response_seconds_qy)] <- 0
dataFrame$valid_incident_rspns_time_indc[is.na(dataFrame$valid_incident_rspns_time_indc)] <- 0
dataFrame$held_indicator[is.na(dataFrame$held_indicator)] <- 0
dataFrame$borough[is.na(dataFrame$borough)] <- 0
dataFrame$incident_dispatch_area[is.na(dataFrame$incident_dispatch_area)] <- 0
dataFrame$zipcode[is.na(dataFrame$zipcode)] <- 0
dataFrame$reopen_indicator[is.na(dataFrame$reopen_indicator)] <- 0
dataFrame$special_event_indicator[is.na(dataFrame$special_event_indicator)] <- 0
dataFrame$standby_indicator[is.na(dataFrame$standby_indicator)] <- 0
dataFrame$transfer_indicator[is.na(dataFrame$transfer_indicator)] <- 0
dataFrame$incident_year[is.na(dataFrame$incident_year)] <- 0
dataFrame$initial_call_type[is.na(dataFrame$initial_call_type)] <- 0
dataFrame$month[is.na(dataFrame$month)] <- 0
dataFrame$dow[is.na(dataFrame$dow)] <- 0

dataFrame$incident_dt[is.na(dataFrame$incident_dt)] <- Sys.time()

dataFrame$first_assign_dt[is.na(dataFrame$first_assign_dt)] <- Sys.time()
#replace the NA date values with the current date and time for now
dataFrame$first_act_dt[is.na(dataFrame$first_act_dt)] <- Sys.time()
dataFrame$first_on_scene_dt[is.na(dataFrame$first_on_scene_dt)] <- Sys.time()
dataFrame$first_to_hosp_dt[is.na(dataFrame$first_to_hosp_dt)] <- Sys.time()
dataFrame$first_hosp_arrival_dt[is.na(dataFrame$first_hosp_arrival_dt)] <- Sys.time()
dataFrame$incident_close_dt[is.na(dataFrame$incident_close_dt)] <- Sys.time()


View(dataFrame)
str(dataFrame)
library ("dplyr")
manhattanDF <- dataFrame %>% filter(dataFrame$borough == 'MANHATTAN')
brooklynDF <- dataFrame %>% filter(dataFrame$borough == 'BROOKLYN')
View(manhattanDF)
View(brooklynDF)

#filtering out all unneeded columns or the ones that cannot be computed to numeric
manhattanDF <- manhattanDF[, -1]
manhattanDF <- manhattanDF[, -1]
manhattanDF <- manhattanDF[, -1]
manhattanDF <- manhattanDF[, -1]
manhattanDF <- manhattanDF[, -2]
manhattanDF <- manhattanDF[, -4]
manhattanDF <- manhattanDF[, -5]
manhattanDF <- manhattanDF[, -5]
manhattanDF <- manhattanDF[, -5]
manhattanDF <- manhattanDF[, -10]
manhattanDF <- manhattanDF[, -10]
manhattanDF <- manhattanDF[, -10]
manhattanDF <- manhattanDF[, -10]
manhattanDF <- manhattanDF[, -21]
View(manhattanDF)

brooklynDF <- brooklynDF[, -1]
brooklynDF <- brooklynDF[, -1]
brooklynDF <- brooklynDF[, -1]
brooklynDF <- brooklynDF[, -1]
brooklynDF <- brooklynDF[, -2]
brooklynDF <- brooklynDF[, -4]
brooklynDF <- brooklynDF[, -5]
brooklynDF <- brooklynDF[, -5]
brooklynDF <- brooklynDF[, -5]
brooklynDF <- brooklynDF[, -10]
brooklynDF <- brooklynDF[, -10]
brooklynDF <- brooklynDF[, -10]
brooklynDF <- brooklynDF[, -10]
brooklynDF <- brooklynDF[, -21]
View(brooklynDF)

manhattanDF$incident_dt <- as.numeric(manhattanDF$incident_dt)
manhattanDF$first_assign_dt <- as.numeric(manhattanDF$first_assign_dt)
manhattanDF$first_act_dt <- as.numeric(manhattanDF$first_act_dt)
manhattanDF$first_on_scene_dt <- as.numeric(manhattanDF$first_on_scene_dt)
manhattanDF$first_to_hosp_dt <- as.numeric(manhattanDF$first_to_hosp_dt)
manhattanDF$first_hosp_arrival_dt <- as.numeric(manhattanDF$first_hosp_arrival_dt)
manhattanDF$incident_close_dt <- as.numeric(manhattanDF$incident_close_dt)
manhattanDF$initial_call_type <- as.numeric(manhattanDF$initial_call_type)
manhattanDF$final_call_type <- as.numeric(manhattanDF$final_call_type)


manhattanDF <- manhattanDF[, -20]
manhattanDF <- manhattanDF[, -19]
manhattanDF <- manhattanDF[, -18]
View(manhattanDF)


brooklynDF$incident_dt <- as.numeric(brooklynDF$incident_dt)
brooklynDF$first_assign_dt <- as.numeric(brooklynDF$first_assign_dt)
brooklynDF$first_act_dt <- as.numeric(brooklynDF$first_act_dt)
brooklynDF$first_on_scene_dt <- as.numeric(brooklynDF$first_on_scene_dt)
brooklynDF$first_to_hosp_dt <- as.numeric(brooklynDF$first_to_hosp_dt)
brooklynDF$first_hosp_arrival_dt <- as.numeric(brooklynDF$first_hosp_arrival_dt)
brooklynDF$incident_close_dt <- as.numeric(brooklynDF$incident_close_dt)
brooklynDF$initial_call_type <- as.numeric(brooklynDF$initial_call_type)
brooklynDF$final_call_type <- as.numeric(brooklynDF$final_call_type)
brooklynDF <- brooklynDF[, -20]
brooklynDF <- brooklynDF[, -19]
brooklynDF <- brooklynDF[, -18]
View(brooklynDF)

#------------------------------------------DATA CLEANING end------------------------------------------------


  
#-------------------------------RANDOM FOREST MODEL EXPLORATION begin---------------------------------------
  
#Random Forest Data Exploration for regression 
library(randomForest)

head(manhattanDF)
View(manhattanDF)

head(brooklynDF)
View(brooklynDF)

#still getting vector memory errors even after filtering by borough
#specific error: "vector memory exhausted (limit reached?)"
#resolved to take a random sample of a set size
randManhattanDF <- manhattanDF[sample(nrow(manhattanDF), size=10000), ]
View(randManhattanDF)

#Creating the "training dataset" and the "Validation dataset"
#first, set the seed
set.seed(100)  
  
# randomly choose 70% of the data points for training and 30% for validation
train <- sample(nrow(randManhattanDF), 0.7*nrow(randManhattanDF), replace = FALSE)
TrainSet <- randManhattanDF[train,]
ValidSet <- randManhattanDF[-train,]
summary(TrainSet)
summary(ValidSet)  


  
help(randomForest)
model1 <- randomForest(incident_dt ~ dispatch_response_seconds_qy + incident_response_seconds_qy + incident_travel_tm_seconds_qy, data = TrainSet, importance = TRUE) 
model1

model2 <- randomForest(incident_response_seconds_qy ~ incident_dt + first_on_scene_dt + incident_close_dt , data = TrainSet, importance = TRUE) 
model2

importance(model1)
importance(model2)

varImpPlot(model1)
varImpPlot(model2)

# Variable importance from model1
importantData <- as.data.frame(importance(model1))
importantData$Var.Names <- row.names(importantData)
library(ggplot2)
ggplot(importantData, aes(x=Var.Names, y=`%IncMSE`)) +
  geom_segment( aes(x=Var.Names, xend=Var.Names, y=0, yend=`%IncMSE`), color="skyblue") +
  geom_point(aes(size = IncNodePurity), color="blue", alpha=0.6) +
  theme_light() +
  coord_flip() +
  theme(legend.position="bottom",panel.grid.major.y = element_blank(),panel.border = element_blank(),axis.ticks.y = element_blank())

# Variable importance from model2
importantDataModel2 <- as.data.frame(importance(model2))
importantDataModel2$Var.Names <- row.names(importantDataModel2)
ggplot(importantDataModel2, aes(x=Var.Names, y=`%IncMSE`)) +
  geom_segment( aes(x=Var.Names, xend=Var.Names, y=0, yend=`%IncMSE`), color="skyblue") +
  geom_point(aes(size = IncNodePurity), color="blue", alpha=0.6) +
  theme_light() +
  coord_flip() +
  theme(legend.position="bottom",panel.grid.major.y = element_blank(),panel.border = element_blank(),axis.ticks.y = element_blank())


#---------------------------------BROOKLYN--------------------------------------

#still getting vector memory errors even after filtering by borough
#specific error: "vector memory exhausted (limit reached?)"
#resolved to take a random sample of a set size
randBrooklynDF <- brooklynDF[sample(nrow(brooklynDF), size=10000), ]
View(randBrooklynDF)


#Creating the "training dataset" and the "Validation dataset"
#first, set the seed
set.seed(100)  

# randomly choose 70% of the data points for training and 30% for validation
train <- sample(nrow(randBrooklynDF), 0.7*nrow(randBrooklynDF), replace = FALSE)
TrainSet <- randBrooklynDF[train,]
ValidSet <- randBrooklynDF[-train,]
summary(TrainSet)
summary(ValidSet)  



help(randomForest)
model1 <- randomForest(incident_dt ~ dispatch_response_seconds_qy + incident_response_seconds_qy + incident_travel_tm_seconds_qy, data = TrainSet, importance = TRUE) 
model1

model2 <- randomForest(incident_response_seconds_qy ~ incident_dt + first_on_scene_dt + incident_close_dt , data = TrainSet, importance = TRUE) 
model2

importance(model1)
importance(model2)

varImpPlot(model1)
varImpPlot(model2)

# Variable importance from model1
importantData <- as.data.frame(importance(model1))
importantData$Var.Names <- row.names(importantData)
library(ggplot2)
ggplot(importantData, aes(x=Var.Names, y=`%IncMSE`)) +
  geom_segment( aes(x=Var.Names, xend=Var.Names, y=0, yend=`%IncMSE`), color="skyblue") +
  geom_point(aes(size = IncNodePurity), color="blue", alpha=0.6) +
  theme_light() +
  coord_flip() +
  theme(legend.position="bottom",panel.grid.major.y = element_blank(),panel.border = element_blank(),axis.ticks.y = element_blank())

# Variable importance from model2
importantDataModel2 <- as.data.frame(importance(model2))
importantDataModel2$Var.Names <- row.names(importantDataModel2)
ggplot(importantDataModel2, aes(x=Var.Names, y=`%IncMSE`)) +
  geom_segment( aes(x=Var.Names, xend=Var.Names, y=0, yend=`%IncMSE`), color="skyblue") +
  geom_point(aes(size = IncNodePurity), color="blue", alpha=0.6) +
  theme_light() +
  coord_flip() +
  theme(legend.position="bottom",panel.grid.major.y = element_blank(),panel.border = element_blank(),axis.ticks.y = element_blank())
  
#-------------------------------RANDOM FOREST MODEL EXPLORATION end-----------------------------------------  



#-------------------------------KNN REGRESSION MODEL EXPLORATION begin---------------------------------------
library(FNN)
library(MASS)

randManhattanDF <- manhattanDF[sample(nrow(manhattanDF), size=10000), ]
View(randManhattanDF)
randBrooklynDF <- brooklynDF[sample(nrow(brooklynDF), size=10000), ]
View(randBrooklynDF)

#Checking the scales of the variables because the variable with a large scale
#will have a larger effect on distances between observations when using the KNN 
#Algorithm 

#Checking the Variances on features/Colums using the var() function in R
var(randManhattanDF[,3]) 
var(randManhattanDF[,1]) 
var(randManhattanDF[,2]) 



#standardize the variables for all the columns except the Incident Response Seconds variable which we are going predict.
incidentResponseSeconds <- randManhattanDF[,2]
incidentResponseSeconds # what we are predicting 
plot(incidentResponseSeconds)

#use only a subset for the predictors
vars <- c("incident_dt",
           "first_assign_dt",
           "first_act_dt",
           "first_on_scene_dt",
           "first_to_hosp_dt",
           "incident_close_dt")


#Now we want to Standardize the columns except the Incident Response Seconds column 
standardizedManhattanDF <- data.frame(scale(randManhattanDF[vars] ))
var(standardizedManhattanDF[,3]) 
var(standardizedManhattanDF[,1])
var(standardizedManhattanDF[,2])
View(standardizedManhattanDF)

# test set
testIndex <- sample(1:nrow(standardizedManhattanDF), nrow(standardizedManhattanDF)*.25)
testData <- standardizedManhattanDF[testIndex,]
testIncidentRS <- incidentResponseSeconds[testIndex]

# train set
trainData <- standardizedManhattanDF[-testIndex,]
trainIncidentRS <- incidentResponseSeconds[-testIndex]


library(class)
library(dplyr)
library(caret)
# set seed
set.seed(101)

table(is.na(testData))
table(is.na(trainData))


help("knnreg")
predictedIncidentRS <- knnreg(trainIncidentRS ~ incident_dt + first_assign_dt + first_act_dt + first_on_scene_dt + first_to_hosp_dt + incident_close_dt, trainData, k = 10)
testData
head(testData)
predictedValues <- predict(predictedIncidentRS, testData)
predictedValues
plot(predictedValues)
head(predictedIncidentRS)


missClassError <- mean(testIncidentRS - predictedValues)
print(missClassError)

#---------------------------------BROOKLYN--------------------------------------


#Checking the Variances on features/Colums using the var() function in R
var(randBrooklynDF[,3]) 
var(randBrooklynDF[,1]) 
var(randBrooklynDF[,2]) 


#standardize the variables for all the columns except the Incident Response Seconds variable which we are going predict.
incidentResponseSeconds <- randBrooklynDF[,2]
incidentResponseSeconds # what we are predicting 

#use only a subset for the predictors
vars <- c("incident_dt",
          "first_assign_dt",
          "first_act_dt",
          "first_on_scene_dt",
          "first_to_hosp_dt",
          "incident_close_dt")


#Now we want to Standardize the columns except the Incident Response Seconds column 
standardizedBrooklynDF <- data.frame(scale(randBrooklynDF[vars] ))
var(standardizedBrooklynDF[,3]) 
var(standardizedBrooklynDF[,1])
var(standardizedBrooklynDF[,2])
View(standardizedBrooklynDF)

# test set
testIndex <- sample(1:nrow(standardizedBrooklynDF), nrow(standardizedBrooklynDF)*.25)
testData <- standardizedBrooklynDF[testIndex,]
testIncidentRS <- incidentResponseSeconds[testIndex]

# train set
trainData <- standardizedBrooklynDF[-testIndex,]
trainIncidentRS <- incidentResponseSeconds[-testIndex]


library(class)
library(dplyr)
library(caret)
# set seed
set.seed(101)

table(is.na(testData))
table(is.na(trainData))


help("knnreg")
predictedIncidentRS <- knnreg(trainIncidentRS ~ incident_dt + first_assign_dt + first_act_dt + first_on_scene_dt + first_to_hosp_dt + incident_close_dt, trainData, k = 10)
testData
head(testData)
predictedValues <- predict(predictedIncidentRS, testData)
predictedValues
head(predictedIncidentRS)


missRegError <- mean(testIncidentRS - predictedValues)
print(missRegError)


#----------------------------------MODEL 2--------------------------------------


#use only a subset for the predictors
timeVars <- c("dispatch_response_seconds_qy",
          "incident_response_seconds_qy",
          "incident_travel_tm_seconds_qy")

#standardize the variables for all the columns except the Incident Date variable which we are going predict.
incidentDate <- randManhattanDF[,10]
incidentDate # what we are predicting 

#Now we want to Standardize the columns except the Incident Date column 
standardizedManhattanDF <- data.frame(scale(randManhattanDF[timeVars] ))
var(standardizedManhattanDF[,3]) 
var(standardizedManhattanDF[,2])
var(standardizedManhattanDF[,1])
View(standardizedManhattanDF)

# test set
testIndex <- sample(1:nrow(standardizedManhattanDF), nrow(standardizedManhattanDF)*.25)
testData <- standardizedManhattanDF[testIndex,]
testIncidentDate <- incidentDate[testIndex]

# train set
trainData <- standardizedManhattanDF[-testIndex,]
trainIncidentDate <- incidentDate[-testIndex]


library(class)
library(dplyr)
library(caret)
# set seed
set.seed(101)

table(is.na(testData))
table(is.na(trainData))


help("knnreg")
predictedIncidentDate <- knnreg(trainIncidentDate ~ dispatch_response_seconds_qy + incident_response_seconds_qy + incident_travel_tm_seconds_qy, trainData, k = 10)
testData
head(testData)
predictedValues <- predict(predictedIncidentDate, testData)
predictedValues
head(predictedIncidentRS)


missClassError <- mean(testIncidentDate - predictedValues)
print(missClassError)



#standardize the variables for all the columns except the Incident Date variable which we are going predict.
incidentDate <- randBrooklynDF[,10]
incidentDate # what we are predicting 

#Now we want to Standardize the columns except the Incident Date column 
standardizedBrooklynDF <- data.frame(scale(randBrooklynDF[timeVars] ))
var(standardizedBrooklynDF[,3]) 
var(standardizedBrooklynDF[,2])
var(standardizedBrooklynDF[,1])
View(standardizedBrooklynDF)

# test set
testIndex <- sample(1:nrow(standardizedBrooklynDF), nrow(standardizedBrooklynDF)*.25)
testData <- standardizedBrooklynDF[testIndex,]
testIncidentDate <- incidentDate[testIndex]

# train set
trainData <- standardizedBrooklynDF[-testIndex,]
trainIncidentDate <- incidentDate[-testIndex]


library(class)
library(dplyr)
library(caret)
# set seed
set.seed(101)

table(is.na(testData))
table(is.na(trainData))


help("knnreg")
predictedIncidentDate <- knnreg(trainIncidentDate ~ dispatch_response_seconds_qy + incident_response_seconds_qy + incident_travel_tm_seconds_qy, trainData, k = 10)
testData
head(testData)
predictedValues <- predict(predictedIncidentDate, testData)
predictedValues
head(predictedIncidentRS)


missClassError <- mean(testIncidentDate - predictedValues)
print(missClassError)


#-------------------------------KNN REGRESSION MODEL EXPLORATION end-----------------------------------------


