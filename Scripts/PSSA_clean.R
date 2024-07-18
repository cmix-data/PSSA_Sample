library(here)
library(dplyr)
library(tidyr)


#import PSSA data (from the state)
pssa <- read.csv(here("Raw Data", "2023 PSSA School level data.csv"))
#import demographics (from PSD)
demos <- read.csv(here("Raw Data", "2022-2023 Enrollment & Demographics.csv"))
#list of Philly schools so I can get the ULCS code (from PSD)
philly_schools <- read.csv(here("Raw Data", "philadelphia-sd-school_list-2022-2023.csv"))


#format all names in PSSA df
pssa <- pssa |>
  dplyr::rename_with(~ str_replace_all(., "\\.", "_"))

#get names to match in demos df
demos <- demos |>
  dplyr::select(-c(SchoolYear, LearningNetwork, Sector, SubSector, 
                   NotELCount, NotELPCT, NotIEPCount, NotIEPPCT)) |>
  dplyr::rename("ULCS_Code" = ULCSCode,
                "School_Name" = SchoolName,
                "Grade_Level" = GradeLevel,
                "Student_Enrollment" = StudentEnrollment)


#add enrollment for philly schools
philly <- philly_schools |>
  dplyr::select(PA.Code, ULCS.Code, Publication.Name) |>
  dplyr::rename_with(~ str_replace_all(., "\\.", "_"))

demos <- dplyr::left_join(demos, philly, by = c("ULCS_Code"))

#I'm working just with grades 3-8
enroll <- demos |>
  subset(Grade_Level != "00") |>
  subset(Grade_Level != "01") |>
  subset(Grade_Level != "02") |>
  subset(Grade_Level != "09") |>
  subset(Grade_Level != "10") |>
  subset(Grade_Level != "11") |>
  subset(Grade_Level != "12") |>
  subset(Grade_Level != "All Grades") |>
  select(PA_Code, Grade_Level, Student_Enrollment) |>
  rename("School_Number" = PA_Code,
         "Grade" = Grade_Level) |>
  mutate("Grade" = case_when(
    Grade == "03" ~ "3",
    Grade == "04" ~ "4",
    Grade == "05" ~ "5",
    Grade == "06" ~ "6",
    Grade == "07" ~ "7",
    Grade == "08" ~ "8")) |>
  mutate("Group" = "All Students")

name <- demos %>%
  subset(Grade_Level == "All Grades") |>
  select(PA_Code, Publication_Name) |>
  rename(School_Number = PA_Code)


pssa$School_Number <- as.character(pssa$School_Number)

pssa <- left_join(pssa, enroll, by = c("School_Number", "Grade", "Group"))
pssa <- left_join(pssa, name, by = c("School_Number"))

pssa <- pssa |>
  subset(County == "PHILADELPHIA") |>
  subset(Group == "All Students") |>
  subset(Grade != "Total") |>
  subset(Subject != "Science")


#check for schools without enrollment numbers or a publication name
pssa_matched <- pssa |>
  subset(!is.na(Publication_Name))
  

anti <- anti_join(pssa, pssa_matched, by = "School_Number")


if(length(anti$School_Number) == 0) {
  "All schools had a match!"
} else {
  paste0("Schools without match: ", length(unique(anti$School_Number)))
  unique(anti$School_Name)
}




# okay get the numerators where reported
pssa_matched <- pssa_matched |>
  subset(!is.na(Percent_Advanced)) |>
  mutate("Advanced" = round(Number_Scored * (Percent_Advanced/100), 0),
         "Proficient" = round(Number_Scored * (Percent_Proficient/100), 0),
         "Basic" = round(Number_Scored * (Percent_Basic/100), 0),
         "Below Basic" = round(Number_Scored * (Percent_Below_Basic/100), 0),
         "Proficient + Advanced" = Proficient + Advanced)

pssa_matched_long <- pssa_matched |>
  select(Year, AUN, School_Number, District_Name, School_Name, Publication_Name, Subject, Grade, 
         Number_Scored, Advanced, Proficient, Basic, `Below Basic`, `Proficient + Advanced`) |>
  tidyr::pivot_longer(c(Advanced, Proficient, Basic, `Below Basic`, `Proficient + Advanced`), names_to = "PSSA", values_to = "count") |>
  mutate("pct" = count/Number_Scored,
         "pctlabel" = paste0(round(pct*100, 0), "%"))


##save save save
save(pssa_matched, file = here("Clean Data", "pssa_matched.rda"))
save(pssa_matched_long, file = here("Clean Data", "pssa_matched_long.rda"))



#
#make the summaries
#


pssa.proxmatched.grd <- pssa.matched %>%
  subset(Prox.Match.Boolean == TRUE) %>%
  group_by(Subject, Grade, PSSA) %>%
  summarise(count = sum(count)) %>%
  mutate(pct = count/sum(count),
         pctlabel = paste0(round(pct * 100), "%")) %>%
  mutate("MatchType" = "Proximity") %>%
  mutate("Proximity.Match" = "All") %>%
  mutate("SchoolName" = "All")


pssa.proxmatched_three.five <- pssa.matched %>%
  subset(Prox.Match.Boolean == TRUE) %>%
  subset(Grade != "6") %>%
  subset(Grade != "7") %>%
  subset(Grade != "8") %>%
  subset(Grade != "Total") %>%
  group_by(Subject, PSSA) %>%
  summarise(count = sum(count)) %>%
  mutate(pct = count/sum(count),
         pctlabel = paste0(round(pct * 100), "%")) %>%
  mutate("MatchType" = "Proximity") %>%
  mutate("Proximity.Match" = "All") %>%
  mutate("Grade" = "3-5") %>%
  mutate("SchoolName" = "All")

pssa.proxmatched_three.five_scl <- pssa.matched %>%
  subset(Prox.Match.Boolean == TRUE) %>%
  subset(Grade != "6") %>%
  subset(Grade != "7") %>%
  subset(Grade != "8") %>%
  subset(Grade != "Total") %>%
  group_by(Subject, SchoolName, Proximity.Match, PSSA) %>%
  summarise(count = sum(count)) %>%
  mutate(pct = count/sum(count),
         pctlabel = paste0(round(pct * 100), "%")) %>%
  mutate("MatchType" = "Proximity") %>%
  mutate("Grade" = "3-5")

pssa.proxmatched_six.eight <- pssa.matched %>%
  subset(Prox.Match.Boolean == TRUE) %>%
  subset(Grade != "3") %>%
  subset(Grade != "4") %>%
  subset(Grade != "5") %>%
  subset(Grade != "Total") %>%
  group_by(Subject, PSSA) %>%
  summarise(count = sum(count)) %>%
  mutate(pct = count/sum(count),
         pctlabel = paste0(round(pct * 100), "%")) %>%
  mutate("MatchType" = "Proximity") %>%
  mutate("Proximity.Match" = "All") %>%
  mutate("Grade" = "6-8") %>%
  mutate("SchoolName" = "All")

pssa.proxmatched_six.eight_scl <- pssa.matched %>%
  subset(Prox.Match.Boolean == TRUE) %>%
  subset(Grade != "3") %>%
  subset(Grade != "4") %>%
  subset(Grade != "5") %>%
  subset(Grade != "Total") %>%
  group_by(Subject, SchoolName, Proximity.Match, PSSA) %>%
  summarise(count = sum(count)) %>%
  mutate(pct = count/sum(count),
         pctlabel = paste0(round(pct * 100), "%")) %>%
  mutate("MatchType" = "Proximity") %>%
  mutate("Grade" = "6-8")



pssa.proxmatched <- rbind(pssa.proxmatched.grd, pssa.proxmatched_three.five, pssa.proxmatched_six.eight,
                          pssa.proxmatched_six.eight_scl, pssa.proxmatched_three.five_scl)

pssa.proxmatched <- pssa.proxmatched %>%
  rename("School.Match" = Proximity.Match)

summary.pssa.matched_s23 <- rbind(pssa.proxmatched)


#add the school-by-school
byschool.prox <- pssa.matched %>%
  select(Subject, Grade, PSSA, count, pct, pctlabel, SchoolName, Proximity.Match) %>%
  rename("School.Match" = Proximity.Match) %>%
  mutate(MatchType = "Proximity") %>%
  subset(!is.na(School.Match))

# byschool.demo <- pssa.matched %>%
#   select(Subject, Grade, PSSA, count, pct, pctlabel, SchoolName, Demographic.Match) %>%
#   rename("School.Match" = Demographic.Match) %>%
#   mutate(MatchType = "Demographics") %>%
#   subset(!is.na(School.Match))


summary.pssa.matched_s23 <- rbind(summary.pssa.matched_s23, byschool.prox)


summary.pssa.matched_s23 <- summary.pssa.matched_s23 %>%
  mutate("Subject" = case_when(
    Subject == "English Language Arts" ~ "Language Arts",
    Subject == "Math" ~ "Mathematics"))

summary.pssa.matched_s23[summary.pssa.matched_s23$Grade == "Total", "Grade"] <- "All"


#add the IMS data
imspssa <- s23.pssa %>%
  subset(Grade != "2-8") %>%
  subset(Grade != "2") %>%
  subset(LS.EB == "All") %>%
  select(-c(LS.EB)) %>%
  mutate("MatchType" = "IMS") %>%
  mutate("School.Match" = SchoolName) 

count <- imspssa %>%
  select(Subject, Grade, PSSA, count, SchoolName, MatchType, School.Match)

imspssa <- imspssa %>%
  select(-c(count, pctlabel)) %>%
  pivot_wider(names_from = `PSSA`, values_from = `pct`) %>%
  mutate("Advanced" = replace_na(Advanced, 0)) %>%
  mutate("Proficient" = replace_na(Proficient, 0)) %>%
  mutate("Basic" = replace_na(Basic, 0)) %>%
  mutate("Below Basic" = replace_na(`Below Basic`, 0)) %>%
  pivot_longer(-c(Subject, Grade, SchoolName, MatchType, School.Match), names_to = "PSSA", values_to = "pct") %>%
  mutate("pctlabel" = paste0(round(pct * 100), "%"))

imspssa <- left_join(imspssa, count, by = c("Subject", "Grade", "PSSA", "SchoolName", "MatchType", "School.Match"))



imspssa[imspssa$Grade == "3-8", "Grade"] <- "All"

summary.pssa.matched_s23 <- rbind(summary.pssa.matched_s23, imspssa)


summary.pssa.matched_s23$Subject <- factor(summary.pssa.matched_s23$Subject)
summary.pssa.matched_s23$Grade <- factor(summary.pssa.matched_s23$Grade, levels = c("All", "3-5", "6-8", 
                                                                                    "2", "3", "4", "5", "6", "7", "8"))
summary.pssa.matched_s23$MatchType <- factor(summary.pssa.matched_s23$MatchType, levels = c("IMS", "Proximity"))
summary.pssa.matched_s23$SchoolName <- factor(summary.pssa.matched_s23$SchoolName)
summary.pssa.matched_s23$SchoolName <- forcats::fct_relevel(summary.pssa.matched_s23$SchoolName, "All")
summary.pssa.matched_s23$School.Match <- factor(summary.pssa.matched_s23$School.Match)


# test <- distinct(summary.pssa.matched_s23, Subject, Grade, PSSA, MatchType, School.Match, SchoolName, pct, .keep_all = TRUE)
# test <- summary.pssa.matched_s23[duplicated(summary.pssa.matched_s23),]



#create the Proficient + Advanced summary
prof.adv <- summary.pssa.matched_s23 %>%
  select(-c(count, pctlabel)) %>%
  pivot_wider(names_from = `PSSA`, values_from = `pct`) %>%
  mutate("Advanced" = replace_na(Advanced, 0)) %>%
  mutate("Proficient" = replace_na(Proficient, 0)) %>%
  mutate("Proficient + Advanced" = `Advanced` + `Proficient`) %>%
  pivot_longer(-c(Subject, Grade, SchoolName, School.Match, MatchType), names_to = "PSSA", values_to = "pct") %>%
  mutate(pctlabel = paste0(c(round(`pct`, 2)*100), "%")) %>%
  subset(PSSA == "Proficient + Advanced")


summary.pssa.matched_s23 <- rbind(summary.pssa.matched_s23, prof.adv)


summary.pssa.matched_s23$PSSA <- factor(summary.pssa.matched_s23$PSSA, levels = c("Proficient + Advanced", 
                                                                                  "Advanced", "Proficient", 
                                                                                  "Basic", "Below Basic"))

#last thing, add the timing
summary.pssa.matched_s23 <- summary.pssa.matched_s23 %>%
  mutate("Term" = "Spring 2022-2023")



##save save save
save(summary.pssa.matched_s23, file = here("#Stitched and Cleaned Datasets", 
                                           "Summaries", "MAP", "_summary_MAP_s23_pssa_match.rda"))
