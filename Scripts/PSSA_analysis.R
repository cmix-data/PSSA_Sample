library(here)
library(dplyr)

#bring in the clean data
load(here("Clean Data", "pssa_matched_long.rda"))
load(here("Clean Data", "pssa_matched.rda"))

year <- as.numeric(pssa_matched_long[1, "Year"])

# all grades
pssa_district <- pssa_matched_long |>
  group_by(District_Name, Subject, PSSA) |>
  summarise(count = sum(count)) |>
  mutate(pct = count/sum(count),
         pctlabel = paste0(round(pct * 100), "%")) |>
  mutate("Publication_Name" = "All") |>
  mutate("Grade" = "All") |>
  mutate("Year" = year)

pssa_school <- pssa_matched_long |>
  group_by(District_Name, Publication_Name, Subject, PSSA) |>
  summarise(count = sum(count)) |>
  mutate(pct = count/sum(count),
         pctlabel = paste0(round(pct * 100), "%")) |>
  mutate("Grade" = "All") |>
  mutate("Year" = year)

# get results by individual grade
pssa_bygrade_district <- pssa_matched_long |>
  group_by(District_Name, Subject, Grade, PSSA) |>
  summarise(count = sum(count)) |>
  mutate(pct = count/sum(count),
         pctlabel = paste0(round(pct * 100), "%")) |>
  mutate("Publication_Name" = "All") |>
  mutate("Year" = year)


pssa_bygrade_school <- pssa_matched_long |>
  group_by(District_Name, Publication_Name, Subject, Grade, PSSA) |>
  summarise(count = sum(count)) |>
  mutate(pct = count/sum(count),
         pctlabel = paste0(round(pct * 100), "%")) |>
  mutate("Year" = year)


# get results for grades 3-5
pssa_bygrade_district_threefive <- pssa_matched_long |>
  subset(Grade != "6") |>
  subset(Grade != "7") |>
  subset(Grade != "8") |>
  group_by(District_Name, Subject, PSSA) |>
  summarise(count = sum(count)) |>
  mutate(pct = count/sum(count),
         pctlabel = paste0(round(pct * 100), "%")) |>
  mutate("Publication_Name" = "All") |>
  mutate("Grade" = "3-5") |>
  mutate("Year" = year)


pssa_bygrade_school_threefive <- pssa_matched_long |>
  subset(Grade != "6") |>
  subset(Grade != "7") |>
  subset(Grade != "8") |>
  group_by(District_Name, Publication_Name, Subject, PSSA) |>
  summarise(count = sum(count)) |>
  mutate(pct = count/sum(count),
         pctlabel = paste0(round(pct * 100), "%")) |>
  mutate("Grade" = "3-5") |>
  mutate("Year" = year)

# get results for grades 6-8
pssa_bygrade_district_sixeight <- pssa_matched_long |>
  subset(Grade != "3") |>
  subset(Grade != "4") |>
  subset(Grade != "5") |>
  group_by(District_Name, Subject, PSSA) |>
  summarise(count = sum(count)) |>
  mutate(pct = count/sum(count),
         pctlabel = paste0(round(pct * 100), "%")) |>
  mutate("Publication_Name" = "All") |>
  mutate("Grade" = "6-8") |>
  mutate("Year" = year)


pssa_bygrade_school_sixeight <- pssa_matched_long |>
  subset(Grade != "3") |>
  subset(Grade != "4") |>
  subset(Grade != "5") |>
  group_by(District_Name, Publication_Name, Subject, PSSA) |>
  summarise(count = sum(count)) |>
  mutate(pct = count/sum(count),
         pctlabel = paste0(round(pct * 100), "%")) |>
  mutate("Grade" = "6-8") |>
  mutate("Year" = year)


pssa_summary <- rbind(pssa_district, pssa_school, 
                      pssa_bygrade_district, pssa_bygrade_school,
                      pssa_bygrade_district_threefive, pssa_bygrade_school_threefive,
                      pssa_bygrade_district_sixeight, pssa_bygrade_school_sixeight)



# % tested
tested <- pssa_matched |>
  select(c(Year, District_Name, Publication_Name, Grade, Subject, Number_Scored, Student_Enrollment))

# all grades
tested_district <- tested |>
  group_by(District_Name, Subject) |>
  summarise(Number_Scored = sum(Number_Scored),
            Student_Enrollment = sum(Student_Enrollment)) |>
  mutate("Publication_Name" = "All") |>
  mutate("Grade" = "All") |>
  mutate("Year" = year)

tested_school <- tested |>
  group_by(District_Name, Publication_Name, Subject) |>
  summarise(Number_Scored = sum(Number_Scored),
            Student_Enrollment = sum(Student_Enrollment)) |>
  mutate("Grade" = "All") |>
  mutate("Year" = year)

# tested by grade
tested_district_bygrade <- tested |>
  group_by(District_Name, Subject, Grade) |>
  summarise(Number_Scored = sum(Number_Scored),
            Student_Enrollment = sum(Student_Enrollment)) |>
  mutate("Publication_Name" = "All") |>
  mutate("Year" = year)


# tested grades 3-5
tested_district_threefive <- tested |>
  subset(Grade != "6") |>
  subset(Grade != "7") |>
  subset(Grade != "8") |>
  group_by(District_Name, Subject) |>
  summarise(Number_Scored = sum(Number_Scored),
            Student_Enrollment = sum(Student_Enrollment)) |>
  mutate("Publication_Name" = "All") |>
  mutate("Grade" = "3-5") |>
  mutate("Year" = year)

tested_school_threefive <- tested |>
  subset(Grade != "6") |>
  subset(Grade != "7") |>
  subset(Grade != "8") |>
  group_by(District_Name, Publication_Name, Subject) |>
  summarise(Number_Scored = sum(Number_Scored),
            Student_Enrollment = sum(Student_Enrollment)) |>
  mutate("Grade" = "3-5") |>
  mutate("Year" = year)

# tested grades 6-8
tested_district_sixeight <- tested |>
  subset(Grade != "3") |>
  subset(Grade != "4") |>
  subset(Grade != "5") |>
  group_by(District_Name, Subject) |>
  summarise(Number_Scored = sum(Number_Scored),
            Student_Enrollment = sum(Student_Enrollment)) |>
  mutate("Publication_Name" = "All") |>
  mutate("Grade" = "6-8") |>
  mutate("Year" = year)

tested_school_sixeight <- tested |>
  subset(Grade != "3") |>
  subset(Grade != "4") |>
  subset(Grade != "5") |>
  group_by(District_Name, Publication_Name, Subject) |>
  summarise(Number_Scored = sum(Number_Scored),
            Student_Enrollment = sum(Student_Enrollment)) |>
  mutate("Grade" = "6-8") |>
  mutate("Year" = year)

tested_summary <- rbind(tested, 
                        tested_district_bygrade, 
                        tested_district, tested_school,
                        tested_district_threefive, tested_school_threefive,
                        tested_district_sixeight, tested_school_sixeight)

tested_summary <- tested_summary |>
  mutate(pct = Number_Scored / Student_Enrollment) |>
  mutate(pctlabel = paste0(round(pct * 100), "%"))



##save save save
save(pssa_summary, file = here("Clean Data", "summary_pssa.rda"))
save(tested_summary, file = here("Clean Data", "summary_tested.rda"))