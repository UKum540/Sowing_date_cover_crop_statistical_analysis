
#setwd("H:/From_SCIENCE/Documents/Klimon/S?tidsfors?g/")
dat1 <- "read_measured_datafile"

dat1 <- mutate(dat1, ID=factor(ID), Species=factor(Species), Date=factor(Date), Block=factor(Block), Organ=factor(Organ))
dat1 <- dat1 %>%
  mutate(Species = recode(Species, "Oat" = "Oats", "Radish" = "Fodder radish"))


## OBS: totally different picture when including the RFs! 

# Pivot data from long to wide format
dat1_wide <- dat1 %>%
  pivot_wider(names_from = Organ, values_from = c(C,N,CN,DM,RS))

dat1_wide

################# total C 
model<-lmer(C_C_total ~ Species * Date + (1|Block), data=dat1_wide)
anova_result <- anova(model)


# transformation 
model2<-lmer(sqrt(C_C_total) ~ Species * Date + (1|Block), data=dat1_wide)

qqnorm(residuals(model2)) 
anova_result <- anova(model2)
print(anova_result) 

# overlall effects 
drop1(model2, test= "Chisq")
model3<-lmer(sqrt(C_C_total) ~ Species + Date + (1|Block), data=dat1_wide)
drop1(model3, test="Chisq")

# pairwise comparisons 
pairs(emmeans(model2, ~ Species))
pairs(emmeans(model2, ~ Date))

################ shoot C 
model<-lmer(C_Shoot ~ Species * Date + (1|Block), data=dat1_wide)
anova_result <- anova(model)

# transformation 
model2<-lmer(sqrt(C_Shoot) ~ Species * Date + (1|Block), data=dat1_wide)
anova_result <- anova(model2)

# overlall effects 
drop1(model2, test= "Chisq")
model3<-lmer(sqrt(C_Shoot) ~ Species + Date + (1|Block), data=dat1_wide)
drop1(model3, test="Chisq")

# pairwise comparisons 
pairs(emmeans(model2, ~ Species))
pairs(emmeans(model2, ~ Date))

################## root C top 
model<-lmer(C_Root_total_top ~ Species * Date + (1|Block), data=dat1_wide)
anova_result <- anova(model)

# transformation 
model2<-lmer(log(C_Root_total_top) ~ Species * Date + (1|Block), data=dat1_wide)

anova_result <- anova(model2)

# overlall effects 
drop1(model2, test= "Chisq")
model3<-lmer(sqrt(C_Root_total_top) ~ Species + Date + (1|Block), data=dat1_wide)
drop1(model3, test="Chisq")

# pairwise comparisons 
pairs(emmeans(model2, ~ Species))
pairs(emmeans(model2, ~ Date))

################## root C sub
model<-lmer(C_Root_total_sub ~ Species * Date + (1|Block), data=dat1_wide)
anova_result <- anova(model)

# transformation 
model2<-lmer(log(C_Root_total_sub) ~ Species * Date + (1|Block), data=dat1_wide)

# overlall effects 
drop1(model2, test= "Chisq")
model3<-lmer(sqrt(C_Root_total_sub) ~ Species + Date + (1|Block), data=dat1_wide)
drop1(model3, test="Chisq")

# pairwise comparisons 
pairs(emmeans(model2, ~ Species))
pairs(emmeans(model2, ~ Date))

#################### stats for N #############################

################# total N 
model <- lmer(N_C_total ~ Species * Date + (1|Block), data=dat1_wide)
anova_result <- anova(model)

# transformation 
model2 <- lmer(sqrt(N_C_total) ~ Species * Date + (1|Block), data=dat1_wide)

anova_result <- anova(model2)


# overall effects 
drop1(model2, test= "Chisq")
model3 <- lmer(sqrt(N_C_total) ~ Species + Date + (1|Block), data=dat1_wide)
anova_result <- anova(model3)
print(anova_result) 
drop1(model3, test="Chisq")

# pairwise comparisons 
pairs(emmeans(model2, ~ Species))
pairs(emmeans(model2, ~ Date))

################ shoot N 
model <- lmer(N_Shoot ~ Species * Date + (1|Block), data=dat1_wide)

# transformation 
model2 <- lmer(sqrt(N_Shoot) ~ Species * Date + (1|Block), data=dat1_wide)

# overall effects 
drop1(model2, test= "Chisq")
model3 <- lmer(sqrt(N_Shoot) ~ Species + Date + (1|Block), data=dat1_wide)
drop1(model3, test="Chisq")

# pairwise comparisons 
pairs(emmeans(model2, ~ Species))
pairs(emmeans(model2, ~ Date))

################## root N top 
model <- lmer(N_Root_total_top ~ Species * Date + (1|Block), data=dat1_wide)
anova_result <- anova(model)

# transformation 
model2 <- lmer(log(N_Root_total_top) ~ Species * Date + (1|Block), data=dat1_wide)

# overall effects 
drop1(model2, test= "Chisq")
model3 <- lmer(sqrt(N_Root_total_top) ~ Species + Date + (1|Block), data=dat1_wide)
drop1(model3, test="Chisq")

# pairwise comparisons 
pairs(emmeans(model2, ~ Species))
pairs(emmeans(model2, ~ Date))

################## root N sub
model <- lmer(N_Root_total_sub ~ Species * Date + (1|Block), data=dat1_wide)
anova_result <- anova(model)

# transformation 
model2 <- lmer(log(N_Root_total_sub) ~ Species * Date + (1|Block), data=dat1_wide)

# overall effects 
drop1(model2, test= "Chisq")
model3 <- lmer(sqrt(N_Root_total_sub) ~ Species + Date + (1|Block), data=dat1_wide)
drop1(model3, test="Chisq")

# pairwise comparisons 
pairs(emmeans(model2, ~ Species))
pairs(emmeans(model2, ~ Date))


################# mean and SE table of all variables ###########################

# Subset the data and select relevant columns using dplyr functions explicitly
N_total <- dat1 %>%
  filter(Organ == "C_total") %>%
  dplyr::select(Date, Species, N)

# Check the structure of your subsetted data to confirm
str(N_total)
N_total

# Group and summarize data to calculate mean and SE using dplyr functions explicitly
mean_N_total <- N_total %>%
  dplyr::group_by(Date, Species) %>%
  dplyr::summarize(
    mean_N = mean(N, na.rm = TRUE),
    se_N = sd(N, na.rm = TRUE) / sqrt(dplyr::n())
  ) %>%
  dplyr::ungroup()

# Print the summarized data
print(mean_N_total)

### RS ratio 

# Subset the data and select relevant columns using dplyr functions explicitly
RS_ratio <- dat1 %>%
  filter(Organ == "Shoot") %>%
  dplyr::select(Date, Species, Block, RS)

# Check the structure of your subsetted data to confirm
str(RS_ratio)
RS_ratio

# Group and summarize data to calculate mean and SE using dplyr functions explicitly
mean_RS <- RS_ratio %>%
  dplyr::group_by(Date, Species) %>%
  dplyr::summarize(
    mean_RS = mean(RS, na.rm = TRUE),
    se_RS = sd(RS, na.rm = TRUE) / sqrt(dplyr::n())
  ) %>%
  dplyr::ungroup()

# Print the summarized data
print(mean_RS)

# stats 
model <- lmer(RS ~ Species * Date + (1|Block), data=RS_ratio)
anova_result <- anova(model)

# transformation 
model2 <- lmer(log(RS) ~ Species * Date + (1|Block), data=RS_ratio)

# overall effects 
drop1(model, test= "Chisq")
model3 <- lmer(log(RS) ~ Species + Date + (1|Block), data=RS_ratio)
drop1(model3, test="Chisq")

# pairwise comparisons 
cld(emmeans(model2, ~ Species | Date))
cld(emmeans(model2, ~ Date | Species))


### Shoot CN ratio ########

# Subset the data and select relevant columns using dplyr functions explicitly
CN_shoot <- dat1 %>%
  filter(Organ == "Shoot") %>%
  dplyr::select(Date, Species, Block, CN)

# Group and summarize data to calculate mean and SE using dplyr functions explicitly
mean_CN_shoot <- CN_shoot %>%
  dplyr::group_by(Date, Species) %>%
  dplyr::summarize(
    mean_CN_shoot = mean(CN, na.rm = TRUE),
    se_CN_shoot = sd(CN, na.rm = TRUE) / sqrt(dplyr::n())
  ) %>%
  dplyr::ungroup()

# Print the summarized data
print(mean_CN_shoot)

# stats 
model <- lmer(CN ~ Species * Date + (1|Block), data=CN_shoot)
anova_result <- anova(model)

# transformation 
model2 <- lmer(log(CN) ~ Species * Date + (1|Block), data=CN_shoot)

# overall effects 
drop1(model, test= "Chisq")
model3 <- lmer(1/sqrt(CN) ~ Species + Date + (1|Block), data=CN_shoot)
drop1(model3, test="Chisq")

# pairwise comparisons 
cld(emmeans(model2, ~ Species | Date))
cld(emmeans(model2, ~ Date | Species))


### root CN ######## 

# Subset the data and select relevant columns using dplyr functions explicitly
CN_root <- dat1 %>%
  filter(Organ == "Root_top") %>%
  dplyr::select(Date, Species, Block, CN)

# Group and summarize data to calculate mean and SE using dplyr functions explicitly
mean_CN_root <- CN_root %>%
  dplyr::group_by(Date, Species) %>%
  dplyr::summarize(
    mean_CN_root = mean(CN, na.rm = TRUE),
    se_CN_root = sd(CN, na.rm = TRUE) / sqrt(dplyr::n())
  ) %>%
  dplyr::ungroup()

# Print the summarized data
print(mean_CN_root)

# stats 
model <- lmer(CN ~ Species * Date + (1|Block), data=CN_root)
anova_result <- anova(model)

# transformation 
model2 <- lmer(log(CN) ~ Species * Date + (1|Block), data=CN_root)


# overall effects 
drop1(model, test= "Chisq")
model3 <- lmer(1/sqrt(CN) ~ Species + Date + (1|Block), data=CN_root)
drop1(model3, test="Chisq")

# pairwise comparisons 
cld(emmeans(model2, ~ Species | Date))
cld(emmeans(model2, ~ Date | Species))


#### shoot DM ##########

# Subset the data and select relevant columns using dplyr functions explicitly
DM <- dat1 %>%
  filter(Organ == "Shoot") %>%
  dplyr::select(Date, Species, DM)

# Group and summarize data to calculate mean and SE using dplyr functions explicitly
mean_DM <- DM %>%
  dplyr::group_by(Date, Species) %>%
  dplyr::summarize(
    mean_DM = mean(DM, na.rm = TRUE),
    se_DM = sd(DM, na.rm = TRUE) / sqrt(dplyr::n())
  ) %>%
  dplyr::ungroup()

# Print the summarized data
print(mean_DM)


############### calculating MEANS and SE across organs ###############

# Group and summarize data to calculate mean and SE using dplyr functions explicitly
means_all <- dat1 %>%
  dplyr::group_by(Date, Species, Organ) %>%
  dplyr::summarize(
    mean_C = mean(C, na.rm = TRUE),
    se_C = sd(C, na.rm = TRUE) / sqrt(dplyr::n())
  ) %>%
  dplyr::ungroup()

# Print the summarized data
print(means_all)
