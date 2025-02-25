
# Data
leaching <- "read_measured_datafile"

leaching <- leaching%>%filter(!LeachingReduction<0) ## two negative values were removed

# Numeric variables
leaching <- leaching %>%  mutate(across(c(SeedingTime_4,Leaching,LeachingReduction),as.numeric))
# Factors
leaching <- leaching %>% mutate(across(c(DrainagePeriod,SeedingTime,Treatment,Site,
                  Block,Species),factor))

## working
seedtime.mod <- glmmTMB(log(LeachingReduction) ~poly(SeedingTime_4,2) * DrainagePeriod * Species * Site +
                          (1 + SeedingTime_4|DrainagePeriod:Site:Species:Block),
                          family = gaussian(link = "identity"), 
                        data = leaching)


# Model reduction # all 3 and 4 way interaction which were not significant were removed

seedtime.mod2 <- update(seedtime.mod, .~. -poly(SeedingTime_4,2):DrainagePeriod:Species:Site)
seedtime.mod3 <- update(seedtime.mod2, .~. -DrainagePeriod:Species:Site)

summary(seedtime.mod3)
car::Anova(seedtime.mod3)

# Save fitted values for plotting the regression line in Figure 4

leaching$ModelFits <- fitted(seedtime.mod3)

# Pairwise comparisons after back transformation
species_means <- emmeans(seedtime.mod3, pairwise ~ Species|Site, type = "response")
species_means

Season_means <- emmeans(seedtime.mod3, pairwise ~ DrainagePeriod, type = "response")
Season_means


### analysis for N utpake

# Data
nup <-"read_measured_datafile"

# Remove the single missing value for Leaching reduction
nup <- nup[which(is.na(nup$Nuptake_tot)== FALSE),]

# Numeric variables
nup <-
  nup %>%
  mutate(across(c(SeedingTime_4,
                  SeedingTime,
                  Nuptake_tot),
                as.numeric))

# Factors
nup <-
  nup %>%
  mutate(across(c(Harvest_year,
                  Site,
                  Treatment,
                  Block,
                  Species),
                factor))


# Model to use
seedtime.mod <- lmer((Nuptake_tot) ~ poly(SeedingTime_4,2) * Species * Harvest_year*Site+
                       (1|Block:Harvest_year:Site), data=nup)
# Model reduction
seedtime.mod2 <- update(seedtime.mod, .~. - poly(SeedingTime_4,2):Species:Harvest_year:Site)
seedtime.mod3 <- update(seedtime.mod2, .~. -Species:Harvest_year:Site)
seedtime.mod4 <- update(seedtime.mod3, .~. -poly(SeedingTime_4,2):Harvest_year:Site)

# Model results
anova(seedtime.mod4)
summary(seedtime.mod4)


# # Save fitted values for plotting the regression line in Figure 2

nup$ModelFits <- fitted(seedtime.mod4)

## contrasts for comparison based on year and site

Year_means <- emmeans(seedtime.mod4, pairwise ~ Harvest_year|Site)
Year_means

## contrasts based on species and site
Species_means <- emmeans(seedtime.mod4, pairwise ~ Species|Site)
Species_means


