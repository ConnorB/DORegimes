siteFlowAnova <- aov(avgDO.pctsat ~ Site * flowState, data = doMet)
summary(siteFlowAnova)
TukeyHSD(siteFlowAnova, "Site")
TukeyHSD(siteFlowAnova, "flowState")


# Tukey's HSD post-hoc test
tukey_results <- TukeyHSD(siteFlowAnova)

# Extracting the Tukey HSD results for the main effect or interaction of interest
tukey_sites <- tukey_results$`Site:flowState`

# Generate compact letter displays (CLDs)
cld <- multcompView::multcompLetters4(siteFlowAnova, tukey_results)

# Extract the CLDs for plotting
cld_df <- data.frame(SiteflowState = names(cld$`Site:flowState`$Letters), 
                     Letters = cld$`Site:flowState`$Letters)

# Merging the CLDs with the original data for plotting
plot_data <- doMet %>%
  unite("SiteflowState", Site, flowState, remove = FALSE, sep = ":")

plot_data <- merge(plot_data, cld_df, by = "SiteflowState")

# Creating the boxplot with CLDs
plot_data %>%
  ggplot(aes(x = Site, y = avgDO.pctsat, color = flowState)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.75), alpha = 0.5) +  
  scale_color_brewer(palette = "Dark2") +
  geom_hline(yintercept = 50, linetype = "dashed") +
  geom_text(aes(label = Letters, y = max(avgDO.pctsat, na.rm = T) + 2.5), 
            position = position_dodge(width = 0.75), 
            vjust = 0.2, 
            size = 5) +
  labs(x = "Site", y = expression(Daily ~ DO["%Sat"])) +
  theme_few() +
  theme(legend.title = element_blank(), legend.position = "bottom")


siteFlowAnova <- aov(DOpctsat.amp ~ Site * flowState, data = doMet)
summary(siteFlowAnova)
TukeyHSD(siteFlowAnova, "Site")
TukeyHSD(siteFlowAnova, "flowState")


# Tukey's HSD post-hoc test
tukey_results <- TukeyHSD(siteFlowAnova)

# Extracting the Tukey HSD results for the main effect or interaction of interest
tukey_sites <- tukey_results$`Site:flowState`

# Generate compact letter displays (CLDs)
cld <- multcompView::multcompLetters4(siteFlowAnova, tukey_results)

# Extract the CLDs for plotting
cld_df <- data.frame(SiteflowState = names(cld$`Site:flowState`$Letters), 
                     Letters = cld$`Site:flowState`$Letters)

# Merging the CLDs with the original data for plotting
plot_data <- doMet %>%
  unite("SiteflowState", Site, flowState, remove = FALSE, sep = ":")

plot_data <- merge(plot_data, cld_df, by = "SiteflowState")

# Creating the boxplot with CLDs
plot_data %>%
  ggplot(aes(x = Site, y = DOpctsat.amp, color = flowState)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.75), alpha = 0.5) +  
  scale_color_brewer(palette = "Dark2") +
  geom_text(aes(label = Letters, y = max(DOpctsat.amp, na.rm = T)+2.5), 
            position = position_dodge(width = 0.75), 
            vjust = 0.2, 
            size = 5) +
  labs(x = "Site", y = expression(Daily ~ DO["%Sat"] ~ "Amplitude")) +
  theme_few() +
  theme(legend.title = element_blank(), legend.position = "bottom")


siteSeasonAnova <- aov(avgDO.pctsat ~ Site * season, data = doMet %>% filter(season %in% c("Spring", "Summer")))
summary(siteSeasonAnova)
TukeyHSD(siteSeasonAnova, "Site")
TukeyHSD(siteSeasonAnova, "season")


# Tukey's HSD post-hoc test
tukey_results <- TukeyHSD(siteSeasonAnova)

# Extracting the Tukey HSD results for the main effect or interaction of interest
tukey_sites <- tukey_results$`Site:season`

# Generate compact letter displays (CLDs)
cld <- multcompView::multcompLetters4(siteSeasonAnova, tukey_results)

# Extract the CLDs for plotting
cld_df <- data.frame(SiteSeason = names(cld$`Site:season`$Letters), 
                     Letters = cld$`Site:season`$Letters)

# Merging the CLDs with the original data for plotting
plot_data <- doMet %>%
  unite("SiteSeason", Site, season, remove = FALSE, sep = ":")

plot_data <- merge(plot_data, cld_df, by = "SiteSeason")

# Creating the boxplot with CLDs
plot_data %>%
  ggplot(aes(x = Site, y = avgDO.pctsat, color = season)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.75), alpha = 0.5) +  
  scale_color_manual(values = c("Spring" = "#275E4D", "Summer" = "#FEB424", "Winter" = "#1073d6", "Fall" = "#743203"))+
  geom_text(aes(label = Letters, y = 110), 
            position = position_dodge(width = 0.75), 
            vjust = 0.2, 
            size = 5) +
  labs(x = "Site", y = expression(Daily ~ DO["%Sat"])) +
  theme_few() +
  theme(legend.title = element_blank(), legend.position = "bottom")+
  facet_wrap(~flowState, ncol = 1)


siteSeasonAnova <- aov(DOpctsat.amp ~ Site * season, data = doMet %>% filter(season %in% c("Spring", "Summer")))
summary(siteSeasonAnova)
TukeyHSD(siteSeasonAnova, "Site")
TukeyHSD(siteSeasonAnova, "season")


# Tukey's HSD post-hoc test
tukey_results <- TukeyHSD(siteSeasonAnova)

# Extracting the Tukey HSD results for the main effect or interaction of interest
tukey_sites <- tukey_results$`Site:season`

# Generate compact letter displays (CLDs)
cld <- multcompView::multcompLetters4(siteSeasonAnova, tukey_results)

# Extract the CLDs for plotting
cld_df <- data.frame(SiteSeason = names(cld$`Site:season`$Letters), 
                     Letters = cld$`Site:season`$Letters)

# Merging the CLDs with the original data for plotting
plot_data <- doMet %>%
  unite("SiteSeason", Site, season, remove = FALSE, sep = ":")

plot_data <- merge(plot_data, cld_df, by = "SiteSeason")

# Creating the boxplot with CLDs
plot_data %>%
  ggplot(aes(x = Site, y = DOpctsat.amp, color = season)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.75), alpha = 0.5) +  
  scale_color_manual(values = c("Spring" = "#275E4D", "Summer" = "#FEB424", "Winter" = "#1073d6", "Fall" = "#743203"))+
  geom_text(aes(label = Letters, y = 110), 
            position = position_dodge(width = 0.75), 
            vjust = 0.2, 
            size = 5) +
  labs(x = "Site", y = expression(Daily ~ DO["%Sat"] ~ "Amplitude")) +
  theme_few() +
  theme(legend.title = element_blank(), legend.position = "bottom")+
  facet_wrap(~flowState, ncol = 1)



doMet$WaterYear <- as.factor(doMet$WaterYear)
siteSeasonAnova <- aov(DOpctsat.amp ~ Site * season * WaterYear, data = doMet %>% filter(season %in% c("Spring", "Summer")))
summary(siteSeasonAnova)
TukeyHSD(siteSeasonAnova, "Site")
TukeyHSD(siteSeasonAnova, "season")


# Tukey's HSD post-hoc test
tukey_results <- TukeyHSD(siteSeasonAnova)

# Extracting the Tukey HSD results for the main effect or interaction of interest
tukey_sites <- tukey_results$`Site:season:WaterYear`

# Generate compact letter displays (CLDs)
cld <- multcompView::multcompLetters4(siteSeasonAnova, tukey_results)

# Extract the CLDs for plotting
cld_df <- data.frame(SiteSeason = names(cld$`Site:season:WaterYear`$Letters), 
                     Letters = cld$`Site:season:WaterYear`$Letters)

# Merging the CLDs with the original data for plotting
plot_data <- doMet %>%
  unite("SiteSeason", Site, season, remove = FALSE, sep = ":")

plot_data <- merge(plot_data, cld_df, by = "SiteSeason")

# Creating the boxplot with CLDs
plot_data %>%
  ggplot(aes(x = Site, y = DOpctsat.amp, color = season)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.75), alpha = 0.5) +  
  scale_color_manual(values = c("Spring" = "#275E4D", "Summer" = "#FEB424", "Winter" = "#1073d6", "Fall" = "#743203"))+
  geom_text(aes(label = Letters, y = 110), 
            position = position_dodge(width = 0.75), 
            vjust = 0.2, 
            size = 5) +
  labs(x = "Site", y = expression(Daily ~ DO["%Sat"] ~ "Amplitude")) +
  theme_few() +
  theme(legend.title = element_blank(), legend.position = "bottom")+
  facet_wrap(~flowState, ncol = 1)