# SET UP --------------
#Install and load packages
pkgs <- c("multcomp", "emmeans", "tidyverse", "effects", "lme4", "lmerTest", "psych","ggforce")
lapply(pkgs[!(pkgs %in% installed.packages())], install.packages)
lapply(pkgs, library, character.only = TRUE)

#Set graphics themes and colors
cbp1 <- c("#999999", "#E69F00", "#56B4E9", "#009E73",
          "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
theme_update(text = element_text(size = 18),
      axis.text.x = element_text(size = 18, color = "black"), axis.title.x = element_text(size = 21, margin = margin(t = 0, r = 0, b = 10, l = 0)),
      axis.text.y = element_text(size = 18,  color = "black"), axis.title.y = element_text(size = 21, margin = margin(t = 0, r = 10, b = 0, l = 0)), 
      panel.background = element_blank(),panel.border = element_blank(), panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(), axis.line = element_line(colour = "white"), 
      axis.ticks.length = unit(.25, "cm"), axis.ticks = element_line(size = 1, lineend = "round"),
      legend.key = element_rect(fill = "white"))

# CREATE DATA SET ---------------------------------------------------------------
ds <- read_csv("summary_stats_LSS1.csv", na = "NaN")
ds <- mutate(ds, id = factor(id))
ds[ds["id"] == 13, "search_eyex_speed"] <- NA

# ANALYSES --------------

# Convert from wide to long format =======
dv_vars <- c("walk_eyex_std","search_eyex_std","walk_eyey_std","search_eyey_std")
iv_vars <- c("id")

ds %>% 
  select(c(iv_vars,dv_vars)) %>% 
  gather(key = "cond", value = "std", dv_vars) -> dsl

dsl$dim <- factor(ifelse(is.na(str_extract(dsl$cond,"x")),"y","x"))
dsl$task <- factor(ifelse(is.na(str_extract(dsl$cond,"walk")),"search","walk"),levels = c("walk","search"),labels = c("Walk","Search"))

# Let's summarize some data =======
dsl %>% 
  summarise(stdev = mean(std, na.rm = T), n = n(), se = sd(std, na.rm = T)/sqrt(n))

dsl %>% 
  filter(dim == "x") %>%
  summarise(stdev = mean(std, na.rm = T), n = n(), se = sd(std, na.rm = T)/sqrt(n))

dsl %>% 
  group_by(dim, task) %>% 
  summarise(stdev = mean(std, na.rm = T), n = n(), se = sd(std, na.rm = T)/sqrt(n))
  
# Let's split some data =======
dsl %>% 
  split(.$dim) %>% 
  map(~ lmer(std ~ task + (1|id),data = .)) %>% 
  map(anova)

### Let's break what we did #####
dsl_x <- filter(dsl, dim == "x")
res_x <- lmer(std ~ task + (1|id),data = dsl_x)
anova(res_x)

dsl_y <- filter(dsl, dim == "y")
res_y <- lmer(std ~ task + (1|id),data = dsl_y)
anova(res_y)

# Graph the data =======
ds_sum <-  dsl %>% group_by(dim, task) %>% 
  summarise(stdev = mean(std, na.rm = T), n = n(), se = sd(std, na.rm = T)/sqrt(n), ymin = stdev - se, ymax = stdev + se) 

#Plot summary
ggplot(data = filter(ds_sum, dim == "x"), aes(x = task, group = task,ymin = ymin, ymax = ymax, y= stdev)) + 
  geom_pointrange()

#Plot individuals
ggplot(data = filter(dsl, dim == "x"), aes(y = std, x = task, color = task)) + 
  geom_sina(maxwidth = .5, position = position_dodge(.6), alpha = .5, size = 3)

#Plot both
ggplot() + 
  geom_sina(data = filter(dsl, dim == "x"), aes(y = std, x = task, color = task), maxwidth = .5, position = position_dodge(.6), alpha = .5, size = 3) +
  geom_errorbar(data = ds_sum, aes(x = task, group = task,ymin = ymin, ymax = ymax), size =1, width = .3, position = position_dodge(.6)) 

#Make it pretty
ggplot() + 
  geom_sina(data = filter(dsl, dim == "x"), aes(y = std, x = task, color = task), maxwidth = .5, position = position_dodge(.6), alpha = .5, size = 3) +
  geom_errorbar(data = filter(ds_sum, dim == "x"), aes(x = task, group = task,ymin = ymin, ymax = ymax), size =1, width = .3, position = position_dodge(.6)) +
  scale_color_manual(values = cbp1[c(7,6)], guide = F) + 
  scale_y_continuous(breaks = seq(0,20,5), limits = c(0,20)) + 
  labs(x = "Task", y = "Position SD (º)") 
#ggsave("lss1_position_sd.pdf", units = "in", width = 4, height = 4)

#Plot X and Y (need X and Color aes)
ggplot() + 
  geom_sina(data = dsl, aes(y = std, x = dim, color = task), maxwidth = .5, position = position_dodge(.6), alpha = .5, size = 3) +
  geom_errorbar(data = ds_sum, aes(x = dim, group = task,ymin = ymin, ymax = ymax), size =1, width = .3, position = position_dodge(.6)) +
  scale_color_manual(values = cbp1[c(7,6)], name = "Task") + 
  scale_y_continuous(breaks = seq(0,20,5), limits = c(0,20)) + 
  labs(x = "", y = "Position SD (º)") 


