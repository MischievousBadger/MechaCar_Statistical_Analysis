library(dplyr)

mechacar_mpg <- read.csv(file = 'MechaCar_mpg.csv', check.names = F, stringsAsFactors = F)
View(mechacar_mpg)

lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD, data = mechacar_mpg)

summary(lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD, data = mechacar_mpg))

suspension_coil <- read.csv(file = 'Suspension_Coil.csv', check.names = F, stringsAsFactors = F)
View(suspension_coil)

Mean = mean(suspension_coil$PSI)
Median = mean(suspension_coil$PSI)
Variance = var(suspension_coil$PSI)
SD = sd(suspension_coil$PSI)

total_summary <- data.frame(Mean, Median, Variance, SD)

lot_summary <- suspension_coil %>% group_by(Manufacturing_Lot) %>% summarize(Mean= mean(PSI), Median= median(PSI), Variance= var(PSI), SD = sd(PSI), .groups = 'keep')

t.test(suspension_coil$PSI, mu= 1500)

t.test(subset(suspension_coil, Manufacturing_Lot == 'Lot1')$PSI, mu=1500)

t.test(subset(suspension_coil, Manufacturing_Lot == 'Lot2')$PSI, mu=1500)

t.test(subset(suspension_coil, Manufacturing_Lot == 'Lot3')$PSI, mu=1500)

