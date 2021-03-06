# MechaCar Statistical Analysis

## Linear Regression to Predict MPG
-  Vehicle_weight, spoiler_angle and AWD variables/coefficients provided a non-random amount of variance to the mpg values in the dataset. 
-  The slope of the linear model is considered to be non-zero because the p-value (5.35e-11) is less than zero. 
-  This linear model predicts mpg of MechaCar prototypes effectively approximately 72% of the time as the R-squared value is 71.49%. 

<img width="722" alt="Screen Shot 2022-04-17 at 10 42 57 AM" src="https://user-images.githubusercontent.com/90944163/163721927-9c0ebd98-4d74-409c-8163-2c211a6dd579.png">

<img width="722" alt="Screen Shot 2022-04-17 at 10 44 46 AM" src="https://user-images.githubusercontent.com/90944163/163721940-6778e8ab-7d30-4b03-a651-45038c20295c.png">

## Summary Statistics on Suspension Coils
The design specifications for the MechaCar suspension coils dictate that the variance of the suspension coils must not exceed 100 pounds per square inch.  The current manufacturing data in total_summary shows that the overall variance between the 3 lots is under 100 psi; however, in the lot_summary, Lot3 shows a variance well over the acceptable amount, at 170.28. 

<img width="1131" alt="Screen Shot 2022-04-17 at 11 45 34 AM" src="https://user-images.githubusercontent.com/90944163/163724265-560dbc30-4348-4ed5-943d-7e6a87f9d4b9.png">

<img width="1131" alt="Screen Shot 2022-04-17 at 11 45 47 AM" src="https://user-images.githubusercontent.com/90944163/163724273-5f06487f-9618-4ecb-8be4-9b35196ca7a0.png">

## T-Tests on Suspension Coils
Review of the results of the T-tests on suspensions coils across all lots show that the p-value is not low enough (0.06028) to reject null hypothesis and not statistically different from the population mean.  Similarly, lot 1 and 2 p-values (1 and 0.6072, respectively) are also not low enough to reject the null hypothesis and are not statistically different from the population mean. However, lot 3 p-value is under 0.05 at 0.0417, which is low enough to reject the null hypothesis and is statistically different from the population mean.  

### All Lots
<img width="424" alt="Screen Shot 2022-04-17 at 12 10 48 PM" src="https://user-images.githubusercontent.com/90944163/163725269-b2c383aa-f258-4d7b-b5ca-6c35e9b63b71.png">

### Lot 1
<img width="554" alt="Screen Shot 2022-04-17 at 12 11 14 PM" src="https://user-images.githubusercontent.com/90944163/163725276-76677428-56cd-451d-998d-066673a0ce86.png">

### Lot 2
<img width="554" alt="Screen Shot 2022-04-17 at 12 12 33 PM" src="https://user-images.githubusercontent.com/90944163/163725284-6c911c1d-6aec-4d5f-a042-5f58b37f0a57.png">

### Lot 3
<img width="554" alt="Screen Shot 2022-04-17 at 12 14 23 PM" src="https://user-images.githubusercontent.com/90944163/163725294-08f70335-1d7f-4928-9011-47757364d664.png">


## Study Design:  MechaCar vs Competition
### What metric or metrics are you going to test?
A possible statistical study that can quantify how the MechaCar performs against the competition would be a comparison of fuel efficiency across various conditions (i.e. city vs. highway driving). 

### What is the null hypothesis or alternative hypothesis?
H0: There is no statistical difference between the MechaCar mpg and the competition mpg.
Ha: The mean MechaCar mpg is greater than the mean competition mpg. 

### What statistical test would you use to test the hypothesis, and why?
I would use the t-test to compare data across the competitors.  This test was used in the current project for suspension coils between MechaCars to obtain the appropriate information to compare manufacturing lots and could be used to similarly compare competitors. 

### What data is needed to run the statistical test?
A dataset of information from similar car classes (compact, subcompact, etc.) would be used; variables could include engine size, vehicle weight, AWD (similar to the dataset utilized in the MechaCar MPG linear regression testing above). 
