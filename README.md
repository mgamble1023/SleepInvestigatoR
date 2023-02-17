# SleepInvestigatoR
A function for the analysis of scored rodent sleep

## Download R

## File Types

The most fool proof option is to have all files saved as .csv with no header (see image below). Raw sirenia files are also accepted. By default file.type is set to 'cvs', if the user wants to use raw sirenia files they must change file.type = 'Sirenia'.

(include image)

## File Naming

For easiest processing of files I reccomend the following naming convention for all scored sleep files: 'AnimalID' / 'id factor'_'Grouping factor' / 'Treatment' / 'Condition'_'whatever else'. Underscores are safest option though it should recognize any punctuation marks between items of the name.

So example files would read: Mouse1_CondtionA_scores, Mouse1_ConditionB_scores, etc. 

SleepInvestigator allows you to set either or both id.factor or grouping.factor to 'FALSE' in which case SleepInvestigator will supply NAs for groups and internally delienate animals respectively

## Before you run SleepInvestigatoR

SleepInvestgatoR uses base R, the data.table package, and tidyverse to perform its operations. These must be installed prior to use if they have **never** been installed before. This only has to be done once using install.packages function. The user can quickly do this by deleting the hashtages before the install.packages functions at the top of the script under the 'Necessary Packages' header and hitting run.

**Everytime** before you use SleepInvestigatoR you must load the data.table and tidyverse packages by using the library function. Similar to before the user can run these library lines at the top of the script.

Lastly, you must designate a filepath which must be the folder name (and associated path) that you put all the scored sleep files in (note no other files can be in this folder)

E.g. filepath for windows: "C:/Users/Mackenzie/Documents/Test Data" (note: the forward slashes)

(image on finding file path)

After naming your filepath being careful to maintain the quotes and forward slashes you can highlight the three lines of code under the 'Set working directory...' header and run

## Loading SleepInvestigatoR

Now put your cursor in front of the 'SleepInvestigatoR' function and hit run. This will load the function into R so you can use it and will take a few seconds to complete.

Once loaded you can now type into your console: SleepInvestigatoR('User determined parameters') and hit enter to run the function. See below for parameter options.

## Parameter options

There are 18 parameters which you can set they are listed below:

1. FileNames

   By default FileNames are named FileNames. Just type FileNames = FileNames to be thorough.
   
2. file.type

   By default file.type = 'csv' to read .csv files. Change file.type = "Sirenia" if that is what you are using.
   
5. epoch.size

   By default epoch.size = 4 (which is in seconds). Set to the number of seconds you scored in.
   
7. max.hours

   If max.hours is set to some number then it will truncate all file lengths so they equal this number of hours so all files are consistent. Useful if you want to trim
   excess scored epochs that are irrelevant. By default this is set to 'NULL' and so wont run.
   
9. byBlocks

   By default this is set to 'NULL' so won't run. If you change byBlocks = 'some number' it will divide the each file into that many blocks. For example, if all your        files are six hours long then setting byBlocks = 6 will result in hourly data and if set to 3 instead you will have bihourly data. 
   
11. byTotal

    By default this is set to 'TRUE' which means that it calculates all measures across the entire scored sleep period. For example, if all your files are 12 hours long all measures will be calculated over these 12 hours. Both byBlocks and byTotal are able to be run together producing two outputs one for each.
    
13. score.checker

    By defualt this is set to 'TRUE'. Score.checker looks for any Wake to REM transitions across all files which under most circumstances is user error. It will stop the fucntion and notify the user what file and where this presumed error occured so it can be corrected. The function will have to be run again. If you are studying narcolepsy, for example, where is behavior is expected score.checker be set to 'FALSE' and it will not flag these transitions as issues.
    
15. score.value.changer

    SleepInvestigatoR requires sleep-wake to be scored in a 1,2,3 format where 1 = Wake, 2 = NREMS, and 3 = REMS. If you scored your sleep in another format then you can specify which values are Wake, NREMS, and REMS in that order and it will change them into 1,2,3 format. For example, if you score sleep as Wake is equal to 101, NREMS = 102, and REMS = 103 then you would write score.checker = c(101,102,103). By default this is set to 'NULL' and will not run.
   
17. sleep.adjust
18. id.factor
19. group.factor
20. NREM.cutoff
21. REM.cutoff
22. Wake.cutoff
23. Sleep.cutoff
24. Propensity.cutoff
25. data.format
26. save.name
