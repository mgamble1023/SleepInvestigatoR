# SleepInvestigatoR
A function for the analysis of scored rodent sleep

## Overview

SleepInvestigatoR is a flexible R function used for processing scored rodent sleep. It takes multiple idividual scored sleep files and outputs a single .csv file containing 84 different measures. Sleep can be scored in any software of one's choosing as long as scores are output to a .csv file with no header and contains only three states: Wake, NREMS, and REMS. Lastly, there are several plotting functions incoporated for quick generation of graphics to immediately check the output of the most commonly reported measures. A link to a video can be found at the bottom of this page running through how the function is used.

Note: I use '' quotes below to indicate what should be typed as is **without** quotes and "" double quotes to indicate what needs to be typed **with** quotes in the function. R, however, does not distinguish between different quotes and this dichotomy is just for clarity within this document. It should also be said that R is case sensitive and starts at 1 when counting unlike other languages.

## Download R and R Studio

There are many resources on the internet including videos to help you easily download R and R Studio. SleepInvestigatoR does not require anything fancy. 

One resource is provided below:

https://rstudio-education.github.io/hopr/starting.html

## File Types

The most fool proof option is to have all files saved as .csv with no header (see bottom of file). Raw sirenia files are also accepted. By default 'file.type' is set to 'csv', if the user wants to use raw sirenia files they must change 'file.type = "Sirenia"'.

## File Naming

For easiest processing of files I reccomend the following naming convention for all scored sleep files: Animal ID/id factor_Grouping factor/Treatment/Condition_whatever else. Underscores are safest option though it should recognize any punctuation marks between items of the name.

Example files would read: Mouse1_CondtionA_scores, Mouse1_ConditionB_scores, etc. 

SleepInvestigator allows you to set either or both id.factor or grouping.factor to 'FALSE' in which case SleepInvestigator will supply NAs for groups and internally delienate animals respectively.

## Before you run SleepInvestigatoR

SleepInvestgatoR uses base R, the data.table package, and tidyverse to perform its operations. These must be installed prior to use if they have **never** been installed before. This only has to be done once using the install.packages function. The user can quickly do this by deleting the hashtages before the install.packages functions at the top of the script under the 'Necessary Packages' header and hitting run.

**Everytime** before you use SleepInvestigatoR you must load the data.table and tidyverse packages by using the library function. Similar to before, the user can run these library lines at the top of the script.

Lastly, you must designate a filepath which must be the folder name (and associated path) that you put all the scored sleep files in (note no other files can be in this folder)

E.g. filepath for windows: "C:/Users/Mackenzie/Documents/Test Data" (note: the forward slashes)

After naming your filepath (being careful to maintain the quotes and forward slashes) you can highlight the three lines of code under the 'Set working directory...' header and click run

## Loading SleepInvestigatoR

Now put your cursor in front of the 'SleepInvestigatoR' function and hit run. This will load the function into R so you can use it (this will take a few seconds to complete).

Once loaded you can now type into your console: 'SleepInvestigatoR('User determined parameters')' and hit enter to run the function. See below for parameter options.

## Parameter options

There are 18 parameters which you can set, they are listed below:

**1. FileNames**

   By default 'FileNames' are named FileNames. Just type 'FileNames = FileNames' to be thorough.
   
**2. file.type**

   By default 'file.type = "csv"' to read .csv files. Change 'file.type = "Sirenia"' if that is what you are using.
   
**3. epoch.size**

   By default 'epoch.size = 4' (which is in seconds). Set to the number of seconds you scored in.
   
**4. max.hours**

   If 'max.hours' is set to some number then it will truncate **all** file lengths so they equal this number of hours so all files are consistent. Useful if you want to    trim excess scored epochs that are irrelevant. By default this is set to 'NULL' and so wont run.
   
**5. byBlocks**

   By default this is set to 'NULL' so it won't run. If you change 'byBlocks = # (some number)' it will divide each file into that many blocks. For example, if all your    files are six hours long then setting 'byBlocks = 6' will result in hourly data and if set to 3 instead you will have bihourly data. 
   
**6. byTotal**

    By default this is set to 'TRUE' which means that it calculates all measures across the entire scored sleep period. For example, if all your files are 12 hours long     all measures will be calculated over these 12 hours. Both byBlocks and byTotal are able to be run together producing two outputs one for each.
    
**7. score.checker**

    By default this is set to 'TRUE'. Score.checker looks for any Wake to REM transitions across all files which under most circumstances is user error. It will stop the     function and notify the user what file and where this presumed error occured so it can be corrected. The function will then have to be run again. If you are studying     narcolepsy, for example, where this behavior is expected score.checker can be set to 'FALSE' and it will not flag these transitions as issues.
    
**8. score.value.changer**

    SleepInvestigatoR requires sleep-wake to be scored in a 1,2,3 format where 1 = Wake, 2 = NREMS, and 3 = REMS. If you scored your sleep in another format then you can     specify which values are Wake, NREMS, and REMS in that order and it will change them into 1,2,3 format. For example, if you score sleep as Wake is equal to 101,         NREMS = 102, and REMS = 103 then you would write 'score.checker = c(101,102,103)'. By default this is set to 'NULL' and will not run.
   
**9. sleep.adjust**

   sleep.adjust can be used to crop off the beginning of a scored sleep file. There are four options: 'NREMS Onset', 'REMS Onset', 'Sleep Onset', or a user defined fixed    number of epochs. NREMS Onset and REMS Onset will set truncate each animal's file to start at first NREMS or REMS bout (minimum bout length determined in a different    function). Sleep Onset will allow for a combination of NREMS and REMS at the set minimum bout length. By default this is set to 'NULL' and will not run.

**10. id.factor**

   By default this is set to 'TRUE' indicating that that the first word in the file name of each file name is the animal id. When set to 'FALSE' the function will create    an id to keep track of separate files essentially turning it into a file indicator and not an animal indicator

**11. group.factor**

   By default this is set to 'TRUE' indicating that the second word in the file name following punctuation (e.g., underscore) is the grouping factor such as treatment or    condition. When set to 'FALSE' the group.factor column will be filled with NAs

**12. NREM.cutoff**

   Sets the mininum number of uninterrupted epochs to be consider a formal bout of NREMS. This is dependent on epoch.size. So if epoch.size is set to 4 seconds then        setting 'NREM.cutoff = 15' means that the minimum bout length counted as a NREMS bout is 60 secs. What you set as the cutoff will determine other statistics dependent    on bout length. You can set this to 1 so the minimum bout length is equivalent to one epoch. By default this is set to 15.

**13. REM.cutoff**

   See NREM.cutoff for details

**14. Wake.cutoff**

   See NREM.cutoff for details

**15. Sleep.cutoff**

   Similar to other cutoff parameters except this considers NREMS and REMS collapsed so a mixture of the two states can be used to reach the set criterion.

**16. Propensity.cutoff**

   Used to set the cutoff value for minimum bout length in propensity measurements. Value chosen here will be used for **both** states in the propensity measurement. So    if looking at the average duration between Wake and NREMS, setting 'Propensity.cutoff = 15' with an epoch.size of 4 sec will mean that for the bout to be counted both    wake and NREMS will need to be 60 seconds in duration. See NREM.cutoff for more details.

**17. data.format**

   By default the format is set 'long' and can be set to 'wide' instead allowing for easy input into a variety of statistical programs

**18. save.name**

   Choose a name (put into quotes) for the output to saved as. This .csv file will be placed in the same folder as the sleep scored files. For example, 'save.name =        "Test"'. Be sure to remove the analyzed file from the scored sleep files if you want to run SleepInvestigator again as it will produce an error.
   
## Measures


## Plotting

   A small plotting function is included as a separate script for quick and efficient visualization of scored sleep analyzed by SleepInvestigatoR. It plots the most        common sleep sleep measures for a byTotal or byBlock data frame. This includes: percent of each sleep-wake state, latency to NREMS + REMS, state change transition        counts, sleep-wake bout counts, and sleep-wake bout durations. This function allows the user to quickly get a feel for the data to determine if there are possible        issues and look at trends. The graphs for simplicity and ease of fitting on one 'page' mostly omit error margins. This may be changed in future iterations.
   
   ### Function
   
   plot.overview('user defined parameters)
   
   ### Parameters
   
      1. 

## Example Images

   File Organization
   
   ![Model](Images/File_Organization.png)
   
   File Naming and Folder Organization
   
   ![Model](Images/File_Naming_Folder_Organization.png)
   
   Plotting
   
   ![Model]()

## Videos

   1. Download script + install and load necessary packages
   
   2. File format, naming, and setting working directory
   
   3. Load SleepInvestigatoR + run
   
   4. View output
   
   5. Plotting
 
