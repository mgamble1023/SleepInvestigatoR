# SleepInvestigatoR
A function for the analysis of rodent scored sleep

## Download R

## File Types

The most fool proof option is to have all files saved as .csv with no header (see image below). Raw sirenia files are also accepted.

(include image)

## File Naming

For easiest processing of files I reccomend the following naming convention for all scored sleep files: 'AnimalID' / 'id factor'_'Grouping factor' / 'Treatment' / 'Condition'_'whatever else'. Underscores are safest option though it should recognize any punctuation marks between items of the name.

So example files would read: Mouse1_Fentanyl_scores, Mouse1_Methadone_scores, etc. 

SleepInvestigator allows you to set either or both id.factor or grouping.factor to 'FALSE' in which case SleepInvestigator will supply NAs for groups and internally delienate animals respectively
