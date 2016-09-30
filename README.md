# Year of the Donald
Mock 2016 general election analysis using 2016 primary data joined with demographic data about each county. A couple machine learning algorithms predict which candidate wins each county.  
Full report in [`full_report/year_of_the_donald.md`](https://github.com/aedobbyn/year-of-the-donald/blob/master/full_report/year_of_the_donald.md); abridged report in [`abridged/donald_abridged.md`](https://github.com/aedobbyn/year-of-the-donald/blob/master/abridged/donald_abridged.md) (less code, more output).

![white_plot](./abridged/donald_abridged_files/figure-html/white_plot-1.png)

## Workflow
* Import data from local SQL database
* Compare Clinton and Trump as if they were on the same ballot using the number of votes they received per county in the primaries (false elquivaleny alert)
* Make a K nearest neighbors and a random forest classification algorithm that use county facts (e.g., population, percent female, percent black, etc.) to classify which candidate will  in a mock Clinton-Trump head-to-head
* Create some visualisations by state  

## Files
* [Primary data](https://www.kaggle.com/benhamner/2016-us-election) from Kaggle in `primary_results.csv`, county data in `county_facts_abr.csv`  
* Compiled markdown in `full_report/year_of_the_donald.md`
* Shortened version in `abridged/donald_abridged.md`
* Straight up R script in `full_report/year_of_the_donald.R`


