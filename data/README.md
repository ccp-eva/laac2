## Data files

Data files used in the analyses including descriptions of the relevant variables.

### social_relation_data

Data files used for fitting social relation models by species as exported from Zoo Monitor. All files have the same structure

* `Configuration Name`: Species
* `SessionID`: identifier of scan session
* `DateTime`: date of scan session
* `Focal Name`: Focal individual
* `All Occurrence Behavior Social Modifier`: all individuals within arm's reach separated by comma

### data_binomial_models_combined.csv

Aggregated data to fit learning slopes models. See `laac2_data_trial.csv` for description of additional variables.

* `time_cat`: categorical variable of time point
* `n`: number of trials completed by subject on this time point in this task
* `sum`: number of trials scored as "correct" for subject on this time point in this task

### inhibit_searched_data.csv

Data file used to analyze response strategies in the inhibit-searched tasks. See `laac2_data_trial.csv` for description of additional variables.

* `search_1`: location searched in first choice round
* `search_2`: location searched in second choice round
* `search_3`: location searched in third choice round

### laac2_data_task.csv                 

Data aggregated by task, individual and time point. See `laac2_data_trial.csv` for description of additional variables.

* `performance`: proportion correct trials, proportion of trials with repeated search for self-ordered-search

### laac2_data_trial.csv                 

Trial by trial raw data.

* `date`: day the data was collected (YYYYMMDD)
* `time_point`: time point
* `session`: session within time point (data collection for each time point was split across sessions)
* `group`: housing group (two chimpanzee groups; otherwise by species)
* `subject`: subject
* `task`: task (see below for details)
* `trial`: trial within task
* `correct_location`: baited location
* `pick`: location picked by the subject
* `condition`: only applicable to population-to-sample (see below)
* `code`: coding of choice, see below for details per task
* `age`: age in years
* `sick_severity`,`test_day`, etc.: a detailed description of the predictor variables included in the data frames can be found in the supplementary material.

### laac_complete.txt                 

Selected data to fit Rasch models described in the appendix of the supplementary material. This includes data from phase 2 of Bohn et al., 2023 (see paper and supplement for details). See `laac2_data_trial.csv` for description of variables.

## Tasks
* `population`/`pop`: Population-to-sample, 12 trials, binary outcome (`0` or `1`) - two conditions: `straight` and `crossed`
* `reasoning`/`reas` : Probabilistic-reasoning, 12 trials, binary outcome (`0` or `1`)     
* `communication`/`comm` : Communicative-cues, 12 trials, binary outcome (`0` or `1`)   
* `visible_food`/`vfood` : Ignore-visible-food, 12 trials, binary outcome (`0` or `1`)
* `inhibit_searched`/`search`: Self-ordered-search, 12 trials, count outcome (`0`, `1` or `2`) 
* `attention`/`attent`: Attention-following, 12 trials, binary outcome (`0` or `1`)



 
