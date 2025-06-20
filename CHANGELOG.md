# Pre-release changes/updates

# accountTMB 0.3.1

## New data model (log-normal - `datamod_lognorm()`)

- Added a new (log-normal) data model `datamod_lognorm()`


# dpmaccount 0.3.0

-  Fixed indexing discrepancy between exposure and flows in cpp code. 
   Significant fix as this has effects on estimates and simulations, especially in 
   high migration cohorts.

- Modified `estimate_account()` to create a seed_list for each cohort independently 
  rather than a single seed_list for all cohorts, to prevent spurious correlations 
  between cohorts.

- Modified `components.dpmaccount_results()` to use the extended cohort-based seed_list.

- Added a utility function `convert_old_dpmaccount_results()` to allow dpmaccount_results objects created pre-v0.2.0 to be updated to use the cohort-level seed_list allowing them to work with the updated `components.dpmaccount_results()` methods (`augment_population()`, `augment_events()`, `augment_rates()`). 

- Added functionality to draw from the posterior predictive distributions.

- Added a check to `datamod_nbinom()` to catch negative, or zero dispersions in disp dataframes and throw an error message if any are detected. 

-  Modified check_sysmod function to allow for cohort-period specification of 
   sysmods with cohort-time subsetting for 'births' that is currently allowed 
   in the age-period specification (restricting the ages of mothers to 
   reasonable ranges).


# dpmaccount 0.2.0

Second release of initial package development.

This version of `dpmaccount` was used in the production the **July 2024 release** of 'Admin-based population estimates for local authorities in England and Wales: Mid-2021, mid-2022 and provisional mid-2023'.

See [here](https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/adminbasedpopulationestimatesforlocalauthoritiesinenglandandwales) for published estimates.
See also [here](https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/internationalmigration/articles/adminbasedpopulationestimates/previousreleases) for our list of associated articles.

**Note**: Admin-based population estimates are official statistics in development while we refine methods and data sources. 
They do not replace our official mid-year population estimates and should not be used for decision making. These statistics should not be used without this warning.


The following changes occurred between v0.1.0 and v0.2.0

Seeds for the 'random'  components of the package are explicitly set as part of
    'estimate_account()' and saved within the dpmaccount results object
    (output from estimate_account()) to improve reproducibility and
    reduce the requirement for seeding before any calls to augment
    functions.
 
-   'estimate_account' now accepts a new input 'seed_in' which can be
    any integer (defaults to NULL)
 
-   'estimate_account' now calls a new 'make_seed_account()' function to
    create a list of 7 seeds which are then stored within the
    dpmaccount_results object
 
-   New seeds:
    -   prior_rate_seed - used in 'augment_rates()' function for drawing
        a prior rate
    -   draw_counts_seed - used in 'components_cohort_account()'
        function for generating demographic counts for a cohort from a
        fitted 'comod' object
    -   draw_par_datamods_seed - used in 'components_cohort_datamods()'
        function for extracting values for data models for a cohort from
        a fitted 'comod' object
    -   draw_rates_ins_seed - used in 'components_cohort_rates()'
        function for generating demographic rates for a cohort from a
        fitted 'comod' object
    -   draw_rates_outs_seed - used in 'components_cohort_rates()'
        function for generating demographic rates for a cohort from a
        fitted 'comod' object
    -   draw_rates_dth_seed - used in 'components_cohort_rates()'
        function for generating demographic rates for a cohort from a
        fitted 'comod' object
    -   draw_rates_bth_seed - used in 'components_cohort_rates()'
        function for generating demographic rates for a cohort from a
        fitted 'comod' object
 
-   Usage of 'estimate_account()' with the default seed_in = NULL will
    still result in the new seed_list being created and stored, however
    these seeds won't be reproducible. For reproducibility this
    'seed_in' should now always be used instead of setting the seed
    externally (e.g. set.seed(seed = 1), augment_population(res))
    
 - `datamod_t()`, `datamod_nbinom()`, and `datamod_poisson()` have each acquired an argument called `scale_ratio`, which allows coverage ratios to be treated as uncertain
 
- Updated implicit checks on 'sex' variable coding for data used to create system & data models to make them explicit. Previously, the only requirement was to have a 'Female' code in data used to create system models. 

 - New errors will be thrown when data used to create data models/system models has levels for the 'sex' variable other than 'Female' and 'Male'
 
- Added a new (poisson) data model `datamod_poisson()`

- Added a new (negative binomial) data model `datamod_nbinom()`

- Changed `summary.dpmaccount_results()` so that it creates an 
  object of class `"dpmaccount_results_summary"`, but does
  not print the object. (Previously it did.) The revised
  summary function assumes that `na_rm` is `TRUE` rather than
  giving the user the choice. (It's very unlikely that users
  would ever look up documentation for a summary function,
  so giving users a choice is pointless complexity.)
 
- Added a print method for objects of class
  `"dpmaccount_results_summary"`, and included
  a 'snapshot test' for it (see
  https://r-pkgs.org/testing-basics.html)
  
- Added internal function `check_no_na()`, which throws an error
  if variable `x` in data frame `df` has an NA
  
- Constructor function 'sysmod' will throw a warning if
  the 'mean' variable in data frame 'mean' contains
  values less than 1e-06 or (provided 'nm_series' is not "ins")
  larger than 10.

 - Documentation updated and typos fixed

 - Structure of package (location of functions) updated 

# dpmacount 0.1.0

Initial development release.

This version of `dpmaccount` was used in the production the **December 2023 release** of 'Admin-based population estimates for local authorities in England and Wales: Mid-2021, mid-2022 and provisional mid-2023'.

See [here](https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/adminbasedpopulationestimatesforlocalauthoritiesinenglandandwales) for published estimates.
See also [here](https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/internationalmigration/articles/adminbasedpopulationestimates/previousreleases) for our list of associated articles.

**Note**: Admin-based population estimates are official statistics in development while we refine methods and data sources. 
They do not replace our official mid-year population estimates and should not be used for decision making. These statistics should not be used without this warning.
