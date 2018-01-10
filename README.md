# featuristic

**Install** 

```
library("devtools")  
install_github("sysmedlab/featuristic",dependencies = TRUE)    
library(featuristic)
```  
**Documentation**
- See https://docs.google.com/document/d/1isXpxsgUR-WMvgUvj-3lhcM4W2qvfuZOUX04ELjX9Cc/edit?usp=sharing 
for a step-by-step guide on how to use the package
- See the _function_overview.csv_ file in this repository for an overview of all 
the functions (and data sets) included in the package 


````
**Datasets**  
- *gagne_code*: Icd9 code - gagne comorbidity category crosswalk (http://scholar.harvard.edu/gagne/software/combined-comorbidity-score) [* note the crosswalk included in the package includes gagne categories that are assigned a 0 weight (these are not included in the here referenced, publicly available version of the crosswalk)]
- *zip_class*: US zip code - city/state crosswalk

**Miscellaneous Notes**
- Requires the newest version of devtools (https://github.com/hadley/devtools) to be installed
```
library(devtools)  
install_github("hadley/devtools")
```
**Development**

- To contribute:
```
# 1. Clone the repo
git clone https://github.com/sysmedlab/featuristic.git

# 2. Make/Save any changes 
- Ensure that all dependencies are included in @import statements at the function top and are 
listed in the Imports (CRAN Dependency)/ Remotes (Github Dependency) fields of the DESCRIPTION file
- Recompile the package by executing the 'package_management /package_update.sh' script (Note: Prior to 
compilation all dependencies need to be (manually) installed)
- Confirm that the compilation was successful by checking the 'package_management/package_update.Rout' 
file and the 'function_overview.csv'

# 2. Create a new branch
git checkout -b [branch name]

# 3. Push all changes to the branch (assuming all changes have been committed)
git push origin [branch name]

* Note: Please see the contribution guide prior to committing 
any changes: 'package_management/contribution_guide.txt'

# 4. Test by installing from the branch
library(devtools)
install_git("git://github.com/sysmedlab/featuristic.git", branch = "[branch name])
```

# Overview of feature construction

The feature construction process enables the user to go from "data", which could be information that is qualitative (race = black) or quantitative (HbA1c = 6.4%) to "features", which is a *quantitative representation* of this information, which can then be fed into a predictive model. This is a critical part of all supervised machine learning models that are used to make predictions (by contrast, unsupervised models create their own features as part of the learning process). As such, there are few general pointers that guide this process:

1. Domain knowledge: Since this process involves creating "predictors" that are then quantitatively treated in models, it allows us to imbue the model with domain specific knowledge in some cases. For example, when creating diagnosis-related features, we can (and do) count the different comorbidities in each timeframe of interest, in addition to just counting diagnoses naively. This is intended to provide a model with additional information that some diagnosis types can be grouped and have meaning beyond other diagnosis. (Just how much meaning is determined by the model itself)
2. Generalizability: Since this process is integral to all ML projects, it is essential that the code herein is generalizable across projects, contingent upon a consistent format and structure to the underlying data.
3. Flexibility: While generalizability is critical, it is also important that this code is flexible in catering to the specific needs of individual projects.  Further, we might want to look back to 3 years of data in one project, but only 6 months in another. These are all use cases that this code handles.
4. Modularity: Having stages and a structure of code that can work independently is key. For example, in a project, the only features of interest might start out as being procedures, demographics, vitals and diagnoses, and after some analysis might be reduced to simply demographics and vitals. It is key that this code handles this situation modularly, such that the new set of features can be compiled without having to re-assemble and re-construct all features.

# Structure of the feature construction process

In going from a single race column to "features for race", consider what might happen - if the column contains text, say, "white", "black", hispanic", "other", then turning this column to features would involve converting it into a *set of* columns, containing 0/1 indicators, so `race_black`, `race_white`, ... , `race_other`. In this single example, what was one column in data is now 4 columns of features. It is thus easy to see how expansive this process can be for data, where generating several tens of thousands of columns of features from different data types is not in the least inconceivable. This necessitates an approach that ensures only the minimal amount of data is being used in creating features. As such, the feature construction process is broken into 4 stages going from data to fully assembled features. These are outlined below:

1. Stage 0 [creating master data subsets from master datasets]: in this stage, we start with all the relevant master datasets that contain information relevant to the cohort and create a single master data subset that is contains only the subset of data that is relevant to our cohort. While fairly simple, this ensures the actual construction of features is not constrained by the size of the cohort. Further, it simplifies the following steps when the initial cohort spans multiple "master data cohorts", eg. the primary care population spans the iCMP cohort and a subset of the BWH ED 2010-12 cohort.

2. Stage 1 [creating the features / feature "assembly"]: this is the core logic stage that turns data to features, proceeding by type of feature (`dem`, `dia`, `med`, `lvs`, `lab`, `prc`, `mic`, `enc`, `ed`). This is explained in depth below.

3. Stage 2 [modify the features]: this stage modifies the "raw features" created in stage 1 through dropping, imputation and / or both, as the case may be. It helps handle missingness by treating missing values and features missing certain values as specified in the `control` file. Importantly, it also implements the "dropping" criteria: if a feature is missing / zero for greater than `x`% of a cohort, it might make sense to drop the feature, because it's too rare to be meaningful in a model / because it might be misleading. This stage prunes all raw feature sets, ensuring only features that less sparse than the dropping thresholds are retained.

4. Stage 3 [compiling the features]: Once "raw mod" features are created for each type of data set, they are finally put together to create one large cohesive data table in this stage. The output of this stage is the final feature set. The compilation of features cannot be done after stage 1 because we rely on stage 2 to drop many redundant features, thereby making filesize manageable, before compiling them into one cohesive set of features.

In addition to speeding up the process and keeping file sizes in check, the code is constructed such that the  4 stages are independent. Thus, the user is able to re-create features with different dropping thresholds by simple re-running stage 3, 4, rather than having to repeat all stages of the process. In other words, as long as the cohort underlying the project does not change, the user need not repeat the entire feature construction process only to create features with a different set of underlying settings.

# Creating the features

## What you need to begin



# Feature Construction

The actual code that constructs the different feature sets is broken into distinct code files for each feature. Most feature sets have separate code files that deal with data from `oncdrs` since those datasets contain slightly different / differently structured data (there is an option to combine these features, though it is recommended that be used with caution). This section captures what happens in each of those files.

1. `dia`

The following diagnosis features can be created for each timeframe of interest:

- count of zocat in each timeframe
- count of zocat mod in each timeframe
- count of multi ccs cat in each timeframe
- count of each gagne category in each timeframe
- cumulative gagne score in each timeframe
- days to last zocat dia in each timeframe
- days to last multi ccs cat in each timeframe