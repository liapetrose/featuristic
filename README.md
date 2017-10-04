# featuristic

**Install** 

```
library("devtools")  
install_github("sysmedlab/featuristic",dependencies = TRUE)    
library(featuristic)
```  
**Documentation**
- See https://docs.google.com/document/d/1isXpxsgUR-WMvgUvj-3lhcM4W2qvfuZOUX04ELjX9Cc/edit?usp=sharing for a 
full documentation
- See function_overview.csv for an overview of all the functions (and data sets) included in the package 

````

**Datasets**  
- *gagne_code*: Icd9 code - gagne comorbidity category crosswalk (http://scholar.harvard.edu/gagne/software/combined-comorbidity-score) [* note the crosswalk included in the package includes gagne categories that are assigned a 0 weight (these are not included in the here referenced, publicly available version of the crosswalk)]
- *zip_class*: US zip code - city/state crosswalk

**Note**
- Requires the newest version of devtools (https://github.com/hadley/devtools) to be installed
```
library(devtools)  
install_github("hadley/devtools")
```
**Development**
- Package is actively being developed and extended

- To contribute:
````
# 1. Clone the repo
git clone https://github.com/sysmedlab/featuristic.git

# 2. Make/Save any changes 
- Ensure that all dependencies are included in @import statements at the function top and are 
listed in the Imports (CRAN Dependency)/ Remotes (Github Dependency) fields of the DESCRIPTION file
- Recompile the package by executing the 'package_management/package_update.sh' script (Note: Prior to 
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
````
