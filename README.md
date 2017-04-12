# FEATure

**Install** 

```
library("devtools")  
install_github("claramarquardt/FEATure",dependencies = TRUE)    
library(FEATure)
```  
**Documentation**
- See function_overview.csv for an overview of all the functions (and data sets) included in the package 
- Usage: 
````
1. Generate a local copy of the control.R template located at FEATure/template/control.R. 
2. Modify the settings contained in the control.R file (comments provided)
3. *Construct features - Stage I*
	feature_construction([path to local version of the control.R file])
3. *Compile features - Stage II*
	feature_compilation([path to local version of the control.R file])
````

**Datasets**  
- *gagne_code*: Icd9 code - gagne comorbidity category crosswalk (http://scholar.harvard.edu/gagne/software/combined-comorbidity-score) [* note the crosswalk included in the package includes gagne categories that are assigned a 0 weight (these are not included in the here referenced, publicly available version of the crosswalk)]
- *zip_class*: US zip code - city/state crosswalk
- *flu_data_cdc*: CDC city/state level flu data
- *variable_list_default*: Default feature groupings, i.e. feature subsets

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
git clone https://github.com/ClaraMarquardt/FEATure.git

# 2. Create a new branch
git checkout -b [branch name]

# 3. Push all changes to the branch (assuming all changes have been committed)
git push origin [branch name]

# 4. Test by installing from the branch
library(devtools)
install_git("git://github.com/ClaraMarquardt/FEATure.git", branch = "[branch name])
````
