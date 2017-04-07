# FEATure

**Install** 

```
library("devtools")  
install_github("claramarquardt/FEATure",dependencies = TRUE)    
library(FEATure)
```  
**Documentation**
- See function_overview.csv for an overview of all the functions (and data sets) included in the package 

**Datasets**  

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
