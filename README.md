## seam: Synthetic Estimated Average Matchup

Housed within this repository is the SEAM (Synthetic Estimated average matchup)
method for describing batterversus pitcher matchups in baseball, both
numerically and visually. The methodology is accessible through a Shiny application
that allows users to evaluate any batter-pitcher matchup that has occurred or 
could have occurred in  the  last  five  years. 

Overall, this interactive tool has utility for anyone interested in baseball 
ranging from casual fans and sports writers to individual players and team executives.

### Usage

The shiny application can be accessed via:

<https://seam.stat.illinois.edu/app/>

Moreover, for a person copy, the application can be downloaded and used directly
from GitHub with:

```r
# Install dependencies
install.packages(c("shiny", "shinyjs", "DT", "dplyr",
                   "ggplot2", "ggtern", "data.table",
                   "shinyhelper", "DT"), Ncpus = 3L)

# Launch the Shiny Application and install packages
shiny::runGitHub("seam", "cdyoung")
```

### Authors

[Charlie Young](https://github.com/cdyoung), [David Dalpiaz](https://daviddalpiaz.com), and [Daniel Eck](https://stat.illinois.edu/directory/profile/dje13).

### Licensing

GPL (>= 2)
