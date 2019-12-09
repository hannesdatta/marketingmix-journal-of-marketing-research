  _____          __  __            _        _   _               ______ _           _   _      _ _   _               
 |  __ \        |  \/  |          | |      | | (_)             |  ____| |         | | (_)    (_) | (_)              
 | |  | | ___   | \  / | __ _ _ __| | _____| |_ _ _ __   __ _  | |__  | | __ _ ___| |_ _  ___ _| |_ _  ___  ___     
 | |  | |/ _ \  | |\/| |/ _` | '__| |/ / _ \ __| | '_ \ / _` | |  __| | |/ _` / __| __| |/ __| | __| |/ _ \/ __|    
 | |__| | (_) | | |  | | (_| | |  |   <  __/ |_| | | | | (_| | | |____| | (_| \__ \ |_| | (__| | |_| |  __/\__ \    
 |_____/ \___/  |_|  |_|\__,_|_|  |_|\_\___|\__|_|_| |_|\__, | |______|_|\__,_|___/\__|_|\___|_|\__|_|\___||___/    
                                                         __/ |                                                      
  _____            _ _         _____  _  __  __         |___/        ______                          _              
 |  __ \          | | |       |  __ \(_)/ _|/ _|          (_)       |  ____|                        (_)             
 | |__) |___  __ _| | |_   _  | |  | |_| |_| |_ ___ _ __   _ _ __   | |__   _ __ ___   ___ _ __ __ _ _ _ __   __ _  
 |  _  // _ \/ _` | | | | | | | |  | | |  _|  _/ _ \ '__| | | '_ \  |  __| | '_ ` _ \ / _ \ '__/ _` | | '_ \ / _` | 
 | | \ \  __/ (_| | | | |_| | | |__| | | | | ||  __/ |    | | | | | | |____| | | | | |  __/ | | (_| | | | | | (_| | 
 |_|  \_\___|\__,_|_|_|\__, | |_____/|_|_| |_| \___|_|    |_|_| |_| |______|_| |_| |_|\___|_|  \__, |_|_| |_|\__, | 
                        __/ |                                                                   __/ |         __/ | 
                  _   _|___/                _                      _   __  __            _     |___/     ___ |___/  
                 | | |  __ \               | |                    | | |  \/  |          | |      | |    |__ \       
   __ _ _ __   __| | | |  | | _____   _____| | ___  _ __   ___  __| | | \  / | __ _ _ __| | _____| |_ ___  ) |      
  / _` | '_ \ / _` | | |  | |/ _ \ \ / / _ \ |/ _ \| '_ \ / _ \/ _` | | |\/| |/ _` | '__| |/ / _ \ __/ __|/ /       
 | (_| | | | | (_| | | |__| |  __/\ V /  __/ | (_) | |_) |  __/ (_| | | |  | | (_| | |  |   <  __/ |_\__ \_|        
  \__,_|_| |_|\__,_| |_____/ \___| \_/ \___|_|\___/| .__/ \___|\__,_| |_|  |_|\__,_|_|  |_|\_\___|\__|___(_)        
                                                   | |                                                              
                                                   |_|                                                              
												   
												   
by 
Hannes Datta*												   
Harald J. van Heerde
Marnik G. Dekimpe
Jan-Benedict E.M. Steenkamp

* corresponding author (h.datta@tilburguniversity.edu)


################################################################################################################

1) BUILD INSTRUCTIONS

1.1) DEPENDENCIES

- install git, available at https://git-scm.com/download/win
- install GNU Make, available at http://gnuwin32.sourceforge.net/packages/make.htm
- install R, preferrably into your C drive's root directory (e.g., C:\R\...)
  o Within R, install the following packages, run the following code to install packages

	libs <- c('MASS', 'reshape2', 'zoo', 'lattice',
	'compiler', 'latticeExtra', 'stargazer', 'knitr', 'xtable', 
	'car', 'kableExtra', 'stringi', 'ggplot2', 'ggpubr', 'plyr', 
	'gridExtra', 'grid', 'lme4', 'ggthemes', 'textreg', 'sjstats', 
	'Hmisc', 'xlsx', 'data.table', 'stringr', 'psych')

for (lib in libs) {cat(lib,fill=T);install.packages(lib)}

1.2) FOLDER STRUCTURE

Please create a fresh root directory for this project, now referred to as \root.

\root\data <- contains all the raw data, available as a backup copy at S3. 
              Contact the correspnding author to receive it.
			  
\root\shared\code <- GitHub repository, containing code

1.3) CHECKOUT REPOSITORY

Navigate to \root, and checkout the Git code repository, which will automatically be written to \root\spotify.

> git clone https://github.com/hannesdatta/gfk_singapore.git

For the repository to be checked out, you require a username and password (sign up with GitHub first), and you need to be 
registered as a contributor/viewer of this repository (contact the authors).

1.4) REPOSITORY STRUCTURE

\root\spotify\derived       Data preparation
\root\spotify\analysis      Analysis Part 1: Market-share attraction model
\root\spotify\post-analysis Analysis Part 2: Second-stage analysis and tables
\root\spotify\draft         Stores literature reference
\root\spotify\slides_beamer Figures/tables for slides

Each directory contains a makefile in a subdirectory \code.


<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

IMPORTANT: You need to set the dependencies/paths to software in a 
           file called C:\make_research (no file ending!)
		   
This is the content of the file on Hannes's system:

# path to cmd.exe for command line interface, should be the same
SHELL=C:/Windows/System32/cmd.exe
# path to 64-bit R
RBIN64=c:/R/R-3.6.1/bin/x64/
# path to 32-bit R
RBIN32=c:/R/R-3.6.1/bin/i386/

<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

