#     _____          _                        __                     _____                   _     _    __         
#    / ____|        | |                      / _|                   / ____|                 | |   (_)  / _|        
#   | (___     ___  | |_   _   _   _ __     | |_    ___    _ __    | (___    _ __     ___   | |_   _  | |_   _   _ 
#    \___ \   / _ \ | __| | | | | | '_ \    |  _|  / _ \  | '__|    \___ \  | '_ \   / _ \  | __| | | |  _| | | | |
#    ____) | |  __/ | |_  | |_| | | |_) |   | |   | (_) | | |       ____) | | |_) | | (_) | | |_  | | | |   | |_| |
#   |_____/   \___|  \__|  \__,_| | .__/    |_|    \___/  |_|      |_____/  | .__/   \___/   \__| |_| |_|    \__, |
#                                 | |                                       | |                               __/ |
#                                 |_|                                       |_|                              |___/ 
#     __                                   _   _                        _                    _               __    
#    / /                                  | | (_)                      | |                  (_)              \ \   
#   | |    ___        __ _              __| |  _   _ __    ___    ___  | |_    ___    _ __   _    ___   ___   | |  
#   | |   / _ \      / _` |            / _` | | | | '__|  / _ \  / __| | __|  / _ \  | '__| | |  / _ \ / __|  | |  
#   | |  |  __/  _  | (_| |  _   _    | (_| | | | | |    |  __/ | (__  | |_  | (_) | | |    | | |  __/ \__ \  | |  
#   | |   \___| (_)  \__, | (_) ( )    \__,_| |_| |_|     \___|  \___|  \__|  \___/  |_|    |_|  \___| |___/  | |  
#    \_\              __/ |     |/                                                                           /_/   
#                    |___/                                                                                         


# Load required packages
require(data.table)
require(knitr)

username = Sys.info()["nodename"] # determines who's running the scripts (e.g., Bart, George, Hannes)

if (username=="EG0224") { # Hannes @ UvT
	dirs <- NULL
	dirs$temp = "d:\\Datta\\Dropbox\\Tilburg\\Projects\\GfK Singapore\\temp\\"
	dirs$svn = "d:\\Datta\\Dropbox\\Tilburg\\Projects\\GfK Singapore\\SVN_GfkSingapore\\"
	#dirs$dropbox = "d:\\Datta\\Dropbox\\Tilburg\\Projects\\Ownership to Streaming\\Spotify (shared)\\" # 
	try(setwd('D:/Datta/Dropbox/Tilburg/Projects/GfK Singapore'),silent=T)
	}
	
if (username=="HANNESULTRABOOK") { # Hannes @ Private PC
	dirs <- NULL
#	dirs$temp = "d:\\Eigene Dateien\\Dropbox\\Tilburg\\Projects\\Ownership to Streaming\\temp\\"
	#dirs$svn = "d:\\Eigene Dateien\\Dropbox\\Tilburg\\Projects\\Ownership to Streaming\\Spotify SVN\\"
	#dirs$raw_usage = "d:\\Eigene Dateien\\dropbox\\tilburg\\scraper spotify\\" # only available on Hannes' system
	#dirs$raw_usageSQL = "d:\\Eigene Dateien\\dropbox\\tilburg\\scraper spotify SQL\\" # only available on Hannes' system
	#dirs$raw_clients = "d:\\Eigene Dateien\\Dropbox\\Tilburg\\Projects\\Ownership to Streaming\\Scraper\\" # only available on Hannes' system
	#dirs$dropbox = "d:\\Eigene Dateien\\Dropbox\\Tilburg\\Projects\\Ownership to Streaming\\Spotify (shared)\\" # 
	#try(setwd('D:/Eigene Dateien/Dropbox/Tilburg/Projects/Ownership to streaming'),silent=T)
	}
