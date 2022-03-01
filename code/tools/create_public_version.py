### CREATES A "PUBLIC" VERSION OF THE REPLICATION PACKAGE
### BY REMOVING FILES MARKET AS CONFIDENTIAL WITH
### SUMMARY FILE INFORMATION.

from datetime import datetime
import os
import pandas as pd
import shutil
 
df = pd.read_csv('docs/files_in_repository.csv', keep_default_na=False)

pd.set_option('display.max_columns', 500)
pd.set_option('display.width', 1000)

path = 'public/'
try:
    shutil.rmtree(path, ignore_errors=False, onerror=None)
except:
    1+1
    
cnt=0

for index, row in df.iterrows():
    cnt+=1
    print(row['filename'], row['path'])
    os.makedirs(os.path.dirname(path+row['full_filepath']), exist_ok=True)
      
    if (row['confidential']==False):
      shutil.copy2(row['full_filepath'], path+row['full_filepath'])
    else:
      f=open(path+row['full_filepath']+'.txt','w')
      f.write('This file replaces the original file (' + row['full_filepath'] + '), which has been marked as confidential and is only available upon request for the purpose of replication. For more information, see readme of this replication package.')
      f.close()
    
    #if (cnt>50): break

print('done')

f = open('public/log.txt', 'w')
f.write('Finished building public copy of repository.\n')
f.close()