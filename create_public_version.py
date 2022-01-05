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
shutil.rmtree(path, ignore_errors=False, onerror=None)

cnt=0

for index, row in df.iterrows():
    cnt+=1
    print(row['filename'], row['path'])
    os.makedirs(os.path.dirname(path+row['full_filepath']), exist_ok=True)
      
    if (row['confidential']==False):
      shutil.copy2(row['full_filepath'], path+row['full_filepath'])
    else:
      f=open(path+row['full_filepath'],'w')
      f.write('File has been marked as confidential.\nIt is only available upon request for the purpose of replication.')
      f.close()
    
    #if (cnt>50): break

print('done')
