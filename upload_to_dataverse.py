from datetime import datetime
import json
import requests  # http://docs.python-requests.org/en/master/
import os

#from pandasd import pandas

# Set parameters

dataverse_server = 'https://dataverse.nl' # no trailing slash
api_key = os.environ['DATAVERSE_TOKEN'] # stored in system's environment variables
persistentId = 'doi:10.34894/EVPJTY' # doi or hdl of the dataset

def upload(source, target_path, target_filename, restricted = False):

  with open(source, mode='rb') as file: # b is important -> binary
    file_content = file.read()
  
  files = {'file': (target_filename, file_content)}

  restrict_par = 'false'
  if (restricted==True): restrict_par = 'true'
  
  params = dict(description='',
                directoryLabel = target_path,
                #categories=['Data'],
                restrict=restrict_par)

  params_as_json_string = json.dumps(params)

  payload = dict(jsonData=params_as_json_string)
  
  url_persistent_id = '%s/api/datasets/:persistentId/add?persistentId=%s&key=%s' % (dataverse_server, persistentId, api_key)

  print('making request: %s' % url_persistent_id)
  r = requests.post(url_persistent_id, data=payload, files=files)

  return(r)

#print(upload('fn').json())
import pandas as pd

df = pd.read_csv('docs/files_in_repository.csv', keep_default_na=False)

log = open('dataverse_log.json', 'w', encoding = 'utf-8')
import json
cnt=0
for index, row in df.iterrows():
    cnt+=1
    print(row['filename'], row['path'])
    
    try: 
      res = upload(row['full_filepath'], row['path'], row['filename'], restricted=row['confidential']).json()
    except:
      res = {}
    out = {'full_filepath': row['full_filepath'],
           'request': res}
    log.write(json.dumps(out)+'\n')
    if (cnt>50): break
log.close()
print('done')
