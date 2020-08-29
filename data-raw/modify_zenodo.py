import argparse
import requests
from pathlib import Path, PurePath

# https://developers.zenodo.org

def getToken(tokenPath):
  with open(Path(tokenPath), 'r') as f:
    token = f.read().replace('\n', '')
  return token

def getBaseUrl(sandbox = True):
  sandy = 'sandbox.' if sandbox else ''
  baseUrl = f'https://{sandy}zenodo.org/api/deposit/depositions'
  return baseUrl

def createDeposition(tokenPath, sandbox = True):
  token = getToken(tokenPath)
  params = {'access_token': token}
  baseUrl = getBaseUrl(sandbox)
  r = requests.post(baseUrl, params = params, json = {})
  return r

def uploadFileToDeposition(rDepo, filePath, tokenPath):
  token = getToken(tokenPath)
  params = {'access_token': token}
  bucketUrl = rDepo.json()['links']['bucket']
  fileName = PurePath(filePath).name
  with open(filePath, 'rb') as f:
    r = requests.put(f'{bucketUrl}/{fileName}', data = f, params = params)
  return r

def createNewVersionOfDeposition(depoId, tokenPath, sandbox = True):
  token = getToken(tokenPath)
  params = {'access_token': token}
  baseUrl = getBaseUrl(sandbox)
  r = requests.post(f'{baseUrl}/{depoId}/actions/newversion', params = params)
  rNew = requests.get(r.json()['links']['latest_draft'], params = params)
  return rNew

def deleteFilesFromDeposition(rDepo, tokenPath):
  token = getToken(tokenPath)
  params = {'access_token': token}
  depoId = rDepo.json()['id']
  bucketUrl = rDepo.json()['links']['bucket']
  baseUrl = '/'.join(bucketUrl.split('/')[:4]) + '/deposit/depositions'
  rList = []
  for f in rDepo.json()['files']:
    fileId = f['id']
    r = requests.delete(f'{baseUrl}/{depoId}/files/{fileId}', params = params)
    rList.append(r)
  return rList

def parse_args():
  parser = argparse.ArgumentParser(description = 'Modify a Zenodo deposition.')
  parser.add_argument('-f', '--filePath', required = True)
  parser.add_argument('-t', '--tokenPath', required = True)
  parser.add_argument('-m', '--mode', default = 'create',
                      choices = ['create', 'update'])
  parser.add_argument('-d', '--depoId')
  parser.add_argument('-s', '--sandbox', action = 'store_true')
  return parser.parse_args()

def main(filePath, tokenPath, mode = 'create', depoId = None, sandbox = True):
  if mode == 'create':
    r1 = createDeposition(tokenPath, sandbox)
    r2 = uploadFileToDeposition(r1, filePath, tokenPath)
    return r2
  elif mode == 'update' and depoId is not None:
    r1 = createNewVersionOfDeposition(depoId, tokenPath, sandbox)
    r2 = deleteFilesFromDeposition(r1, tokenPath)
    r3 = uploadFileToDeposition(r1, filePath, tokenPath)
    return r3
  else:
    raise ValueError('I award you no points.')

if __name__ == '__main__':
    args = parse_args()
    main(**vars(args))
