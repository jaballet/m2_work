##
##import os
##os.getcwd()
##
##os.chdir('D:\my_Code_Py\ss_30')
##os.getcwd()

import pandas as pd
data32=pd.read_csv('ss_30.csv',encoding='CP949')

print(data32.head())
data32.columns.tolist()

part32=data32[['apart_id', 'local1', 'local2', 'local3', 'apart', 'lat', 'lng', 'age', 'sex', 'living', 'type', 'married', 'agv_s', 'trans_s', 'env_s', 'live_s', 'manage_s', 'OBJECTID', 'HAKGUDO_ID', 'HAKGUDO_NM']]
print(part32.head())

import numpy as np
region_list=np.unique(part32['HAKGUDO_ID'])
print(region_list)
print(len(region_list))

#평균값 구해서 저장

part32_real=[]

for i, part32_content in enumerate(region_list):
    part32_data=part32[part32['HAKGUDO_ID']==region_list[i]]
    mean_part32=np.mean(part32_data.iloc[:,12:17])
    row_data=[part32_content]
    row_data.extend(mean_part32)

    part32_real.append(row_data)
    
part32_real=pd.DataFrame(part32_real)
part32_real.columns=['HAKGUDO_ID','Aagv_s', 'Atrans_s', 'Aenv_s', 'Alive_s', 'Amanage_s']

print(part32_real.head())

#데이터 병합하기 
p_part32=pd.merge(part32,part32_real,on='HAKGUDO_ID')
print(p_part32.head())

print(p_part32.columns.tolist())

p_part32=pd.DataFrame(p_part32)
p_part32.columns=['apart_id', 'local1', 'local2', 'local3', 'apart', 'lat', 'lng', 'age', 'sex', 'living', 'type', 'married', 'agv_s', 'trans_s', 'env_s', 'live_s', 'manage_s', 'OBJECTID', 'HAKGUDO_ID', 'HAKGUDO_NM', 'Aagv_s', 'Atrans_s', 'Aenv_s', 'Alive_s', 'Amanage_s']
print(p_part32.head())

##저장은 실행창에서
p_part32.to_csv('D:\my_Code_Py\ss_30\pp32.csv',encoding='CP949')
