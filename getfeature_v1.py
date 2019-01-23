#!/usr/bin/python
#coding:utf-8
#@ZHOU_YING
#2018-10-27
from allensdk.core.cell_types_cache import CellTypesCache
from allensdk.core.nwb_data_set import NwbDataSet
from allensdk.ephys.extract_cell_features import extract_cell_features
from collections import defaultdict
import pandas as pd
import os
ctc=CellTypesCache(manifest_file='/mnt/f/allen_cell_type/manifest.json')
path='/mnt/f/temp/allen/'
os.chdir(path)
basename="_ephys.nwb"
file_list=pd.read_csv('id.csv',header=None,usecols=[0],skiprows=1)
features_list=['tau','input_resistance','vm_for_sag','fi_fit_slope','sag','rheobase_i','v_baseline']
features=defaultdict(list)
for i in range(0,len(file_list)):
    filename=str(file_list.loc[i,0])+basename
    data_set = NwbDataSet(filename)
    sweeps = ctc.get_ephys_sweeps(file_list.loc[i,0])
    # group the sweeps by stimulus 
    sweep_numbers = defaultdict(list)
    for sweep in sweeps:
        sweep_numbers[sweep['stimulus_name']].append(sweep['sweep_number'])
    
    # calculate features
    cell_features = extract_cell_features(data_set, sweep_numbers['Ramp'], sweep_numbers['Short Square'],sweep_numbers['Long Square'])
    cell_features=cell_features['long_squares']  #仅提取Long Squares实验模式下的信息
    features["Id"].append(str(file_list.loc[i,0]))
    for j in range(0,len(features_list)):
        features[features_list[j]].append(cell_features[features_list[j]])
features=pd.DataFrame(features)
features.to_csv('features1.csv',index=False)  #将提取的数据框存储为csv文件