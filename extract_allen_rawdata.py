#!/usr/bin/python
#coding:utf-8
#@ZHOU_YING
#2018-11-07

from allensdk.core.cell_types_cache import CellTypesCache
from allensdk.core.nwb_data_set import NwbDataSet
from collections import defaultdict
import pandas as pd
import os
import numpy as np
ctc=CellTypesCache(manifest_file='/mnt/f/allen_cell_type/manifest.json')
path='/mnt/f/temp/allen/'
savepath='/mnt/f/temp/allen_rawdata/'
os.chdir(path)
basename="_ephys.nwb"
file_list=pd.read_csv('id.csv',header=None,usecols=[0],skiprows=1)
#info=defaultdict(list)
for i in range(0,len(file_list)):
	filename=str(file_list.loc[i,0])+basename
	sweep_numbers = defaultdict(list)
	data=pd.DataFrame()
	data_set = NwbDataSet(filename)
	sweeps = ctc.get_ephys_sweeps(file_list.loc[i,0])
	for sweep in sweeps:
		sweep_numbers[sweep['stimulus_name']].append(sweep['sweep_number'])
	sweep_numbers=sweep_numbers['Long Square']
	for num in range(0,len(sweep_numbers)):
		sweep_number = sweep_numbers[num]
		sweep_data = data_set.get_sweep(sweep_number)
		stim = sweep_data['stimulus']*1000000000000
		voltage = sweep_data['response']*1000
		colname_v='Vmon'+str(num)
		colname_stim='Stim'+str(num)
		len1=len(stim)
		len2=len(data)
		len3=abs(len2-len1)
		len4=len(voltage)
		if len2==0:
			data.loc[:,colname_v]=voltage
			data.loc[:,colname_stim]=stim
		else:
			if len3==0:
				data.loc[:,colname_v]=voltage
				data.loc[:,colname_stim]=stim
			else:
				if len2>len1:
					nomean_v=[voltage[len4-1]]*len3
					nomean_i=[stim[len1-1]]*len3
					voltage=np.append(voltage,nomean_v)
					stim=np.append(stim,nomean_i)
					data.loc[:,colname_v]=voltage
					data.loc[:,colname_stim]=stim
				else:
					stim=np.delete(stim,range(len2,len1))
					voltage=np.delete(voltage,range(len2,len1))
					data.loc[:,colname_v]=voltage
					data.loc[:,colname_stim]=stim
	#info['id'].append(file_list.loc[i,0])
	#info['sampling_rate'].append(sweep_data['sampling_rate'])
	#info['index_start'] .append(sweep_data['index_range'][0])
	#info['index_end'].append(sweep_data['index_range'][1])
	savename=savepath+str(file_list.loc[i,0])+'.txt'
	data.to_csv(savename,sep='\t',index=False)
#info=pd.DataFrame(info)
#info.to_csv('info.csv',index=False)