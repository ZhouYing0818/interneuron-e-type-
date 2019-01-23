#!/usr/bin/python
#coding:utf-8
#@ZHOU_YING
#2018-12-11
#alle,threshold,Amp,AP_width,spike_half_width统计

import numpy as np
import os
import matplotlib.pyplot as plt
import pandas as pd
import efel
from collections import defaultdict

def save_threshold(sweep_id):
	threshold_array=threshold1[sweep_id]['AP_begin_voltage']
	threshold_array=np.append(threshold_array,[])
	index1=len(threshold_array)
	for n in range(0,index1):
		spike_index='spike'+'_'+str(n+1)
		threshold.loc[sweep_id+idx1,spike_index]=threshold_array[n]
	#threshold.to_csv('threshold.csv',index=False)
	return
	
def save_AP_amp(sweep_id):
	AP_amp_array=AP_amp1[sweep_id]['AP_amplitude']
	AP_amp_array=np.append(AP_amp_array,[])
	index1=len(AP_amp_array)
	for n in range(0,index1):
		spike_index='spike'+'_'+str(n+1)
		AP_amp.loc[sweep_id+idx2,spike_index]=AP_amp_array[n]
	#AP_amp.to_csv('AP_amp.csv',index=False)
	return
	
def save_AP_width(sweep_id):
	AP_width_array=AP_width1[sweep_id]['AP_width']
	AP_width_array=np.append(AP_width_array,[])
	index1=len(AP_width_array)
	for n in range(0,index1):
		spike_index='spike'+'_'+str(n+1)
		AP_width.loc[sweep_id+idx3,spike_index]=AP_width_array[n]
	#AP_width.to_csv('AP_width.csv',index=False)
	return
	
def save_spike_half_width(sweep_id):
	spike_half_width_array=spike_half_width1[sweep_id]['spike_half_width']
	spike_half_width_array=np.append(spike_half_width_array,[])
	index1=len(spike_half_width_array)
	for n in range(0,index1):
		spike_index='spike'+'_'+str(n+1)
		spike_half_width.loc[sweep_id+idx4,spike_index]=spike_half_width_array[n]
	#spike_half_width.to_csv('spike_half_width.csv',index=False)
	return
	
def save_AP_amplitude_change(sweep_id):
	AP_amplitude_change_array=AP_amplitude_change1[sweep_id]['AP_amplitude_change']
	AP_amplitude_change_array=np.append(AP_amplitude_change_array,[])
	index1=len(AP_amplitude_change_array)
	for n in range(0,index1):
		spike_index='spike'+'_'+str(n+1)
		AP_amplitude_change.loc[sweep_id+idx5,spike_index]=AP_amplitude_change_array[n]
	return
	
def save_AP_rise_time(sweep_id):
	AP_rise_time_array=AP_rise_time1[sweep_id]['AP_rise_time']
	AP_rise_time_array=np.append(AP_rise_time_array,[])
	index1=len(AP_rise_time_array)
	for n in range(0,index1):
		spike_index='spike'+'_'+str(n+1)
		AP_rise_time.loc[sweep_id+idx6,spike_index]=AP_rise_time_array[n]
	return
	
def save_AP_fall_time(sweep_id):
	AP_fall_time_array=AP_fall_time1[sweep_id]['AP_fall_time']
	AP_fall_time_array=np.append(AP_fall_time_array,[])
	index1=len(AP_fall_time_array)
	for n in range(0,index1):
		spike_index='spike'+'_'+str(n+1)
		AP_fall_time.loc[sweep_id+idx7,spike_index]=AP_fall_time_array[n]
	return
	
def save_AP_rise_rate(sweep_id):
	AP_rise_rate_array=AP_rise_rate1[sweep_id]['AP_rise_rate']
	AP_rise_rate_array=np.append(AP_rise_rate_array,[])
	index1=len(AP_rise_rate_array)
	for n in range(0,index1):
		spike_index='spike'+'_'+str(n+1)
		AP_rise_rate.loc[sweep_id+idx8,spike_index]=AP_rise_rate_array[n]
	return
	
def save_AP_fall_rate(sweep_id):
	AP_fall_rate_array=AP_fall_rate1[sweep_id]['AP_fall_rate']
	AP_fall_rate_array=np.append(AP_fall_rate_array,[])
	index1=len(AP_fall_rate_array)
	for n in range(0,index1):
		spike_index='spike'+'_'+str(n+1)
		AP_fall_rate.loc[sweep_id+idx9,spike_index]=AP_fall_rate_array[n]
	return
	
def save_AP_rise_rate_change(sweep_id):
	AP_rise_rate_change_array=AP_rise_rate_change1[sweep_id]['AP_rise_rate_change']
	AP_rise_rate_change_array=np.append(AP_rise_rate_change_array,[])
	index1=len(AP_rise_rate_change_array)
	for n in range(0,index1):
		spike_index='spike'+'_'+str(n+1)
		AP_rise_rate_change.loc[sweep_id+idx10,spike_index]=AP_rise_rate_change_array[n]
	return
	
def save_AP_fall_rate_change(sweep_id):
	AP_fall_rate_change_array=AP_fall_rate_change1[sweep_id]['AP_fall_rate_change']
	AP_fall_rate_change_array=np.append(AP_fall_rate_change_array,[])
	index1=len(AP_fall_rate_change_array)
	for n in range(0,index1):
		spike_index='spike'+'_'+str(n+1)
		AP_fall_rate_change.loc[sweep_id+idx11,spike_index]=AP_fall_rate_change_array[n]
	return
	
path='/mnt/f/temp/allen_rawdata/'
os.chdir(path)
filelist=pd.read_csv('filelist.csv',header=None)
sample_rate=pd.read_csv('/mnt/f/temp/allen/info.csv',header=None,usecols=[3],skiprows=1)

#threshold=pd.DataFrame()
#AP_amp=pd.DataFrame()
#AP_width=pd.DataFrame()
#spike_half_width=pd.DataFrame()
AP_amplitude_change=pd.DataFrame()

AP_rise_time=pd.DataFrame()
AP_fall_time=pd.DataFrame()
AP_rise_rate=pd.DataFrame()
AP_fall_rate=pd.DataFrame()
AP_rise_rate_change=pd.DataFrame()
AP_fall_rate_change=pd.DataFrame()

for i in range(0,len(filelist)):
	filename=filelist.loc[i,0]
	#feature['Id'].append(filename.split(".")[0])
	data=np.loadtxt(filename,skiprows=1)#*1000 #导入数据
	t=1000/float(sample_rate.loc[i,3])
	time=np.arange(0,len(data))*t#构建时间向量
	stimulus=pd.DataFrame(data[:,1])
	V=pd.DataFrame(data[:,0])
	length=len(data[0,:])
	index=length/2
	hyper_num=0
	if sample_rate.loc[i,3]==200000:
		stim_index=300000
	else:
		stim_index=80000
	stim=np.array([data[stim_index,1]])
	#将data中的stim和V数据分开
	for j in range(1,index):
		num_stim=2*j+1
		num_v=2*j
		stimulus.loc[:,j]=pd.DataFrame(data[:,num_stim])
		V.loc[:,j]=pd.DataFrame(data[:,num_v])
		stim_i=data[stim_index,num_stim]
		unstim_i=data[0,num_stim]
		stim=np.append(stim,data[stim_index,num_stim])
		if stim_i<unstim_i:
			hyper_num=hyper_num+1
	#构建域上和域下的sweep列表
	sweep_num=V.columns.size
	spike_sweeps=[]
	subthreshold_sweeps=[]
	sweeps=[]
	hyper_sweeps=[]
	for k in range(0,sweep_num):
		sweep={}
		sweep['T']=time
		sweep['V']=V.loc[:,k]
		sweep['stim_start'] = [1020]
		sweep['stim_end'] = [2020]
		spikecount = efel.getFeatureValues([sweep], ['Spikecount'])
		sweeps[k:k+1]=[sweep]
		if k<=hyper_num:
			l=len(hyper_sweeps)
			hyper_sweeps[l:l+1]=[sweep]
		else:
			if spikecount[0]['Spikecount'][0]==0:
				a=len(subthreshold_sweeps)
				subthreshold_sweeps[a:a+1]=[sweep]
			else:
				b=len(spike_sweeps)
				spike_sweeps[b:b+1]=[sweep]
	rheobase_sweep=[spike_sweeps[0]]
	
	#threshold1=efel.getFeatureValues(spike_sweeps,['AP_begin_voltage'])
	#AP_amp1=efel.getFeatureValues(spike_sweeps,['AP_amplitude'])
	#AP_width1=efel.getFeatureValues(spike_sweeps,['AP_width'])
	#spike_half_width1=efel.getFeatureValues(spike_sweeps,['spike_half_width'])
	AP_amplitude_change1=efel.getFeatureValues(spike_sweeps,['AP_amplitude_change'])
	
	#升/降支参数
	AP_rise_time1=efel.getFeatureValues(spike_sweeps,['AP_rise_time'])
	AP_fall_time1=efel.getFeatureValues(spike_sweeps,['AP_fall_time'])
	AP_rise_rate1=efel.getFeatureValues(spike_sweeps,['AP_rise_rate'])
	AP_fall_rate1=efel.getFeatureValues(spike_sweeps,['AP_fall_rate'])
	AP_rise_rate_change1=efel.getFeatureValues(spike_sweeps,['AP_rise_rate_change'])
	AP_fall_rate_change1=efel.getFeatureValues(spike_sweeps,['AP_fall_rate_change'])
	
	#idx1=len(threshold)
	#idx2=len(AP_amp)
	#idx3=len(AP_width)
	#idx4=len(spike_half_width)
	idx5=len(AP_amplitude_change)
	
	idx6=len(AP_rise_time)
	idx7=len(AP_fall_time)
	idx8=len(AP_rise_rate)
	idx9=len(AP_fall_rate)
	idx10=len(AP_rise_rate_change)
	idx11=len(AP_fall_rate_change)
	
	for sweep_id in range(0,len(spike_sweeps)):
		#threshold.loc[sweep_id+idx1,'Id']=filename.split(".")[0]
		#threshold.loc[sweep_id+idx1,'sweep_id']=sweep_id+1
		
		#AP_amp.loc[sweep_id+idx2,'Id']=filename.split(".")[0]
		#AP_amp.loc[sweep_id+idx2,'sweep_id']=sweep_id+1
		
		#AP_width.loc[sweep_id+idx3,'Id']=filename.split(".")[0]
		#AP_width.loc[sweep_id+idx3,'sweep_id']=sweep_id+1
		
		#spike_half_width.loc[sweep_id+idx4,'Id']=filename.split(".")[0]
		#spike_half_width.loc[sweep_id+idx4,'sweep_id']=sweep_id+1
		
		AP_amplitude_change.loc[sweep_id+idx5,'Id']=filename.split(".")[0]
		AP_amplitude_change.loc[sweep_id+idx5,'sweep_id']=sweep_id+1
		
		AP_rise_time.loc[sweep_id+idx6,'Id']=filename.split(".")[0]
		AP_rise_time.loc[sweep_id+idx6,'sweep_id']=sweep_id+1
		
		AP_rise_rate.loc[sweep_id+idx7,'Id']=filename.split(".")[0]
		AP_rise_rate.loc[sweep_id+idx7,'sweep_id']=sweep_id+1
		
		AP_fall_rate.loc[sweep_id+idx8,'Id']=filename.split(".")[0]
		AP_fall_rate.loc[sweep_id+idx8,'sweep_id']=sweep_id+1
		
		AP_fall_time.loc[sweep_id+idx9,'Id']=filename.split(".")[0]
		AP_fall_time.loc[sweep_id+idx9,'sweep_id']=sweep_id+1
		
		AP_rise_rate_change.loc[sweep_id+idx10,'Id']=filename.split(".")[0]
		AP_rise_rate_change.loc[sweep_id+idx10,'sweep_id']=sweep_id+1
		
		AP_fall_rate_change.loc[sweep_id+idx11,'Id']=filename.split(".")[0]
		AP_fall_rate_change.loc[sweep_id+idx11,'sweep_id']=sweep_id+1
		
		#save_threshold(sweep_id)
		#save_AP_amp(sweep_id)
		#save_AP_width(sweep_id)
		#save_spike_half_width(sweep_id)
		save_AP_amplitude_change(sweep_id)
		
		save_AP_rise_time(sweep_id)
		save_AP_fall_time(sweep_id)
		save_AP_rise_rate(sweep_id)
		save_AP_fall_rate(sweep_id)
		save_AP_rise_rate_change(sweep_id)
		save_AP_fall_rate_change(sweep_id)
	
	#threshold.to_csv('threshold.csv',index=False)
	#AP_amp.to_csv('AP_amp.csv',index=False)
	#AP_width.to_csv('AP_width.csv',index=False)
	#spike_half_width.to_csv('spike_half_width.csv',index=False)
	AP_amplitude_change.to_csv('AP_amplitude_change.csv',index=False)
	
	AP_rise_time.to_csv('AP_rise_time.csv',index=False)
	AP_fall_time.to_csv('AP_fall_time.csv',index=False)
	AP_rise_rate.to_csv('AP_rise_rate.csv',index=False)
	AP_fall_rate.to_csv('AP_fall_rate.csv',index=False)
	AP_rise_rate_change.to_csv('AP_rise_rate_change.csv',index=False)
	AP_fall_rate_change.to_csv('AP_fall_rate_change.csv',index=False)