#!/usr/bin/python
#coding:utf-8
#@ZHOU_YING
#2018-11-14
import numpy as np
import os
import matplotlib.pyplot as plt
import pandas as pd
import efel
from collections import defaultdict

def mean_freq_fun(stim_time):
	mean_frequency=np.array([])
	for i in range(0,len(peak_index)):
		spikecount=len(peak_index[i]['peak_indices'])
		freq=spikecount/float(stim_time)*1000
		mean_frequency=np.append(mean_frequency,freq)
	return mean_frequency
	
def ISI_value_fun(spike_sweeps,data):
	AP_height=efel.getFeatureValues(spike_sweeps,['AP_height'])
	peak_index=efel.getFeatureValues(spike_sweeps,['peak_indices'])
	peak_time=efel.getFeatureValues(spike_sweeps,['peak_time'])
	ISI=[]
	for i in range(0,len(AP_height)):
		id=np.array([])
		ISI_1=np.array([])
		AP_height1=AP_height[i]['AP_height']
		for j in range(0,len(AP_height1)):
			if AP_height1[j]<0:
				id=np.append(id,j)
		peak_index1=np.delete(peak_index[i]['peak_indices'],id)
		peak_time1=np.delete(peak_time[i]['peak_time'],id)
		peak_index[i]['peak_indices']=peak_index1
		peak_index[i]['peak_time']=peak_time1
		for k in range(0,len(peak_index1)-1):
			T=peak_time1[k+1]-peak_time1[k]
			ISI_1=np.append(ISI_1,T)
		ISI[i:i+1]=[ISI_1]
	return ISI,peak_index,peak_time
	
def ISI_CV_fun(ISI):
	mean=np.mean(ISI)
	sd=np.std(ISI)
	ISI_CV=sd/mean
	return ISI_CV,mean
	
def adaption_index_fun(ISI):
	adaption_sum=0
	for i in range(1,len(ISI)):
		ISI_i=ISI[i]
		ISI_i_1=ISI[i-1]
		adaption_sum=adaption_sum+(np.float(ISI_i-ISI_i_1)/np.float(ISI_i+ISI_i_1))
	adaption_index=adaption_sum/(len(ISI)-1)
	return adaption_index
	
def adaption_fun(ISI):
	max_freq=2/min(ISI)
	idx=len(ISI)
	steady_freq=3/(ISI[idx-1]+ISI[idx-2])
	adaption=(max_freq-steady_freq)/max_freq
	return adaption
	
def ISI_fit(ISI):
	x=np.arange(0,len(ISI))
	y=ISI
	ISI_slope,_=np.polyfit(x,y,1)
	R2=np.corrcoef(x, y)[0,1]
	return ISI_slope,R2
	
def last_ISI_end_fun(t_peak):
	index=len(t_peak)
	last_ISI_end=2020-t_peak[index-1]
	return last_ISI_end

path='/mnt/f/temp/allen_rawdata/'
os.chdir(path)
filelist=pd.read_csv('filelist.csv',header=None)
sample_rate=pd.read_csv('/mnt/f/temp/allen_rawdata/info.csv',header=None,usecols=[3],skiprows=1)
feature=pd.DataFrame()
all_ISI=pd.DataFrame()
for i in range(0,len(filelist)):
	filename=filelist.loc[i,0]
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
	#rheobase_sweep=[spike_sweeps[0]]
	#求平均voltage_base
	ISI_value,peak_index,peak_time=ISI_value_fun(spike_sweeps,data)
	mean_freq=mean_freq_fun(1000)
	index=len(feature)
	for sweep_id in range(0,len(ISI_value)):
		feature.loc[sweep_id+index,'Id']=filename.split(".")[0]
		all_ISI.loc[sweep_id+index,'Id']=filename.split(".")[0]
		feature.loc[sweep_id+index,'sweep_id']=sweep_id+1
		all_ISI.loc[sweep_id+index,'sweep_id']=sweep_id+1
		ISI=ISI_value[sweep_id]
		index1=len(ISI)
		feature.loc[sweep_id+index,'mean_freq']=mean_freq[sweep_id]
		feature.loc[sweep_id+index,'latency']=efel.getFeatureValues([spike_sweeps[sweep_id]],['time_to_first_spike'])[0]['time_to_first_spike'][0] 
		feature.loc[sweep_id+index,'last_ISI_end']=last_ISI_end_fun(peak_time[sweep_id]['peak_time'])
		for n in range(0,index1):
			ISI_index='ISI'+'_'+str(n+1)
			all_ISI.loc[sweep_id+index,ISI_index]=ISI[n]
		if index1>=2:
			ISI_CV,ISI_mean=ISI_CV_fun(ISI)
			feature.loc[sweep_id+index,'ISI_CV']=ISI_CV
			feature.loc[sweep_id+index,'mean']=ISI_mean
			feature.loc[sweep_id+index,'adaption_index']=adaption_index_fun(ISI)
			feature.loc[sweep_id+index,'adaption']=adaption_fun(ISI)
			ISI_slope,ISI_R2=ISI_fit(ISI)
			feature.loc[sweep_id+index,'ISI_slope']=ISI_slope
			feature.loc[sweep_id+index,'ISI_R2']=ISI_R2
	feature.to_csv('features_ISI.csv',index=False)
	all_ISI.to_csv('ISI.csv',index=False)