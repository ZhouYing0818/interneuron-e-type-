#!/usr/bin/python
#coding:utf-8
#@ZHOU_YING
#2018-11-13
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
	T=float(1200)/len(data)
	AP_height=efel.getFeatureValues(spike_sweeps,['AP_height'])
	peak_index=efel.getFeatureValues(spike_sweeps,['peak_indices'])
	ISI=[]
	for i in range(0,len(AP_height)):
		id=np.array([])
		ISI_1=np.array([])
		AP_height1=AP_height[i]['AP_height']
		for j in range(0,len(AP_height1)):
			if AP_height1[j]<0:
				id=np.append(id,j)
		peak_index1=np.delete(peak_index[i]['peak_indices'],id)
		peak_index[i]['peak_indices']=peak_index1
		for k in range(0,len(peak_index1)-1):
			t=(peak_index1[k+1]-peak_index1[k])/10
			ISI_1=np.append(ISI_1,t)
		ISI[i:i+1]=[ISI_1]
	return ISI,peak_index
	
def ISI_CV_fun(ISI):
	mean=np.mean(ISI)
	sd=np.std(ISI)
	ISI_CV=sd/mean
	return ISI_CV
	
def adaption_index_fun(ISI):
	adaption_sum=0
	for i in range(1,len(ISI)):
		ISI_i=ISI[i]
		ISI_i_1=ISI[i-1]
		if ISI_i>0 or ISI_i_1>0:
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
	
	

path='/mnt/f/temp/JSNephysRawdata/second/'
os.chdir(path)
filelist=pd.read_csv('filelist.csv',header=None)
feature=pd.DataFrame()
ISIs=pd.DataFrame()
for i in range(0,len(filelist)):
	filename=filelist.loc[i,0]
	#feature['Id'].append(filename.split(".")[0])
	data=np.loadtxt(filename,skiprows=1)*1000 #导入数据
	time=np.arange(0,1200,float(1200)/len(data))#构建时间向量
	stimulus=pd.DataFrame(data[:,1])
	V=pd.DataFrame(data[:,0])
	length=len(data[0,:])
	index=length/2
	hyper_num=0
	stim=np.array([data[17823,1]])
	#将data中的stim和V数据分开
	for j in range(1,index):
		num_stim=2*j+1
		num_v=2*j
		stimulus.loc[:,j]=pd.DataFrame(data[:,num_stim])
		V.loc[:,j]=pd.DataFrame(data[:,num_v])
		stim_i=data[17823,num_stim]
		unstim_i=data[0,num_stim]
		stim=np.append(stim,data[17823,num_stim])
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
		sweep['stim_start'] = [200]
		sweep['stim_end'] = [1000]
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
	#求平均voltage_base
	#rheobase_index=len(subthreshold_sweeps)+hyper_num+1
	ISI_value,peak_index=ISI_value_fun(spike_sweeps,data)
	mean_freq=mean_freq_fun(800)
	index=len(feature)
	for sweep_id in range(0,len(ISI_value)):
		feature.loc[sweep_id+index,'Id']=filename.split(".")[0]
		feature.loc[sweep_id+index,'sweep_id']=sweep_id+1
		ISIs.loc[sweep_id+index,'Id']=filename.split(".")[0]
		ISIs.loc[sweep_id+index,'sweep_id']=sweep_id+1
		ISI=ISI_value[sweep_id]
		index1=len(ISI)
		for n in range(0,index1):
			ISI_index='ISI'+'_'+str(n+1)
			ISIs.loc[sweep_id+index,ISI_index]=ISI[n]
		if index1>=2:
			feature.loc[sweep_id+index,'mean_freq']=mean_freq[sweep_id]
			feature.loc[sweep_id+index,'mean_ISI']=np.mean(ISI)
			feature.loc[sweep_id+index,'ISI_CV']=ISI_CV_fun(ISI)
			feature.loc[sweep_id+index,'adaption_index']=adaption_index_fun(ISI)
			feature.loc[sweep_id+index,'adaption']=adaption_fun(ISI)
			ISI_slope,ISI_R2=ISI_fit(ISI)
			feature.loc[sweep_id+index,'ISI_slope']=ISI_slope
			feature.loc[sweep_id+index,'ISI_R2']=ISI_R2
	feature.to_csv('features_ISI.csv',index=False)
	ISIs.to_csv('ISI.csv',index=False)