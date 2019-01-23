#!/usr/bin/python
#coding:utf-8
#@ZHOU_YING
#2018-12-10
#AHP/ADP相关参数计算
import numpy as np
import os
import matplotlib.pyplot as plt
import pandas as pd
import efel
from collections import defaultdict

def ADP_idx_fun(start,end,AHP_AP_begin):
	min=[]
	max=[]
	for i in range(start,end-3):
		diff1=AHP_AP_begin[i+1]-AHP_AP_begin[i]
		diff2=AHP_AP_begin[i+2]-AHP_AP_begin[i+1]
		if diff1<0 and diff2>0:
			min=np.append(min,i+1)
		if diff1>0 and diff2<0:
			max=np.append(max,i+1)
		
	ADP_idx=0
	max_idx=[]
	for i in range(0,len(max)-20):
		diff1=AHP_AP_begin[max[i+4]]-AHP_AP_begin[max[i]]
		diff2=AHP_AP_begin[max[i+8]]-AHP_AP_begin[max[i]]
		diff3=AHP_AP_begin[max[i+12]]-AHP_AP_begin[max[i]]
		diff4=AHP_AP_begin[max[i+16]]-AHP_AP_begin[max[i]]
		diff5=AHP_AP_begin[max[i+20]]-AHP_AP_begin[max[i]]
		if diff1<0 and diff2<0 and diff3<0 and diff4<0 and diff5<0:
			max_idx=np.append(max_idx,max[i])

	if len(max_idx)>=3:	
		for i in range(0,len(max_idx)-2):
			diff1=AHP_AP_begin[max_idx[i+1]]-AHP_AP_begin[max_idx[i]]
			diff2=AHP_AP_begin[max_idx[i+2]]-AHP_AP_begin[max_idx[i]]
			if diff1<0 and diff2<0:
				ADP_idx=max_idx[i]
				break
	else:
		if len(max_idx)>0:
			v_max=0
			for i in range(0,len(max_idx)):
				v=voltage[max_idx[i]]
				if v>v_max:
					v_max=v
					ADP_idx=max_idx[i]
		else:
			ADP_idx=0
	return ADP_idx
	
def AHP_amplitude_1st_save():
	AHP_idx_array=AHP_idx[sweep_id]['min_AHP_indices']
	AHP_idx_array=np.append(AHP_idx_array,[])/(10*tt)
	threshold_array=threshold[sweep_id]['AP_begin_voltage']
	threshold_array=np.append(threshold_array,[])
	index1=len(AHP_idx_array)
	if len(AHP_idx_array)==len(threshold_array):
		for n in range(0,index1):
			AHP_voltage_1st=voltage[int(AHP_idx_array[n])]
			AHP_amplitude=abs(AHP_voltage_1st-threshold_array[n])
			spike_index='spike'+'_'+str(n+1)
			AHP_amplitude_1st.loc[sweep_id+idx1,spike_index]=AHP_amplitude
	return
	
def AHP_latency_1st_save():
	AHP_idx_array=AHP_idx[sweep_id]['min_AHP_indices']
	AHP_idx_array=np.append(AHP_idx_array,[])/(10*tt)
	threshold_idx_array=AP_begin_idx[sweep_id]['AP_begin_indices']
	threshold_idx_array=np.append(threshold_idx_array,[])/(10*tt)
	index1=len(threshold_idx_array)
	if len(AHP_idx_array)==len(threshold_idx_array):
		for n in range(0,index1):
			AHP_latency=abs(AHP_idx_array[n]-threshold_idx_array[n])*tt
			spike_index='spike'+'_'+str(n+1)
			AHP_latency_1st.loc[sweep_id+idx2,spike_index]=AHP_latency
	return
	
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
	
def ADP_save():
	AHP_idx_array=AHP_idx[sweep_id]['min_AHP_indices']
	AHP_idx_array=np.append(AHP_idx_array,[])/(10*tt)
	threshold_idx_array=AP_begin_idx[sweep_id]['AP_begin_indices']
	threshold_idx_array=np.append(threshold_idx_array,[])/(10*tt)
	threshold_array=threshold[sweep_id]['AP_begin_voltage']
	threshold_array=np.append(threshold_array,[])
	index1=len(threshold_idx_array)
	mean_ISI=np.mean(ISI_value[sweep_id])
	next_begin_idx=np.delete(threshold_idx_array,0)
	next_begin_idx=np.append(next_begin_idx,threshold_idx_array[index1-1]+mean_ISI/tt)
	if len(AHP_idx_array)==len(threshold_array):
		for n in range(0,index1):
			AHP_idx_single=int(AHP_idx_array[n])
			next_begin_idx_single=int(next_begin_idx[n])
			AHP_AP_begin=voltage[AHP_idx_single:next_begin_idx_single]
			ADP_idx=ADP_idx_fun(AHP_idx_single,next_begin_idx_single-2,AHP_AP_begin)
			spike_index='spike'+'_'+str(n+1)
			if ADP_idx!=0:
				ADP.loc[sweep_id+idx3,spike_index]=1
			
				ADP_amplitude.loc[sweep_id+idx4,spike_index]=abs(threshold_array[n]-voltage[ADP_idx])
			
				ADP_AP_begin=voltage[np.int(ADP_idx):next_begin_idx_single]
				second_AHP_idx=int(np.where(ADP_AP_begin==np.min(ADP_AP_begin))[0][0]+ADP_idx)
				AHP_amplitude_2nd.loc[sweep_id+idx5,spike_index]=abs(threshold_array[n]-voltage[second_AHP_idx])
			
				delta_AHP_amplitude.loc[sweep_id+idx6,spike_index]=abs(voltage[second_AHP_idx])-abs(voltage[AHP_idx_single])
			
				AHP_latency_2nd.loc[sweep_id+idx7,spike_index]=abs(second_AHP_idx-threshold_idx_array[n])*tt
			
				delta_AHP_latency.loc[sweep_id+idx8,spike_index]=abs(second_AHP_idx-AHP_idx_single)*tt
			
				ADP_latency.loc[sweep_id+idx9,spike_index]=abs(ADP_idx-threshold_idx_array[n])*tt
			else:
				ADP.loc[sweep_id+idx3,spike_index]=0
			
				ADP_amplitude.loc[sweep_id+idx4,spike_index]=''
			
				ADP_AP_begin=voltage[np.int(ADP_idx):next_begin_idx_single]
				second_AHP_idx=int(np.where(ADP_AP_begin==np.min(ADP_AP_begin))[0][0]+ADP_idx)
				AHP_amplitude_2nd.loc[sweep_id+idx5,spike_index]=''
			
				delta_AHP_amplitude.loc[sweep_id+idx6,spike_index]=''
			
				AHP_latency_2nd.loc[sweep_id+idx7,spike_index]=''
			
				delta_AHP_latency.loc[sweep_id+idx8,spike_index]=''
			
				ADP_latency.loc[sweep_id+idx9,spike_index]=''
	
path='/mnt/f/temp/JSNephysRawdata/'
os.chdir(path)
filelist=pd.read_csv('filelist.csv',header=None)
features=defaultdict(list)

AHP_amplitude_1st=pd.DataFrame()
AHP_latency_1st=pd.DataFrame()
ADP=pd.DataFrame()
ADP_amplitude=pd.DataFrame()
AHP_amplitude_2nd=pd.DataFrame()
delta_AHP_amplitude=pd.DataFrame()
AHP_latency_2nd=pd.DataFrame()
delta_AHP_latency=pd.DataFrame()
ADP_latency=pd.DataFrame()

for i in range(0,len(filelist)):
	filename=filelist.loc[i,0]
	print str(i)+'  '+'working...'+filename
	data=np.loadtxt(filename,skiprows=1)*1000 #导入数据
	time=np.arange(0,1200,float(1200)/len(data))#构建时间向量
	stimulus=pd.DataFrame(data[:,1])
	V=pd.DataFrame(data[:,0])
	length=len(data[0,:])
	index=length/2
	hyper_num=0
	#stim=np.array([data[17823,1]])
	#将data中的stim和V数据分开
	for j in range(1,index):
		num_stim=2*j+1
		num_v=2*j
		stimulus.loc[:,j]=pd.DataFrame(data[:,num_stim])
		V.loc[:,j]=pd.DataFrame(data[:,num_v])
		#stim_i=data[17823,num_stim]
		unstim_i=data[0,num_stim]
		#stim=np.append(stim,data[17823,num_stim])
		#if stim_i<unstim_i:
			#hyper_num=hyper_num+1
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
	
	AHP_idx=AHP_idx=efel.getFeatureValues(spike_sweeps,['min_AHP_indices'])
	AP_begin_idx=efel.getFeatureValues(spike_sweeps,['AP_begin_indices'])
	threshold=efel.getFeatureValues(spike_sweeps,['AP_begin_voltage'])
	ISI_value,_=ISI_value_fun(spike_sweeps,data)

	idx1=len(AHP_amplitude_1st)
	idx2=len(AHP_latency_1st)
	idx3=len(ADP)
	idx4=len(ADP_amplitude)
	idx5=len(AHP_amplitude_2nd)
	idx6=len(delta_AHP_amplitude)
	idx7=len(AHP_latency_2nd)
	idx8=len(delta_AHP_latency)
	idx9=len(ADP_latency)
	
	for sweep_id in range(0,len(spike_sweeps)):
		AHP_amplitude_1st.loc[sweep_id+idx1,'Id']=filename.split(".")[0]
		AHP_amplitude_1st.loc[sweep_id+idx1,'sweep_id']=sweep_id+1
		
		AHP_latency_1st.loc[sweep_id+idx2,'Id']=filename.split(".")[0]
		AHP_latency_1st.loc[sweep_id+idx2,'sweep_id']=sweep_id+1
		
		ADP.loc[sweep_id+idx3,'Id']=filename.split(".")[0]
		ADP.loc[sweep_id+idx3,'sweep_id']=sweep_id+1
		
		ADP_amplitude.loc[sweep_id+idx4,'Id']=filename.split(".")[0]
		ADP_amplitude.loc[sweep_id+idx4,'sweep_id']=sweep_id+1
		
		AHP_amplitude_2nd.loc[sweep_id+idx5,'Id']=filename.split(".")[0]
		AHP_amplitude_2nd.loc[sweep_id+idx5,'sweep_id']=sweep_id+1
		
		delta_AHP_amplitude.loc[sweep_id+idx6,'Id']=filename.split(".")[0]
		delta_AHP_amplitude.loc[sweep_id+idx6,'sweep_id']=sweep_id+1
		
		AHP_latency_2nd.loc[sweep_id+idx7,'Id']=filename.split(".")[0]
		AHP_latency_2nd.loc[sweep_id+idx7,'sweep_id']=sweep_id+1
		
		delta_AHP_latency.loc[sweep_id+idx8,'Id']=filename.split(".")[0]
		delta_AHP_latency.loc[sweep_id+idx8,'sweep_id']=sweep_id+1
		
		ADP_latency.loc[sweep_id+idx9,'Id']=filename.split(".")[0]
		ADP_latency.loc[sweep_id+idx9,'sweep_id']=sweep_id+1
		
		time=spike_sweeps[sweep_id]['T']
		voltage=spike_sweeps[sweep_id]['V']
		tt=float(1200)/len(voltage)
		
		AHP_amplitude_1st_save()
		AHP_latency_1st_save()
		#ADP_save()
		
	AHP_amplitude_1st.to_csv('AHP_amplitude_1st.csv',index=False)
	AHP_latency_1st.to_csv('AHP_latency_1st.csv',index=False)
	#ADP.to_csv('ADP.csv',index=False)
	#ADP_amplitude.to_csv('ADP_amplitude.csv',index=False)
	#AHP_amplitude_2nd.to_csv('AHP_amplitude_2nd.csv',index=False)
	#delta_AHP_amplitude.to_csv('delta_AHP_amplitude.csv',index=False)
	#AHP_latency_2nd.to_csv('AHP_latency_2nd.csv',index=False)
	#delta_AHP_latency.to_csv('delta_AHP_latency.csv',index=False)
	#ADP_latency.to_csv('ADP_latency.csv',index=False)