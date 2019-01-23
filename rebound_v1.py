#!/usr/bin/python
#coding:utf-8
#@ZHOU_YING
#2018-12-10
#rebound检索，参数计算
import numpy as np
import os
import matplotlib.pyplot as plt
import pandas as pd
import efel
from collections import defaultdict

path='/mnt/f/temp/JSNephysRawdata/second/'
os.chdir(path)
filelist=pd.read_csv('filelist.csv',header=None)
rebound=pd.DataFrame()

def rebound_plot(rebound_sweeps):
	savepath='/mnt/f/temp/JSNephysRawdata/picture/rebound_plot2/'
	num=len(rebound_sweeps)
	fig=plt.figure(figsize=(4,2*num))
	for i in range(0,num):
		ax1=fig.add_subplot(num,1,i+1)
		ax1.plot(time,rebound_sweeps[i]['V'],lw=0.5,c='k')
		ax1.set_title(filename.split(".")[0]+'_'+'sweep '+str(i))
		ax1.set_xlabel('Time(ms)')
		ax1.set_ylabel('Voltage(mV)')
	savename=savepath+filename.split(".")[0]+'.eps'
	fig.tight_layout()
	fig.savefig(savename,format='eps')
	plt.close('all')
	return

for i in range(0,len(filelist)):
	filename=filelist.loc[i,0]
	print str(i)+'  '+'working...'+filename
	data=np.loadtxt(filename,skiprows=1)*1000 #导入数据
	
	time1=np.arange(200,1000,float(1200)/len(data))#构建时间向量1
	time2=np.arange(1000,1200,float(1200)/len(data))#构建时间向量2
	time=np.arange(0,1200,float(1200)/len(data))#构建时间向量
	
	start1=int(float(200)/(float(1200)/len(data)))
	end1=int(float(1000)/(float(1200)/len(data)))
	
	start2=int(float(1000)/(float(1200)/len(data)))
	end2=int(float(1200)/(float(1200)/len(data)))
	
	#stim部分
	stimulus1=pd.DataFrame(data[start1:end1,1])
	V1=pd.DataFrame(data[start1:end1,0])
	
	#stim结束后的部分
	stimulus2=pd.DataFrame(data[start2:end2,1])
	V2=pd.DataFrame(data[start2:end2,0])
	
	#完整sweep
	stimulus=pd.DataFrame(data[:,1])
	V=pd.DataFrame(data[:,0])
	
	length=len(data[0,:])
	index=length/2
	#hyper_num=0
	#stim=np.array([data[4000,1]])
	#将data中的stim和V数据分开
	for j in range(1,index):
		num_stim=2*j+1
		num_v=2*j
		
		stimulus1.loc[:,j]=pd.DataFrame(data[start1:end1,num_stim])
		V1.loc[:,j]=pd.DataFrame(data[start1:end1,num_v])
		
		stimulus2.loc[:,j]=pd.DataFrame(data[start2:end2,num_stim])
		V2.loc[:,j]=pd.DataFrame(data[start2:end2,num_v])
		
		stimulus.loc[:,j]=pd.DataFrame(data[:,num_stim])
		V.loc[:,j]=pd.DataFrame(data[:,num_v])
	
	sweep_num=V.columns.size
	sweeps1=[]
	sweeps2=[]
	sweeps=[]
	rebound_sweeps=[]
	index=len(rebound)
	for k in range(0,sweep_num):
		sweep1={}
		sweep1['T']=time1
		sweep1['V']=V1.loc[:,k]
		sweep1['stim_start'] = [200]
		sweep1['stim_end'] = [1000]
		
		sweep2={}
		sweep2['T']=time2
		sweep2['V']=V2.loc[:,k]
		sweep2['stim_start'] = [1000]
		sweep2['stim_end'] = [1200]
		
		sweep={}
		sweep['T']=time
		sweep['V']=V.loc[:,k]
		sweep['stim_start'] = [1000]
		sweep['stim_end'] = [1200]
		
		spikecount1 = efel.getFeatureValues([sweep1], ['Spikecount'])
		spikecount2 = efel.getFeatureValues([sweep2], ['Spikecount'])
		
		sweeps1[k:k+1]=[sweep1]
		sweeps2[k:k+1]=[sweep2]
		sweeps[k:k+1]=[sweep]
		
		rebound.loc[k+index,'Id']=filename.split(".")[0]
		rebound.loc[k+index,'sweep_id']=k+1

		if spikecount1[0]['Spikecount'][0]==0:
			if spikecount2[0]['Spikecount'][0]==0:
				rebound.loc[k+index,'rebound_flag']=0
			else:
				rebound.loc[k+index,'rebound_flag']=spikecount2[0]['Spikecount'][0]
				a=len(rebound_sweeps)
				rebound_sweeps[a:a+1]=[sweep]
				
				rebound_amplitude=efel.getFeatureValues([sweep2], ['AP_amplitude'])
				rebound_amplitude=rebound_amplitude[0]['AP_amplitude']
				rebound_amplitude=np.append(rebound_amplitude,[])
				rebound_width=efel.getFeatureValues([sweep2], ['AP_width'])
				rebound_half_width=efel.getFeatureValues([sweep2], ['spike_half_width'])
				rebound_threshold=efel.getFeatureValues([sweep2], ['AP_begin_voltage'])
				steady_state=efel.getFeatureValues([sweep1], ['steady_state_hyper'])
				rebound_peak_voltage=efel.getFeatureValues([sweep2], ['peak_voltage'])
				steady_state=steady_state[0]['steady_state_hyper'][0]
				rebound_peak_voltage=max(rebound_peak_voltage[0]['peak_voltage'])
				rebound.loc[k+index,'rebound_height']=rebound_peak_voltage-steady_state
				
				for spike_num in range(0,spikecount2[0]['Spikecount'][0]):
					spike_index_amplitude='spike'+'_'+str(spike_num+1)+'_'+'rebound_amplitude'
					spike_index_width='spike'+'_'+str(spike_num+1)+'_'+'rebound_width'
					spike_index_half_width='spike'+'_'+str(spike_num+1)+'_'+'rebound_half_width'
					spike_index_threshold='spike'+'_'+str(spike_num+1)+'_'+'rebound_threshold'
					if rebound_amplitude.all():
						rebound.loc[k+index,spike_index_amplitude]=rebound_amplitude[spike_num]
						rebound.loc[k+index,spike_index_width]=rebound_width[0]['AP_width'][spike_num]
						rebound.loc[k+index,spike_index_half_width]=rebound_half_width[0]['spike_half_width'][spike_num]
						rebound.loc[k+index,spike_index_threshold]=rebound_threshold[0]['AP_begin_voltage'][spike_num]
					
	if len(rebound_sweeps)!=0:
		rebound_plot(rebound_sweeps)

	rebound.to_csv('rebound_feature.csv',index=False)