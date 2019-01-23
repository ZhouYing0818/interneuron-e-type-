#!/usr/bin/python
#coding:utf-8
#@ZHOU_YING
#2018-12-3
import numpy as np
import os
import matplotlib.pyplot as plt
import pandas as pd
import efel
from collections import defaultdict

def threshold_AP_amp_plot(threshold,AP_amp):
	#savepath='/mnt/f/temp/JSNephysRawdata/picture/threshold_AP_amp_plot/'+filename.split(".")[0]+'_ISI_plot'+'/'
	savepath='/mnt/f/temp/JSNephysRawdata/picture/threshold_AP_amp_plot/'
	#os.makedirs(savepath)
	num=len(threshold)
	fig=plt.figure(figsize=(24,2*num))
	for i in range(0,num):
		threshold_array=threshold[i]['AP_begin_voltage']
		threshold_array=np.append(threshold_array,[])
		AP_amp_array=AP_amp[i]['AP_amplitude']
		AP_amp_array=np.append(AP_amp_array,[])
		#if threshold_array.all()&AP_amp_array.all():
		num1=len(threshold_array)
		ax1=fig.add_subplot(num,6,3*i+1)
			#plt.scatter(range(0,num1),ISI_array)
			#plt.title(filename.split(".")[0]+'_'+'ISI_values '+str(i))
			#plt.xlabel('Index')
			#plt.ylabel('Time(ms)')
		ax1.scatter(range(0,num1),threshold_array,s=2,c='k')
		ax1.set_title(filename.split(".")[0]+'_'+'threshold '+str(i))
		ax1.set_xlabel('Index')
		ax1.set_ylabel('Voltage(mV)')
			#savename_ISI=savepath+filename.split(".")[0]+'_'+'ISI_'+str(i)+'.eps'
			#plt.savefig(savename_ISI,format='eps')
		num2=len(AP_amp_array)
		ax2=fig.add_subplot(num,6,3*i+2)
		ax2.scatter(range(0,num2),AP_amp_array,s=2,c='k')
		ax2.set_title(filename.split(".")[0]+'_'+'AP_amp '+str(i))
		ax2.set_xlabel('Index')
		ax2.set_ylabel('Voltage(mV)')
		ax3=fig.add_subplot(num,6,3*i+3)
			#plt.close('all')
			#plt.plot(time,spike_sweeps[i]['V'])
			#plt.title(filename.split(".")[0]+'_'+'ISI_sweep '+str(i))
			#plt.xlabel('Time(ms)')
			#plt.ylabel('Voltage(mV)')
		ax3.plot(time,spike_sweeps[i]['V'],lw=0.5,c='k')
		ax3.set_title(filename.split(".")[0]+'_'+'sweep '+str(i))
		ax3.set_xlabel('Time(ms)')
		ax3.set_ylabel('Voltage(mV)')
			#savename_sweep=savepath+filename.split(".")[0]+'_'+'sweep_'+str(i)+'.eps'
			#plt.savefig(savename_sweep,format='eps')
	savename=savepath+filename.split(".")[0]+'.eps'
	fig.tight_layout()
	fig.savefig(savename,format='eps')
	plt.close('all')
	return

path='/mnt/f/temp/JSNephysRawdata/'
os.chdir(path)
filelist=pd.read_csv('filelist.csv',header=None)
features=defaultdict(list)
for i in range(0,len(filelist)):
	filename=filelist.loc[i,0]
	features['Id'].append(filename.split(".")[0])
	data=np.loadtxt(filename,skiprows=1)*1000 #导入数据
	time=np.arange(0,1200,float(1200)/len(data))#构建时间向量
	stimulus=pd.DataFrame(data[:,1])
	V=pd.DataFrame(data[:,0])
	length=len(data[0,:])
	index=length/2
	hyper_num=0
	stim=np.array([data[4000,1]])
	#将data中的stim和V数据分开
	for j in range(1,index):
		num_stim=2*j+1
		num_v=2*j
		stimulus.loc[:,j]=pd.DataFrame(data[:,num_stim])
		V.loc[:,j]=pd.DataFrame(data[:,num_v])
		stim_i=data[4000,num_stim]
		unstim_i=data[0,num_stim]
		stim=np.append(stim,data[4000,num_stim])
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
	threshold=efel.getFeatureValues(spike_sweeps,['AP_begin_voltage'])
	AP_amp=efel.getFeatureValues(spike_sweeps,['AP_amplitude'])
	threshold_AP_amp_plot(threshold,AP_amp)