#!/usr/bin/python
#coding:utf-8
#@ZHOU_YING
#2018-11-2
#ISI信息提取
import numpy as np
import os
import matplotlib.pyplot as plt
import pandas as pd
import efel
from collections import defaultdict
from matplotlib.gridspec import GridSpec

def ISI_plot(ISI_value):
	#savepath='/mnt/f/temp/JSNephysRawdata/picture/ISI_value/'+filename.split(".")[0]+'_ISI_plot'+'/'
	savepath='/mnt/f/temp/JSNephysRawdata/picture/ISI_value/'
	#os.makedirs(savepath)
	num=len(ISI_value)
	fig=plt.figure(figsize=(21,3*(int(num/2)+1)))
	j=0
	for i in range(0,num):
		ISI_array=ISI_value[i]
		if ISI_array.all():
			num1=len(ISI_array)
			if i%2==0:
				ax1=plt.subplot2grid((int(num/2)+1, 7), (i/2, 0))
				ax2=plt.subplot2grid((int(num/2)+1, 7), (i/2, 1), colspan=2)
			else:
				ax1=plt.subplot2grid((int(num/2)+1, 7), ((i-1)/2, 3))
				ax2=plt.subplot2grid((int(num/2)+1, 7), ((i-1)/2, 4), colspan=2)
			#idx1=(3+3*(-1)^(i+1))/2
			#plt.scatter(range(0,num1),ISI_array)
			#plt.title(filename.split(".")[0]+'_'+'ISI_values '+str(i))
			#plt.xlabel('Index')
			#plt.ylabel('Time(ms)')
			ax1.scatter(range(0,num1),ISI_array,s=2,c='k')
			ax1.set_title(filename.split(".")[0]+'_'+'ISI_values '+str(i))
			ax1.set_xlabel('Index')
			ax1.set_ylabel('Time(ms)')
			#savename_ISI=savepath+filename.split(".")[0]+'_'+'ISI_'+str(i)+'.eps'
			#plt.savefig(savename_ISI,format='eps')
			#idx2=(5+3*(-1)^(i+1))/2
			
			#plt.close('all')
			#plt.plot(time,spike_sweeps[i]['V'])
			#plt.title(filename.split(".")[0]+'_'+'ISI_sweep '+str(i))
			#plt.xlabel('Time(ms)')
			#plt.ylabel('Voltage(mV)')
			ax2.plot(time,spike_sweeps[i]['V'],lw=0.5,c='k')
			ax2.set_title(filename.split(".")[0]+'_'+'ISI_sweep '+str(i))
			ax2.set_xlabel('Time(ms)')
			ax2.set_ylabel('Voltage(mV)')
			stim_pre='Stimulus='+str(stim[rheobase_index+i])+'mA'
			ax2.text(1000,max(spike_sweeps[i]['V'])-10,stim_pre)
			#savename_sweep=savepath+filename.split(".")[0]+'_'+'sweep_'+str(i)+'.eps'
			#plt.savefig(savename_sweep,format='eps')
	savename=savepath+filename.split(".")[0]+'.eps'
	fig.tight_layout()
	fig.savefig(savename,format='eps')
	plt.close('all')
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
			t=(peak_index1[k+1]-peak_index1[k])*T
			ISI_1=np.append(ISI_1,t)
		ISI[i:i+1]=[ISI_1]
	return ISI,peak_index
	
def fi_slope(mean_freq,stim,rheobase_index,filename):
	spike_stim=stim[rheobase_index:sweep_num]
	a,b=np.polyfit(spike_stim,mean_freq,1)
	savename_fi_tit='/mnt/f/temp/JSNephysRawdata/picture/fi_fit/'+filename.split(".")[0]+'_'+'fi_fit'+'.eps'
	plt.scatter(spike_stim,mean_freq,s=2,c='k')
	x=np.arange(0,max(spike_stim))
	y=a*x+b
	plt.plot(x,y,lw=0.5,c='k')
	plt.title(filename.split(".")[0]+'_'+'fi_fit\n')
	plt.xlabel('Stimulus(pA)')
	plt.ylabel('Mean_Frequnency(Hz)')
	plt.savefig(savename_fi_tit,format='eps')
	plt.close('all')
	#plt.show()
	return 
	
def mean_freq_fun(stim_time):
	mean_frequency=np.array([])
	for i in range(0,len(peak_index)):
		spikecount=len(peak_index[i]['peak_indices'])
		freq=spikecount/float(stim_time)*1000
		mean_frequency=np.append(mean_frequency,freq)
	return mean_frequency
	
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

path='/mnt/f/temp/JSNephysRawdata/'
os.chdir(path)
filelist=pd.read_csv('filelist.csv',header=None)
features_ISI=pd.DataFrame()
ISIs=pd.DataFrame()
for i in range(0,len(filelist)):
	filename=filelist.loc[i,0]
	print str(i)+'  '+'working...'+filename
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
	rheobase_index=len(subthreshold_sweeps)+hyper_num+1
	#计算ISI_values
	ISI_value,peak_index=ISI_value_fun(spike_sweeps,data)
	mean_freq=mean_freq_fun(800)
	ISI_plot(ISI_value)
	fi_slope(mean_freq,stim,rheobase_index,filename)
	
	index=len(spike_sweeps)
	for sweep_id in range(0,len(ISI_value)):
		ISIs.loc[sweep_id+index,'Id']=filename.split(".")[0]
		ISIs.loc[sweep_id+index,'sweep_id']=sweep_id+1
		features_ISI.loc[sweep_id+index,'Id']=filename.split(".")[0]
		features_ISI.loc[sweep_id+index,'sweep_id']=sweep_id+1
		ISI=ISI_value[sweep_id]
		index1=len(ISI)
		for n in range(0,index1):
			ISI_index='ISI'+'_'+str(n+1)
			ISIs.loc[sweep_id+index,ISI_index]=ISI[n]
		if index1>=2:
			features_ISI.loc[sweep_id+index,'mean_freq']=mean_freq[sweep_id]
			features_ISI.loc[sweep_id+index,'ISI_CV']=ISI_CV_fun(ISI)
			features_ISI.loc[sweep_id+index,'adaption_index']=adaption_index_fun(ISI)
			features_ISI.loc[sweep_id+index,'adaption']=adaption_fun(ISI)
			ISI_slope,ISI_R2=ISI_fit(ISI)
			features_ISI.loc[sweep_id+index,'ISI_slope']=ISI_slope
			features_ISI.loc[sweep_id+index,'ISI_R2']=ISI_R2
	ISIs.to_csv('ISIs.csv',index=False)
	features_ISI.to_csv('features_ISI.csv',index=False)