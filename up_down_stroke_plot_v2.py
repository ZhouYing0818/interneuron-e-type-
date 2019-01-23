#!/usr/bin/python
#coding:utf-8
#@ZHOU_YING
#2018-11-23
import numpy as np
import os
import matplotlib.pyplot as plt
import pandas as pd
import efel
from collections import defaultdict

def up_down_stroke_plot():
	#savepath='/mnt/f/temp/JSNephysRawdata/picture/threshold_AP_amp_plot/'+filename.split(".")[0]+'_ISI_plot'+'/'
	savepath='/mnt/f/temp/JSNephysRawdata/picture/up_down_stroke_plot_2/'
	#os.makedirs(savepath)
	num=len(AP_rise_time1)
	fig=plt.figure(figsize=(28,2*num))
	for i in range(0,num):
		AP_rise_time1_array=AP_rise_time1[i]['AP_rise_time']
		AP_rise_time1_array=np.append(AP_rise_time1_array,[])
		
		AP_fall_time1_array=AP_fall_time1[i]['AP_fall_time']
		AP_fall_time1_array=np.append(AP_fall_time1_array,[])
		
		AP_rise_rate1_array=AP_rise_rate1[i]['AP_rise_rate']
		AP_rise_rate1_array=np.append(AP_rise_rate1_array,[])
		
		AP_fall_rate1_array=AP_fall_rate1[i]['AP_fall_rate']
		AP_fall_rate1_array=np.append(AP_fall_rate1_array,[])
		
		AP_rise_rate_change1_array=AP_rise_rate_change1[i]['AP_rise_rate_change']
		AP_rise_rate_change1_array=np.append(AP_rise_rate_change1_array,[])
		
		AP_fall_rate_change1_array=AP_fall_rate_change1[i]['AP_fall_rate_change']
		AP_fall_rate_change1_array=np.append(AP_fall_rate_change1_array,[])
		
		#if threshold_array.all()&AP_amp_array.all():
		num1=len(AP_rise_time1_array)
		ax1=fig.add_subplot(num,7,7*i+1)
			#plt.scatter(range(0,num1),ISI_array)
			#plt.title(filename.split(".")[0]+'_'+'ISI_values '+str(i))
			#plt.xlabel('Index')
			#plt.ylabel('Time(ms)')
		ax1.scatter(range(0,num1),AP_rise_time1_array,s=2,c='k')
		ax1.set_title(filename.split(".")[0]+'_'+'AP_rise_time '+str(i))
		ax1.set_xlabel('Index')
		ax1.set_ylabel('Time(ms)')
			#savename_ISI=savepath+filename.split(".")[0]+'_'+'ISI_'+str(i)+'.eps'
			#plt.savefig(savename_ISI,format='eps')
		
		num2=len(AP_fall_time1_array)
		ax2=fig.add_subplot(num,7,7*i+2)
		ax2.scatter(range(0,num2),AP_fall_time1_array,s=2,c='k')
		ax2.set_title(filename.split(".")[0]+'_'+'AP_fall_time '+str(i))
		ax2.set_xlabel('Index')
		ax2.set_ylabel('Time(ms)')
		
		num3=len(AP_rise_rate1_array)
		ax3=fig.add_subplot(num,7,7*i+3)
		ax3.scatter(range(0,num3),AP_rise_rate1_array,s=2,c='k')
		ax3.set_title(filename.split(".")[0]+'_'+'AP_rise_rate '+str(i))
		ax3.set_xlabel('Index')
		ax3.set_ylabel('Rate')
		
		num4=len(AP_fall_rate1_array)
		ax4=fig.add_subplot(num,7,7*i+4)
		ax4.scatter(range(0,num4),AP_fall_rate1_array,s=2,c='k')
		ax4.set_title(filename.split(".")[0]+'_'+'AP_fall_rate '+str(i))
		ax4.set_xlabel('Index')
		ax4.set_ylabel('Rate')
		
		num5=len(AP_rise_rate_change1_array)
		ax5=fig.add_subplot(num,7,7*i+5)
		ax5.scatter(range(0,num5),AP_rise_rate_change1_array,s=2,c='k')
		ax5.set_title(filename.split(".")[0]+'_'+'AP_rise_rate_change '+str(i))
		ax5.set_xlabel('Index')
		ax5.set_ylabel('Rate')
		
		num6=len(AP_fall_rate_change1_array)
		ax6=fig.add_subplot(num,7,7*i+6)
		ax6.scatter(range(0,num6),AP_fall_rate_change1_array,s=2,c='k')
		ax6.set_title(filename.split(".")[0]+'_'+'AP_fall_rate_change '+str(i))
		ax6.set_xlabel('Index')
		ax6.set_ylabel('Rate')
		
		ax7=fig.add_subplot(num,7,7*i+7)
			#plt.close('all')
			#plt.plot(time,spike_sweeps[i]['V'])
			#plt.title(filename.split(".")[0]+'_'+'ISI_sweep '+str(i))
			#plt.xlabel('Time(ms)')
			#plt.ylabel('Voltage(mV)')
		ax7.plot(time,spike_sweeps[i]['V'],lw=0.5,c='k')
		ax7.set_title(filename.split(".")[0]+'_'+'sweep '+str(i))
		ax7.set_xlabel('Time(ms)')
		ax7.set_ylabel('Voltage(mV)')
			#savename_sweep=savepath+filename.split(".")[0]+'_'+'sweep_'+str(i)+'.eps'
			#plt.savefig(savename_sweep,format='eps')
	savename=savepath+filename.split(".")[0]+'.eps'
	fig.tight_layout()
	fig.savefig(savename,format='eps')
	plt.close('all')
	return

path='/mnt/f/temp/JSNephysRawdata/second/'
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
	
	AP_rise_time1=efel.getFeatureValues(spike_sweeps,['AP_rise_time'])
	AP_fall_time1=efel.getFeatureValues(spike_sweeps,['AP_fall_time'])
	AP_rise_rate1=efel.getFeatureValues(spike_sweeps,['AP_rise_rate'])
	AP_fall_rate1=efel.getFeatureValues(spike_sweeps,['AP_fall_rate'])
	AP_rise_rate_change1=efel.getFeatureValues(spike_sweeps,['AP_rise_rate_change'])
	AP_fall_rate_change1=efel.getFeatureValues(spike_sweeps,['AP_fall_rate_change'])
	
	up_down_stroke_plot()