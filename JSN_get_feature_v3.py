#!/usr/bin/python
#coding:utf-8
#@ZHOU_YING
#2018-10-31
import numpy as np
import os
import matplotlib.pyplot as plt
import pandas as pd
import efel
from collections import defaultdict

def ave_voltage_base(voltage_base,sweep_num):
	"求平均voltage_base函数"
	sum_voltage_base=0
	for i in range(0,sweep_num):
		voltage=voltage_base[i]['voltage_base'][0]
		sum_voltage_base=sum_voltage_base+voltage
	average=sum_voltage_base/sweep_num
	return average
def ave_tau(tau):
	"求平均tau函数"
	sum_tau=0
	#for i in range(0,len(tau)):
	time_constant=tau[0]['time_constant'][0]
		#sum_tau=sum_tau+time_constant
	#average=sum_tau/len(tau)
	return time_constant
	
def Rin_fun(stimulus,hyper_num):
	"求输入阻抗"
	sum_Rin=0
	voltage_deflection=efel.getFeatureValues(hyper_sweeps,['voltage_deflection'])
	for i in range(0,hyper_num):
		Rin=voltage_deflection[i]['voltage_deflection'][0]/stimulus.loc[4000,i]
		sum_Rin=sum_Rin+Rin
	ave_Rin=sum_Rin/len(voltage_deflection)
	return ave_Rin

def fi_slope(mean_freq,stim,rheobase_index,filename):
	spike_stim=stim[rheobase_index:sweep_num]
	a,b=np.polyfit(spike_stim,mean_freq,1)
	savename_fi_tit='/mnt/f/temp/JSNephysRawdata/picture/fi_fit/'+filename.split(".")[0]+'_'+'fi_fit'+'.eps'
	plt.scatter(spike_stim,mean_freq)
	x=np.arange(0,max(spike_stim))
	y=a*x+b
	plt.plot(x,y)
	plt.title(filename.split(".")[0]+'_'+'fi_fit\n')
	plt.xlabel('Stimulus(mA)')
	plt.ylabel('Mean_Frequnency(Hz)')
	plt.savefig(savename_fi_tit,format='eps')
	plt.close('all')
	#plt.show()
	return a

def ave_sag_amplitude(hyper_num):
	sag_amplitude=efel.getFeatureValues(hyper_sweeps,['sag_amplitude'])
	sum=0
	for i in range(0,hyper_num):
		sag=sag_amplitude[i]['sag_amplitude'][0]
		sum=sum+sag
	average=sum/hyper_num
	return average

def sag_ratio1(hyper_num):
	sag_ratio=efel.getFeatureValues(hyper_sweeps,['sag_ratio1'])
	sum=0
	for i in range(0,hyper_num):
		sag=sag_ratio[i]['sag_ratio1'][0]
		sum=sum+sag
	average=sum/hyper_num
	return average
	
def mean_freq_fun(stim_time):
	_,peak_index=ISI_value_fun(spike_sweeps,data)
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
			t=(peak_index1[k+1]-peak_index1[k])*T
			ISI_1=np.append(ISI_1,t)
		ISI[i:i+1]=[ISI_1]
	return ISI,peak_index

path='/mnt/f/temp/JSNephysRawdata/'
os.chdir(path)
filelist=pd.read_csv('filelist.csv',header=None)
feature=defaultdict(list)
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
	#求平均voltage_base
	voltage_base=efel.getFeatureValues(sweeps,['voltage_base']) 
	feature['ave_voltage_base'].append(ave_voltage_base(voltage_base,sweep_num)) #平均基线电压
	rheobase_index=len(subthreshold_sweeps)+hyper_num+1
	feature['rheobase_i'].append(stimulus.loc[4000,rheobase_index]) #基强度
	mean_freq=mean_freq_fun(800)
	feature['fi_fit_slope'].append(fi_slope(mean_freq,stim,rheobase_index,filename)) #fi_fit_slope
	if hyper_num!=0:
		tau=efel.getFeatureValues(hyper_sweeps,['time_constant'])
		feature['tau'].append(ave_tau(tau)) #时间常数
		feature['sag_amplitude'].append(ave_sag_amplitude(hyper_num))
		feature['sag_ratio'].append(sag_ratio1(hyper_num))#sag/base-mini
		feature['Rin'].append(Rin_fun(stimulus,hyper_num)) #输入阻抗
	else:
		feature['tau'].append('') #时间常数
		feature['sag_amplitude'].append('')
		feature['sag_ratio'].append('')#sag/base-mini
		feature['Rin'].append('') #输入阻抗
	features=pd.DataFrame(feature)
	features.to_csv('features1.csv',index=False)