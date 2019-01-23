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

def ISI_plot(ISI_value):
	#savepath='/mnt/f/temp/JSNephysRawdata/picture/ISI_value/'+filename.split(".")[0]+'_ISI_plot'+'/'
	savepath='/mnt/f/temp/allen_rawdata/picture/ISI_value/'
	#os.makedirs(savepath)
	num=len(ISI_value)
	fig=plt.figure(figsize=(30,3*(int(num/2)+1)))
	j=0
	for i in range(0,num):
		ISI_array=ISI_value[i]
		if ISI_array.all():
			num1=len(ISI_array)
			if i%2==0:
				ax1=plt.subplot2grid((int(num/2)+1, 10), (i/2, 0))
				ax2=plt.subplot2grid((int(num/2)+1, 10), (i/2, 1), colspan=4)
			else:
				ax1=plt.subplot2grid((int(num/2)+1, 10), ((i-1)/2, 5))
				ax2=plt.subplot2grid((int(num/2)+1, 10), ((i-1)/2, 6), colspan=4)
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
			stim_pre='Stimulus='+str(stim[rheobase_index+i])+'pA'
			ax2.text(2020,max(spike_sweeps[i]['V'])-10,stim_pre)
			#savename_sweep=savepath+filename.split(".")[0]+'_'+'sweep_'+str(i)+'.eps'
			#plt.savefig(savename_sweep,format='eps')
	savename=savepath+filename.split(".")[0]+'.eps'
	fig.tight_layout()
	fig.savefig(savename,format='eps')
	plt.close('all')
	return

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
	for i in range(0,len(tau)):
		time_constant=tau[i]['time_constant'][0]
		sum_tau=sum_tau+time_constant
	average=sum_tau/len(tau)
	return average
def Rin_fun(stimulus,hyper_num):
	"求输入阻抗"
	sum_Rin=0
	voltage_deflection=efel.getFeatureValues(hyper_sweeps,['voltage_deflection'])
	for i in range(0,hyper_num):
		Rin=voltage_deflection[i]['voltage_deflection'][0]/stimulus.loc[stim_index,i]
		sum_Rin=sum_Rin+Rin
	ave_Rin=sum_Rin/len(voltage_deflection)*1000
	return ave_Rin

def fi_slope(mean_freq,stim,rheobase_index,filename):
	spike_stim=stim[rheobase_index:sweep_num]
	a,b=np.polyfit(spike_stim,mean_freq,1)
	savename_fi_tit='/mnt/f/temp/allen_rawdata/picture/fi_fit/'+filename.split(".")[0]+'_'+'fi_fit'+'.eps'
	plt.scatter(spike_stim,mean_freq,c='k')
	x=np.arange(0,max(spike_stim))
	y=a*x+b
	plt.plot(x,y,c='k')
	plt.title(filename.split(".")[0]+'_'+'fi_fit\n')
	plt.xlabel('Stimulus(pA)')
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
	#_,peak_index=ISI_value_fun(spike_sweeps,data)
	mean_frequency=np.array([])
	for i in range(0,len(peak_index)):
		spikecount=len(peak_index[i]['peak_indices'])
		freq=spikecount/float(stim_time)*1000
		mean_frequency=np.append(mean_frequency,freq)
	return mean_frequency
	
def ISI_value_fun(spike_sweeps,data):
	AP_height=efel.getFeatureValues(spike_sweeps,['AP_height'])
	peak_index=efel.getFeatureValues(spike_sweeps,['peak_indices'])
	ISI=[]
	#first_ISI=np.array([])
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
			T=(peak_index1[k+1]-peak_index1[k])*t
			ISI_1=np.append(ISI_1,T)
		ISI[i:i+1]=[ISI_1]
	return ISI,peak_index

path='/mnt/f/temp/allen_rawdata/'
os.chdir(path)
filelist=pd.read_csv('filelist.csv',header=None)
sample_rate=pd.read_csv('info.csv',header=None,usecols=[3],skiprows=1)
feature=defaultdict(list)
for i in range(0,len(filelist)):
	filename=filelist.loc[i,0]
	feature['Id'].append(filename.split(".")[0])
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
	#求平均voltage_base
	#voltage_base=efel.getFeatureValues(sweeps,['voltage_base']) 
	#feature['ave_voltage_base(mV)'].append(ave_voltage_base(voltage_base,sweep_num)) #平均基线电压
	rheobase_index=len(subthreshold_sweeps)+hyper_num+1
	feature['rheobase_i(pA)'].append(stimulus.loc[stim_index,rheobase_index]) #基强度
	#计算ISI_values
	ISI_value,peak_index=ISI_value_fun(spike_sweeps,data)
	ISI_plot(ISI_value)
	#feature['first_ISI'].append(first_ISI)
	mean_freq=mean_freq_fun(1000)
	feature['fi_fit_slope'].append(fi_slope(mean_freq,stim,rheobase_index,filename)) #fi_fit_slope
	#if hyper_num!=0:
		#tau=efel.getFeatureValues(hyper_sweeps,['time_constant'])
		#feature['tau'].append(ave_tau(tau)) #时间常数
		#feature['sag_amplitude(mV)'].append(ave_sag_amplitude(hyper_num))
		#feature['sag_ratio'].append(sag_ratio1(hyper_num))#sag/base-mini
		#feature['Rin'].append(Rin_fun(stimulus,hyper_num)) #输入阻抗
	#else:
		#feature['tau'].append('') #时间常数
		#feature['sag_amplitude(mV)'].append('')
		#feature['sag_ratio'].append('')#sag/base-mini
		#feature['Rin'].append('') #输入阻抗
	#features=pd.DataFrame(feature)
	#features.to_csv('features1.csv',index=False)