#!/usr/bin/python
#coding:utf-8
#@ZHOU_YING
#2019-1-7
import numpy as np
import os
import matplotlib.pyplot as plt
import pandas as pd
path='/mnt/f/temp/allen_rawdata/'
os.chdir(path)
filelist=pd.read_csv('filelist.csv',header=None)
sample_rate=pd.read_csv('/mnt/f/temp/allen_rawdata/info.csv',header=None,usecols=[3],skiprows=1)

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
		
	savename_stim='/mnt/f/temp/allen_rawdata/picture/stim_v/'+filename.split(".")[0]+'_'+'stim'+'.eps'
	savename_V='/mnt/f/temp/allen_rawdata/picture/stim_v/'+filename.split(".")[0]+'_'+'V'+'.eps'
	plt.plot(time,stimulus)
	plt.title(filename.split(".")[0]+'_'+'Stimulus\n')
	plt.xlabel('Time(ms)')
	plt.ylabel('Stimulus(mA)')
	plt.savefig(savename_stim,format='eps')
	#plt.show()
	plt.close()
	plt.plot(time,V)
	plt.title(filename.split(".")[0]+'_'+'Sweeps\n')
	plt.xlabel('Time(ms)')
	plt.ylabel('Voltage(mV)')
	plt.savefig(savename_V,format='eps')
	#plt.show()
	plt.close()