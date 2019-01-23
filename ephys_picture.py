#!/usr/bin/python
#coding:utf-8
#@ZHOU_YING
#2018-10-31
import numpy as np
import os
import matplotlib.pyplot as plt
import pandas as pd
path='/mnt/f/temp/CJWRawdata/'
os.chdir(path)
filelist=pd.read_csv('filelist.csv',header=None)
for i in range(0,len(filelist)):
	filename=filelist.loc[i,0]
	data=np.loadtxt(filename,skiprows=1)*1000
	time=np.arange(0,1200,float(1200)/len(data))
	print len(data)
	stimulus=pd.DataFrame(data[:,1])
	V=pd.DataFrame(data[:,0])
	length=len(data[0,:])
	index=length/2
	for j in range(1,index):
		num_stim=2*j+1
		num_v=2*j
		stimulus.loc[:,j]=pd.DataFrame(data[:,num_stim])
		V.loc[:,j]=pd.DataFrame(data[:,num_v])
	savename_stim='/mnt/f/temp/CJWRawdata/picture/'+filename.split(".")[0]+'_'+'stim'+'.eps'
	savename_V='/mnt/f/temp/CJWRawdata/picture/'+filename.split(".")[0]+'_'+'V'+'.eps'
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