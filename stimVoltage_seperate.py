#!/usr/bin/python
#coding:utf-8
#@ZHOU_YING
#2018-11-2
#将stimulus和voltage分离成两个文件
import numpy as np
import os
import pandas as pd
from collections import defaultdict

path='/mnt/f/temp/JSNephysRawdata/'
os.chdir(path)
filelist=pd.read_csv('filelist.csv',header=None)
for i in range(0,len(filelist)):
	filename=filelist.loc[i,0]
	data=np.loadtxt(filename,skiprows=1)*1000
	stimulus=pd.DataFrame(data[:,1])
	Voltage=pd.DataFrame(data[:,0])
	length=len(data[0,:])
	index=length/2
	for j in range(1,index):
		num_stim=2*j+1
		num_v=2*j
		stimulus.loc[:,j]=pd.DataFrame(data[:,num_stim])
		Voltage.loc[:,j]=pd.DataFrame(data[:,num_v])
	savename_stim='/mnt/f/temp/JSNephysRawdata/seperate/'+filename.split(".")[0]+'_'+'stim'+'.csv'
	savename_V='/mnt/f/temp/JSNephysRawdata/seperate/'+filename.split(".")[0]+'_'+'V'+'.txt'
	stimulus.to_csv(savename_stim,index=False,header=None)
	Voltage.to_csv(savename_V, sep='\t', index=False,header=None)