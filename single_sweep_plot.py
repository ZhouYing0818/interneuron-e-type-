#!/usr/bin/python
#coding:utf-8
#@ZHOU_YING
#2018-12-13
#sweep amp threshold 拐点参数提取
import numpy as np
import os
import matplotlib.pyplot as plt
import pandas as pd
import efel
from collections import defaultdict

def amp_change_plot(single):
	savepath='/mnt/f/temp/allen_rawdata/picture/amp_change_plot/'
	num=len(single)
	fig=plt.figure(figsize=(12,2*num))
	for k in range(0,num):
		single_sweep=single[single.sweep_id==k+1]
		single_sweep=single_sweep.dropna(axis=1)
		single_sweep=np.array(single_sweep.drop(["Id","sweep_id"],axis=1))[0]
		single_sweep=np.delete(single_sweep,0)
		num1=len(single_sweep)
		ax1=fig.add_subplot(num,3,k+1)
		ax1.scatter(range(0,num1),single_sweep,s=2,c='k')
		ax1.set_title(id+'_'+'amplitude '+str(k))
		ax1.set_xlabel('Index')
		ax1.set_ylabel('Voltage(mV)')
	savename=savepath+str(id)+'_amplotude.eps'
	fig.tight_layout()
	fig.savefig(savename,format='eps')
	plt.close('all')
	return
		

path='/mnt/f/allen_cell_type/filtdata/'
os.chdir(path)
amp_rawdata=pd.read_csv('amp_new.csv')
filelist=pd.read_csv('/mnt/f/temp/allen_rawdata/filelist.csv',header=None)

for i in range(0,len(filelist)):
	filename=filelist.loc[i,0]
	id=filename.split(".")[0]
	single=amp_rawdata[amp_rawdata.Id==int(id)]
	amp_change_plot(single)
	
		
			
