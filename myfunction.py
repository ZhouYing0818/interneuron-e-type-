#!/usr/bin/python
#coding:utf-8
#@ZHOU_YING
#2018-11-3
#ISI计算

def ISI_value(spike_sweeps,data):
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
	return ISI peak_index

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
		Rin=voltage_deflection[i]['voltage_deflection'][0]/stimulus.loc[4000,i]
		sum_Rin=sum_Rin+Rin
	ave_Rin=sum_Rin/len(voltage_deflection)
	return ave_Rin

def fi_slope(mean_freq,stim,rheobase_index,filename):
	mean_frequency=np.array([])
	for i in range(0,sweep_num-rheobase_index):
		mean_frequency=np.append(mean_frequency,mean_freq[i]['mean_frequency'][0])
	spike_stim=stim[rheobase_index:sweep_num]
	a,b=np.polyfit(spike_stim,mean_frequency,1)
	savename_fi_tit='/mnt/f/temp/JSNephysRawdata/picture/fi_fit/'+filename.split(".")[0]+'_'+'fi_fit'+'.tif'
	plt.scatter(spike_stim,mean_frequency)
	x=np.arange(0,max(spike_stim))
	y=a*x+b
	plt.plot(x,y)
	plt.title(filename.split(".")[0]+'_'+'fi_fit\n')
	plt.xlabel('Stimulus(mA)')
	plt.ylabel('Mean_Frequnency(Hz)')
	plt.savefig(savename_fi_tit,format='tiff')
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
	
def mean_freq(stim_time)
	_,peak_index=ISI_value(spike_sweeps,data)
	mean_frequency=[]
	for i in range(0,len(peak_index)):
		spikecount=len(peak_index[i])
		mean_frequency1=spikecount/float(stim_time)
	mean_frequency=[mean_frequency1]
	return mean_frequency
		