def ADP_idx_fun(start,end,AHP_AP_begin):
	min=[]
	max=[]
	for i in range(start,end-2):
		diff1=AHP_AP_begin[i+1]-AHP_AP_begin[i]
		diff2=AHP_AP_begin[i+2]-AHP_AP_begin[i+1]
		if diff1<0 and diff2>0:
			min=np.append(min,i+1)
		if diff1>0 and diff2<0:
			max=np.append(max,i+1)
		
	ADP_idx=0
	max_idx=[]
	for i in range(0,len(max)-20):
		diff1=AHP_AP_begin[max[i+4]]-AHP_AP_begin[max[i]]
		diff2=AHP_AP_begin[max[i+8]]-AHP_AP_begin[max[i]]
		diff3=AHP_AP_begin[max[i+12]]-AHP_AP_begin[max[i]]
		diff4=AHP_AP_begin[max[i+16]]-AHP_AP_begin[max[i]]
		diff5=AHP_AP_begin[max[i+20]]-AHP_AP_begin[max[i]]
		if diff1<0 and diff2<0 and diff3<0 and diff4<0 and diff5<0:
			max_idx=np.append(max_idx,max[i])

	if len(max_idx)>=3:	
		for i in range(0,len(max_idx)-2):
			diff1=AHP_AP_begin[max_idx[i+1]]-AHP_AP_begin[max_idx[i]]
			diff2=AHP_AP_begin[max_idx[i+2]]-AHP_AP_begin[max_idx[i]]
			if diff1<0 and diff2<0:
				ADP_idx=max_idx[i]
				break
	else:
		if len(max_idx)>0:
			v_max=0
			for i in range(0,len(max_idx)):
				v=voltage[max_idx[i]]
				if v>v_max:
					v_max=v
					ADP_idx=max_idx[i]
		else:
			ADP_idx=0
	return ADP_idx
		
#ADP_AP_begin=voltage[np.int(ADP_idx):end]
#second_AHP_idx=np.where(ADP_AP_begin==np.min(ADP_AP_begin))[0][0]+ADP_idx	