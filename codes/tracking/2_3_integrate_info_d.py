#!/usr/bin/env python3
# -*- coding: utf-8 -*-
#python>=3.9
"""
Created on 2021/10/5

@author: dsato
"""

import sys, os, glob
import numpy as np
import pandas as pd
import datetime

# python 2_3_integrate_info_d.py {fixed_track.tsv} {video_info.tsv} {fixed_track_final.tsv}

prefix = os.path.splitext(os.path.basename(sys.argv[1]))[0]
date = prefix.split('_')[0]
info = prefix.split('_')[2] + '_' + prefix.split('_')[3]
place = info.split('-')[0]
strain = info.split('-')[1]
sex = info.split('-')[2]
age = info.split('-')[3]
n_inds = info.split('-')[4]
N = info.split('-')[5]

infile = str(sys.argv[1])
timeline = str(sys.argv[2])
outfile = str(sys.argv[3])

def reduce_mem_usage(df):
    """ iterate through all the columns of a dataframe and modify the data type
        to reduce memory usage.        
    """
    start_mem = df.memory_usage().sum() / 1024**2
    print('Memory usage of dataframe is {:.2f} MB'.format(start_mem))
    
    for col in df.columns:
        col_type = df[col].dtype
        
        if col_type != object:
            c_min = df[col].min()
            c_max = df[col].max()
            if str(col_type)[:3] == 'int':
                if c_min > np.iinfo(np.int8).min and c_max < np.iinfo(np.int8).max:
                    df[col] = df[col].astype(np.int8)
                elif c_min > np.iinfo(np.int16).min and c_max < np.iinfo(np.int16).max:
                    df[col] = df[col].astype(np.int16)
                elif c_min > np.iinfo(np.int32).min and c_max < np.iinfo(np.int32).max:
                    df[col] = df[col].astype(np.int32)
                elif c_min > np.iinfo(np.int64).min and c_max < np.iinfo(np.int64).max:
                    df[col] = df[col].astype(np.int64)  
            else:
                if c_min > np.finfo(np.float16).min and c_max < np.finfo(np.float16).max:
                    df[col] = df[col].astype(np.float16)
                elif c_min > np.finfo(np.float32).min and c_max < np.finfo(np.float32).max:
                    df[col] = df[col].astype(np.float32)
                else:
                    df[col] = df[col].astype(np.float64)
        else:
            df[col] = df[col].astype('category')
    end_mem = df.memory_usage().sum() / 1024**2
    print('Memory usage after optimization is: {:.2f} MB'.format(end_mem))
    print('Decreased by {:.1f}%'.format(100 * (start_mem - end_mem) / start_mem))
    return df

def weighted_ma(li):
	weight = np.arange(len(li)) + 1
	wma = np.sum(weight * li) / weight.sum()
	return wma

if __name__ == '__main__':
	df1 = pd.read_csv(timeline, dtype = {'TimeStamp':'str'}, sep='\t')
	dict11 = dict(zip(df1['No'], df1['TimeStamp']))
	dict12 = dict(zip(df1['No'], df1['Stimuli']))
	df1lt = [datetime.datetime.strptime(v,'%Y%m%d%H%M%S.%f') for v in list(df1['TimeStamp'])]
	df1ltd = [float(str(df1lt[i]-df1lt[i-1])[-9:]) if i>0 else 0 for i,v in enumerate(df1lt)]
	df1ltdt = [float(str((df1lt[i]-df1lt[0]).seconds) + '.' + '{0:06d}'.format((df1lt[i]-df1lt[0]).microseconds)) if i>0 else 0 for i,v in enumerate(df1lt)]
	dict21 = dict(zip(df1['No'], df1ltd))
	dict22 = dict(zip(df1['No'], df1ltdt))

	df2 = reduce_mem_usage(pd.read_table(infile))

	dict3, dict4_x, dict4_y, dict5 = dict(), dict(), dict(), dict()
	for fly in list(pd.Series.unique(df2['id'])):
		lx = list(df2[df2['id'] == fly]['pos_x'])
		ly = list(df2[df2['id'] == fly]['pos_y'])
		ltup = [(i, fly) for i in list(df2[df2['id'] == fly]['frame'])]

		# calculate weighted moving average
		num_roll_pos = 10
		dfx_w = pd.DataFrame(lx).rolling(num_roll_pos-1).apply(weighted_ma, raw=True) # average over before 9 + current time stamps
		lx_w = list(dfx_w.to_numpy().ravel())
		dict4_x = dict4_x | dict(zip(ltup, lx_w))
		dfy_w = pd.DataFrame(ly).rolling(num_roll_pos-1).apply(weighted_ma, raw=True) # average over before 9 + current time stamps
		ly_w = list(dfy_w.to_numpy().ravel())
		dict4_y = dict4_y | dict(zip(ltup, ly_w))

		# calculate distance diffrence
		lxd = np.array([lx_w[i]-lx_w[i-1] if i>0 else 0 for i,v in enumerate(lx_w)])
		lyd = np.array([(ly_w[i]-ly_w[i-1])*(-1) if i>0 else 0 for i,v in enumerate(ly_w)])
		ld = (lxd**2 + lyd**2)**(1/2)
		dict3 = dict3 | dict(zip(ltup, ld))

		# calculate angle
		time_prepost = 50
		lx_w_d = np.array([lx_w[i+time_prepost]-lx_w[i-time_prepost] if i >= time_prepost and i < len(lx_w)-time_prepost else 0 for i,v in enumerate(lx_w)])
		ly_w_d = np.array([(ly_w[i+time_prepost]-ly_w[i-time_prepost])*(-1) if i >= time_prepost and i < len(ly_w)-time_prepost else 0 for i,v in enumerate(ly_w)])

		# set same angle as previous time stamp when individuals did not move much
		move_threshold = 5
		lx_w_d = np.array([lx_w_d[i-1] if lxd[i] < move_threshold else v for i,v in enumerate(lx_w_d)])
		ly_w_d = np.array([ly_w_d[i-1] if lyd[i] < move_threshold else v for i,v in enumerate(ly_w_d)])

		la =  np.arctan2(ly_w_d, lx_w_d)
		dict5 = dict5 | dict(zip(ltup, la))

	df2['prefix'] = date
	df2['place'] = place
	df2['strain'] = strain
	df2['sex'] = sex
	df2['age'] = age
	df2['n_inds'] = n_inds
	df2['N'] = N
	df2['timestamp'] = df2.apply(lambda x: dict11[x.frame], axis = 1)
	df2['seconds_diff'] = df2.apply(lambda x: dict21[x.frame], axis = 1)
	df2['seconds_total'] = df2.apply(lambda x: dict22[x.frame], axis = 1)
	df2['stimuli'] = df2.apply(lambda x: dict12[x.frame], axis = 1)
	df2['pos_x_wma'] = df2.apply(lambda x: dict4_x[(x.frame, x.id)], axis = 1)
	df2['pos_y_wma'] = df2.apply(lambda x: dict4_y[(x.frame, x.id)], axis = 1)
	df2['travelled_dist_diff'] = df2.apply(lambda x: dict3[(x.frame, x.id)], axis = 1)
	df2['angle_diff_based'] = df2.apply(lambda x: dict5[(x.frame, x.id)], axis = 1)
	print('Added some information!\n')
	df2.to_csv(outfile, sep='\t', index=False)
