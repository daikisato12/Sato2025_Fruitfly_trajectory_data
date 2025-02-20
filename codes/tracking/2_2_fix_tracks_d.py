#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on 2021/10/2

@author: dsato
"""

import sys, os, glob
os.environ["OPENBLAS_NUM_THREADS"] = str(sys.argv[2])
os.environ["OMP_NUM_THREADS"] = str(sys.argv[2])

import numpy as np
import pandas as pd

# python 2_2_fix_tracks_d.py {raw_track.csv} {number_core}

prefix = os.path.splitext(os.path.basename(sys.argv[1]))[0]
info = prefix.split('_')[2] + '_' + prefix.split('_')[3]
place = info.split('-')[0]
trackfile = str(sys.argv[1])
outfile = trackfile[:-4] + '_fixed.tsv'

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

def check_circular_list(li):
	flag = 0
	dict_li = dict()
	for item in li:
		dict_li[item[0]] = item[1]
	for item in li:
		item1 = item[0]
		item_tmp = item[0]
		flag2 = 0
		out_li = list()
		while item_tmp in dict_li.keys():
			out_li.append([item_tmp,dict_li[item_tmp]])
			item_tmp2 = item_tmp
			item_tmp = dict_li[item_tmp]
			dict_li.pop(item_tmp2)
			flag2 = 1
		if item_tmp == item1 and flag2 == 1:
			flag = 1
			return flag, out_li
			break
	return [flag]

def fix_tracks(df1, center_x, center_y, r, threshold):
	df2 = df1.copy()
	for i in range(4,frame_list[-1]+1): # timastamp 1-3 are not included in the dataset
		fixid_list = [1,2,3]
		while len(fixid_list) > 0:
			dist_dict = dict()
			pos_x = dict()
			pos_y = dict()
			for idn in id_list:
				dist_from_center = ((df2['pos_x'][idn][i]-(center_x))**2 + (df2['pos_y'][idn][i]-(center_y))**2)**(1/2)
				if dist_from_center > r: # if outside masking area
					print(i, 'out of arena', idn, dist_from_center, df2['pos_x'][idn][i], df2['pos_x'][idn][i-1], df2['pos_y'][idn][i], df2['pos_y'][idn][i-1])
					df2['pos_x'][idn][i] = df2['pos_x'][idn][i-1]
					df2['pos_y'][idn][i] = df2['pos_y'][idn][i-1]
					print(i, idn, dist_from_center, df2['pos_x'][idn][i], df2['pos_x'][idn][i-1], df2['pos_y'][idn][i], df2['pos_y'][idn][i-1])
				dist_dict[idn] = ((df2['pos_x'][idn][i]-df2['pos_x'][idn][i-1])**2 + (df2['pos_y'][idn][i]-df2['pos_y'][idn][i-1])**2)**(1/2)
				pos_x[idn] = df2['pos_x'][idn][i]
				pos_y[idn] = df2['pos_y'][idn][i]
			for k1 in range(len(id_list)-1):
				for k2 in range(k1+1,len(id_list)):
					id1 = id_list[k1]
					id2 = id_list[k2]
					if pos_x[id1] == pos_x[id2] and pos_y[id1] == pos_y[id2]:
						print(i, id1, id2, pos_x[id1], pos_y[id1])
						print('Error: Two individuals are located in the same position.')
						if dist_dict[id1] > dist_dict[id1]:
							print(i, id1, df2['pos_x'][id1][i], df2['pos_x'][id1][i-1], df2['pos_y'][id1][i], df2['pos_y'][id1][i-1])
							df2['pos_x'][id1][i] = df2['pos_x'][id1][i-1]
							df2['pos_y'][id1][i] = df2['pos_y'][id1][i-1]
							print(i, id1, df2['pos_x'][id1][i], df2['pos_x'][id1][i-1], df2['pos_y'][id1][i], df2['pos_y'][id1][i-1])
						else:
							print(i, id2, df2['pos_x'][id2][i], df2['pos_x'][id2][i-1], df2['pos_y'][id2][i], df2['pos_y'][id2][i-1])
							df2['pos_x'][id2][i] = df2['pos_x'][id2][i-1]
							df2['pos_y'][id2][i] = df2['pos_y'][id2][i-1]
							print(i, id2, df2['pos_x'][id2][i], df2['pos_x'][id2][i-1], df2['pos_y'][id2][i], df2['pos_y'][id2][i-1])
			item_list = sorted(dist_dict.items(), key=lambda x:x[1])
			fixid_list = list()
			nn_dist_dict = dict()
			for k in range(len(item_list)):
				nn_dist = 1000000
				id1 = item_list[k][0]
				for idn in id_list:
					dist_tmp = ((df2['pos_x'][id1][i]-df2['pos_x'][idn][i-1])**2 + (df2['pos_y'][id1][i]-df2['pos_y'][idn][i-1])**2)**(1/2)
					if dist_tmp < nn_dist:
						nn_dist = dist_tmp
						nn_id = idn
					elif dist_tmp == nn_dist and id1 == idn:
						nn_id = id1
				nn_dist_dict[item_list[k][0]] = [item_list[k][0], nn_id, nn_dist]
				if id1 != nn_id:
					fixid_list.append([id1, nn_id, 
					f'{id1} nn_dist: {nn_dist}', 
					f'{id1} dist_diff: {item_list[k][1]}', 
					f'{id1} pos_x: {pos_x[id1]}', f'{id1} pos_y: {pos_y[id1]}'])
			print(i, fixid_list)
			if len(fixid_list) > 1:
				if check_circular_list(fixid_list)[0] == 1: # when circular pairs are in the list
					df3 = df2.copy()
					for item in check_circular_list(fixid_list)[1]:
						id1 = item[0]
						id2 = item[1]
						print(i, id1, df2['pos_x'][id1][i], df2['pos_x'][id1][i-1], df2['pos_y'][id1][i], df2['pos_y'][id1][i-1])
						print(i, id2, df2['pos_x'][id2][i], df2['pos_x'][id2][i-1], df2['pos_y'][id2][i], df2['pos_y'][id2][i-1])
						df2['pos_x'][id2][i-3:] = df3['pos_x'][id1][i-3:] # change all the data from current frame
						df2['pos_y'][id2][i-3:] = df3['pos_y'][id1][i-3:] # change all the data from current frame
						print(i, id1, df2['pos_x'][id1][i], df2['pos_x'][id1][i-1], df2['pos_y'][id1][i], df2['pos_y'][id1][i-1])
						print(i, id2, df2['pos_x'][id2][i], df2['pos_x'][id2][i-1], df2['pos_y'][id2][i], df2['pos_y'][id2][i-1])
				else:
					for i2 in range(1,len(id_list)):
						for item in fixid_list:
							id1 = item[0]
							if id1 == item_list[-i2][0]:
								print(i, id1, item_list[-i2], df2['pos_x'][id1][i], df2['pos_x'][id1][i-1], df2['pos_y'][id1][i], df2['pos_y'][id1][i-1])
								df2['pos_x'][id1][i] = df2['pos_x'][id1][i-1]
								df2['pos_y'][id1][i] = df2['pos_y'][id1][i-1]
								print(i, id1, item_list[-i2], df2['pos_x'][id1][i], df2['pos_x'][id1][i-1], df2['pos_y'][id1][i], df2['pos_y'][id1][i-1])
								break
			elif len(fixid_list) == 1:
				id1 = fixid_list[0][0]
				id2 = fixid_list[0][1]
				if dist_dict[id2] < 1 and dist_dict[id2] != 0:
					if dist_dict[id1] < threshold: # leave as it is when considered to be trully jumping (< 30 pixel/frame (300-500mm/sec))
						print(i, fixid_list, dist_dict[id1], dist_dict[id2])
						fixid_list = list()
						break
					elif dist_dict[id2] < threshold: #しょうがないのでid2の移動量が30pixel以下ならこちらを飛ばす
						df3 = df2.copy()
						print(i, id1, df2['pos_x'][id1][i], df2['pos_x'][id1][i-1], df2['pos_y'][id1][i], df2['pos_y'][id1][i-1])
						print(i, id2, df2['pos_x'][id2][i], df2['pos_x'][id2][i-1], df2['pos_y'][id2][i], df2['pos_y'][id2][i-1])
						df2['pos_x'][id1][i-3:], df2['pos_x'][id2][i-3:] = df3['pos_x'][id2][i-3:], df3['pos_x'][id1][i-3:]
						df2['pos_y'][id1][i-3:], df2['pos_y'][id2][i-3:] = df3['pos_y'][id2][i-3:], df3['pos_y'][id1][i-3:]
						print(i, id1, df2['pos_x'][id1][i], df2['pos_x'][id1][i-1], df2['pos_y'][id1][i], df2['pos_y'][id1][i-1])
						print(i, id2, df2['pos_x'][id2][i], df2['pos_x'][id2][i-1], df2['pos_y'][id2][i], df2['pos_y'][id2][i-1])
						break
				else:
					print(i, id1, df2['pos_x'][id1][i], df2['pos_x'][id1][i-1], df2['pos_y'][id1][i], df2['pos_y'][id1][i-1])
					df2['pos_x'][id1][i] = df2['pos_x'][id1][i-1]
					df2['pos_y'][id1][i] = df2['pos_y'][id1][i-1]
					print(i, id1, df2['pos_x'][id1][i], df2['pos_x'][id1][i-1], df2['pos_y'][id1][i], df2['pos_y'][id1][i-1])
					break
			else:
				print(i, fixid_list)
				break
		else:
			continue
	print('Fixed tracks!\n')
	return df2

if __name__ == '__main__':
	df = pd.read_csv(trackfile, dtype = {'frame':'int','pos_x':'float','pos_y':'float','id':'str','center_x':'int','center_y':'int'})#.drop('Unnamed: 0', axis=1)
	center_x = df['center_x'][0]
	center_y = df['center_y'][0]
	df1 = (df.pivot(index='frame', columns='id', values=['pos_x','pos_y']).sort_index(level=[1,0]))
	id_list = list(df1['pos_x'])
	frame_list = list(df1.index)

	# fix tracks
	df_fixed = fix_tracks(reduce_mem_usage(df1), center_x, center_y, 70, 30) # arena radius: 70 pixel, max moving threshold: 30 pixel per frame
	# make one-level column
	df_fixed.columns = df_fixed.columns.map('_'.join).str.strip()
	# remove index
	df_fixed = df_fixed.reset_index()

	# pivot_longer
	df_fixed_tmp = df_fixed.melt(id_vars='frame', var_name='item', value_name='value')
	df_fixed_tmp['xy'] = df_fixed_tmp.item.str[0:5]
	df_fixed_tmp['id'] = df_fixed_tmp.item.str[6:]
	df_fixed = df_fixed_tmp.pivot_table(index=['frame','id'], columns='xy',values='value').reset_index()

	df_fixed.to_csv(outfile, sep='\t', index=False)
