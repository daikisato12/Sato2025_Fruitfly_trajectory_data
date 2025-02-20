#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Modified from original "3d Termite_collective_behaviour.ipynb" in https://github.com/vivekhsridhar/tracktor
Created on 2021/10/2
Last modified on 2021/12/9
@author: dsato
"""

import os, sys
os.environ["OPENBLAS_NUM_THREADS"] = str(sys.argv[4])
os.environ["OMP_NUM_THREADS"] = str(sys.argv[4])

import numpy as np
import pandas as pd
import tracktor as tr
import cv2
import random
random.seed(12345)

from sklearn.cluster import KMeans
from scipy.optimize import linear_sum_assignment
from scipy.spatial.distance import cdist

from threadpoolctl import threadpool_info
from pprint import pp

pp(threadpool_info())

# python 1_1_run_tracktor.py {raw_video.mp4} {first_image.jpg} {raw_data_dir} {number_core}

# Global parameters
# colours is a vector of BGR values which are used to identify individuals in the video
# t_id is termite id and is also used for individual identification
# number of elements in colours should be greater than n_inds (THIS IS NECESSARY FOR VISUALISATION ONLY)
# number of elements in t_id should be greater than n_inds (THIS IS NECESSARY TO GET INDIVIDUAL-SPECIFIC DATA)

video = os.path.splitext(os.path.basename(sys.argv[1]))[0]

n_inds = int(video.split('_')[2].split('-')[4])
t_id = [f'Fly{i}' for i in range(1,n_inds+1)]
colours = [(59,185,246),(18,190,111),(221,46,190),(183, 201, 64),(0,0,250),(88,127,59)]#,(64,21,183),(98,61,10),(144,149,173),(0,0,0),(255,255,255),(81,127,249),(190,159,134)]

# this is the block_size and offset used for adaptive thresholding (block_size should always be odd)
# these values are critical for tracking performance
block_size = 13
offset = 15

# the scaling parameter can be used to speed up tracking if video resolution is too high (use value 0-1)
scaling = 1.0

# minimum area and maximum area occupied by the animal in number of pixels
# this parameter is used to get rid of other objects in view that might be hard to threshold out but are differently sized
min_area = 5
max_area = 150

# mot determines whether the tracker is being used in noisy conditions to track a single object or for multi-object
# using this will enable k-means clustering to force n_inds number of animals
mot = True

# in this example, we use a circular mask to ignore area outside the petri-dish
# mask_offset represents offset of circle from centre of the frame
dict_x = {'no1': -5, 'no2': -3, 'no3': -2, 'no4': 2 , 'no5': -5, 'no6': -3, 'no7': -2, 'no8': 2, 'no9': -5, 'no10': -3, 'no11': -2, 'no12': 2}
dict_y = {'no1': -5, 'no2': -5, 'no3': -5, 'no4': -5, 'no5': -1, 'no6': -1, 'no7': -1, 'no8': -1, 'no9': 4, 'no10': 4, 'no11': 4, 'no12': 4}

## Auto detection of the center coordinate of circle arena
def detect_circles(gray):
    # detect circles in the image
    circles = cv2.HoughCircles(gray, cv2.HOUGH_GRADIENT, 5, 1000, param1=10, param2=20, minRadius=65, maxRadius=75)
    # ensure at least some circles were found
    if circles is not None:
        # convert the (x, y) coordinates and radius of the circles to integers
        circles = np.round(circles[0, :]).astype("int")
        # loop over the (x, y) coordinates and radius of the circles
        for (x, y, r) in circles:
            offset_x = x-80
            offset_y = y-80
        return(offset_x, offset_y)
    else:
        print('Auto-detection of circle failed. Used a previosly set coordinate of arena center.')
        return(dict_x[video.split('_')[2].split('-')[0]], dict_y[video.split('_')[2].split('-')[0]])

image = cv2.imread(sys.argv[2])
gray = cv2.cvtColor(image, cv2.COLOR_BGR2GRAY)
mask_offset_x, mask_offset_y = detect_circles(gray)
temp_mask_x, temp_mask_y = mask_offset_x, mask_offset_y
flag = "Auto-detected"
if abs(dict_x[video.split('_')[2].split('-')[0]] - mask_offset_x) > 5 or abs(dict_y[video.split('_')[2].split('-')[0]] - mask_offset_y) > 5:
	if abs(dict_x[video.split('_')[2].split('-')[0]] - mask_offset_x) > 5:
		mask_offset_x = int((dict_x[video.split('_')[2].split('-')[0]] + mask_offset_x)/2)
	if abs(dict_y[video.split('_')[2].split('-')[0]] - mask_offset_y) > 5:
		mask_offset_y = int((dict_y[video.split('_')[2].split('-')[0]] + mask_offset_y)/2)
	flag = "Manually curated, auto_offset_x:{}, auto_offset_y:{}".format(temp_mask_x, temp_mask_y)

print('Coordinates:{}, mask_offset_x:{}, mask_offset_y:{}'.format(flag, mask_offset_x, mask_offset_y))
mask_radius = 74

# name of source video and paths
input_vidpath = os.path.dirname(sys.argv[1]) + "/" + video + '.mp4'
output_vidpath = os.path.dirname(sys.argv[3]) + "/" + video + '_tracked.avi'
output_filepath = os.path.dirname(sys.argv[3]) + "/" + video + '_tracked.csv'
codec = 'DIVX' # try other codecs if the default doesn't work ('DIVX', 'avc1', 'XVID') note: this list is non-exhaustive

# run tracking

## Open video
cap = cv2.VideoCapture(input_vidpath)
if cap.isOpened() == False:
    sys.exit('Video file cannot be read! Please check input_vidpath to ensure it is correctly pointing to the video file')

## Video writer class to output video with contour and centroid of tracked object(s)
# make sure the frame size matches size of array 'final'
fourcc = cv2.VideoWriter_fourcc(*codec)
output_framesize = (int(cap.read()[1].shape[1]*scaling),int(cap.read()[1].shape[0]*scaling))
out = cv2.VideoWriter(filename = output_vidpath, fourcc = fourcc, fps = 60.0, frameSize = output_framesize, isColor = True)

## Individual location(s) measured in the last and current step
meas_last = list(np.zeros((n_inds,2)))
meas_now = list(np.zeros((n_inds,2)))

df = []
last = 0

while(True):
    # Capture frame-by-frame
    ret, frame = cap.read()
    
    this = cap.get(1)
    if ret == True:
        # Preprocess the image for background subtraction
        frame = cv2.resize(frame, None, fx = scaling, fy = scaling, interpolation = cv2.INTER_LINEAR)
        
        # Apply mask to ignore area outside the petri dish
        mask = np.zeros((frame.shape[0], frame.shape[1]))
        cv2.circle(mask, (mask.shape[1]//2 + mask_offset_x, mask.shape[0]//2 + mask_offset_y), mask_radius, mask_radius, -1)
        frame[mask == 0] = 0
        
        thresh = tr.colour_to_thresh(frame, block_size, offset)
        # Custom background subtraction
        bg = thresh.copy()
        cv2.circle(bg, (bg.shape[1]//2 + mask_offset_x, bg.shape[0]//2 + mask_offset_y), 0, 0, -1)
        bgsub = thresh - bg
        
        final, contours, meas_last, meas_now = tr.detect_and_draw_contours(frame, thresh, meas_last, meas_now, min_area, max_area)
        if len(meas_now) != n_inds:
            if len(meas_now) == 0: #added to avoid error
                meas_now = meas_last
            else:
                contours, meas_now = tr.apply_k_means(contours, n_inds, meas_now)
        
        row_ind, col_ind = tr.hungarian_algorithm(meas_last, meas_now)
        final, meas_now, df = tr.reorder_and_draw(final, colours, n_inds, col_ind, meas_now, df, mot, this)
        
        # Create output dataframe
        for i in range(n_inds):
            df.append([this, meas_now[i][0], meas_now[i][1], t_id[i]])
        
        # Display the resulting frame
        out.write(final)
            
    if last >= this:
        break
    
    last = this

## Write positions to file
df = pd.DataFrame(np.matrix(df), columns = ['frame','pos_x','pos_y', 'id'])
df['center_x'] = 80 + mask_offset_x
df['center_y'] = 80 + mask_offset_y
df.to_csv(output_filepath, sep=',', index=False)

## When everything done, release the capture
cap.release()
out.release()