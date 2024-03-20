#!/usr/bin/env python3


import os
import json
import sys
import struct
import numpy as np

from event import *
from analysis import *

#-------------------------------------------------------------------------------------
def process_infos(startinfo, eventinfos):

    # 处理一些原始数据，计算一些比例，占比等参数
  
    eventinfos.append(cal_fraction((eventinfos[43] + eventinfos[44]) * 1000, eventinfos[1], "exe_misp_MPKI"))
    eventinfos.append(cal_fraction((eventinfos[53] + eventinfos[54]) * 1000, eventinfos[1], "com_misp_MPKI"))
    eventinfos.append(cal_fraction((eventinfos[53]) * 1000, eventinfos[1], "com_br_MPKI"))
    eventinfos.append(cal_fraction((eventinfos[54]) * 1000, eventinfos[1], "com_jalr_MPKI"))
    eventinfos.append(cal_fraction((eventinfos[55]) * 1000, eventinfos[1], "com_ret_MPKI"))
    eventinfos.append(cal_fraction((eventinfos[56]) * 1000, eventinfos[1], "com_call_MPKI"))

    eventinfos.append(cal_fraction(eventinfos[1], eventinfos[0], "user_ipc"))
    # eventinfos.append(cal_fraction(eventinfos[31],eventinfos[32],"f3_hit_misp_rate"))
    eventinfos.append(cal_fraction(eventinfos[37] * 1000, eventinfos[1], "com_br_ftb_entry_hit_MPKI"))
    eventinfos.append(cal_fraction(eventinfos[38] * 1000, eventinfos[1], "com_br_ftb_slot_hit_MPKI"))


    #eventinfos.append(cal_fraction(eventinfos[35],eventinfos[50],"tage_jalr_hit_rate"))
    # eventinfos.append(cal_fraction(eventinfos[32],eventinfos[31],"tage_br_misp/hit"))

    eventinfos.append(cal_fraction(eventinfos[57], eventinfos[0], "fetch_buffer_empty_rate"))

    eventinfos.extend(cal_percentage(eventinfos[59:67]))
    return []


#----------------------------------------------------------------------------------#

if len(sys.argv) < 2: 
    print("parameters are not enough.\n ./process.py logfile")
    exit()

if not os.path.exists(sys.argv[1]):
    print(sys.argv[1]+" is not exist.")
    exit()

# 读取事件名称
eventdict = read_eventlist()
# 从log文件中获取信息
startinfo, eventinfos = readEventInfo(sys.argv[1], eventdict)
# 计算一些相关性，同时增加一些事件
process_infos(startinfo, eventinfos)
# 保存event信息到csv文件中
saveEventInfo(startinfo, eventinfos, "h", sys.argv[1])