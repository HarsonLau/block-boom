#!/usr/bin/env python3


import matplotlib as mpl
import numpy as np
import matplotlib.pyplot as plt
from matplotlib.ticker import MaxNLocator
from matplotlib.ticker import MultipleLocator
import matplotlib.ticker as ticker
from matplotlib.backends.backend_pdf import PdfPages

import csv
import os
import sys
import math

from pyparsing import alphanums

mpl.rcParams['font.sans-serif'] = ['Times New Roman']
mpl.rcParams['font.serif'] = ['Times New Roman']
mpl.rcParams['axes.unicode_minus'] = False  # 解决保存图像是负号'-'显示为方块的问题,或者转换负号为字符串

class LineData:
    value = []
    def __init__(self):
        self.value = []

    def addinfo(self, value):
        if isinstance(value, str):
            self.value.append(float(value))
        else:
            self.value.append(value)

    def op(self, other, optype):
        temp = LineData()
        idx = 0
        if isinstance(other, float) or isinstance(other, int):
            while idx < len(self.value):
                if optype == "-":
                    val = self.value[idx] - other
                elif optype == "*":
                    val = self.value[idx] * other
                elif optype == "/":
                    val = 1.0 * self.value[idx] / other
                else:
                    val = self.value[idx] + other
                temp.addinfo(val)
                idx = idx + 1

        if isinstance(other, LineData):
            if len(other.value) != len(self.value):
                return temp
            while idx < len(self.value):
                if optype == "-":
                    val = self.value[idx] - other.value[idx]
                elif optype == "*":
                    val = self.value[idx] * other.value[idx]
                elif optype == "/":
                    val = 1.0 * self.value[idx] / other.value[idx]
                else:
                    val = self.value[idx] + other.value[idx]
                temp.addinfo(val)
                idx = idx + 1
        return temp

    # 重载加减乘除运算
    def __add__(self, other):
        return self.op(other, "+")

    def __sub__(self, other):
        return self.op(other, "-")

    def __mul__(self, other):
        return self.op(other, "*")

    def __truediv__(self, other):
        return self.op(other, "/")


def cal_correlation(val1, val2):
    if not(np.any(val2)) or not(np.any(val2)):
        return 0
    ab = np.array([val1, val2])
    cor_info = np.corrcoef(ab)
    return cor_info[0,1]


datas = {}
def readdata(filename):
    f = open(filename, 'r')
    reader = csv.reader(f)
    for row in reader:
        temp = LineData()
        idx = 1
        while idx < len(row):
            if len(row[idx]) > 0:
                temp.addinfo(row[idx])
            idx = idx + 1
        # datas.append(temp)
        if len(row[0]) == 0:
            row[0] = "stage"
        datas[row[0]] = temp
    f.close()



def setMaxMin(minvalue, maxvalue, nstep):
    step = (maxvalue - minvalue)/nstep
    t = math.pow(10, -1*math.floor(math.log10(step)))
    minval = int(minvalue*t)/t
    maxval = math.ceil(maxvalue*t)/t
    step = (maxval - minval)/nstep
    t = math.pow(10, -1*math.floor(math.log10(step)))
    newstep = round(step*t)
    if(newstep<step*t):
        step = newstep+0.5
    else:
        step = newstep
    step = step / t
    maxval = minval + step * nstep
    return minval, maxval, step


def draw_pecetage(xvalue, lnames, rnames, xlabel, ylabel1, ylabel2):
    def make_label(value, pos):
        return '%0.0f%%' % (100. * value)

    print("draw percetage: " + ylabel1 + " & " + ylabel2)
    fig, ax = plt.subplots(figsize=(12, 4))
    ccolors = plt.get_cmap('RdYlBu')(np.linspace(0.85, 0.15, len(lnames)+len(rnames)))
    # ccolors = plt.get_cmap('RdYlGn')(np.linspace(0.85, 0.15, len(lnames)+len(rnames)))
    
    bottom_value = datas[lnames[0]]*0.0
    idx = 0
    while idx < len(lnames):
        ax.bar(xvalue, datas[lnames[idx]].value, width=1.0, label=lnames[idx], color=ccolors[idx], bottom=bottom_value.value)
        bottom_value = datas[lnames[idx]] + bottom_value
        idx = idx + 1
    
    ax.yaxis.set_major_formatter(ticker.FuncFormatter(make_label))
    ax.yaxis.set_major_locator(MaxNLocator(11))
    ax.xaxis.set_major_locator(MaxNLocator(20))
    ax.set_xlim([0, max(xvalue)])
    ax.set_ylim([0,1.1])
    ax.set_yticks(np.arange(0, 1.1, 0.1)[0:11])
    ax.set_xlabel(xlabel, fontsize = 12, labelpad = 1)
    ax.set_ylabel(ylabel1, fontdict={"family": "Times New Roman", "size": 12}, labelpad = 0)  
    ax.tick_params(axis="x", which="major", length=4, labelrotation=45, pad = 0.2, labelsize = 10)
    ax.tick_params(axis="y", which="major", length=4, pad = 0.1, labelsize = 10)
    ax.grid(axis='y', linestyle ='--', alpha = 0.5)
    
    if len(rnames) != 0:
        ax2 = ax.twinx() 
        maxvalue = 0.0
        minvalue = min(datas[rnames[0]].value)
        cidx = len(lnames)
        for line in rnames:
            maxvalue = max(maxvalue, max(datas[line].value))
            minvalue = min(minvalue, min(datas[line].value))
            ax2.plot(xvalue, datas[line].value, '-', label=line, color=ccolors[cidx], lw = 2)
            cidx = cidx + 1
        
        ax2.yaxis.set_major_locator(MaxNLocator(11))
        minvalue, maxvalue, step = setMaxMin(minvalue, maxvalue, 10)
        ax2.set_ylim([minvalue, maxvalue+step])
        ax2.set_yticks(np.arange(minvalue, maxvalue+step, step)[0:11])
        ax2.tick_params(axis="y", which="major", length=4, pad = 0.2, labelsize = 10)
        ax2.set_ylabel(ylabel2, fontdict={"family": "Times New Roman", "size": 12})

    # 将图例置于当前坐标轴下
    fig.legend(loc='upper right', bbox_to_anchor=(1, 1.02),frameon=False, bbox_transform=ax.transAxes, ncol=(len(lnames)+len(rnames)))
    # fig.legend(loc = 'upper right', bbox_to_anchor=(1,1.1), bbox_transform=ax.transAxes, ncol=(len(lnames)+len(rnames)))#frameon=False
    plt.subplots_adjust(bottom =0.15, left=0.06, right = 0.95, top=0.91)  
    # plt.show()
    return fig
    

def draw_lines(xvalue, lnames, rnames, xlabel, ylabel1, ylabel2):    
    print("draw line: " + ylabel1 + " & " + ylabel2)
    fig, ax = plt.subplots(figsize=(12, 4))
    ccolors = plt.get_cmap('Paired')(np.linspace(0.80, 0.10, len(lnames)+len(rnames)))
    
    cidx = 0
    maxvalue = 0.0
    minvalue = min(datas[lnames[0]].value)
    for line in lnames:
        maxvalue = max(maxvalue, max(datas[line].value))
        minvalue = min(minvalue, min(datas[line].value))
        ax.plot(xvalue, datas[line].value, '-', label=line, color=ccolors[cidx], lw = 2)
        cidx = cidx + 1
    
    # ax.yaxis.set_major_formatter(ticker.FuncFormatter(make_label))
    ax.xaxis.set_major_locator(MaxNLocator(20))
    ax.yaxis.set_major_locator(MaxNLocator(11))
    ax.tick_params(axis="x", which="major", length=4, labelrotation=45, pad = 0.2, labelsize = 10)
    ax.tick_params(axis="y", which="major", length=4, pad = 0.2, labelsize = 10)
    ax.set_xlabel(xlabel, fontsize = 12, labelpad = 1)
    ax.set_ylabel(ylabel1, fontdict={"family": "Times New Roman", "size": 12})
    
    ax.set_xlim([0, max(xvalue)])
    # print(minvalue, maxvalue)
    minvalue, maxvalue, step = setMaxMin(minvalue, maxvalue, 10)
    # print(minvalue, maxvalue)
    ax.set_ylim([minvalue, maxvalue+step])
    ax.set_yticks(np.arange(minvalue, maxvalue+step, step)[0:11])
    ax.grid(axis='y', linestyle ='--', alpha = 0.5)

    if len(rnames) != 0:
        ax2 = ax.twinx() 
        maxvalue = max(datas[rnames[0]].value)
        minvalue = min(datas[rnames[0]].value)
        cidx = len(lnames)
        for line in rnames:
            maxvalue = max(maxvalue, max(datas[line].value))
            minvalue = min(minvalue, min(datas[line].value))
            ax2.plot(xvalue, datas[line].value, '-', label=line, color=ccolors[cidx], lw = 2)
            cidx = cidx + 1
        ax2.tick_params(axis="y", which="major", length=4, pad = 0.2, labelsize = 10)
        ax2.set_ylabel(ylabel2, fontdict={"family": "Times New Roman", "size": 12})
        ax2.yaxis.set_major_locator(MaxNLocator(11))
        minvalue, maxvalue, step = setMaxMin(minvalue, maxvalue, 10)
        # print(minvalue, maxvalue)
        ax2.set_ylim([minvalue, maxvalue+step])
        ax2.set_yticks(np.arange(minvalue, maxvalue+step, step)[0:11])
    # 将图例置于当前坐标轴下
    fig.legend(loc='upper right', bbox_to_anchor=(1, 1.02),frameon=False, bbox_transform=ax.transAxes, ncol=(len(lnames)+len(rnames)))
    plt.subplots_adjust(bottom =0.15, left=0.06, right = 0.95, top=0.91)  
    # plt.show()
    return fig
    



def drawplot(resname):
    figs = []
    xvalue = datas['stage'].value
    xlabel = "Running Stage (200M Instructions)"
    # leftnames = ['fetch_buffer_1_8_perc','fetch_buffer_2_8_perc','fetch_buffer_3_8_perc','fetch_buffer_4_8_perc','fetch_buffer_5_8_perc','fetch_buffer_6_8_perc','fetch_buffer_7_8_perc','fetch_buffer_8_8_perc']
    # figs.append(draw_pecetage(xvalue, leftnames, [], xlabel, "Fetch Buffer Occupancy", ""))


        # 'ddbee18-M': 'block-8-TAGE-Fix',
        # 'ddbee18-L': 'block-16-TAGE-Fix',
        # '804474c-M': 'Base-8-TAGE',
        # '804474c-L': 'Base-16-TAGE',

    # 8 Bytes

    leftnames = ['com_misp_MPKI', 'com_br_MPKI', 'com_jalr_MPKI', 'com_br_ftb_entry_hit_MPKI', 'com_br_ftb_slot_hit_MPKI']
    figs.append(draw_lines(xvalue, leftnames, [], xlabel, "MPKI",""))

    pp = PdfPages(resname)
    idx = 0
    for fig in figs:
        print("save "+str(idx)+","+str(len(figs)))
        idx = idx + 1
        pp.savefig(fig, bbox_inches='tight', dpi=200)
    pp.close()



if len(sys.argv) < 2: 
    print("parameters are not enough.\n ./process.py logfile")
    exit()

if not os.path.exists(sys.argv[1]):
    print(sys.argv[1]+" is not exist.")
    exit()

readdata(sys.argv[1])
# 
(filepath,tempfilename) = os.path.split(sys.argv[1])
(filepref,filesuff) = os.path.splitext(tempfilename)
resname = filepath
if len(sys.argv) == 3:
    resname = sys.argv[2]
if not os.path.exists(resname):
    os.makedirs(resname)
resname = resname + "/" + filepref + ".pdf"
print("result pdf name: " + resname)
drawplot(resname)
