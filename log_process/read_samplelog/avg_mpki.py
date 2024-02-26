import argparse
import csv
import os

import matplotlib.pyplot as plt
import matplotlib as mpl

if __name__ == "__main__":
    parser = argparse.ArgumentParser(
        description="calculate the average mpki for each log file")
    parser.add_argument("--input", nargs='+', help="The input file path")
    parser.add_argument("--benchmark", help="The benchmark name")
    parser.add_argument("--dir", help="the data directory")
    args = parser.parse_args()
    # filelist: the files whose name ends with .log in dir
    file_list = []
    if args.dir != None:
        for root, dirs, files in os.walk(args.dir):
            for file in files:
                if args.benchmark in file and '.log' in file:
                    file_list.append(os.path.join(root, file))
    else:
        file_list = args.input
    
    hash2config = {'d63a7e6a7b8556f519bb0f482728c7018542622e':'base',
    '9f8a97e32339da8ae24358b269b6072cdc438d9e':'6-1024-9',
    '948eab349b4f1fd15eb4251997f276ead74779a1':'6-512-9',
    '9b47a9ca03532efdac398e7bd214b436d9193267':'6-256-9',
    '55a30b5c4774c93430e40d1af0c8d2ea272f99e7':'6-128-9',
    '05f66321a2cee03ba3e5fd23b27221565cc2e68a':'6-1024-8',
    '64c0e5393ee2d17faa6f4021a86f0269f0fba8f9':'6-512-8',
    '632f580d5a696c77cf6c1a246e2b7f19f36a5cb4':'6-256-8',
    'df774d2d723cdae2050403b0d59372216927c4fd':'6-128-8',
    '125a459b7e2310e23478a4732c36b383f5b71a50':'6-1024-7',
    '5108f7e7e673eed5396932befa8777e05f296d2a':'6-512-7',
    '47045a86e26c416ee18ddfbadb385fff7fe58251':'6-256-7',
    '3ff5a552e15aba05d444ade657339caeb904d437':'6-128-7'
    }

    exe_misp_dict={}
    com_misp_dict={}

    for file in file_list: 
        # the input file is named as "bwaves-hash-cnt_eventinfo_h.csv"
        # get the hash part from the basename of the file
        #iterate through the lines of the file


        hash = os.path.basename(file).split('-')[1] 
        # get the corresponding configuration
        if hash not in hash2config:
            continue
        config = hash2config[hash]
        insts =0
        exe_misp_br =0
        com_misp_br =0
        exe_misp_jalr = 0
        com_misp_jalr = 0
        with open(file, 'r') as f:
            for line in f:
                # {"type": "event  1", "value": 200004107}
                if 'event 1' in line:
                    insts = insts + int(line.split(':')[2].split('}')[0])
                # event 43,   exe_misp_br
                elif 'event 43' in line:
                    exe_misp_br = exe_misp_br + int(line.split(':')[2].split('}')[0])
                # event 44,   exe_misp_jalr
                elif 'event 44' in line:
                    exe_misp_jalr = exe_misp_jalr + int(line.split(':')[2].split('}')[0])
                # event 53,   com_misp_br
                elif 'event 53' in line:
                    com_misp_br = com_misp_br + int(line.split(':')[2].split('}')[0])
                # event 54,   com_misp_jalr
                elif 'event 54' in line:
                    com_misp_jalr = com_misp_jalr + int(line.split(':')[2].split('}')[0])
        exe_misp = exe_misp_br + exe_misp_jalr
        com_misp = com_misp_br + com_misp_jalr

        exe_misp_dict[config] = exe_misp * 1000 / insts
        com_misp_dict[config] = com_misp * 1000 / insts

    
    config_list = ['base', '6-1024-9', '6-512-9', '6-256-9', '6-128-9', '6-1024-8', '6-512-8', '6-256-8', '6-128-8', '6-1024-7', '6-512-7', '6-256-7', '6-128-7']
    # exe_misp_list = []
    # fill the exe_misp_list with the values in exe_misp_dict
    exe_misp_list = [exe_misp_dict[config] for config in config_list]
    # com_misp_list = []
    com_misp_list = [com_misp_dict[config] for config in config_list]


    fig = plt.figure(figsize=(12,4))    # 设置画布大小

    plt.bar(config_list,exe_misp_list)
    plt.show()
    # save the image into the data dir as benchmark_exe_misp.png
    plt.savefig(os.path.join(args.dir, args.benchmark + '_exe_misp.png'))

    for i in range(len(config_list)):
        print(config_list[i], exe_misp_list[i])



    