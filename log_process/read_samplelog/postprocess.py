# Since we want to use multiple lines on one graph to present the evaluation data
# under different configurations.
# However, due to the experimental framework,
# the amount of data that can be obtained in each experiment is uncertain.
# To compare multiple polylines,
# we need to truncate redundant data points to align the experimental data.

import argparse
import csv
import os

if __name__ == "__main__":
    parser = argparse.ArgumentParser(
        description="Postprocess the data to align the data points")
    parser.add_argument("--input", nargs='+', help="The input file path")
    # specify the bechmark name
    parser.add_argument("--benchmark", help="The benchmark name")
    parser.add_argument("--dir", help="the data directory")
    args = parser.parse_args()

    # file list : the files whose name contains eventlist in dir
    file_list = []
    if args.dir != None:
        for root, dirs, files in os.walk(args.dir):
            for file in files:
                if args.benchmark in file and "eventinfo" in file and "csv" in file:
                    file_list.append(os.path.join(root, file))
    else:
        file_list = args.input

    path_prefix=""
    if args.dir != None:
        path_prefix = args.dir + "/"
    else:
        path_prefix = os.path.dirname(args.input[0]) + "/"

    #logname=$(realpath ./data/$benchname-$config-$logcnt.log)

    # open all the files in the input list, find out the minimum length of all the rows
    min_length = 100000000
    for file in file_list:
        with open(file, 'r') as f:
            reader = csv.reader(f)
            is_first_row = True
            for row in reader:
                if is_first_row:
                    is_first_row = False
                    continue
                nonempty_count = 0
                for col in row:
                    if col != '':
                        nonempty_count += 1
                if nonempty_count < min_length:
                    min_length = nonempty_count

    hash2config = {
    # 'd63a7e6a7b8556f519bb0f482728c7018542622e':'base',
    # 'b8cdcb02d0cb4efec580941a0a8a122c73ce3eee':'6-2048-9',
    '9f8a97e32339da8ae24358b269b6072cdc438d9e':'6-1024-9',
    # '948eab349b4f1fd15eb4251997f276ead74779a1':'6-512-9',
    # '9b47a9ca03532efdac398e7bd214b436d9193267':'6-256-9',
    # '55a30b5c4774c93430e40d1af0c8d2ea272f99e7':'6-128-9',
    # 'f6fde52c85604a6fb82ad081e90d4cfcb7efed4e':'6-2048-8',
    '05f66321a2cee03ba3e5fd23b27221565cc2e68a':'6-1024-8',
    # '64c0e5393ee2d17faa6f4021a86f0269f0fba8f9':'6-512-8',
    # '632f580d5a696c77cf6c1a246e2b7f19f36a5cb4':'6-256-8',
    # 'df774d2d723cdae2050403b0d59372216927c4fd':'6-128-8',
    # '1c4a65b09427c67f0ec82b25b14429ea385891b7':'6-2048-7',
    '125a459b7e2310e23478a4732c36b383f5b71a50':'6-1024-7',
    # '5108f7e7e673eed5396932befa8777e05f296d2a':'6-512-7',
    # '47045a86e26c416ee18ddfbadb385fff7fe58251':'6-256-7',
    # '3ff5a552e15aba05d444ade657339caeb904d437':'6-128-7'
    }

    whitelist=["exe_misp_MPKI","user_ipc" ,"tage_br_misp_rate" ,"tage_br_hit_rate" ,"tage_jalr_misp_rate" ,"tage_jalr_hit_rate" ,"tage_br_misp/hit"]
    
    #open all the files in the input list, truncate the rows to the minimum length
    #skip all the rows with the first column not containing 'tage_br'
    #add a prefix to the first column to indicate the configuration
    #write the data to a file named by the benchmark name
    with open(path_prefix+args.benchmark +'row=1024'+ '_post.csv', 'w', newline='') as f:
        writer = csv.writer(f)
        # write ,1,2,……,min_length to the first row
        header_row =[i for i in range(min_length)]
        header_row[0]=None
        writer.writerow(header_row)

        for file in file_list: 
            # the input file is named as "bwaves-hash-cnt_eventinfo_h.csv"
            # get the hash part from the basename of the file
            hash = os.path.basename(file).split('-')[1] 
            # get the corresponding configuration
            if hash not in hash2config:
                continue
            config = hash2config[hash]
            with open(file, 'r') as f1:
                reader = csv.reader(f1)
                for row in reader:
                    if row[0] not in whitelist:
                        continue
                    # add a prefix for row[0], which is the branch predictor name
                    row[0] = config + '_' + row[0]
                    writer.writerow(row[:min_length])

    # # open the output file ,print the non-empty column counts of each row        
    # with open(args.benchmark + '_post.csv', 'r') as f:
    #     reader = csv.reader(f)
    #     for row in reader:
    #         nonempty_count = 0
    #         for col in row:
    #             if col != '':
    #                 nonempty_count += 1
    #         print(nonempty_count)

