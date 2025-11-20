#!/usr/bin/python
# -*- coding: utf-8 -*-

import os
import math
import cmath
import sys
import getopt
import shutil


def usage():
    print("""'-p'+number of rotation degrees 
for example: -p 10""")


dict1 = {"case01": 0, "case02": 0, "case03": 0, "case04": 0}

num_file = 10  # Time of execution (seed)


def copyFiles(src, dest):
    src_files = os.listdir(src)
    for file_name in src_files:
        full_file_name = os.path.join(src, file_name)
        if os.path.isfile(full_file_name):
            shutil.copy(full_file_name, dest)


def readfile(name_file, method):
    list_cbmp = []
    list_t = []
    list_iters = []  # To store iteration counts for each file
    for i in range(num_file):  # Time of execution (seed)
        cbmp = None  # 初始化为None，确保最后一行有效数据会覆盖
        t = 0.0
        iters = 0  # Iteration counter
        str3 = "%s" % i
        filename = name_file + "_" + str3  # Add underscore between file name and seed
        if not os.path.isfile(filename):  # Check if the file exists before processing
            print(f"[WARN] File not found: {filename}")
            continue
        with open(filename, 'r') as infile:  # 使用with语句更安全
            data = infile.readlines()
            # 遍历所有行，保留最后一行有效数据
            for line in data:
                str_line = line.split()
                if len(str_line) < 4:  # Ensure there are enough columns (at least 4 columns)
                    continue
                try:
                    # 直接更新为当前行数据（最后一行会覆盖前面的）
                    iters = int(str_line[0])  # First column: iterations
                    line_float = float(str_line[1])  # Second column: result
                    t = float(str_line[2])  # Third column: time
                    cbmp = line_float  # 保留当前行的结果（最后一行会成为最终值）
                except ValueError as e:
                    print(f"[ERROR] Invalid data in file {filename}: {line} (Error: {e})")
                    continue
        if cbmp is not None:  # 只添加有效的数据
            list_cbmp.append(cbmp)
            list_t.append(t)
            list_iters.append(iters)

    return (list_cbmp, list_t, list_iters)


def search_best(list_in):
    if not list_in:  # If list_in is empty, return 0
        return 0
    # 初始化最好值为列表第一个元素（改为找最小值）
    best = list_in[0]
    for val in list_in:
        if val < best:  # 比较逻辑从>改为<
            best = val
    return best


def num_best(list_in, b):
    co = 0
    for val in list_in:  # 简化循环方式
        if val == b:
            co += 1
    return co


def cal_avg_time(list_in, list_in2, b):
    sum_up = 0.0
    cou = 0
    for i in range(len(list_in)):  # 使用len(list_in)适配实际长度
        if list_in[i] == b:
            sum_up += list_in2[i]
            cou += 1
    avg = sum_up / cou if cou > 0 else 0.0  # Avoid division by zero
    return avg


def cal_avg(list_in):
    if not list_in:  # 处理空列表
        return 0.0
    sum_up = sum(list_in)  # 简化求和方式
    avg = sum_up / len(list_in)
    return avg


def cal_dev(list_in, avg):
    if not list_in:
        return 0.0
    temp_sum = sum(math.pow(val - avg, 2) for val in list_in)
    dev = math.sqrt(temp_sum / len(list_in))  # 用math.sqrt更合适（实数）
    return dev


def cal_RR(list_in, avg):
    if not list_in or avg == 0:  # 避免除零
        return 0.0
    temp_sum = sum(math.pow((val - avg) / avg, 2) for val in list_in)
    RR = math.sqrt(temp_sum / len(list_in))
    return RR


def verify_avancer(old, new):
    return (new - old)


try:
    opt, args = getopt.getopt(sys.argv[1:], 'p:')
    for name, deg in opt:
        if name in ('-p'):
            print(deg)
except getopt.GetoptError:
    usage()
    sys.exit(2)

outfile = "Result.txt"
infile = "../instance_name.txt"
linked = "_"
linename = ""
rootdir = "../Sol/"
naf = ""
str_met0 = "0"
str_met1 = "1"
str_met2 = "2"
str_met3 = "3"
num_instance = 1

# 确保summary目录操作安全
if os.path.exists('summary'):
    shutil.rmtree('summary')
os.mkdir('summary')
os.chdir('summary')

# Generate the file of syntax
with open(outfile, 'w') as of:  # 使用with语句
    with open(infile) as seo:
        data = seo.read().rstrip().splitlines()
        for f in data:
            lc = []
            lt = []
            naf = rootdir + f
            (lc, lt, li) = readfile(naf, str_met2)

            if not lc:  # Skip empty lists
                print(f"[WARN] No data found for case: {f}")
                continue

            cbmp_best = search_best(lc)  # 此时获取的是最小值
            num_b = num_best(lc, cbmp_best)
            cbmp_avg = cal_avg(lc)
            time_avg = cal_avg_time(lc, lt, cbmp_best)
            cbmp_dev = cal_dev(lc, cbmp_avg)
            flag = verify_avancer(dict1[f], cbmp_best)
            iter_avg = cal_avg_time(lc, li, cbmp_best)  # Average iterations for best results

            linename = f
            of.write("{0:<30} {1:^10.4f} {2:^10.4f} {3:^10.4f} {4:^10.4f} {5:^10} {6:^10} {7:^10.2f}\n".format(
                linename, cbmp_best, cbmp_avg, time_avg, cbmp_dev, flag, num_b, iter_avg))