import csv
import os
import sys

os.chdir('summary')

# 删除旧的CSV文件（如果存在）
for f in ['R0.csv', 'R1.csv', 'R2.csv', 'R3.csv', 'R.csv']:
    if os.path.exists(f):
        os.remove(f)

infile = "Result.txt"
outfile = "R.csv"

def writecsv(infile, outfile):
    with open(outfile, 'w', newline='') as csvfile:
        writer = csv.writer(csvfile)
        with open(infile, 'r') as fi:
            for line in fi:
                line = line.strip()
                if line:
                    # 按空格拆分并写入CSV
                    writer.writerow(line.split())

# 调用函数生成CSV
writecsv(infile, outfile)