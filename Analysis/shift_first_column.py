#!/usr/bin/env python
# -*- coding: utf-8 -*-

import sys
import numpy as np

def main():
    if len(sys.argv) != 2:
        print(f"用法: python {sys.argv[0]} 输入文件 输出文件")
        sys.exit(1)

    in_path = sys.argv[1]
    out_path = sys.argv[1]

    # 读入数据（默认以空格分隔）
    data = np.loadtxt(in_path)

    # 第一列整体减去第一行的第一列值
    data[:, 0] = data[:, 0] - data[0, 0]

    # 保存结果，保持 6 位小数，空格分隔
    np.savetxt(out_path, data, fmt="%.6f", delimiter=" ")

if __name__ == "__main__":
    main()
