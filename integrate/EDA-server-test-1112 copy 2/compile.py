#!/usr/bin/env python3
import os
import sys
import shutil
import getopt

def safe_rmtree(p):
    if os.path.exists(p):
        shutil.rmtree(p)

def safe_mkdir(p):
    os.makedirs(p, exist_ok=True)

def main(argv):
    # 参数：-l repeats（每个算例重复次数），-s seed_start（初始随机种子）
    repeats = 10
    seed_start = 0

    try:
        opts, _ = getopt.getopt(argv, "l:s:h", ["limi=", "seed_start=", "help"])
    except getopt.GetoptError:
        print('Usage: compile.py -l <repeats> -s <seed_start>')
        sys.exit(2)

    for opt, arg in opts:
        if opt in ("-h", "--help"):
            print('Usage: compile.py -l <repeats> -s <seed_start>')
            sys.exit(0)
        elif opt in ("-l", "--limi"):
            repeats = int(arg)
        elif opt in ("-s", "--seed_start", "--seed-start"):
            seed_start = int(arg)

    # 准备目录
    safe_rmtree('output'); safe_mkdir('output')
    safe_rmtree('error');  safe_mkdir('error')
    safe_rmtree('Sol');    safe_mkdir('Sol')

    # 编译可执行文件
    exename = 'Execute'
    cpp_glob = "exe_cpp/*.cpp"
    cmd_compile = f'g++ {cpp_glob} -O3 -lm -Wall -o {exename}'
    ret = os.system(cmd_compile)
    if ret != 0:
        print("Compile failed. Please check your C++ sources under exe_cpp/")
        sys.exit(1)

    instancedir = 'instances'
    filesubmit = "subtsk2.sh"

    if not os.path.isdir(instancedir):
        print(f"Instances dir not found: {instancedir}")
        sys.exit(1)

    # 读取 instances/ 下的所有子文件夹作为 <case no.>（按名称排序）
    all_cases = [d for d in sorted(os.listdir(instancedir))
                 if os.path.isdir(os.path.join(instancedir, d))]

    if not all_cases:
        print("No case folders found under ./instances.")
        sys.exit(1)

    # 生成运行脚本
    # 关键点：在每条命令里 cd 到 instances，再以文件名形式传 4 个文件参数
    # 最终程序内部会打开的实际路径为：instances/<case>/design.xxx
    count = 0
    with open(filesubmit, 'w') as fw:
        fw.write('#!/usr/bin/env bash\nset -euo pipefail\n')
        for case in all_cases:
            # 存在性检查（用真实期望路径做检测，仅提示不影响执行）
            info_chk = os.path.join(instancedir, case, "design.info")
            net_chk  = os.path.join(instancedir, case, "design.net")
            topo_chk = os.path.join(instancedir, case, "design.topo")
            outp_chk = os.path.join(instancedir, case, "design.fpga.out")
            missing = [p for p in (info_chk, net_chk, topo_chk, outp_chk) if not os.path.isfile(p)]
            if missing:
                print(f"[WARN] Missing files for {case}: " + ", ".join(missing))

            for i in range(repeats):
                seed = seed_start + i
                # 在子进程中切到 instances，再运行 ../Execute
                cmd = (
                    f'(cd {instancedir} && '
                    f'../{exename} '
                    f'{case} "design.info" "design.net" "design.topo" "design.fpga.out" {seed} '
                    f'1>../output/{case}_{seed}.out 2>../error/{case}_{seed}.err)'
                )
                fw.write(cmd + '\n')
                count += 1

    os.chmod(filesubmit, 0o755)
    print(f"Generated {filesubmit} with {count} run(s) across {len(all_cases)} case(s).")

if __name__ == "__main__":
    main(sys.argv[1:])
