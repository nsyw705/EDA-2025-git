#!/usr/bin/env bash
set -euo pipefail

# 用法：
#   ./1_Compi_Code.sh [repeats] [seed_start] [workers]
# 默认：每个算例重复10次，种子从0开始，并行数=CPU核心数
REPEATS="${1:-10}"
SEED_START="${2:-0}"

# 计算并行数（第三个参数优先；否则用 nproc；再不行就 1）
if command -v nproc >/dev/null 2>&1; then
  DEFAULT_WORKERS="$(nproc)"
else
  DEFAULT_WORKERS="1"
fi
WORKERS="${3:-$DEFAULT_WORKERS}"

# 清理旧的任务脚本
rm -f subtsk* || true

# 生成并编译；按 REPEATS/SEED_START 生成 subtsk2.sh
python3 compile.py -l "${REPEATS}" -s "${SEED_START}"

# 启动任务（后台并行执行，日志见 output_all_task.log）
bash 2_Sub_Task.sh "${WORKERS}"
