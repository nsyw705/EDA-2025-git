#!/usr/bin/env bash
set -euo pipefail

# 并行进程数（从 1_Compi_Code.sh 传入；未传则兜底）
if command -v nproc >/dev/null 2>&1; then
  DEFAULT_WORKERS="$(nproc)"
else
  DEFAULT_WORKERS="1"
fi
WORKERS="${1:-$DEFAULT_WORKERS}"

LOG="./output_all_task.log"
: > "$LOG"   # 清空旧日志

# 使用 submit_multiprocess.py 并行调度 subtsk2.sh 中的每一条命令
nohup python3 submit_multiprocess.py -c "${WORKERS}" >> "$LOG" 2>&1 &
echo "Launched submit_multiprocess.py with ${WORKERS} workers. Logs -> ${LOG}"
