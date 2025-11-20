#!/usr/bin/env python
# -*- coding: utf-8 -*-
"""
convergence_analysis_template.py

本文件是一个“收敛性分析”模版脚本，用于读取算法运行时输出的采样文件，
并完成以下工作（可按需要开启/关闭）：

1. 读取一个或多个收敛采样文件（time, Global_delay, Best_delay）
2. 计算基本统计量（初始值、最终值、改善比例等）
3. 绘制收敛曲线（Global / Best 随时间变化）
4. 可选：对曲线做平滑（移动平均），便于观察趋势
5. 支持命令行参数，方便不同实验复用

使用示例：
    python convergence_analysis_template.py \
        --inputs run1_conv.txt run2_conv.txt \
        --output-dir ./figures \
        --ma-window 3 \
        --show

后续如需扩展（比如多 seed 聚合、与 baseline 对比），
建议在标注的“可扩展区域”添加代码。
"""

import argparse
from pathlib import Path
from typing import List, Optional

import numpy as np
import pandas as pd
import matplotlib.pyplot as plt

def apply_mpl_style(style: str = "ieee"):
    """
    设置 Matplotlib 风格，使图更接近论文风格。

    style 可选：
        - "ieee": 单栏 IEEE 样式，serif 字体，适合黑白打印
        - "acm": 颜色稍丰富，适合彩色 PDF
        - "mono": 完全黑白，靠线型区分曲线
    """
    base = {
        "font.family": "serif",
        "font.size": 9,
        "axes.labelsize": 8,
        "axes.titlesize": 8,
        "xtick.labelsize": 8,
        "ytick.labelsize": 8,
        "legend.fontsize": 5.5,
        "axes.linewidth": 0.8,
        "lines.linewidth": 1.2,
        "lines.markersize": 4,
        "figure.figsize": (4.0, 2.4),  # 单栏图尺寸 (width, height) in inches
        "figure.dpi": 600,
        "savefig.dpi": 600,
        "savefig.bbox": "tight",
    }

    if style == "ieee":
        # 稍偏黑白，颜色克制
        colors = [
            "#000000",  # black
            "#1f77b4",  # muted blue
            "#ff7f0e",  # orange
            "#2ca02c",  # green
            "#d62728",  # red
        ]
        base.update({
            "axes.prop_cycle": plt.cycler("color", colors),
        })

    elif style == "acm":
        # 彩色一点、同时兼顾可读性
        colors = [
            "#1f77b4",  # blue
            "#ff7f0e",  # orange
            "#2ca02c",  # green
            "#9467bd",  # purple
            "#8c564b",  # brown
        ]
        base.update({
            "axes.prop_cycle": plt.cycler("color", colors),
        })

    elif style == "mono":
        # 完全黑白，靠线型区分
        base.update({
            "axes.prop_cycle": plt.cycler("color", ["#000000"]),
        })
        # 线型我们在 plot_convergence 里已经用不同 linestyle 了

    plt.rcParams.update(base)

# ==============================
# 一、数据读取与预处理
# ==============================

def load_conv_file(path: Path,
                   has_header: bool = False) -> pd.DataFrame:
    """
    读取单个收敛采样文件，并返回 DataFrame，列为：
        ['time', 'global', 'best']

    参数
    ----
    path : Path
        收敛采样文件路径
    has_header : bool
        是否包含表头行。
        - 如果你的文件第一行是 "time global best"，设为 True 即可。
        - 默认 False，表示无表头。

    返回
    ----
    df : pd.DataFrame
        包含 time / global / best 三列的 DataFrame
    """
    if has_header:
        df = pd.read_csv(path, delim_whitespace=True)
        # 如果表头名字不是这三个，可以在这里重命名：
        # df = df.rename(columns={'Time(s)': 'time', 'Global': 'global', 'Best': 'best'})
    else:
        df = pd.read_csv(path,
                         delim_whitespace=True,
                         header=None,
                         names=['time', 'global', 'best'])

    # 按时间排序（以防文件中顺序不严格）
    df = df.sort_values('time').reset_index(drop=True)
    return df


def moving_average(series: pd.Series, window: int) -> pd.Series:
    """
    对一维序列做简单的移动平均，用于平滑收敛曲线。

    参数
    ----
    series : pd.Series
        需要平滑的序列（如 Global_delay 随时间变化）
    window : int
        移动窗口大小。window=1 表示不平滑（原始数据）。

    返回
    ----
    pd.Series
        平滑后的序列，与原序列长度相同（前面若干个点会是 NaN）。
    """
    if window <= 1:
        return series
    return series.rolling(window=window, min_periods=1,center=False).mean()


# ==============================
# 二、统计指标计算
# ==============================

def summarize_convergence(df: pd.DataFrame,
                          label: str = "") -> None:
    """
    打印单个实验的基本收敛统计信息。

    可根据需要在这里增加更多指标，比如：
    - 最佳值出现的时间
    - 收敛速度（达到某个阈值所需时间）
    - 早期提升比例 vs 后期提升比例 等

    参数
    ----
    df : pd.DataFrame
        包含 time/global/best 的数据
    label : str
        用于标记当前实验（比如文件名或 run_id）
    """
    t0 = df['time'].iloc[0]
    t_end = df['time'].iloc[-1]

    global_init = df['global'].iloc[0]
    global_final = df['global'].iloc[-1]

    best_init = df['best'].iloc[0]
    best_final = df['best'].iloc[-1]

    # 这里假设“越小越好”（delay）。如果是相反，可以自行调整公式。
    global_improve_abs = global_init - global_final
    best_improve_abs = best_init - best_final

    global_improve_pct = global_improve_abs / global_init * 100 if global_init != 0 else np.nan
    best_improve_pct = best_improve_abs / best_init * 100 if best_init != 0 else np.nan

    print("=" * 60)
    print(f"[Convergence Summary] {label}")
    print(f"  Time range   : {t0:.3f} s  ->  {t_end:.3f} s")
    print(f"  Global delay : {global_init:.4f}  ->  {global_final:.4f} "
          f" (Δ={global_improve_abs:.4f}, {global_improve_pct:.2f}%)")
    print(f"  Best delay   : {best_init:.4f}  ->  {best_final:.4f} "
          f" (Δ={best_improve_abs:.4f}, {best_improve_pct:.2f}%)")

    # 可扩展区域：例如输出“最佳值第一次出现的时间”
    # idx_best = df['best'].idxmin()
    # print(f"  Best achieved at t = {df['time'].iloc[idx_best]:.3f} s")


# ==============================
# 三、绘图函数
# ==============================

def plot_convergence(df_list: List[pd.DataFrame],
                     labels: List[str],
                     output_dir: Path,
                     out_prefix: str,
                     ma_window: int = 1,
                     show: bool = False,
                     tmax: float | None = None,
                    legend_loc: str = "best",
                    clip_first: bool = False,
                     clip_ratio: float = 10.0) -> None:
    """
    绘制收敛曲线图。支持多条曲线（例如多个 seed / 多个实验文件）。

    目前的默认行为：
    - 每个输入文件画两条线：Global_delay 和 Best_delay
    - 不做多文件聚合，只是放在同一张图上对比

    你可以在“可扩展区域”增加：
    - 多文件的平均曲线 / 置信区间
    - 与 baseline 的对比曲线等

    参数
    ----
    df_list : List[pd.DataFrame]
        多个实验的数据列表
    labels : List[str]
        每个数据对应的标签（用于图例）
    output_dir : Path
        图像输出目录
    out_prefix : str
        输出文件名前缀，例如 "case1_seed1"
    ma_window : int
        移动平均窗口大小（平滑用）。1 表示不平滑。
    show : bool
        是否在绘制后显示图像（plt.show()）。适合交互式调试。
    """
    output_dir.mkdir(parents=True, exist_ok=True)

    fig, ax = plt.subplots()        # 按照创建顺序循环使用不同线型/marker
    line_styles = ["-", "--", "-.", ":",(0, (5, 1, 1, 1)), (0, (3, 1, 1, 1))]
    # markers = ["o", "s", "D", "^", "v", "x", "*", "P"]
    line_idx = 0  # 全局计数器：每画一条线 +1
    # 先收集处理好的曲线，方便后面统一画 & 统一控制坐标轴
    curves = []
    clipped_initials = []   # 记录被剪掉的首点值，方便下面写注释

    for df, label in zip(df_list, labels):
        # 如果设置了 tmax，只保留 time <= tmax 的部分用于绘图
        if tmax is not None:
            df = df[df["time"] <= tmax].copy()
            if df.empty:
                print(f"[WARN] {label}: no points with time <= {tmax}, skip in plot.")
                continue

        time = df["time"].to_numpy()
        global_series = df["global"]
        global_smooth = moving_average(global_series, ma_window).to_numpy()

        if len(global_smooth) == 0:
            continue

        # 判断是否要剪掉首点
        if clip_first and len(global_smooth) >= 2:
            g0, g1 = global_smooth[0], global_smooth[1]
            if g1 != 0 and g0 > clip_ratio * g1:
                # 认为首点是“异常大”的起点，不画出来，只记录下来
                clipped_initials.append(g0)
                time_plot = time[1:]
                g_plot = global_smooth[1:]
            else:
                time_plot = time
                g_plot = global_smooth
        else:
            time_plot = time
            g_plot = global_smooth

        if len(g_plot) == 0:
            continue

        curves.append((time_plot, g_plot, label))

    if not curves:
        print("[WARN] No data to plot.")
        return

    # 统一画图
    for time_plot, g_plot, label in curves:
        style_g = line_styles[line_idx % len(line_styles)]
        line_idx += 1
        plt.plot(
            time_plot,
            g_plot,
            linestyle=style_g,
            label=f"{label}",
        )

    if tmax is not None:
        plt.xlim(0, tmax)
    ax.set_xlabel("Time (s)")
    ax.set_ylabel("Delay")
    ax.tick_params(
        direction="in",
        length=3.5,
        width=0.6,
        top=False,
        right=False,
    )
    # plt.title("Convergence Curve (Global & Best vs Time)")
    # plt.title("Convergence Curve ")
    # 如果剪掉了首点，在图里说明一下
    if clip_first and clipped_initials:
        init_vals = np.array(clipped_initials)
        init_min = init_vals.min()
        init_max = init_vals.max()
        if np.allclose(init_min, init_max, rtol=1e-3):
            text = f"Initial delay ≈ {init_min:.2e} (same for all curves,not shown for readability)"
        else:
            text = (f"Initial delays ≈ [{init_min:.2e}, {init_max:.2e}] "
                    f"(not shown for readability)")
        ax = plt.gca()
        ax.text(
            0.01, 0.99, text,
            transform=ax.transAxes,
            ha="left", va="top",
            fontsize=8,
        )
    ax.legend(
    loc=legend_loc,
    # bbox_to_anchor=(0.5, 1.15),  # 稍微靠近图一点
    ncol=2,                      # 3 列
    frameon=False,
    # fontsize=4,                  # 图例文字更小
    handlelength=1.2,            # 图例线段短一点
    handletextpad=0.4,           # 线和文字的间距小一点
    columnspacing=0.8,           # 列与列之间更紧凑
    # borderpad=0.2,               # 图例边缘留白更小
    markerscale=0.8,             # 如果有 marker，也一起缩小
)

    # plt.legend(loc=legend_loc,frameon=False)
    # plt.grid(True, linestyle=':')

    # 输出文件名：<prefix>_conv.png
    out_path = output_dir / f"{out_prefix}_conv.svg"
    fig.tight_layout()
    fig.savefig(out_path, dpi=640)
    print(f"[INFO] Convergence figure saved to: {out_path}")

    if show:
        plt.show()
    else:
        plt.close()

    # ============================
    # 可扩展区域：多文件聚合分析
    # ============================
    # 例如：
    #   - 对不同种子、多次运行的 Best 曲线做插值到统一时间轴，
    #     然后计算平均值 ± 标准差，并画带阴影的区域。
    #   - 或者对 global_final 进行箱线图统计等。
    #
    # 由于不同实验的 time 采样点未必对齐，如果要做聚合，
    # 可以：
    #   1. 选定一个统一时间轴（如 np.linspace(0, Tmax, Npoints)）
    #   2. 对每条曲线用 np.interp 插值到该时间轴
    #   3. 再对插值后的矩阵按列求 mean / std
    # 相关代码可根据需要自行补充。


# ==============================
# 四、命令行接口
# ==============================

def parse_args() -> argparse.Namespace:
    """
    命令行参数解析。

    常用参数说明：
    --inputs:    一个或多个收敛采样文件路径
    --output-dir:输出图像和中间结果的目录
    --prefix:   输出文件名前缀（比如案例名）
    --ma-window:移动平均窗口大小（平滑用）
    --has-header:采样文件是否包含表头行
    --show:     是否弹出图像窗口显示
    """


    parser = argparse.ArgumentParser(
        description="收敛性分析模版脚本（基于 time / Global_delay / Best_delay）。"
    )
    parser.add_argument(
        "--style",
        type=str,
        default="ieee",
        choices=["ieee", "acm", "mono"],
        help="绘图风格预设：ieee / acm / mono（默认 ieee）"
    )
    parser.add_argument(
        "--inputs",
        type=str,
        nargs="+",
        required=True,
        help="收敛采样文件路径列表，例如: run1_conv.txt run2_conv.txt"
    )
    parser.add_argument(
        "--names",
        type=str,
        nargs="+",
        help="每个输入文件对应的一条线的名字，数量应与 --inputs 一致，如：--names seed1 seed2 ..."
    )
    parser.add_argument(
        "--output-dir",
        type=str,
        default="./conv_figures",
        help="图像输出目录（默认: ./conv_figures）"
    )
    parser.add_argument(
        "--prefix",
        type=str,
        default="case",
        help="输出文件名前缀（例如案例名），默认: case"
    )
    parser.add_argument(
        "--ma-window",
        type=int,
        default=1,
        help="移动平均窗口大小，用于平滑曲线（默认: 1，表示不平滑）"
    )
    parser.add_argument(
        "--has-header",
        action="store_true",
        help="如果采样文件包含表头行（如 time global best），加上此参数。"
    )
    parser.add_argument(
        "--show",
        action="store_true",
        help="是否在绘制后弹出图像窗口显示（默认不显示，只保存）。"
    )
    parser.add_argument(
        "--tmax",
        type=float,
        default=None,
        help="只绘制 time <= tmax 的部分，比如 --tmax 100 表示最多画到 100s"
    )
    parser.add_argument(
        "--legend-loc",
        type=str,
        default="best",
        help="图例位置，如 'best', 'upper right', 'upper left', 'lower left', 'lower right' 等"
    )
    parser.add_argument(
        "--clip-first",
        action="store_true",
        help="当首个点远大于第二个点时，将其从图中去掉，并在图内文字说明初始延迟"
    )
    parser.add_argument(
        "--clip-ratio",
        type=float,
        default=10.0,
        help="触发 --clip-first 的阈值：如果第一个点 > ratio × 第二个点，则视为异常大起点"
    )


    return parser.parse_args()


# ==============================
# 五、主流程入口
# ==============================

def main():
    args = parse_args()
    apply_mpl_style(args.style)

    input_paths = [Path(p) for p in args.inputs]
    output_dir = Path(args.output_dir)

    df_list: List[pd.DataFrame] = []
    labels: List[str] = []

   
    # 简单检查一下数量是否匹配
    if args.names is not None and len(args.names) != len(input_paths):
        print("[WARN] --names 的个数与 --inputs 不一致，将对多出来的部分使用文件名作为标签。")

    for idx, path in enumerate(input_paths):
        if not path.exists():
            print(f"[WARN] File not found, skip: {path}")
            continue

        df = load_conv_file(path, has_header=args.has_header)
        df_list.append(df)

        # 有自定义名字就用自定义的，没有就用文件名
        if args.names is not None and idx < len(args.names):
            labels.append(args.names[idx])
        else:
            labels.append(path.stem)

        summarize_convergence(df, label=path.name)


    if not df_list:
        print("[ERROR] No valid input files. Exit.")
        return

    plot_convergence(df_list=df_list,
                     labels=labels,
                     output_dir=output_dir,
                     out_prefix=args.prefix,
                     ma_window=args.ma_window,
                     show=args.show,
                     tmax=args.tmax,
                     legend_loc=args.legend_loc, clip_first=args.clip_first,
        clip_ratio=args.clip_ratio,)


if __name__ == "__main__":
    main()
