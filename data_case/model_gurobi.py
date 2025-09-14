from  gurobipy import *

def read_data(file_path):
    info_path = f"{file_path}/design.info"
    net_path = f"{file_path}/design.net"
    topo_path = f"{file_path}/design.topo"
    fpga_out_path = f"{file_path}/design.fpga.out"
    design_info = {}
    design_net = []
    design_topo = {}
    design_fpga_out = {}
    fpga_out_list = []  
    # 读取 design.info
    try:
        with open(info_path, 'r') as f:
           for line in f:
                line = line.strip()
                if line:
                    fpga, max_channels = line.split()
                    design_info[fpga] = int(max_channels)
        # 这里简单将内容按行存储，实际可根据需求解析，比如提取每个FPGA节点的最大对外连接通道数量等
    except Exception as e:
        print(f"读取 design.info 出错: {e}")
    # 转换为有序列表（方案2）
    if design_info:
        sorted_fpgas = sorted(design_info.keys(), key=lambda x: int(x[1:]))
        info_list = [design_info[fpga] for fpga in sorted_fpgas]
    # 读取 design.net
    try:
        with open(net_path, 'r') as f:
            for line in f:
                line = line.strip()
                if line:
                    parts = line.split()
                    source_node = parts[0]
                    weight = int(parts[1])
                    target_nodes = parts[2:]
                    design_net.append((source_node, weight, target_nodes))
    except Exception as e:
        print(f"读取 design.net 出错: {e}")
    # 读取 design.topo
    try:
        with open(topo_path, 'r') as f:
        # 按行存储FPGA之间的物理连接关系信息
            for line in f:
                    line = line.strip()
                    if line:
                        fpga_part, vec_part = line.split(':')
                        fpga = fpga_part.strip()
                        conn_vec = [int(x.strip()) for x in vec_part.split(',')]
                        design_topo[fpga] = conn_vec
    except Exception as e:
        print(f"读取 design.topo 出错: {e}")
     # 转换为有序列表（方案1）
    if design_topo:
        sorted_fpgas = sorted(design_topo.keys(), key=lambda x: int(x[1:]))
        topo_list = [[fpga, design_topo[fpga]] for fpga in sorted_fpgas]
    # 读取 design.fpga.out
    try:
        with open(fpga_out_path, 'r') as f:
        # 按行存储逻辑节点到物理FPGA的映射结果
            for line in f:
                    line = line.strip()
                    if line:
                        fpga_part, nodes_part = line.split(':')                                    
                        fpga = fpga_part.strip()
                        logic_nodes = [node.strip() for node in nodes_part.split()]
                        design_fpga_out[fpga] = logic_nodes
    except Exception as e:
        print(f"读取 design.fpga.out 出错: {e}")
    if design_fpga_out:
        # 1. 提取所有FPGA名，并按「数字部分」排序（避免F10排在F2前面）
        # 排序逻辑：取FPGA名中"F"后的数字（如"F3"→3），按数字升序排列
        sorted_fpgas = sorted(
            design_fpga_out.keys(),
            key=lambda x: int(x[1:])  # x是FPGA名（如"F1"），x[1:]取"1"并转整数
        )
        
        # 2. 按排序后的FPGA顺序，生成列表（每行对应一个FPGA的逻辑节点）
        fpga_out_list = [design_fpga_out[fpga] for fpga in sorted_fpgas]

    return design_info, info_list, design_net, design_topo, topo_list, design_fpga_out, fpga_out_list

try:
    file_path='./case02' 
    design_info, info_list, design_net, design_topo, topo_list, design_fpga_out, fpga_out_list = read_data(file_path)
    nums_fpga = len(design_info)
    nums_net=len(design_net)
    # print("Number of nets:", nums_net)
    # print("Topo List:", topo_list)
    # print("Info List:", info_list)
    max_channel = max(design_info.values()) #Max channels per FPGA
    M=2*max(info_list) #大M
    N=M
    W_total=sum(topo_list[i][1][j] for i in range(nums_fpga) for j in range(i+1,nums_fpga) if topo_list[i][1][j]>0)#当前总通道数
    print("Total channels:", W_total)

    # design.net处理
    virtual_points = set()
    virtual_points_start = set()
    virtual_edges = set()
    net_edges = []
    for entry in design_net:
        i = entry[0]  # 起点
        edges = [(i, j) for j in entry[2]]
        net_edges.append(edges)  # 记录net中的边
        virtual_points.add(i)
        for j in entry[2]:  # 终点
            virtual_points.add(j)
            virtual_edges.add((i, j))  # 记录存在(i, j)
    virtual_points = list(virtual_points)
    virtual_edges = list(virtual_edges)
    net_edges = list(net_edges)
    # print("Virtual Points:", virtual_points)
    # print("Net Edges:", net_edges)
# 变量
    model=Model("FPGA")
    model.Params.NonConvex = 2
    model.Params.TimeLimit = 3600  # 设置时间限制为300秒
    df_change=model.addVars(nums_fpga, nums_fpga, vtype=GRB.INTEGER, name="df_change",lb=-max_channel,ub=max_channel) # FPGA之间的连接变化量
    Ue = model.addVars(nums_fpga, nums_fpga, vtype=GRB.BINARY, name="Ue") #通道调整后，F_p与F_q是否连接,即无向边e ={p,q}是否存在
    # 对称性约束df_change
    for p in range(nums_fpga):
        model.addConstr(df_change[p, p] == 0, name=f"df_sym_{p}_{p}")
        for q in range(p + 1, nums_fpga):
            model.addConstr(df_change[p, q] == df_change[q, p], name=f"df_sym_{p}_{q}")
    # 对称性约束Ue
    for p in range(nums_fpga):
        model.addConstr(Ue[p, p] == 0, name=f"Ue_sym_{p}_{p}")
        for q in range(p + 1, nums_fpga):
            model.addConstr(Ue[p, q] == Ue[q, p], name=f"Ue_sym_{p}_{q}")
    W_e = model.addVars(nums_fpga, nums_fpga, vtype=GRB.INTEGER, name="W_e", lb=0, ub=max_channel)  # 调整后，𝐹𝑝与𝐹𝑞之间的连接通道数量
    # 对称性约束W_e
    for p in range(nums_fpga):
        model.addConstr(W_e[p, p] == 0, name=f"We_sym_{p}_{p}")
        for q in range(p + 1, nums_fpga):
            model.addConstr(W_e[p, q] == W_e[q, p], name=f"We_sym_{p}_{q}")
    # X=model.addVars([(net_id, edge) for net_id in range(nums_net) for edge in net_edges[net_id]],nums_fpga,nums_fpga,vtype=GRB.BINARY,name="X") #对n_i的源端s_i和接收端t_{ij}，是否选有向弧(p,q)（选的是{\ n}_i\ 中的弧，1为选）
    X = model.addVars(
    (
        (net_id, u, v, p, q)
        for net_id in range(nums_net)
        for (u, v) in net_edges[net_id]
        for p in range(nums_fpga)
        for q in range(nums_fpga)
    ),
    vtype=GRB.BINARY,
    name="X")
    for net_id in range(nums_net):
        for (i, j) in net_edges[net_id]:  # 遍历所有虚拟边
            for p in range(nums_fpga):
                model.addConstr(
                    X[net_id, i, j, p, p] == 0,
                    name=f"sym_X_{net_id}_{i}_{j}_{p}_{p}"  # 约束名称，便于调试
                )
    # Y=model.addVars([(net_id, edge) for net_id in range(nums_net) for edge in net_edges[net_id]],nums_fpga,nums_fpga,vtype=GRB.BINARY,name="Y") #对n_i的源端s_i和接收端t_{ij}，是否选无向边e ={p,q}（选的是拓扑图G的边，1为选）
    Y = model.addVars(
        (
            (net_id, u, v, p, q)
            for net_id in range(nums_net)
            for (u, v) in net_edges[net_id]
            for p in range(nums_fpga)
            for q in range(nums_fpga)
        ),
        vtype=GRB.BINARY,
        name="Y"
    )
    # 对称性约束Y 
    for net_id in range(nums_net):
        for (i, j) in net_edges[net_id]:  # 遍历所有虚拟边
            for p in range(nums_fpga):
                for q in range(p + 1, nums_fpga):  # 仅处理p < q，避免重复约束
                    # 约束：FPGA对(p,q)与(q,p)的映射关系等价
                    model.addConstr(
                        Y[net_id, i,j, p, q] == Y[net_id, i,j, q, p],
                        name=f"sym_Y_{net_id}_{i}_{j}_{p}_{q}"  # 约束名称，便于调试
                    )
    Z_e = model.addVars(nums_net, nums_fpga, nums_fpga, vtype=GRB.BINARY, name="Z_e")  # n_i的源端s_i是否至少一次选无向边e（1为选）
    # 对称性约束Z_e
    for i in range(nums_net):
        for p in range(nums_fpga):
            for q in range(p + 1, nums_fpga):  # 仅处理p < q，避免重复约束
                # 约束：FPGA对(p,q)与(q,p)的映射关系等价
                model.addConstr(
                    Z_e[i, p, q] == Z_e[i, q, p],
                    name=f"sym_Ze_{i}_{p}_{q}"  # 约束名称，便于调试
                ) 
# 辅助变量 
    J_ij=model.addVars([(net_id, edge) for net_id in range(nums_net) for edge in net_edges[net_id]],vtype=GRB.INTEGER,name="J_ij",lb=0,ub=nums_fpga) #n_i从源端s_i到接收端t_{ij}跳跨的FPGA总数，用于计算布线延迟
    G_ij=model.addVars(nums_net,nums_fpga,nums_fpga,vtype=GRB.BINARY,name="G_ij") #斯坦纳树骨架，即n_i对应的斯坦纳树是否使用有向弧(p,q)
    H_i=model.addVars(nums_net,nums_fpga,vtype=GRB.INTEGER,name="H_i",lb=0) #n_i的源端s_i到接收端t_{ij}跳跨的FPGA总数，用于计算布线延迟
# 目标函数相关变量
    r_pq = model.addVars(nums_fpga, nums_fpga, vtype=GRB.INTEGER, name="r_pq", lb=0)  # FPGA p 和 FPGA q 的TDM 比率
    N_pq = model.addVars(nums_fpga, nums_fpga, vtype=GRB.INTEGER, name="N_pq", lb=0)
    D_i= model.addVars(nums_net, vtype=GRB.CONTINUOUS, name="D_i", lb=0)  # n_i的最大布线延迟

# 约束条件
    # net:出入流平衡约束
    for net_id in range(nums_net):
        for (i,j) in net_edges[net_id]:
            is_same_fpga = False
            #  判断是否在同一FPGA上
            for p in range(nums_fpga):
                if i in fpga_out_list[p] and j in fpga_out_list[p]:
                    is_same_fpga = True
                    print(f"Net {net_id} edge ({i},{j}) is on the same FPGA F{p+1}")
                    break
            if is_same_fpga:
                # 同一FPGA上，直接跳过该边的流平衡约束
                model.addConstr(quicksum(X[net_id,i,j,a,b] for a in range(nums_fpga) for b in range(nums_fpga)) == 0,
                name=f"same_fpga_zerohop_{net_id}_{i}_{j}")
            else:
                for p in range(nums_fpga):
                    #     # 特殊情况：源端和接受端在同一FPGA上  //暂时不考虑
                    #     if i in fpga_out_list[p] and j in fpga_out_list[p]:
                    #         model.addConstr(quicksum(X[net_id,i,j,a,b] for a in range(nums_fpga) for b in range(nums_fpga)) == 0,
                    # name=f"same_fpga_zerohop_{net_id}_{i}_{j}_{p}")
                    #         break
                        # 源端si对应的Fp
                        if i in fpga_out_list[p] :
                            model.addConstr(quicksum(X[net_id, i, j, p, q] for q in range(nums_fpga)) == 1, name=f"source_out0_{net_id}_{i}_{j}_{p}")
                            model.addConstr(quicksum(X[net_id, i, j, q, p] for q in range(nums_fpga)) == 0, name=f"source_out1_{net_id}_{i}_{j}_{p}")
                        # 接收端tij对应的Fq
                        elif j in fpga_out_list[p] :
                            model.addConstr(quicksum(X[net_id, i, j, q, p] for q in range(nums_fpga))==1, name=f"target_in0_{net_id}_{i}_{j}_{p}")
                            model.addConstr(quicksum(X[net_id, i, j, p, q] for q in range(nums_fpga))==0, name=f"target_in1_{net_id}_{i}_{j}_{p}")
                        # 中间节点Fk
                        else:
                            model.addConstr(quicksum(X[net_id, i, j, q, p] for q in range(nums_fpga))==quicksum(X[net_id, i, j, p, q] for q in range(nums_fpga)), name=f"inter_{net_id}_{i}_{j}_{p}")
        # 有向图无向图对称约束
    for net_id in range(nums_net):
        for (i,j) in net_edges[net_id]:
            for p in range(nums_fpga):
                for q in range(nums_fpga):
                    model.addConstr(X[net_id, i,j, p, q] <= Y[net_id, i,j, p, q], name=f"direct_undirect_{net_id}_{i}_{j}_{p}_{q}")   
                    model.addConstr(X[net_id, i,j, q, p] <= Y[net_id, i,j, q, p], name=f"direct_undirect_{net_id}_{i}_{j}_{q}_{p}") 
                    model.addConstr(X[net_id, i,j, p, q]+X[net_id, i,j, q, p]>=Y[net_id, i,j, p, q], name=f"direct_undirect2_{net_id}_{i}_{j}_{p}_{q}")
                    model.addConstr(X[net_id, i,j, p, q]<=Ue[p,q], name=f"Ue_limit_{net_id}_{i}_{j}_{p}_{q}")
                    model.addConstr(X[net_id, i,j, q, p]<=Ue[q,p], name=f"Ue_limit_{net_id}_{i}_{j}_{q}_{p}")
    # 每个net边只计算一次约束
    for net_id in range(nums_net):
        for (i,j) in net_edges[net_id]:
            for p in range(nums_fpga):
                for q in range(p, nums_fpga):
                    model.addConstr(Y[net_id, i,j, p, q] <= Z_e[net_id, p, q], name=f"Ze_limit_{net_id}_{i}_{j}_{p}_{q}")
    for net_id in range(nums_net):
        for p in range(nums_fpga):
            for q in range(p, nums_fpga):
                model.addConstr(Z_e[net_id, p, q] <= quicksum(Y[net_id, i, j, p, q] for (i, j) in net_edges[net_id]), name=f"Ze_limit2_{net_id}_{p}_{q}")
    # 子环消除约束
    for net_id in range(nums_net):
        for (i,j) in net_edges[net_id]:
            for p in range(nums_fpga):
                for q in range(p, nums_fpga):
                    model.addConstr(X[net_id,i,j,p,q]<=G_ij[net_id,p,q], name=f"Gij_limit_{net_id}_{i}_{j}_{p}_{q}")
    for net_id in range(nums_net):
            for p in range(nums_fpga):
                for q in range(nums_fpga):
                    model.addConstr(G_ij[net_id,p,q]<=quicksum(X[net_id,i,j,p,q] for (i,j) in net_edges[net_id]), name=f"Gij_limit2_{net_id}_{p}_{q}")
                    model.addConstr(H_i[net_id,q]>=H_i[net_id,p]+1-M*(1-G_ij[net_id,p,q]), name=f"Hij_limit_{net_id}_{p}_{q}")
                    model.addConstr(H_i[net_id,p]>=0, name=f"Hij_limit2_{net_id}_{p}_{q}")
                    model.addConstr(H_i[net_id,q]<=N, name=f"Hij_limit3_{net_id}_{p}_{q}")
                    if net_edges[net_id][0][0] in fpga_out_list[p]: #源点
                        model.addConstr(H_i[net_id,p]==0, name=f"Hij_source_{net_id}_{p}")
    for net_id in range(nums_net):
            for q in range(nums_fpga):
                    if  net_edges[net_id][0][0] not in fpga_out_list[q]:
                        model.addConstr(G_ij.sum(net_id,'*',q)<=1, name=f"Gij_target_{net_id}_{q}") 
    # 拓扑变化约束
    for p in range(nums_fpga):
        for q in range(p, nums_fpga):
            model.addConstr(W_e[p,q]==topo_list[p][1][q]+df_change[p,q], name=f"We_df_{p}_{q}")
            model.addConstr(M*Ue[p,q]>=W_e[p,q], name=f"We_Ue1_{p}_{q}")
            # 0910 新增
            model.addConstr(W_e[p,q] >= Ue[p,q], name=f"We_Ue2_{p}_{q}")
    # TDM 比率约束
    for p in range(nums_fpga):
        for q in range(p, nums_fpga):
            model.addConstr(r_pq[p, q] <= 512, name=f"rpq_limit1_{p}_{q}")
    # FPGA 连接通道容量约束
    for p in range(nums_fpga):
        model.addConstr(W_e.sum(p,'*')<=info_list[p], name=f"channel_limit_{p}")
    # 通道调整范围约束   
        # 1) 为 |df_change[p,q]| 引入非负辅助变量 z[p,q]
    df_change_z = model.addVars(nums_fpga, nums_fpga, lb=0.0, name="abs_df")
        # 只对上三角建绝对值约束（p<q），避免重复与对角
    for p in range(nums_fpga):
        for q in range(p+1, nums_fpga):
            model.addGenConstrAbs(df_change_z[p, q], df_change[p, q], name=f"abs_df_{p}_{q}")
        # 2) 求和并加总量限制
    sum_change = quicksum(df_change_z[p, q] for p in range(nums_fpga) for q in range(p+1, nums_fpga))
    model.addConstr(sum_change <= W_total*0.3, name="total_change_limit") #假设总调整量不超过当前总通道数的30%
    # 目标函数
    # 对称性约束r_pq    
    for p in range(nums_fpga):
        model.addConstr(r_pq[p, p] == 0, name=f"rpq_sym_{p}_{p}")
        for q in range(p + 1, nums_fpga):
            model.addConstr(r_pq[p, q] == r_pq[q, p], name=f"rpq_sym_{p}_{q}")
    # 对称性约束N_pq    
    for p in range(nums_fpga):
        model.addConstr(N_pq[p, p] == 0, name=f"Npq_sym_{p}_{p}")
        for q in range(p + 1, nums_fpga):
            model.addConstr(N_pq[p, q] == N_pq[q, p], name=f"Npq_sym_{p}_{q}")
    # 计算N_pq
    for p in range(nums_fpga):
        for q in range( p + 1, nums_fpga):
            model.addConstr(N_pq[p,q]==Z_e.sum('*',p,q), name=f"Npq_calc_{p}_{q}")
    # 计算r_pq
    for p in range(nums_fpga):
        for q in range( p + 1, nums_fpga):
            # 一些上界（给得紧一点越好）
            Nmax = nums_net         # 每个 (p,q) 最多被多少 net 使用；可按实际更紧
            Rmax = 512              # 你后面有 r_pq <= 512 约束
            M1 = 8 * Nmax           # 放松量，覆盖 8*N 的可能上界
            M2 = 8 * Nmax

            # 若该无向边不存在（Ue=0），强制 N_pq=0 和 r_pq=0
            model.addConstr(N_pq[p,q] <= Nmax * Ue[p,q], name=f"N_zero_by_Ue_{p}_{q}")
            model.addConstr(r_pq[p,q] <= Rmax * Ue[p,q], name=f"r_zero_by_Ue_{p}_{q}")

            # Ue=1 时，激活“向上取整到8的倍数”的两条不等式；Ue=0 时用 Big-M 放松
            # 8*N ≤ r*W
            model.addConstr(
                8 * N_pq[p,q] <= r_pq[p,q] * W_e[p,q] + M1 * (1 - Ue[p,q]),
                name=f"ceil_lb_{p}_{q}"
            )
            # 8*N ≥ (r-8)*W + 8
            model.addConstr(
                8 * N_pq[p,q] >= (r_pq[p,q] - 8) * W_e[p,q] + 8 - M2 * (1 - Ue[p,q]),
                name=f"ceil_ub_{p}_{q}"
            )

    # 计算D_i
    for net_id in range(nums_net):
        for (i, j) in net_edges[net_id]:
            model.addConstr(D_i[net_id]>=quicksum(X[net_id,i,j,p,q]*(0.7*r_pq[p,q]+30) for p in range(nums_fpga) for q in range(nums_fpga) if p!=q ), name=f"Di_calc_{net_id}_{i}_{j}")
            # 用于处理延迟为0的情况 在一块fpga
        model.addConstr(D_i[net_id]<=quicksum(X[net_id,i,j,p,q]*(0.7*r_pq[p,q]+30) for (i, j) in net_edges[net_id] for p in range(nums_fpga) for q in range(nums_fpga) ), name=f"Di_calc1_{net_id}")            

    Max_D = model.addVar(vtype=GRB.CONTINUOUS, name="Max_D", lb=0)  # 所有 n_i 的最大布线延迟
    for net_id in range(nums_net):
        model.addConstr(Max_D>=D_i[net_id],name=f"max{net_id}")

    model.setObjective(Max_D,GRB.MINIMIZE)

    model.optimize()
    if model.status == GRB.INFEASIBLE:
        model.computeIIS()
        model.write("model.ilp")
        model.write("model.iis")
    # print("Number of FPGAs:", nums_fpga)
    # print("Design Info:", design_info)
    # print("Design Net:", design_net)
    # print("Design Topo:", design_topo)
    # print("Design FPGA Out:", design_fpga_out)
    # print("FPGA Out List:", fpga_out_list)
       # ===== 写出结果文件（基于已求解变量）=====
       # ===== 导出 X 的“优先度矩阵/列表” =====
    if model.SolCount > 0:
        thr = 1e-9       # 认为“被选中/显著”的阈值，可改为 0.5 等
        top_k = None     # 若只想输出前 K 条，可设整数；None 表示全部
        # 解出后核对：每个 net 的“由 X 推算的路径和” z_i
        alpha, beta = 0.7, 30.0
        for net in range(nums_net):
            zi = 0.0
            val=0.0
            for (i, j) in net_edges[net]:
                val = 0.0
                for p in range(nums_fpga):
                    for q in range(nums_fpga):
                        if p == q: 
                            continue
                        val += X[net, i, j, p, q].X * (beta + alpha * r_pq[p, q].X)
                if val > zi:
                        zi = val
            print(f"net {net+1}: D_i={D_i[net].X:.3f}, z_i={zi:.3f}")


        # 1) 稀疏表：design.X.sparse.csv
        sparse_path = f"{file_path}/design.X.sparse.csv"
        with open(sparse_path, "w", encoding="utf-8") as f:
            f.write("net_id,i,j,p,q,x\n")
            for net_id in range(nums_net):
                # 遍历该 net 的所有逻辑边 (i,j)
                for (i, j) in net_edges[net_id]:
                    # 收集所有 (p,q) 的 x 值
                    rows = []
                    for p in range(nums_fpga):
                        for q in range(nums_fpga):
                            try:
                                val = float(X[net_id, i, j, p, q].X)
                            except KeyError:
                                val = 0.0
                            if val > thr:
                                rows.append((p, q, val))
                    # 按 x 值降序
                    rows.sort(key=lambda t: t[2], reverse=True)
                    # 若设置 top_k，只写前 K 条
                    use_rows = rows if top_k is None else rows[:top_k]
                    for p, q, val in use_rows:
                        f.write(f"{net_id+1},{i},{j},{p+1},{q+1},{val:.6f}\n")
        print(f"Wrote sparse X to: {sparse_path}")

        # 2) 矩阵块：design.X.matrix.txt（人类可读）
        matrix_path = f"{file_path}/design.X.matrix.txt"
        with open(matrix_path, "w", encoding="utf-8") as f:
            for net_id in range(nums_net):
                for (i, j) in net_edges[net_id]:
                    f.write(f"[net {net_id+1}] (i={i}, j={j})\n")
                    # 输出 n×n 矩阵（p 行，q 列）
                    for p in range(nums_fpga):
                        row_vals = []
                        for q in range(nums_fpga):
                            try:
                                val = float(X[net_id, i, j, p, q].X)
                            except KeyError:
                                val = 0.0
                            # 你可以改成四舍五入成 0/1：val = 1 if val>0.5 else 0
                            row_vals.append(f"{val:.3f}")
                        f.write(",".join(row_vals) + "\n")
                    f.write("\n")
        print(f"Wrote block matrices to: {matrix_path}")

        # 3) 优先弧列表：design.X.topk.out（每个 net,(i,j) 的弧按 x 降序）
        topk_path = f"{file_path}/design.X.topk.out"
        with open(topk_path, "w", encoding="utf-8") as f:
            for net_id in range(nums_net):
                for (i, j) in net_edges[net_id]:
                    # 收集、排序
                    arcs = []
                    for p in range(nums_fpga):
                        for q in range(nums_fpga):
                            try:
                                val = float(X[net_id, i, j, p, q].X)
                            except KeyError:
                                val = 0.0
                            if val > thr:
                                arcs.append((p, q, val))
                    if not arcs:
                        continue
                    arcs.sort(key=lambda t: t[2], reverse=True)
                    if top_k is not None:
                        arcs = arcs[:top_k]

                    # 打印
                    f.write(f"[net {net_id+1}] (i={i}, j={j})\n")
                    for p, q, val in arcs:
                        f.write(f"  ({p+1}->{q+1})  x={val:.6f}\n")
                    f.write("\n")
        print(f"Wrote priority lists to: {topk_path}")

    else:
        print("No feasible solution to export X.")

    if model.SolCount > 0:
        for net_id in range(nums_net):
            try:
                print(f"D_{net_id+1} = {D_i[net_id].X:.3f}")
            except Exception:
                print(f"D_{net_id+1} = (unavailable)")
        alpha, beta = 0.7, 30.0
        thr = 0.5  # 判断 X 是否被选中的阈值

        # 逻辑节点 -> FPGA 索引（0-based）
        node_to_fpga = {}
        for p in range(nums_fpga):
            for node in fpga_out_list[p]:
                node_to_fpga[node] = p

        # 对称变量取值（r_pq/W_e）
        def sym_int(var2d, a, b):
            try:
                v = var2d[a, b].X
            except KeyError:
                v = var2d[b, a].X
            return int(round(v))

        def r_val(u, v): return sym_int(r_pq, u, v)
        def w_val(u, v): return sym_int(W_e,  u, v)

        # 利用 X 的解顺着唯一出边重建 (i->j) 路径；无 BFS
        def reconstruct_path_from_X(net_id, i, j):
            ps, pt = node_to_fpga[i], node_to_fpga[j]
            if ps == pt:
                return [], 0.0  # 同 FPGA：不输出

            path = [ps]
            cur = ps
            seen = {ps}
            for _ in range(nums_fpga + 5):     # 保护上限，避免意外环
                if cur == pt:
                    break
                outs = [(q, X[net_id, i, j, cur, q].X)
                        for q in range(nums_fpga)
                        if (cur != q) and (X[net_id, i, j, cur, q].X > thr)]
                if not outs:
                    return [], 0.0  # 没有出边，放弃该接收端
                # 理论上唯一出边；若有多条，取 X 值最大的那条
                outs.sort(key=lambda t: t[1], reverse=True)
                nxt = outs[0][0]
                if nxt in seen:
                    return [], 0.0  # 意外成环，丢弃
                path.append(nxt)
                seen.add(nxt)
                cur = nxt

            if not path or path[-1] != pt:
                return [], 0.0

            # 计算路径时延（逐跳累加 beta + alpha * r_pq）
            delay = 0.0
            for a, b in zip(path[:-1], path[1:]):
                delay += beta + alpha * r_val(a, b)
            return path, delay

        # 1) 取每个 net 的 D_i（优先使用求解得到的 D_i 排序）
        net_delays = []
        for net_id in range(nums_net):
            try:
                net_delays.append((net_id, float(D_i[net_id].X)))
            except Exception:
                # 如 D_i 非该索引方式，可替换为你的访问方法；或回退为 0
                net_delays.append((net_id, 0.0))
        print("Net delays (D_i):", net_delays)

        # 按 D_i 降序
        net_delays.sort(key=lambda t: t[1], reverse=True)
        print("Net delays (D_i):", net_delays)

        # 2) 写 design.route.out（过滤：同 FPGA & 延迟=0 不输出）
        route_out_path = f"{file_path}/design.route.out"
        with open(route_out_path, "w") as f:
            for net_id, _ in net_delays:
                src = design_net[net_id][0]
                sinks = design_net[net_id][2]
                lines = []
                for j in sinks:
                    seq, d = reconstruct_path_from_X(net_id, src, j)
                    if not seq or d <= 0.0 or len(seq) <= 1:
                        continue  # 同 FPGA/零延迟/未成功重建 → 跳过
                    seq_1 = [x + 1 for x in seq]  # 1-based 索引输出
                    lines.append((seq_1, d))

                if not lines:
                    continue  # 该 net 无需跨 FPGA，不输出

                f.write(f"[net {net_id + 1}]\n")  # net ID 从 1 开始
                for seq_1, d in lines:
                    seq_str = "[" + ",".join(str(x) for x in seq_1) + "]"
                    f.write(f"{seq_str} [{d:.1f}]\n")

        print(f"Wrote routing result to: {route_out_path}")

        # 3) 写 design.newtopo（直接用解中的 W_e 邻接矩阵）
        newtopo_path = f"{file_path}/design.newtopo"
        with open(newtopo_path, "w") as f:
            for p in range(nums_fpga):
                row = [str(w_val(p, q)) for q in range(nums_fpga)]
                f.write(f"F{p+1}: " + ",".join(row) + "\n")
        print(f"Wrote new topology to: {newtopo_path}")

    else:
        print("No feasible solution to write outputs.")


        
       
except GurobiError as exception:
    print('Error code ' + str(exception.errno) + ": " + str(exception))
except AttributeError:
    import traceback
    traceback.print_exc()
