/*****************************************************************************************
!!!!!!!!!!!!!!!!!!!!!!!!!!!!注意!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
算例中所有编号都是以1开始，为方便对应，当前数据结构索引为0的位置全部空余，有效索引从1开始
操作时注意：索引全部由1开始！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！
*****************************************************************************************/


#include <iostream>
#include <stdlib.h>
#include <fstream>
#include <string.h>
#include <math.h>
#include<algorithm>
#include <string>
#include <cmath> 
#include <ctime>
#include <sstream>
#include <vector>
#include <unordered_set>
#include <utility> 
#include <functional> 
#include <iomanip>
#include <numeric>
using namespace std;

#define MININT -2147483648
#define MAXINT 2147483647
#define RANDMAX 999
#define SDF_MAX(a,b) ((a)>(b)?(a):(b))
#define SDF_MIN(a,b) ((a)<(b)?(a):(b))

//***********************************************************//
char* rep;
char nameFinalResult[256];
char nameSchedule[256];
char cutname[256];
//***********************************************************//

char* caseName;
char* designInfo;
char* designNet;
char* designTopo;
char* designFpgaOut;
int seed;

int numFPGA; //FPGA数量
int numNet; //Net数量
int numNode; //逻辑节点数量

int** weight_matrix; //FPGA间的连接通道矩阵
int** delta_weight_matrix; //FPGA间变动的通道数量矩阵

int** nets_count_matrix; //跨越各FPGA之间通道的net数量

int* FPGA_max_weight; //各FPGA最大对外的连接通道数量
int* nodes_FPGA; //各逻辑节点对应的FPGA编号
int* length_FPGA_nodes; //各FPGA对应的逻辑节点数量
int** FPGA_nodes; // 各FPGA对应的逻辑节点列表


struct PairHash
{
	size_t operator()(const std::pair<int, int>& p) const noexcept
	{
		// 常见的组合哈希写法：避免简单拼接带来的冲突
		return std::hash<int>{}(p.first) ^ (std::hash<int>{}(p.second) << 1);
	}
};

/*net的结构体*/
typedef struct
{
	int source_node; //net的起点
	int sink_num; // net的终点数量
	vector<int> sink_nodes; //net的终点
	vector<pair<int, int>> available_edges; // net中节点可用的边（即存在连接线，可使用的边）
	vector<vector<pair<int, int>>> path; //路径，0-1三维矩阵：起点到每个终点选择的边，第一维标记终点序号，后两维度是0-1矩阵表示选择的边
	unordered_set<pair<int, int>, PairHash> steiner_tree; //所有路径对应的斯坦纳树骨架
	vector<int> path_jump_count; //每条路径跳跨的FPGA数量
	vector<double> path_delay; //每条路径的延时

}Net;

vector<Net> net_list; // 所有net的数组
double* net_delay; // 每个net的延时

//***********************************************************
// 所需变量：numFPGA、net_list、nodes_FPGA、weight_matrix、delta_weight_matrix、nets_count_matrix、FPGA_max_weight

//***********************************************************
// 读取实例数据的函数
void read_instance()
{
   
}




//*****************************************************************************************
// compute_edge_cost
// 作用：返回物理边 (u,v) 的代价（最短路权重、延迟累加用）
//*****************************************************************************************
double compute_edge_cost(int u, int v)
{
    int cost; 
    return cost;
}




//*****************************************************************************************
// multi_dijkstra（初始化用）
// 作用：求 起点s到终点（t1，t2等）的最短路（以 compute_edge_cost 为边权）
// 输入：s, t
// 输出：out_path（边序列 {(u1,v1),...}）
//*****************************************************************************************
void multi_dijkstra(int s, int t, std::vector<std::pair<int,int>>& out_path) 
{

}


//***********************************************************//
// 检验所需变量：numFPGA、net_list、nodes_FPGA、weight_matrix、delta_weight_matrix、nets_count_matrix、FPGA_max_weight
//输出：返回true/false，并提供错误原因

//*****************************************************************************************
bool check_result()
{

}




//*****************************************************************************************
// generate_initial_solution
// 作用：用 Dijkstra 为所有 net 生成逻辑路径，构建初始合法解；计算 path_delay
// 输入：无（使用全局 net_list、nodes_FPGA、矩阵等）
// 输出：无
//*****************************************************************************************
void generate_initial_solution()
{
 
    // 用 compute_edge_cost 对每个net求net_delay，并记录最大路径延迟的路径。


    // 生成后做一次可行性校验
    check_result();
}


//*****************************************************************************************
// dijkstra_shortest_path（求单条路径用）
// 作用：在 net 上求 候选点到终点 的最短路径（用于AF或尾部补全）
// 输入：起点、终点、可用边
// 输出：路径、路径延迟
//*****************************************************************************************
void dijkstra_shortest_path();





//*****************************************************************************************
// expand_beam_layer（内含 check_result）
// 作用：束搜索“单层扩展”——对每个状态枚举下一跳，计算 f = OA+AF，OB+BF，OC+CF并在生成候选前缀后：
//      1) 在选取下层节点之前判断新增路径是否有物理路径，以及符合TDM比率约束  2) 调用dijkstra_shortest_path计算AF,BF,CF路径；3) 计算 OA+AF，OB+BF，OC+CF；
//     按 f 降序保留Top-K。 
// 输入：state_list, goal_fpga, beamK, old_pairs_this_sink, other_pairs_of_net
// 输出：topk_candidates（按f降序截断到beamK，从o到A、B、C的路径）；
//      
//*****************************************************************************************
void expand_beam_layer();



//*****************************************************************************************
// random_pick_from_topk
// 作用：在“最优的前k条可行路径”中随机选一条作为最终替换路径(后续可替换为贪婪随机)
// 输入：topk_candidates
// 输出：chosen（被选中的 CandidatePath）
//*****************************************************************************************
void random_pick_from_topk();


//*****************************************************************************************
// apply_update_and_refresh
// 作用：用选中的候选路径替换原路径，并更新全局结构（nets_count_matrix / path_delay / net_delay 等）
// 输入：worst（目标net与sink信息），chosen（新路径）
// 输出：无（更新 net, nets_count_matrix, path_delay, net_delay； nodes_FPGA / length_FPGA_nodes / FPGA_nodes 等）
//*****************************************************************************************
void apply_update_and_refresh();


//*****************************************************************************************
// beam_search  ——（束搜索算法：一次迭代的驱动流程，更新版）
// 步骤：
//   (1) 选“最差路径”select_worst_path：定位待重路由的 (对应的net, 起点终点)，若有多条起点终点相同则另作判断
//   (2) 束搜索多层扩展：
//       初始化 beam（只有起点）；循环调用 expand_beam_layer(...), 并在 expand_beam_layer 内部进行可行性校验 check_result
//   (3)  采用“最优的前k条可行路径”
//   (4) 在前k条中随机选一条替代原始路径random_pick_from_topk ，并更新参数apply_update_and_refresh
// 输入：beamK（束宽, 默认3），max_layers（最大扩展层数，默认numFPGA-1），topK（默认=beamK）
// 输出：无（更新 net, nets_count_matrix, path_delay, net_delay；以及工程侧表 nodes_FPGA / length_FPGA_nodes / FPGA_nodes 等）
//*****************************************************************************************
void beam_search(int beamK = 3, int max_layers = -1, int topK = -1)
{
    // ---------- (1) 选“最差路径”和同net的可用连接 ----------
    // vector<vector<pair<int, int>>> worst_path = net_list[i].path[j]
    // 	vector<pair<int, int>> worst_path_availible = net_list[i].available_edges; 
    
 

    // ---------- (2) 束搜索扩展（含可行性校验） ----------


 
    //     expand_beam_layer()



    // ---------- (4) 在Top-K中随机选一条并更新 ----------

    // random_pick_from_topk(topk_candidates, chosen);
    // apply_update_and_refresh(worst, chosen);
}




//*****************************************************************************************
// beam_search  ——（束搜索算法）
// 步骤：
//   (1) 在所有 net 中扫描 path_delay，定位“最大路径延迟”的 net和起点终点；
//   (2) 对 s->g 运行束搜索得到候选路径集合（候选点不一定选最优，可能有随机性）；
//   (3) 可行性校验
//   (4) 计算候选得分（路径代价和）；若最优与原解完全一致或差异很小，则添加随机因素（如轮盘赌）；
//   (5) 更新 nets_count_matrix /  path_delay；
// 输入：beamK（束宽，老师初步建议为3）
// 输出：无
//*****************************************************************************************


void optimize_with_beam_search(double maxTime)
{
    //初始化函数
    generate_initial_solution();

    //迭代运行优化函数

    int iter = 0;
    int no_improve_iter = 0;
    double begin_time = clock();
    
    // 主循环
    while(((clock()-begin_time)/CLOCKS_PER_SEC) < maxTime)
    {
        // 束搜索
        beam_search();

        // 记录最佳解，达到时间/迭代无增长次数过多后推出


    }


}


//*****************************************************************************************
// file_output
// 输出design.route.out（布线结果文件）和design.newtopo（拓扑调整后物理互联结构）文件，并用官方的验证器进行检验
//*****************************************************************************************

void file_output()
{

}



int main(int argc, char** argv)
{
	if (argc == 7)
	{
		/*命令行参数输入顺序*/
		caseName = argv[1]; //case文件夹名字
		designInfo = argv[2]; // design.info
		designNet = argv[3]; // design.net
		designTopo = argv[4]; //design.topo
		designFpgaOut = argv[5]; //design.fpga.out
		seed = atoi(argv[6]); // 随机数种子
		rep = argv[6];
	}
	else
	{
		cout << "Enter some parameters to run the program: " << endl;
		cout << "<instance file name>  <seed>" << endl;
		exit(0);
	}

    srand(seed);
    // 运行时间限制
    double maxRunTime = 20;

    // 读取实例
    read_instance();

    //运行主算法
    optimize_with_beam_search(maxRunTime);

    //输出结果
    file_output();



    





	delete[] FPGA_max_weight;

	delete[] net_delay;


	for (int x = 0; x < numFPGA + 1; x++)
	{
		delete[] weight_matrix[x];
		delete[] delta_weight_matrix[x];
		delete[] nets_count_matrix[x];
		delete[] FPGA_nodes[x];
	}
	delete[] weight_matrix;
	delete[] delta_weight_matrix;
	delete[] nets_count_matrix;

	delete[] FPGA_nodes;
	delete[] nodes_FPGA;
	delete[] length_FPGA_nodes;
}




























