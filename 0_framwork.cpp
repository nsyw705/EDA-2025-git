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
// dijkstra_path
// 作用：求 起点s到终点（t1，t2等）的最短路（以 compute_edge_cost 为边权）
// 输入：s, t
// 输出：out_path（边序列 {(u1,v1),...}）
//*****************************************************************************************
void dijkstra_path(int s, int t, std::vector<std::pair<int,int>>& out_path) 
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
// beam_search  ——（束搜索算法）
// 步骤：
//   (1) 在所有 net 中扫描 path_delay，定位“最大路径延迟”的 net和起点终点；
//   (2) 对 s->g 运行束搜索得到候选路径集合；
//   (3) 可行性校验
//   (4) 计算候选得分（路径代价和）；若最优与原解完全一致或差异很小，则添加随机因素（如轮盘赌）；
//   (5) 更新 nets_count_matrix /  path_delay；
// 输入：beamK（束宽，老师初步建议为3）
// 输出：无
//*****************************************************************************************
void beam_search(int beamK = 3)
{
    // ---------- (1) 选“最差路径” ----------


    // ---------- (2) 束搜索候选 ----------


    // ---------- (3) 可行性检查（可选：只检查受影响对、或全局） ----------



    // ---------- (4) 评估候选 ----------



    // 检查新解的路径延迟与原解无变化，规则：完全一致 或 分数差距很小则添加随机性


    // ---------- (5) 更新解 ----------


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




























