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
#include <unordered_map>
#include <utility> 
#include <functional>
#include <iomanip>
#include <numeric>
#include<random>
#include <iterator>
#include <cassert>
#include<queue>
using namespace std;

#define MININT -2147483648
#define MAXINT 2147483647
#define RANDMAX 999
#define SDF_MAX(a,b) ((a)>(b)?(a):(b))
#define SDF_MIN(a,b) ((a)<(b)?(a):(b))

#define BEAMWIDTH 3//邻域1参数
#define Maxdepth 8 //邻域1参数
#define Max_kdepth 5//邻域1参数
#define SHORTESTROB 0.8//邻域2参数
#define ADJUSTRATIO 0.3//邻域3参数

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
double maxRunTime;
double beginTime;

int numFPGA; //FPGA数量
int numNet; //Net数量
int numNode; //逻辑节点数量
int numTunnel; //原有通道数量
int numAddedTunnel; //增加的通道数量

int R_max; //最大TDM比率
double T; //温度

int** weight_matrix; //FPGA间的连接通道矩阵
int** delta_weight_matrix; //FPGA间变动的通道数量矩阵
int** global_delta_weight_matrix; //搜索到的最优：FPGA间变动的通道数量矩阵

int** nets_count_matrix; //跨越各FPGA之间通道的net数量

int* FPGA_max_weight; //各FPGA最大对外的连接通道数量
int* FPGA_weight; // 各FPGA当前的对外连接通道数量
int* nodes_FPGA; //各逻辑节点对应的FPGA编号
int* length_FPGA_nodes; //各FPGA对应的逻辑节点数量
int** FPGA_nodes; // 各FPGA对应的逻辑节点列表


vector<pair<int, int>> added_arc_record; //增加的通道位置记录

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
	vector<vector<pair<int, int>>> path; //路径，0-1三维矩阵：起点到每个终点选择的边，第一维标记终点序号，后两维度是0-1矩阵表示选择的边
	unordered_map<pair<int, int>, int, PairHash> path_map; // 所有路径中边与其对应的使用数量
	vector<double> path_delay; //每条路径的延时
}Net;
vector<Net> Global, Current; // 所有net的数组，Global记录找到的最优解，Cureent记录搜索过程中变化的解
double* net_delay; // 每个net的延时，用于存储找到的全局最优
double* Global_net_delay; // Global_delay对应的net_delay;
double Global_delay;//Global对应的延迟


//快速排序
void qsort_double(double* s, int* address, int l, int r)//降序
{
	if (l >= r) { return; }
	if (l < r)
	{
		int i = l, j = r;
		double x = s[l];
		int y = address[l];
		while (i < j)
		{
			while (i < j && s[j] <= x)
				j--;
			if (i < j)
			{
				s[i] = s[j];
				address[i] = address[j];
				i++;
			}

			while (i < j && s[i] > x)
				i++;
			if (i < j)
			{
				s[j] = s[i];
				address[j] = address[i];
				j--;
			}
		}
		s[i] = x;
		address[i] = y;
		qsort_double(s, address, l, i - 1);
		qsort_double(s, address, i + 1, r);
	}
}


//快速排序
void qsort_vector(vector<double>& s, vector<int>& address, int l, int r)//升序
{
	if (l >= r) { return; }
	if (l < r)
	{
		int i = l, j = r;
		//double x = s[l + ((r - l) >> 1)];
		//int y = address[l + ((r - l) >> 1)];
		double x = s[l];
		int y = address[l];
		while (i < j)
		{
			while (i < j && s[j] >= x)
				j--;
			if (i < j)
			{
				s[i] = s[j];
				address[i] = address[j];
				i++;
			}

			while (i < j && s[i] < x)
				i++;
			if (i < j)
			{
				s[j] = s[i];
				address[j] = address[i];
				j--;
			}
		}
		s[i] = x;
		address[i] = y;
		qsort_vector(s, address, l, i - 1);
		qsort_vector(s, address, i + 1, r);
	}
}



// ---------- 2) 向量版本：保留升序前 k 小 ----------
void topk_asc_vector(vector<double>& s, vector<int>& address, int k)
{
	int n = (int)s.size();
	assert((int)address.size() == n);
	if (n <= 0 || k <= 0)
	{
		return;
	}
	if (k > n)
	{
		k = n;
	}
	struct Node
	{
		double v;
		int id;
		// 默认优先队列是大根堆；为了“保留最小的 k 个”，我们让堆顶是当前 top-k 中最大的
		bool operator<(const Node& o) const { return v < o.v; } // v 大的更“优先”
	};
	priority_queue<Node> heap; // 大根堆（堆顶是最大）
	for (int i = 0; i < n; ++i)
	{
		if ((int)heap.size() < k)
		{
			heap.push(Node{ s[i], address[i] });
		}
		else if (s[i] < heap.top().v)
		{
			heap.pop();
			heap.push(Node{ s[i], address[i] });
		}
	}
	vector<Node> topk;
	topk.reserve(k);
	while (!heap.empty())
	{
		topk.push_back(heap.top());
		heap.pop();
	}
	sort(topk.begin(), topk.end(), [](const Node& a, const Node& b)
		{
			return a.v < b.v; // 升序
		});

	for (int i = 0; i < k; ++i)
	{
		s[i] = topk[i].v;
		address[i] = topk[i].id;
	}
}

void read_instance()
{
	char temp_caseName[50];
	char designInfoName[50];
	char designNetName[50];
	char designTopoName[50];
	char designFpgaOutName[50];

	//strcpy_s(temp_caseName, caseName);
	//strcat_s(temp_caseName, sizeof(temp_caseName), "/");
	//strcpy_s(designInfoName, temp_caseName);
	//strcpy_s(designNetName, temp_caseName);
	//strcpy_s(designTopoName, temp_caseName);
	//strcpy_s(designFpgaOutName, temp_caseName);

	//strcat_s(designInfoName, sizeof(designInfoName), designInfo);
	//strcat_s(designNetName, sizeof(designNetName), designNet);
	//strcat_s(designTopoName, sizeof(designTopoName), designTopo);
	//strcat_s(designFpgaOutName, sizeof(designFpgaOutName), designFpgaOut);

	std::snprintf(temp_caseName, sizeof temp_caseName, "%s/", caseName);
	std::snprintf(designInfoName, sizeof designInfoName, "%s%s", temp_caseName, designInfo);
	std::snprintf(designNetName, sizeof designNetName, "%s%s", temp_caseName, designNet);
	std::snprintf(designTopoName, sizeof designTopoName, "%s%s", temp_caseName, designTopo);
	std::snprintf(designFpgaOutName, sizeof designFpgaOutName, "%s%s", temp_caseName, designFpgaOut);

	R_max = 512; //最大TDM比率
	T = 300.0;

	ifstream FIC;
	FIC.open(designInfoName);

	if (FIC.fail())
	{
		cout << "1-can not open the file " << designInfoName << endl;
		exit(0);
	}
	if (FIC.eof())
	{
		cout << "2-can not open the file " << designInfoName << endl;
	}
	char str_reading[100];
	numFPGA = 0;
	while (!FIC.eof())
	{
		FIC >> str_reading;
		numFPGA++;
	}
	numFPGA = numFPGA / 2;
	FIC.close();


	FPGA_max_weight = new int[numFPGA + 1];
	FPGA_weight = new int[numFPGA + 1];
	FIC.open(designInfoName);
	if (FIC.fail())
	{
		cout << "1-can not open the file " << designInfoName << endl;
		exit(0);
	}
	if (FIC.eof())
	{
		cout << "2-can not open the file " << designInfoName << endl;
	}

	int FPGA_index = 1;
	for (int x = 0; x < numFPGA * 2; x++)
	{
		FIC >> str_reading;
		if (x % 2 != 0)
		{
			FPGA_max_weight[FPGA_index] = atoi(str_reading);
			FPGA_weight[FPGA_index] = 0;
			FPGA_index++;
		}
	}
	FIC.close();
	FPGA_max_weight[0] = 0;

	std::string net_line;
	numNet = 0;
	int net_index = 0;
	int max_node_index = MININT;
	FIC.open(designNetName);
	if (FIC.fail())
	{
		cout << "1-can not open the file " << designNetName << endl;
		exit(0);
	}
	if (FIC.eof())
	{
		cout << "2-can not open the file " << designNetName << endl;
	}

	while (std::getline(FIC, net_line))
	{
		Net read_net;
		std::istringstream iss(net_line);
		std::string token;
		int node_count = 0;
		int token_count = 0;
		int sink_count = 0;
		while (iss >> token)
		{
			if (token[0] == 'g')
			{
				if (token_count == 0)
				{
					read_net.source_node = std::stoi(token.substr(1));
					if (read_net.source_node > max_node_index)
						max_node_index = read_net.source_node;
					token_count++;
				}
				else
				{
					read_net.sink_nodes.push_back(std::stoi(token.substr(1)));
					if (std::stoi(token.substr(1)) > max_node_index)
						max_node_index = std::stoi(token.substr(1));
					sink_count++;
					token_count++;
				}
			}

		}
		read_net.sink_num = (int)read_net.sink_nodes.size();
		Global.push_back(read_net);
		Current.push_back(read_net);
		numNet++;
	}

	net_delay = new double[numNet];
	Global_net_delay = new double[numNet];
	for (int x = 0; x < numNet; x++)
	{
		net_delay[x] = (double)MAXINT;
		Global_net_delay[x] = (double)MAXINT;
	}

	numNode = max_node_index;
	FIC.close();

	weight_matrix = new int* [numFPGA + 1];
	delta_weight_matrix = new int* [numFPGA + 1];
	global_delta_weight_matrix = new int* [numFPGA + 1];
	nets_count_matrix = new int* [numFPGA + 1];

	for (int i = 0; i < numFPGA + 1; i++)
	{
		weight_matrix[i] = new int[numFPGA + 1];
		delta_weight_matrix[i] = new int[numFPGA + 1];
		global_delta_weight_matrix[i] = new int[numFPGA + 1];
		nets_count_matrix[i] = new int[numFPGA + 1];
		for (int j = 0; j < numFPGA + 1; j++)
		{
			weight_matrix[i][j] = 0;
			delta_weight_matrix[i][j] = 0;
			global_delta_weight_matrix[i][j] = 0;
			nets_count_matrix[i][j] = 0;
		}
	}

	std::string topo_line;
	FIC.open(designTopoName);
	if (FIC.fail())
	{
		cout << "1-can not open the file " << designTopoName << endl;
		exit(0);
	}
	if (FIC.eof())
	{
		cout << "2-can not open the file " << designTopoName << endl;
	}

	for (int i = 0; i <= numFPGA; i++)
	{
		if (i == 0)
			continue;
		FIC >> topo_line;
		FIC >> topo_line;
		std::stringstream ss(topo_line);
		std::string token;
		int index = 1;
		while (std::getline(ss, token, ','))
		{
			if (index <= numFPGA)
			{
				weight_matrix[i][index] = std::stoi(token);
				index++;
			}
		}
		if (index != numFPGA + 1)
		{
			cout << "weight_matrix size error! should be " << numFPGA << " actual " << index;
			exit(-1);
		}
	}
	FIC.close();

	numTunnel = 0;
	numAddedTunnel = 0;
	for (int i = 0; i < numFPGA + 1; i++)
	{
		int sum_tunnel = 0;
		for (int j = 0; j < numFPGA + 1; j++)
		{
			if (weight_matrix[i][j] > 0)
			{
				if (i < j)
					numTunnel += weight_matrix[i][j];
				sum_tunnel += weight_matrix[i][j];
			}
		}
		FPGA_weight[i] = sum_tunnel;
	}


	nodes_FPGA = new int[numNode + 1];
	for (int x = 0; x < numNode + 1; x++)
		nodes_FPGA[x] = 0;

	length_FPGA_nodes = new int[numFPGA + 1];
	FPGA_nodes = new int* [numFPGA + 1];
	for (int i = 0; i < numFPGA + 1; i++)
	{
		length_FPGA_nodes[i] = 0;
		FPGA_nodes[i] = new int[numNode + 1];
		for (int j = 0; j < numNode + 1; j++)
			FPGA_nodes[i][j] = -1;
	}


	std::string fpga_out_line;
	FIC.open(designFpgaOutName);
	if (FIC.fail())
	{
		cout << "1-can not open the file " << designFpgaOutName << endl;
		exit(0);
	}
	if (FIC.eof())
	{
		cout << "2-can not open the file " << designFpgaOutName << endl;
	}

	int fpga_index = 1;
	while (std::getline(FIC, fpga_out_line))
	{
		std::istringstream iss(fpga_out_line);
		std::string token;
		int nodes_count = 1;
		while (iss >> token)
		{
			if (token[0] == 'g' && fpga_index < numFPGA + 1 && nodes_count < numNode + 1)
			{
				FPGA_nodes[fpga_index][nodes_count] = std::stoi(token.substr(1));
				if (FPGA_nodes[fpga_index][nodes_count] < numNode + 1)
					nodes_FPGA[FPGA_nodes[fpga_index][nodes_count]] = fpga_index;
				nodes_count++;
			}
		}
		if (fpga_index < numFPGA + 1)
			length_FPGA_nodes[fpga_index] = nodes_count - 1;

		fpga_index++;
	}


	FIC.close();


	cout << "Successfully read and load all the data!" << endl;

}


void check_read_instance() {
	cout << numFPGA << ' ' << numNet << ' ' << numNode << endl;
	cout << "FPGA_max_weight: "; for (int i = 1; i <= numFPGA; i++) { cout << FPGA_max_weight[i] << ' '; }cout << endl;
	cout << "weight_matrix: " << endl;
	for (int i = 1; i <= numFPGA; i++) {
		for (int j = 1; j <= numFPGA; j++) {
			cout << weight_matrix[i][j] << ' ';
		}
		cout << endl;
	}
	cout << "delta_weight_matrix:" << endl;
	for (int i = 1; i <= numFPGA; i++)
	{
		for (int j = 1; j <= numFPGA; j++)
		{
			cout << delta_weight_matrix[i][j] << ' ';
		}
		cout << endl;
	}
	cout << "nets_count_matrix: " << endl;
	for (int i = 1; i <= numFPGA; i++)
	{
		for (int j = 1; j <= numFPGA; j++)
		{
			cout << nets_count_matrix[i][j] << ' ';
		}
		cout << endl;
	}
	cout << "nodes_FPGA: ";
	for (int i = 1; i <= numNode; i++)
	{
		cout << nodes_FPGA[i] << ' ';
	}
	cout << endl;
	cout << "length_FPGA_nodes: ";
	for (int i = 1; i <= numFPGA; i++)
	{
		cout << length_FPGA_nodes[i] << ' ';
	}
	cout << endl;
	cout << "FPGA_nodes: " << endl;
	for (int i = 1; i <= numFPGA; i++)
	{
		for (int j = 1; j <= length_FPGA_nodes[i]; j++)
		{
			cout << FPGA_nodes[i][j] << ' ';
		}
		cout << endl;
	}
}


int ceil8(double x) { return static_cast<int>(std::ceil(x / 8.0) * 8.0); } // 取8的倍数

bool check_result(vector<Net>& Input_Netgroup, int** delta_weight_M)
{

	//cout << "There is no new topo file" << endl;

	// 2) 从零统计每对 (u,v) 的 nets_count（无向 + 每 net 只计一次）
	std::vector<std::vector<int>> nets_count(numFPGA + 1, std::vector<int>(numFPGA + 1, 0));
	for (int k = 0; k < (int)Input_Netgroup.size(); ++k) {
		// 无向去重：同一 net 中，同一无向通道只计一次
		std::unordered_set<long long> used_once_undirected;
		used_once_undirected.reserve(512);

		for (int j = 0; j < (int)Input_Netgroup[k].path.size(); ++j) {
			const auto& edges = Input_Netgroup[k].path[j];
			for (int m = 0; m < (int)edges.size(); ++m) {
				int u = edges[m].first;
				int v = edges[m].second;
				if (u == v) continue;

				// 规范为无向 (a<b)
				int a = (u < v) ? u : v;
				int b = (u < v) ? v : u;
				long long key = ((long long)a << 32) | (unsigned int)b;

				if (used_once_undirected.insert(key).second) {
					// 对称计数，保证矩阵对称
					nets_count[a][b] += 1;
					nets_count[b][a] += 1;
				}
			}
		}
	}


	// ---------- (A) 单次(i<j)循环合并：改动规模 + TDM + 容量累计 ----------
	int total_physical_links = 0;     // Σ weight(i,j)
	int delta_sum = 0;                // Σ |delta(i,j)|
	vector<int> sum_conn(numFPGA + 1, 0); // 每个FPGA的对外连接数
	vector<int> change_per_fpga(numFPGA + 1, 0);//每个FPGA的对外连接变化


	//cout << endl << "Check FPGA channel capacity constraint" << endl;

	for (int i = 1; i <= numFPGA; ++i) {
		for (int j = i + 1; j <= numFPGA; ++j) {
			int phys = weight_matrix[i][j] + delta_weight_M[i][j];

			total_physical_links += weight_matrix[i][j];
			int d_abs = abs(delta_weight_M[i][j]);
			delta_sum += d_abs;

			change_per_fpga[i] += d_abs;
			change_per_fpga[j] += d_abs;


			sum_conn[i] += phys;
			sum_conn[j] += phys;

			// TDM & 物理缺失
			int nets_on_pair = nets_count[i][j];
			if (nets_on_pair > 0 && phys == 0) {
				cout << "[E1] Denominator is zero in ratio calculation: pair(" << i << "," << j
					<< ") nets=" << nets_on_pair << ", phys=0\n";//ratio计算中分母为0
				return false;
			}
			else if (phys == 0) {
				continue; // nets=0且phys=0，跳过
			}

			// TDM计算：先 ceil(nets/phys)，再 ceil 到8的倍数，≤512
			int tdm_q = (((int)ceil((double)nets_on_pair / (double)phys) + 7) & ~7);

			if (tdm_q > 512) {
				std::cout << "[E2] TDM ratio exceeds limit for pair(" << i << "," << j << "): ceil("
					<< nets_on_pair << "/" << phys << ")=" << ceil(nets_on_pair / phys)
					<< ", quantized=" << tdm_q << " > 512\n";//TDM 超限
				return false;
			}
		}
	}

	//cout << "All FPGA channel capacity satisfies the constraint" << endl;
	//cout << "Total number of connection is: " << std::fixed << std::setprecision(1)
	//	<< (double)total_physical_links << endl << endl;

	// ---------- (B) 改动规模约束 ----------
	if (delta_sum > total_physical_links * 0.3) {
		std::cout << "[E3] Total delta change exceeds limit: delta_sum=" << delta_sum
			<< " > limit=" << total_physical_links * 0.3 << "\n";//改动规模超限
		return false;
	}

	//cout << "Check channel reconfiguration scope constraint" << endl;
	//cout << "Detail info of changing connections :" << endl;




	//for (int f = 1; f <= numFPGA; ++f)
	//	cout << "Number of changing connections of FPGA F" << f << " is: "
	//	<< change_per_fpga[f] << endl;

	//cout << "Total number of changing connections: " << std::fixed << std::setprecision(1)
	//	<< (double)delta_sum << endl;
	//cout << "The variation in connection channels satisfies the constraints" << endl << endl;


	// ---------- (C) 通道容量约束 ----------
	for (int i = 1; i <= numFPGA; ++i) {
		if (sum_conn[i] > FPGA_max_weight[i]) {
			std::cout << "[E4] Channel capacity exceeded: FPGA " << i
				<< " total=" << sum_conn[i]
				<< " > max=" << FPGA_max_weight[i] << "\n";//容量超限
				return false;
		}
	}

	//cout << "Check the max ratio constraints:" << endl;
	//for (int i = 1; i <= numFPGA; ++i) {
	//	for (int j = i + 1; j <= numFPGA; ++j) {
	//		if (nets_count[i][j] <= 0) continue; // 跳过net=0的pair

	//		int nets_on_pair = nets_count[i][j];
	//		int phys = weight_matrix[i][j] + delta_weight_M[i][j];

	//		cout << "FPGA pair (" << i << ", " << j << "): "
	//			<< nets_on_pair << " nets" << endl;
	//		cout << "FPGA pair (" << i << ", " << j << ") has "
	//			<< phys << " connections, limit is " << 512 * phys << "!" << endl;
	//	}
	//}
	//cout << "All FPGA pairs are within the net limit." << endl << endl;


	// ---------- (D) 路径检查 ----------

	for (int k = 0; k < (int)Input_Netgroup.size(); ++k) {
		for (int t = 0; t < (int)Input_Netgroup[k].path.size(); ++t) {

			const auto& P = Input_Netgroup[k].path[t];
			if (P.empty()) continue;

			int s_fpga = nodes_FPGA[Input_Netgroup[k].source_node];
			int g_fpga = nodes_FPGA[Input_Netgroup[k].sink_nodes[t]];
			int cur = s_fpga;

			// 记录已到达过的节点，防环
			std::vector<char> visited(numFPGA + 1, 0);
			if (cur >= 1 && cur <= numFPGA) visited[cur] = 1;

			for (int e = 0; e < (int)P.size(); ++e) {
				int u = P[e].first;
				int v = P[e].second;

				// 1) 有向连续性
				if (u != cur) {
					std::cout << "[E5] Logical path discontinuity in net " << k
						<< " sinkIdx " << t << " cur=" << cur
						<< " edge=(" << u << "," << v << ")\n";
					return false;
				}

				// 2) 物理可达性（含 delta）
				int phys = weight_matrix[u][v] + delta_weight_M[u][v];
				if (phys <= 0) {
					std::cout << "[E6] Physical edge missing for net " << k
						<< " sinkIdx " << t << " edge=(" << u << "," << v << ")\n";
					return false;
				}

				// 3) 成环检查：不能回到此前访问过的任意节点
				if (v >= 1 && v <= numFPGA && visited[v]) {
					std::cout << "[E7] Cycle detected in net " << k
						<< " sinkIdx " << t << " revisiting node " << v << "\n";
					return false;
				}

				// 前进并标记
				cur = v;
				if (cur >= 1 && cur <= numFPGA) visited[cur] = 1;
			}

			// 4) 终点一致性：最后必须到达指定 g_fpga
			if (cur != g_fpga) {
				std::cout << "[E8] Path does not end at sink for net " << k
					<< " sinkIdx " << t << " end=" << cur
					<< " expected=" << g_fpga << "\n";
				return false;
			}
		}
	}



	//cout << "Check whether all nets have routed" << endl;
	//cout << "All nets has routed" << endl << endl;


	// ---------- (E) 计算得分 ----------
	//cout << "Calculate the score of max delay: " << endl;
	double score_max_delay = 0.0;


	for (int k = 0; k < (int)Input_Netgroup.size(); ++k) {
		for (int t = 0; t < (int)Input_Netgroup[k].path_delay.size(); ++t)
			score_max_delay = max(score_max_delay, Input_Netgroup[k].path_delay[t]);
	}
	//cout << "Score of max delay =  " << std::fixed << std::setprecision(1)
	//	<< score_max_delay << endl;




	//重算版本
// ---------- (E) 计算得分（从零重算 + 仅比较最终结果；使用下标for写法） ----------
	//std::cout << "Calculate the score of max delay (from scratch): " << std::endl;



	// 3) 按原口径重算所有路径的延迟，并取最大（分母用 weight_matrix，不含 delta）
	double recomputed_max_delay = 0.0;
	for (int k = 0; k < (int)Input_Netgroup.size(); ++k) {
		for (int j = 0; j < (int)Input_Netgroup[k].path.size(); ++j) {
			double d = 0.0;
			const auto& edges = Input_Netgroup[k].path[j];
			for (int m = 0; m < (int)edges.size(); ++m) {
				int u = edges[m].first;
				int v = edges[m].second;
				if (u == v) continue;

				// 与 calculate_su 保持一致：30 + 0.7 * ceil8(nets/weight)
				d += 30 + 0.7 * ceil8(
					(double)nets_count[u][v] / (weight_matrix[u][v] + delta_weight_M[u][v])
				);
			}
			if (d > recomputed_max_delay) {
				recomputed_max_delay = d;
			}
		}
	}

	//std::cout << "nets_count: " << std::endl;
	//for (int i = 1; i <= numFPGA; ++i) {
	//	for (int j = 1; j <= numFPGA; ++j) {
	//		std::cout << nets_count[i][j] << ' ';
	//	}
	//	std::cout << std::endl;
	//}

	// 4) 比较最终结果（只比较最大值）
	if (recomputed_max_delay != score_max_delay) {
		std::cerr << "path_delay计算错误：最大延迟不一致，已有最大延迟="
			<< score_max_delay << " 验证计算实际最大延迟=" << recomputed_max_delay << std::endl;
		return false;
	}
	return true;
}


void file_output()//输出design.route.out
{
	std::ofstream out("design.route.out", std::ios::out);
	if (!out.is_open())
		return;
	vector<int> order1(numNet);
	iota(order1.begin(), order1.end(), 0);
	stable_sort(order1.begin(), order1.end(),
		[](int a, int b)
		{ return Global_net_delay[a] > Global_net_delay[b]; });
	//cout << Global_net_delay[order1[0]] << "j" << endl;

	out.setf(std::ios::fixed);
	out << std::setprecision(1);

	for (int idx : order1)
	{
		if (Global_net_delay[idx] <= 0.0)
			continue;

		auto& paths = Global[idx].path;
		auto& pdelay = Global[idx].path_delay;

		bool printed_header = false;
		vector<int> order2(pdelay.size());
		iota(order2.begin(), order2.end(), 0);
		stable_sort(order2.begin(), order2.end(),
			[idx](int a, int b)
			{ return Global[idx].path_delay[a] > Global[idx].path_delay[b]; });

		for (int j : order2)
		{
			if (pdelay[j] <= 0.0)
				continue;
			auto& edges = paths[j];
			if (edges.empty())
				continue;

			if (!printed_header)
			{
				out << "[net " << (idx + 1) << "]\n";
				printed_header = true;
			}

			out << '[';
			out << edges.front().first;
			for (int e = 0; e < edges.size(); ++e)
			{
				out << ',' << edges[e].second;
			}
			out << "] [" << pdelay[j] << "]\n";
		}
	}
	out.close();


	//通道重新组网输出
	std::ofstream matrixout("design.newtopo", std::ios::out);
	if (!matrixout.is_open())
		return;

	for (int x = 1; x <= numFPGA; x++)
	{
		matrixout << "F" << x << ": ";
		for (int y = 1; y < numFPGA; y++)
			matrixout << weight_matrix[x][y] + global_delta_weight_matrix[x][y] << ',';
		matrixout << weight_matrix[x][numFPGA] + global_delta_weight_matrix[x][numFPGA];
		matrixout << "\n";
	}
	matrixout.close();
}

bool SA_judge(double current_obj, double new_obj)
{
	if (new_obj < current_obj)
		return true;

	else if (new_obj >= current_obj)
	{
		double prob = exp(-(new_obj - current_obj) / T);
		double rand_value = rand() / double(RAND_MAX);
		if (rand_value <= prob)
			return true;
		else
			return false;
	}
	return false;
}


vector<vector<double>> current_cost(int** weight_matrix, int** nets_count_matrix) // 当前的成本，也就是选择每条边的成本
{
	vector<vector<double>> costn(numFPGA, vector<double>(numFPGA, MAXINT));
	for (int i = 0; i < numFPGA; i++) { costn[i][i] = 0; }
	for (int i = 1; i <= numFPGA; i++) {//计算下一次使用的成本 
		for (int j = i; j <= numFPGA; j++) {
			if (j == i || weight_matrix[i][j] + delta_weight_matrix[i][j] == 0) { continue; }
			int tdm_ratio = ceil8((double)(nets_count_matrix[i][j] + 1) / (weight_matrix[i][j] + delta_weight_matrix[i][j]));
			if (tdm_ratio > R_max) continue; // 最大TDM比率约束
			costn[i - 1][j - 1] = 30 + 0.7 * ceil8((double)(nets_count_matrix[i][j] + 1) / (weight_matrix[i][j] + delta_weight_matrix[i][j]));
			costn[j - 1][i - 1] = costn[i - 1][j - 1];
		}
	}
	return costn;
}

//*****************************************************************************************
// single_dijkstra（求单条路径用）
// 作用：在 net 上求 候选点到终点 的最短路径（用于AF或尾部补全）
// 输入：起点、终点、计算权重依据的跨通道net数量矩阵
// 输出：路径、路径延迟
//*****************************************************************************************
double single_dijkstra(int source_fpga, int sink_fpga, int** net_count_M, double& delay_value)
{
	int* best_path = new int[numFPGA]; //新建一条路径，用于存储新找的最优路径
	//int source_fpga = nodes_FPGA[source_node];//源点对应的FPGA
	//int sink_fpga = nodes_FPGA[sink_node]; //终点对应的FPGA

	if (source_fpga == sink_fpga) // 起点终点在相同FPGA里
	{
		delay_value = 0; //path.clear();
		return 0;
	}
	/*dijkstra*/
	double* dist = new double[numFPGA + 1];
	bool* visited = new bool[numFPGA + 1];
	int* prev = new int[numFPGA + 1];
	for (int x = 0; x < numFPGA + 1; x++)
	{
		if (x == source_fpga)
			dist[x] = 0;
		else
			dist[x] = (double)MAXINT;
		visited[x] = false;
		prev[x] = -1;
		if (x < numFPGA)
			best_path[x] = -1;
	}

	int min_cost_vertic = -1;
	for (int iter = 0; iter < numFPGA; iter++)
	{
		int u = -1;
		double min_cost = (double)MAXINT;
		for (int x = 1; x <= numFPGA; x++)
		{
			if (!visited[x] && dist[x] < min_cost)
			{
				min_cost = dist[x];
				u = x;
			}
		}

		if (u == sink_fpga)
			break;
		visited[u] = true;
		for (int v = 1; v <= numFPGA; v++)
		{
			if (weight_matrix[u][v] + delta_weight_matrix[u][v] == 0) continue;
			if (visited[v]) continue;
			int tmd_ratio = ceil8((double)(net_count_M[u][v] + 1) / (weight_matrix[u][v] + delta_weight_matrix[u][v]));
			if (tmd_ratio > R_max) continue; //判断最大TDM比率约束
			double delay = 30 + (0.7 * (double)tmd_ratio);
			if (dist[v] > dist[u] + delay)
			{
				dist[v] = dist[u] + delay;
				prev[v] = u;
			}
		}
	}

	int current_v = sink_fpga;
	int path_length = 0;
	while (current_v > 0)
	{
		best_path[path_length] = current_v;
		current_v = prev[current_v];
		path_length++;
	}

	//覆盖新路径
	//path.clear();
	//for (int x = 0; x < path_length - 1; x++)
	//{
	//	int u = best_path[x + 1];
	//	int v = best_path[x];
	//	path.push_back({ u,v });
	//}
	//reverse(path.begin(), path.end());

	delay_value = dist[sink_fpga];
	delete[] dist;
	delete[] visited;
	delete[] prev;
	delete[] best_path;
	return delay_value;
}

vector<pair<int, int>> single_dijkstra_1(int source_fpga, int sink_fpga, int** net_count_M)//找最短路径 直接覆盖
{
	int* best_path = new int[numFPGA]; // 新建一条路径，用于存储新找的最优路径

	if (source_fpga == sink_fpga) // 起点终点在相同FPGA里
	{
		return {};
	}
	/*dijkstra*/
	double* dist = new double[numFPGA + 1];
	bool* visited = new bool[numFPGA + 1];
	int* prev = new int[numFPGA + 1];
	for (int x = 0; x < numFPGA + 1; x++)
	{
		if (x == source_fpga)
			dist[x] = 0;
		else
			dist[x] = (double)MAXINT;
		visited[x] = false;
		prev[x] = -1;
		if (x < numFPGA)
			best_path[x] = -1;
	}

	int min_cost_vertic = -1;
	for (int iter = 0; iter < numFPGA; iter++)
	{
		int u = -1;
		double min_cost = (double)MAXINT;
		for (int x = 1; x <= numFPGA; x++)
		{
			if (!visited[x] && dist[x] < min_cost)
			{
				min_cost = dist[x];
				u = x;
			}
		}

		if (u == sink_fpga)
			break;
		visited[u] = true;
		for (int v = 1; v <= numFPGA; v++)
		{
			if (weight_matrix[u][v] + delta_weight_matrix[u][v] == 0)
				continue;
			if (visited[v])
				continue;
			int tmd_ratio = ceil8((double)(net_count_M[u][v] + 1) / (weight_matrix[u][v] + delta_weight_matrix[u][v]));
			if (tmd_ratio > R_max)
				continue; // 判断最大TDM比率约束
			double delay = 30 + (0.7 * (double)tmd_ratio);
			if (dist[v] > dist[u] + delay)
			{
				dist[v] = dist[u] + delay;
				prev[v] = u;
			}
		}
	}

	int current_v = sink_fpga;
	int path_length = 0;
	while (current_v > 0)
	{
		best_path[path_length] = current_v;
		current_v = prev[current_v];
		path_length++;
	}
	// 覆盖新路径
	vector<pair<int, int>>spath;
	for (int x = 0; x < path_length - 1; x++)
	{
		int u = best_path[x + 1];
		int v = best_path[x];
		spath.push_back({ u,v });
	}
	reverse(spath.begin(), spath.end());

	delete[] dist;
	delete[] visited;
	delete[] prev;
	delete[] best_path;
	return spath;
}

std::vector<int> single_dijkstra_withpath(int source_fpga, int sink_fpga, int** net_count_M, const std::vector<char>& forbid)
{
	std::vector<int> path_nodes;
	int* best_path = new int[numFPGA]; //新建一条路径，用于存储新找的最优路径

	//int source_fpga = nodes_FPGA[source_node];//源点对应的FPGA
	//int sink_fpga = nodes_FPGA[sink_node]; //终点对应的FPGA

	if (source_fpga == sink_fpga) // 起点终点相同
	{
		path_nodes.push_back(source_fpga);
		delete[] best_path;
		return path_nodes;
	}

	/*dijkstra*/
	double* dist = new double[numFPGA + 1];
	bool* visited = new bool[numFPGA + 1];
	int* prev = new int[numFPGA + 1];
	for (int x = 0; x < numFPGA + 1; x++)
	{
		if (x == source_fpga)
			dist[x] = 0;
		else
			dist[x] = (double)MAXINT;
		visited[x] = false;
		prev[x] = -1;
		if (x < numFPGA)
			best_path[x] = -1;
	}

	int min_cost_vertic = -1;
	for (int iter = 0; iter < numFPGA; iter++)
	{
		int u = -1;
		double min_cost = (double)MAXINT;
		for (int x = 1; x <= numFPGA; x++)
		{
			if (!visited[x] && dist[x] < min_cost)
			{
				min_cost = dist[x];
				u = x;
			}
		}

		if (u == sink_fpga)
			break;
		visited[u] = true;
		for (int v = 1; v <= numFPGA; v++)
		{
			if (!forbid.empty() && v < (int)forbid.size() && forbid[v] && v != source_fpga) continue;
			if (weight_matrix[u][v] + delta_weight_matrix[u][v] == 0) continue;
			if (visited[v]) continue;
			int tmd_ratio = ceil8((double)(net_count_M[u][v] + 1) / (weight_matrix[u][v] + delta_weight_matrix[u][v]));
			if (tmd_ratio > R_max) continue; //判断最大TDM比率约束
			double delay = 30 + (0.7 * (double)tmd_ratio);
			if (dist[v] > dist[u] + delay)
			{
				dist[v] = dist[u] + delay;
				prev[v] = u;
			}
		}
	}

	int current_v = sink_fpga;
	int path_length = 0;
	while (current_v > 0)
	{
		best_path[path_length] = current_v;
		current_v = prev[current_v];
		path_length++;
	}

	//覆盖新路径
	//path.clear();
	//for (int x = 0; x < path_length - 1; x++)
	//{
	//	int u = best_path[x + 1];
	//	int v = best_path[x];
	//	path.push_back({ u,v });
	//}
	//reverse(path.begin(), path.end());
	 // 输出为 [source, ..., sink]
	for (int i = path_length - 1; i >= 0; --i)
		path_nodes.push_back(best_path[i]);


	delete[] dist;
	delete[] visited;
	delete[] prev;
	delete[] best_path;
	return path_nodes;


}


void multi_dijkstra(int source_node, vector<int> sink_node, vector<vector<pair<int, int>>>& path) {
	int k = (int)sink_node.size();
	int n = numFPGA;
	path.assign(k, {});//初始化
	int source_fpga = nodes_FPGA[source_node];//源点，终点对应的FPGA
	vector<int> sinks_fpga(k);
	for (int i = 0; i < k; ++i) { sinks_fpga[i] = nodes_FPGA[sink_node[i]]; }
	unordered_set<int> fpga_s(sinks_fpga.begin(), sinks_fpga.end());//用于判断起点和终点是否全部搜索到；
	fpga_s.insert(k);
	vector<char> same_fpga(k, 0);//统计源点和终点在同一块fpga的情况
	int nums_same = 0;
	for (int i = 0; i < k; ++i) {
		if (sinks_fpga[i] == source_fpga) {
			same_fpga[i] = 1; nums_same++;
		} // 路径为空，不占跨 FPGA 边
		if (nums_same == k) { return; }//源点和终点都在同一块fpga,直接return；
	}
	// —— Dijkstra
	const double INF = MAXINT;
	vector<double> dist(n, INF);//都是0开始，后续使用的时候 注意索引
	vector<int> parent(n, -1);
	vector<char> vis(n, 0);
	dist[source_fpga - 1] = 0;
	vector<vector<double>>gCost = current_cost(weight_matrix, nets_count_matrix);
	for (int it = 0; it < n; it++) {
		int u = -1;
		double best = INF;
		for (int i = 0; i < n; i++) { if (!vis[i] && dist[i] < best) { best = dist[i]; u = i; } }
		if (u == -1) { break; }
		vis[u] = 1; fpga_s.erase(u + 1);
		// if(fpga_s.empty()){break;}//检查起点和终点是否都搜索完
		for (int v = 0; v < n; ++v) {
			double w = gCost[u][v];
			if (w >= INF) { continue; }
			if (dist[v] > dist[u] + w) {
				dist[v] = dist[u] + w;
				parent[v] = u;
			}
		}
	}
	// 逐个终点回溯路径
	for (int i = 0; i < k; ++i) {
		if (same_fpga[i]) { path[i].clear(); continue; }
		int t = sinks_fpga[i] - 1;
		if (dist[t] >= INF) {
			path[i].clear(); // 不可达
			continue;
		}
		vector<pair<int, int>> edges_rev;
		int v = t;
		while (v != source_fpga - 1)
		{
			int u = parent[v];
			if (u < 0) {
				edges_rev.clear();
				break;
			}
			edges_rev.push_back({ u + 1, v + 1 });//索引从0开始，这里调整为1；
			v = u;
		}
		reverse(edges_rev.begin(), edges_rev.end());
		path[i] = std::move(edges_rev);
	}
}


void calculate_su(vector<Net>& S) //计算解S的delay
{
	for (int i = 0; i < numNet; i++)
	{
		net_delay[i] = 0;//net的整体delay初始化为0；
		int sink_num = S[i].sink_num;
		S[i].path_delay.assign(sink_num, 0);//分配空间，同时初始化0
		auto& path_delay = S[i].path_delay;
		auto& path = S[i].path;
		for (int j = 0; j < sink_num; j++)
		{
			if (path[j].size() == 0) { continue; }
			for (auto edge : path[j])
			{
				if (edge.first != edge.second) { path_delay[j] += 30 + 0.7 * ceil8((double)nets_count_matrix[edge.first][edge.second] / (weight_matrix[edge.first][edge.second] + delta_weight_matrix[edge.first][edge.second])); }
			}
			if (path_delay[j] > net_delay[i]) { net_delay[i] = path_delay[j]; }
		}
	}
}


void calculate_temp_sol(vector<Net>& S, int** delta_nets_count_M, double& obj) //计算解S的delay
{
	double* temp_net_delay = new double[numNet];

	for (int i = 0; i < numNet; i++)
	{
		temp_net_delay[i] = 0;//net的整体delay初始化为0；
		int sink_num = S[i].sink_num;
		auto& path = S[i].path;
		for (int j = 0; j < sink_num; j++)
		{
			double each_path_delay = 0;
			if (path[j].size() == 0) { continue; }
			for (auto edge : path[j])
			{
				if (edge.first != edge.second)
				{
					if ((weight_matrix[edge.first][edge.second] + delta_nets_count_M[edge.first][edge.second]) > 0)
						each_path_delay += 30 + 0.7 * ceil8((double)nets_count_matrix[edge.first][edge.second] / (weight_matrix[edge.first][edge.second] + delta_nets_count_M[edge.first][edge.second]));
					else
						cout << "tunnel removed but route needed! ";
				}
			}
			if (each_path_delay > temp_net_delay[i]) { temp_net_delay[i] = each_path_delay; }
		}
	}

	double max_sol_delay = 0;
	for (int i = 0; i < numNet; i++)
		if (temp_net_delay[i] > max_sol_delay)
			max_sol_delay = temp_net_delay[i];

	obj = max_sol_delay;
	delete[] temp_net_delay;
}


//复制解，将解S2的内容复制给S1
void copy_solution(vector<Net>& S1, vector<Net> S2, int** S1_delta_weight_M, int** S2_delta_weight_M)
{
	if ((int)S1.size() != (int)S2.size())
	{
		cout << "Two solutions' length is not the same!!!!";
		exit(-1);
	}
	else if ((int)S1.size() != numNet)
	{
		cout << "Solution's length do not match the netNum!!!!";
		exit(-1);
	}

	for (int x = 0; x < numNet; x++)
	{
		S1[x].sink_num = S2[x].sink_num;
		S1[x].path = S2[x].path;
		S1[x].path_delay = S2[x].path_delay;
		S1[x].sink_nodes = S2[x].sink_nodes;
		S1[x].source_node = S2[x].source_node;
		S1[x].path_map = S2[x].path_map;
	}

	for (int x = 0; x < numFPGA + 1; x++) //复制通道变化
		for (int y = 0; y < numFPGA + 1; y++)
			S1_delta_weight_M[x][y] = S2_delta_weight_M[x][y];


	calculate_su(S1);
}

void initialize_solution() {//初始化
	for (int n = 0; n < numNet; n++) {
		auto& net = Current[n];
		multi_dijkstra(net.source_node, net.sink_nodes, net.path);
		// 更新累计使用次数
		pair<int, int> arc = {};
		for (auto& edges : net.path)
		{
			for (auto& e : edges)
			{
				int u = e.first, v = e.second;
				if (u > v)
					arc = { v,u };
				else
					arc = { u, v };
				if (net.path_map.find(arc) != net.path_map.end()) //找到边
					net.path_map[arc]++;
				else if (net.path_map.find(arc) == net.path_map.end()) //未找到边
					net.path_map[arc] = 1;
			}
		}
		for (auto& p : net.path_map)
		{
			int a = p.first.first, b = p.first.second;
			++nets_count_matrix[a][b];
			++nets_count_matrix[b][a];
		}
	}

	calculate_su(Current); //初始化后计算delay
	for (int x = 0; x < numNet; x++)
	{
		if (net_delay[x] > Global_delay)
		{
			Global_delay = net_delay[x];
		}
	}
	std::copy(net_delay, net_delay + numNet, Global_net_delay);
	copy_solution(Global, Current, global_delta_weight_matrix, delta_weight_matrix);
}


//*****************************************************************************************
// expand_beam_layer
// 作用：递归地进行束搜索，“单层扩展”——对每个状态枚举下一跳，计算 f = OA+AF，OB+BF，OC+CF并在生成候选前缀后：
// 输入：
// 输出：topk_candidates（按f降序截断到beamK，从o到A、B、C的路径）；
//      
//*****************************************************************************************
void expand_beam_layer_limit_kdepth(int source_fpga, int sink_fpga, int** net_count_M, int& farm_index, vector<pair<int, int>>& new_path, vector<pair<int, int>>& farm_son_index, vector<int>& on_path, int depth) {

	// 使用类似树的结构将路径存进new_delay; 叶节点是包含sink_fpga;on_path用于去环,farm_son_index,在父节点记录子节点索引的数组。
	if (source_fpga == sink_fpga) { return; }
	if (depth >= Max_kdepth) {
		vector<pair<int, int>>cur_path;
		cur_path = single_dijkstra_1(source_fpga, sink_fpga, net_count_M);
		if (cur_path.size() <= 0) { return; }
		if (cur_path[0].first != source_fpga) { cout << "[the current path is error]"; return; }
		int prev = farm_index;
		for (auto& x : cur_path) {
			new_path.push_back(x);
			farm_son_index.resize(new_path.size(), { 0, 0 });
			int child = (int)new_path.size() - 1;
			farm_son_index[prev] = { child, child };
			prev = child;
		}
		return;
	}
	vector<int> sucessor_vertics;
	vector<double> sucessor_delay; // 输入点+后继到终点的总延时
	int index = 0;
	for (int x = 1; x <= numFPGA; x++) // 寻找后继节点
	{
		if (find(on_path.begin(), on_path.end(), x) != on_path.end())
		{
			continue;
		}
		if (x == source_fpga)
		{
			continue;
		}
		if (weight_matrix[source_fpga][x] + delta_weight_matrix[source_fpga][x] == 0)
		{
			continue;
		}
		sucessor_vertics.push_back(x);
		int tmd_ratio = ceil8((double)(net_count_M[source_fpga][x] + 1) / (weight_matrix[source_fpga][x] + delta_weight_matrix[source_fpga][x]));
		if (tmd_ratio > R_max)
		{
			sucessor_vertics.pop_back();
			continue;
		} // 判断最大TDM比率约束
		double edge_delay = 30 + (0.7 * (double)tmd_ratio); // 输入点到其后继节点延时值
		sucessor_delay.push_back(edge_delay);

		double path_delay = (double)MAXINT;
		if (x != sink_fpga)
		{
			single_dijkstra(x, sink_fpga, net_count_M, path_delay); // 从后继节点到终点的dijkstra路径延时值
			sucessor_delay[index] += path_delay;
		}
		index++;
	}
	topk_asc_vector(sucessor_delay, sucessor_vertics, BEAMWIDTH);
	// qsort_vector(sucessor_delay, sucessor_vertics, 0, index - 1);		   // 对 f = OA+AF排序
	int retain_vertics = SDF_MIN(BEAMWIDTH, (int)sucessor_vertics.size()); // 确定束搜索数量
	if (retain_vertics <= 0)
	{
		return;
	}
	sucessor_delay.resize(retain_vertics);
	sucessor_vertics.resize(retain_vertics);
	// 将路径存进树中,随机选一个存
	// int idx = rand() % ((int)sucessor_vertics.size());
	// int chosen_vertex = sucessor_vertics[idx];
	// new_path.push_back({source_fpga, chosen_vertex});

	// 全部存取
	for (auto s_index : sucessor_vertics)
	{
		new_path.push_back({ source_fpga, s_index });
	}
	++depth;
	// 子节点的索引
	farm_son_index.resize(new_path.size(), { 0, 0 });
	farm_son_index[farm_index] = { new_path.size() - retain_vertics, new_path.size() - 1 };
	// cout<<"the size:"<<new_path.size()<<endl;
	// 子节点确定
	for (int sss = 0; sss < retain_vertics; sss++)
	{
		int son_index = new_path.size() - retain_vertics + sss;
		vector<int> new_on_path = on_path;
		new_on_path.push_back(sucessor_vertics[sss]);
		expand_beam_layer_limit_kdepth(sucessor_vertics[sss], sink_fpga, net_count_M, son_index, new_path, farm_son_index, new_on_path, depth);
	}
	on_path.clear();
}

void dfs_fill_path(int t_index, vector<pair<int, int>> curr, vector<vector<pair<int, int>>>& k_paths, vector<pair<int, int>>& t_paths, vector<pair<int, int>>& farm_son) {	//dfs填路径
	bool pushed = false;
	if (t_index) {
		curr.push_back({ t_paths[t_index].first, t_paths[t_index].second });
		pushed = true;
	}
	int l = farm_son[t_index].first, r = farm_son[t_index].second;
	if (l == 0 && r == 0) { k_paths.push_back(curr); }
	else { for (int tt = l; tt <= r; tt++) { dfs_fill_path(tt, curr, k_paths, t_paths, farm_son); } }
	if (pushed) { curr.pop_back(); }

}

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
void re_route_path(int net_id, int sink_index) //邻域1
{
	// ---------- (0) 数据结构与参数设置  ----------
	auto target_net = Current[net_id]; //最差路径所在的net
	int source_node = target_net.source_node; //net中的起点
	int sink_node = target_net.sink_nodes[sink_index]; // net中的终点
	vector<pair<int, int>> new_path;//c存储路径的树
	vector<pair<int, int>> farm_son_index;//存储子节点的索引

	int source_fpga = nodes_FPGA[source_node]; // 起点、终点对应的FPGA
	int sink_fpga = nodes_FPGA[sink_node];

	auto target_path = target_net.path[sink_index]; //最差路径

	vector<vector<pair<int, int>>> candidate_paths; // Top-K候选路径
	vector<double> candidate_paths_delay; //Top-K候选路径对应的延迟
	candidate_paths.assign(BEAMWIDTH, {});
	candidate_paths_delay.assign(BEAMWIDTH, (double)MAXINT);
	int** temp_nets_count_matrix = new int* [numFPGA + 1]; // 搜索过程中调整后的通道数量
	for (int x = 0; x < numFPGA + 1; x++) {
		temp_nets_count_matrix[x] = new int[numFPGA + 1];
		for (int y = 0; y < numFPGA + 1; y++)
			temp_nets_count_matrix[x][y] = nets_count_matrix[x][y];
	}
	// ---------- (1) 假设取消原路径，调整通道数量  ----------
	pair <int, int> arc = {};
	for (auto edges : target_path) //遍历路径中的所有边
	{
		int a = edges.first;
		int b = edges.second;
		if (a > b) { arc = { b, a }; }
		else { arc = { a, b }; }

		if (target_net.path_map.find(arc) == target_net.path_map.end()) //在net中查找此边的使用次数
		{
			cout << "1path_map of Net does not record an arc!!!"; exit(-1);
		}
		else {
			if (target_net.path_map[arc] == 1) //该边在整个net中仅使用一次，取消原路径后，此net不再跨越此边，对应记录减一
			{
				temp_nets_count_matrix[a][b]--; temp_nets_count_matrix[b][a]--;
			}
			target_net.path_map[arc]--;
		}
	}

	// ---------- (2)递归进行束搜索扩展，生成Top-K候选路径 ----------

	vector<int> sucessor_vertics;
	vector<double> sucessor_delay; //输入点+后继到终点的总延时
	int index = 0;
	for (int x = 1; x <= numFPGA; x++) //寻找后继节点
	{
		if (weight_matrix[source_fpga][x] + delta_weight_matrix[source_fpga][x] == 0) continue;
		sucessor_vertics.push_back(x);

		int tmd_ratio = ceil8((double)(temp_nets_count_matrix[source_fpga][x] + 1) / (weight_matrix[source_fpga][x] + delta_weight_matrix[source_fpga][x]));
		if (tmd_ratio > R_max) {
			sucessor_vertics.pop_back();
			continue;
		}													// 判断最大TDM比率约束
		double edge_delay = 30 + (0.7 * (double)tmd_ratio); //输入点到其后继节点延时值
		sucessor_delay.push_back(edge_delay);

		double path_delay = (double)MAXINT;
		if (x != sink_fpga) {
			single_dijkstra(x, sink_fpga, temp_nets_count_matrix, path_delay); //从后继节点到终点的dijkstra路径延时值
			sucessor_delay[index] += path_delay;
		}
		index++;
	}
	topk_asc_vector(sucessor_delay, sucessor_vertics, BEAMWIDTH);
	// qsort_vector(sucessor_delay, sucessor_vertics, 0, index - 1); //对 f = OA+AF排序

	int retain_vertics = SDF_MIN(BEAMWIDTH, (int)sucessor_vertics.size()); // 确定束搜索数量
	if (retain_vertics <= 0)
	{
		for (int x = 0; x < numFPGA + 1; x++) { delete[] temp_nets_count_matrix[x]; }
		delete[] temp_nets_count_matrix;
		return;
	}
	vector<vector<pair<int, int>>> top_k_paths; //最好的k条路径
	new_path.resize(retain_vertics + 1, { 0,0 }); farm_son_index.push_back({ 1,retain_vertics });
	vector<int> on_path = { source_fpga };
	int depth = 1;
	new_path[0] = { -1,-1 };//树顶初始为-1
	for (int x = 0; x < retain_vertics; x++) {
		new_path[x + 1] = { source_fpga, sucessor_vertics[x] };
	}
	for (int x = 0; x < retain_vertics; x++) {
		int farm_id = x + 1;
		on_path.clear();
		on_path.push_back(source_fpga); on_path.push_back(sucessor_vertics[x]);
		expand_beam_layer_limit_kdepth(sucessor_vertics[x], sink_fpga, temp_nets_count_matrix, farm_id, new_path, farm_son_index, on_path, depth);
	}

	vector<std::pair<int, int>> cur;
	dfs_fill_path(0, cur, top_k_paths, new_path, farm_son_index);

	vector<int> topk_check;//收集满足要求的路径，成功连接到sink_fpga
	for (int i = 0; i < top_k_paths.size(); i++) {
		bool ok = true;
		unordered_set<int> checkcf;
		if (top_k_paths[i].back().second != sink_fpga) { continue; }
		for (int j = 0; j < top_k_paths[i].size(); j++) {
			if (checkcf.find(top_k_paths[i][j].second) == checkcf.end()) {
				checkcf.insert(top_k_paths[i][j].second);
			}
			else
			{
				checkcf.clear();
				ok = false;
				break;
			}
		}
		if (!ok) { continue; }
		topk_check.push_back(i);
	}
	if (topk_check.size() == 0) //说明无合法路径
	{
		for (int x = 0; x < numFPGA + 1; x++) { delete[] temp_nets_count_matrix[x]; }
		delete[] temp_nets_count_matrix;
		return;
	}

	// ---------- (3) 在Top-K中随机选一条并更新 -----------
	// random_pick_from_topk(topk_candidates, chosen);
	//static thread_local std::mt19937 rng(std::random_device{}());
	//std::uniform_int_distribution<size_t> dist(0, topk_check.size() - 1);
	//size_t idx = dist(rng);

	int idx = rand() % ((int)topk_check.size());
	int chosen_id = topk_check[idx];
	//恢复假设路径
	target_net.path[sink_index] = top_k_paths[chosen_id];
	pair<int, int> arc_add_2 = {};
	for (auto edges : target_path) // 遍历路径中的所有边
	{
		int a = edges.first; int b = edges.second;
		if (a > b) { arc_add_2 = { b, a }; }
		else { arc_add_2 = { a, b }; }

		if (target_net.path_map.find(arc_add_2) == target_net.path_map.end()) // 在net中查找此边的使用次数
		{
			temp_nets_count_matrix[a][b]++;
			temp_nets_count_matrix[b][a]++;
			target_net.path_map[arc_add_2] = 1;
		}
		else { target_net.path_map[arc_add_2]++; }
	}
	//计算net的延迟
	double temp_net_delay = 0; // net的整体delay初始化为0；
	int temp_sink_num = target_net.sink_num;
	target_net.path_delay.assign(temp_sink_num, 0); // 分配空间，同时初始化0
	auto& path_delay = target_net.path_delay;
	auto& path = target_net.path;
	for (int j = 0; j < temp_sink_num; j++)
	{
		if (path[j].size() == 0)
		{
			continue;
		}
		for (auto edge : path[j])
		{
			if (edge.first != edge.second)
			{
				path_delay[j] += 30 + 0.7 * ceil8((double)temp_nets_count_matrix[edge.first][edge.second] / (weight_matrix[edge.first][edge.second] + delta_weight_matrix[edge.first][edge.second]));
			}
		}
		if (path_delay[j] > temp_net_delay)
		{
			temp_net_delay = path_delay[j];
		}
	}
	double real_net_delay = net_delay[net_id];
	//确保无重复元素：




	//模拟退火
	if (!SA_judge(real_net_delay, temp_net_delay))
	{
		for (int x = 0; x < numFPGA + 1; x++) { delete[] temp_nets_count_matrix[x]; }
		delete[] temp_nets_count_matrix;
		return;
	}


	// apply_update_and_refresh(worst, chosen);
		//实际取消原路径：取消原路径，调整通道数量
	pair<int, int> arc_change = {};
	for (auto edges : Current[net_id].path[sink_index]) // 遍历路径中的所有边
	{
		int a = edges.first; int b = edges.second;
		if (a > b) { arc_change = { b, a }; }
		else { arc_change = { a, b }; }

		if (Current[net_id].path_map.find(arc_change) == Current[net_id].path_map.end()) // 在net中查找此边的使用次数
		{
			cout << "2path_map of Net does not record an arc!!!";
			exit(-1);
		}
		else {
			if (Current[net_id].path_map[arc_change] == 1) // 该边在整个net中仅使用一次，取消原路径后，此net不再跨越此边，对应记录减一
			{
				nets_count_matrix[a][b]--; nets_count_matrix[b][a]--;
			}
			Current[net_id].path_map[arc_change]--;
			if (Current[net_id].path_map[arc_change] == 0) { Current[net_id].path_map.erase(arc_change); }
		}
	}
	//修改原路径同时，调整通道数量
	Current[net_id].path[sink_index] = top_k_paths[chosen_id];
	pair<int, int> arc_add = {};
	for (auto edges : Current[net_id].path[sink_index]) // 遍历路径中的所有边
	{
		int a = edges.first;
		int b = edges.second;
		if (a > b) { arc_add = { b, a }; }
		else { arc_add = { a, b }; }

		if (Current[net_id].path_map.find(arc_add) == Current[net_id].path_map.end()) // 在net中查找此边的使用次数
		{
			nets_count_matrix[a][b]++; nets_count_matrix[b][a]++;
			Current[net_id].path_map[arc_add] = 1;
		}
		else { Current[net_id].path_map[arc_add]++; }
	}
	calculate_su(Current);

	double all_net_delay = 0;
	for (int x = 0; x < numNet; x++)
		if (net_delay[x] > all_net_delay)
			all_net_delay = net_delay[x];

	//if (!check_result(Current)) 
	//{
	//	cout << "the current is error" << endl;
	//	//copy_solution(Global, Current);
	//	//file_output();
	//	exit(-1);
	//}

	//double check_max_delay = 0;
	//for (int x = 0; x < numNet; x++)
	//	if (net_delay[x] > check_max_delay)
	//		check_max_delay = net_delay[x];
	//if (abs(check_max_delay - temp_net_delay) > 1e-3)
	//{
	//	cout << "unmatch in N1: reported " << temp_net_delay << " actual " << check_max_delay;
	//	exit(-1);
	//}


	if (all_net_delay < Global_delay && abs(all_net_delay - Global_delay)>1e-3)
	{
		copy_solution(Global, Current, global_delta_weight_matrix, delta_weight_matrix);
		std::copy(net_delay, net_delay + numNet, Global_net_delay);
		cout << "Global improved in N1: from " << Global_delay << " to " << all_net_delay << " time: " << (clock() - beginTime) / CLOCKS_PER_SEC << endl;
		Global_delay = all_net_delay;

		//bool check = check_result(Global, global_delta_weight_matrix);
		//if (!check) exit(-1);

	}

	for (int x = 0; x < numFPGA + 1; x++) { delete[] temp_nets_count_matrix[x]; }
	delete[] temp_nets_count_matrix;
}


//*****************************************************************************************
// neighbor_replan2——邻域2
// 输入：
//      net_id          —— 目标net的索引（指定待重路由的网络）
//      sink_index      —— 该net中目标终点的索引（对应一条具体的源-汇路径）
//      worst_delay_in  —— 当前最差路径的延时
//      shortest_prob   —— 走最短路径的概率（默认0.90）
// 输出：
//      bool型返回值：
//          true  —— 成功产生新路径并被接受（更新Current、nets_count_matrix、path_map）
//          false —— 新路径被拒绝或不可行（不修改原有解）
//*****************************************************************************************

void neighbor_replan2(int net_id, int sink_index)
{
	// ---------- (0) 数据结构与参数设置 ----------
	auto& target_net = Current[net_id];
	int   source_node = target_net.source_node;
	int   sink_node = target_net.sink_nodes[sink_index];

	int source_fpga = nodes_FPGA[source_node];
	int sink_fpga = nodes_FPGA[sink_node];

	const auto& target_path = target_net.path[sink_index]; // 原最差路径（边序列）

	// 临时 nets_count：搜索过程中使用（不影响全局）
	int** temp_nets_count_matrix = new int* [numFPGA + 1];
	for (int i = 0; i <= numFPGA; ++i) {
		temp_nets_count_matrix[i] = new int[numFPGA + 1];
		for (int j = 0; j <= numFPGA; ++j)
			temp_nets_count_matrix[i][j] = nets_count_matrix[i][j];
	}

	// ---------- (1) 假设取消原路径，调整通道数量（仅动临时矩阵） ----------

	std::pair<int, int> arc;
	for (const auto& e : target_path) {
		int a = e.first, b = e.second;
		if (a > b) arc = { b, a }; else arc = { a, b };
		auto it = target_net.path_map.find(arc);
		if (it == target_net.path_map.end()) {
			std::cerr << "path_map missing arc (" << arc.first << "," << arc.second << ")\n";
			// 清理内存后返回失败
			for (int r = 0; r <= numFPGA; ++r) delete[] temp_nets_count_matrix[r];
			delete[] temp_nets_count_matrix;
			return;
		}
		if (it->second == 1) {
			// 该 net 取消这条边的占用（无向对称）
			temp_nets_count_matrix[arc.first][arc.second]--;
			temp_nets_count_matrix[arc.second][arc.first]--;
		}
		// 注意：不修改 target_net.path_map 里的计数
	}
	// 可选的简单去环
	std::vector<char> on_path(numFPGA + 1, 0);
	std::vector<std::pair<int, int>> new_edges;     // 新路径（边序列）
	int cur = source_fpga;
	on_path[cur] = 1;



	// 收集该 net 除当前 sink 之外已使用的无向边（每网一次）
	std::unordered_set<std::pair<int, int>, PairHash> used_by_this_net_elsewhere;

	for (int sj = 0; sj < target_net.sink_num; ++sj) {
		if (sj == sink_index) continue;

		for (const auto& e : target_net.path[sj]) {
			int a = e.first;
			int b = e.second;
			if (a == b) continue;
			if (a > b) std::swap(a, b); // 无向化，确保 (a,b) 和 (b,a) 等价
			used_by_this_net_elsewhere.insert({ a, b });
		}
	}

	// ---------- (2) 生成当前起点->终点的 Dijkstra 最短路径（缓存整条路径） ----------
	std::vector<int> cached_nodes; // [cur, ..., sink]
	cached_nodes = single_dijkstra_withpath(source_fpga, sink_fpga, temp_nets_count_matrix, on_path);
	size_t cached_pos = 0; // cached_nodes[cached_pos] == 当前节点

	// ---------- (3) 逐步前进：先决定是否走最短路；仅在改走非最短路后重算 Dijkstra ----------


	for (int step = 0; step < numFPGA && cur != sink_fpga; ++step) {
		// 从缓存读取“最短路下一跳”（不做任何计算）
		int next_on_shortest = cached_nodes[cached_pos + 1];

		// 掷硬币：以 shortest_prob 概率走最短路；以 (1 - shortest_prob) 概率改走非最短路合法后继
		bool use_shortest = (rand() % 100) < (int)(SHORTESTROB * 100.0);
		int chosen = next_on_shortest;
		bool turned = false;

		if (!use_shortest) {
			// 仅枚举“非最短路后继”的合法集合（满足 phys>0 且 ceil8((nets+1)/phys) <= R_max，且不回环）
			std::vector<int> alt;
			for (int v = 1; v <= numFPGA; ++v) {
				if (v == cur || v == next_on_shortest) continue;
				if (on_path[v]) continue;
				int phys = weight_matrix[cur][v] + delta_weight_matrix[cur][v];
				if (phys <= 0) continue;
				int tdm_ratio = ceil8((double)(temp_nets_count_matrix[cur][v] + 1) / (double)phys);
				if (tdm_ratio > R_max) continue;
				alt.push_back(v);
			}
			if (!alt.empty()) {
				chosen = alt[rand() % alt.size()];
				turned = true;
			}
			else {
				// 没有可替代后继，只能仍走最短路
				chosen = next_on_shortest;
				use_shortest = true;
			}
		}


		// 追加一步并前进
		new_edges.emplace_back(cur, chosen);
		cur = chosen;
		on_path[cur] = 1;

		if (cur == sink_fpga) break;

		if (turned) {
			// 改走了非最短路：从新位置重新生成最短路缓存
			cached_nodes.clear();
			cached_nodes = single_dijkstra_withpath(cur, sink_fpga, temp_nets_count_matrix, on_path);
			cached_pos = 0;
		}
		else {
			// 沿用原缓存：推进指针
			++cached_pos;
		}
	}


	// ---------- (4) 计算“候选路径”的延时（只算这条路径；与 calculate_su 口径一致） ----------

	double d = 0.0;
	std::unordered_set<std::pair<int, int>, PairHash> once_in_candidate; // 候选路径内去重

	for (const auto& e : new_edges) {
		int u = e.first;
		int v = e.second;
		if (u == v) continue;

		int a = u, b = v;
		if (a > b) std::swap(a, b); // 无向化
		std::pair<int, int> edge = { a, b };

		// 候选路径内去重
		if (!once_in_candidate.insert(edge).second)
			continue;


		int phys = weight_matrix[a][b] + delta_weight_matrix[a][b];
		int base = temp_nets_count_matrix[a][b]; // “假撤销旧路”后的跨网数
		int inc = used_by_this_net_elsewhere.count(edge) ? 0 : 1;

		int tdm = ceil8((double)(base + inc) / (double)phys);
		d += 30.0 + 0.7 * (double)tdm;
	}

	double candidate_delay = d;

	double current_obj = net_delay[net_id];



	if (!SA_judge(current_obj, candidate_delay))
	{
		// 不接受：不改 Current，清理临时资源后返回 false
		for (int r = 0; r <= numFPGA; ++r) delete[] temp_nets_count_matrix[r];
		delete[] temp_nets_count_matrix;
		return;
	}

	// ---------- (6) 被接受：按 re_route_path 口径提交（先撤旧、再加新） ----------
	// 6.1 撤销旧路径（path_map--；当从1->0时 nets_count_matrix 对称-- 并擦除键）
	std::pair<int, int> arc_change;
	for (const auto& e : Current[net_id].path[sink_index]) {
		int a = e.first, b = e.second;
		if (a > b) arc_change = { b, a }; else arc_change = { a, b };
		auto it = Current[net_id].path_map.find(arc_change);
		if (it == Current[net_id].path_map.end()) {
			std::cout << "2path_map of Net does not record an arc!!!";
			for (int r = 0; r <= numFPGA; ++r) delete[] temp_nets_count_matrix[r];
			delete[] temp_nets_count_matrix;
			exit(-1);
		}
		else {
			if (it->second == 1) {
				nets_count_matrix[a][b]--;
				nets_count_matrix[b][a]--;
				Current[net_id].path_map.erase(it);
			}
			else {
				it->second--;
			}
		}
	}



	// 2) 覆盖写入新路径
	Current[net_id].path[sink_index] = new_edges; //（或 top_k_paths[chosen_id]，按你的场景）

	// 3) 新路径生效：path_map++；当从0增到1时，nets_count_matrix 对称++
	{
		std::pair<int, int> arc_add;
		for (const auto& edges : Current[net_id].path[sink_index]) { // 新路径的每条边
			int a = edges.first, b = edges.second;
			if (a > b) arc_add = { b, a }; else arc_add = { a, b };

			auto it = Current[net_id].path_map.find(arc_add);
			if (it == Current[net_id].path_map.end()) {      // 0 -> 1：全局跨边计数对称++
				nets_count_matrix[a][b]++;
				nets_count_matrix[b][a]++;
				Current[net_id].path_map[arc_add] = 1;
			}
			else {
				it->second++;                                 // 已存在：直接累加
			}
		}
	}


	// 6.4 回写该路径延时与该 net 的 net_delay
	Current[net_id].path_delay[sink_index] = candidate_delay;
	double nd = 0.0;
	for (double d : Current[net_id].path_delay) nd = std::max(nd, d);
	net_delay[net_id] = nd;


	calculate_su(Current);

	// ——  校验同一条路径重算的延时是否一致（用于测试，可注释）
	//double recomputed_path = Current[net_id].path_delay[sink_index];
	//if (recomputed_path != candidate_delay) {
	//	std::cerr << "[CHK] path_delay mismatch after recompute: "
	//		<< "expected=" << candidate_delay
	//		<< " got=" << recomputed_path
	//		<< " (net=" << net_id << ", sink=" << sink_index << ")\n";
	//	exit(-1);
	//}

	//double check_max_delay = 0;
	//for (int x = 0; x < numNet; x++)
	//	if (net_delay[x] > check_max_delay)
	//		check_max_delay = net_delay[x];


	//if (abs(candidate_delay - check_max_delay) > 1e-3)
	//{
	//	cout << "unmatch in N2: reported " << candidate_delay << " actual " << check_max_delay;
	//	exit(-1);
	//}
	double all_net_delay = 0;
	for (int x = 0; x < numNet; x++)
		if (net_delay[x] > all_net_delay)
			all_net_delay = net_delay[x];

	if (all_net_delay < Global_delay && abs(all_net_delay - Global_delay)>1e-3)
	{
		cout << "Global improved in N2: from " << Global_delay << " to " << all_net_delay << " time: " << (clock() - beginTime) / CLOCKS_PER_SEC << endl;
		copy_solution(Global, Current, global_delta_weight_matrix, delta_weight_matrix);
		std::copy(net_delay, net_delay + numNet, Global_net_delay);
		Global_delay = all_net_delay;

		//bool check = check_result(Global, global_delta_weight_matrix);
		//if (!check) exit(-1);
	}


	// 释放临时矩阵
	for (int r = 0; r <= numFPGA; ++r) delete[] temp_nets_count_matrix[r];
	delete[] temp_nets_count_matrix;

}

void choose_worst_path(vector<Net> S, int& net_id, int& sink_id)
{
	int n = 1;
	int* net_delay_record = new int[numNet];
	vector<vector <int>> max_nets_set;
	vector<vector<int>> max_sinks_set;
	vector<double> top_net_delays;
	vector<double> top_path_delays;
	max_nets_set.assign(n, {});
	max_sinks_set.assign(n, {});


	for (int x = 0; x < numNet; x++)
		net_delay_record[x] = net_delay[x];

	double max_net_delay = 0, max_path_delay = 0, current_delay = 0;

	for (int i = 0; i < n; i++)
	{
		max_net_delay = 0; max_path_delay = 0;
		for (int x = 0; x < numNet; x++)
		{

			if (net_delay_record[x] > max_net_delay)
				max_net_delay = net_delay_record[x];

			if (net_delay_record[x] > current_delay)
				current_delay = net_delay_record[x];

			for (int y = 0; y < S[x].sink_num; y++)
			{
				if (S[x].path_delay[y] > max_path_delay && net_delay_record[x] >= 0)
				{
					max_path_delay = S[x].path_delay[y];
					max_nets_set[i].clear();
					max_nets_set[i].push_back(x);
					max_sinks_set[i].clear();
					max_sinks_set[i].push_back(y);
				}
				else if (abs(S[x].path_delay[y] - max_path_delay) < 1e-3 && net_delay_record[x] >= 0)
				{
					max_nets_set[i].push_back(x);
					max_sinks_set[i].push_back(y);
				}

			}
		}
		top_net_delays.push_back(max_net_delay);
		top_path_delays.push_back(max_path_delay);

		for (int x = 0; x < numNet; x++)
		{
			if (net_delay_record[x] - max_net_delay < 1e-3 && net_delay_record[x] - max_net_delay >= 0)
				net_delay_record[x] = -1.0;
		}

	}

	int net_idx = rand() % n;
	int path_idx = rand() % (int)max_sinks_set[net_idx].size();
	net_id = max_nets_set[net_idx][path_idx];
	sink_id = max_sinks_set[net_idx][path_idx];

	delete[] net_delay_record;
}


void random_rule_add_tunnel_0(int net_index, int sink_index, int add_tunnel_num, int& added_count, int* temp_fpga_weight, int** temp_delta_weight_matrix, vector<pair<int, int>>& arc_record)
{
	int source_node = Current[net_index].source_node; //net中的起点
	int sink_node = Current[net_index].sink_nodes[sink_index]; // net中的终点
	int source_fpga = nodes_FPGA[source_node]; // 起点、终点对应的FPGA
	int sink_fpga = nodes_FPGA[sink_node];
	auto max_delay_path = Current[net_index].path[sink_index]; //最差路径
	bool* reach_max_weight = new bool[numFPGA + 1];
	pair<int, int> max_delay_arc = {}; //最差路径的最差边
	vector<pair<int, int>> max_nets_arc; //整体跨net数量最多边

	double arc_max_delay = 0;
	int arc_max_nets = 0;
	for (auto arc : max_delay_path)
	{
		double TDM_ratio = ceil8((double)nets_count_matrix[arc.first][arc.second] / (weight_matrix[arc.first][arc.second] + delta_weight_matrix[arc.first][arc.second]));
		if (TDM_ratio > R_max)
		{
			cout << "Input path arc (" << arc.first << ", " << arc.second << ") over TDM max ratio!" << endl;
			cout << "nets count: " << nets_count_matrix[arc.first][arc.second] << endl;
			cout << "tunnel count: " << weight_matrix[arc.first][arc.second] + delta_weight_matrix[arc.first][arc.second] << endl;
			exit(-1);
		}
		double arc_delay = 30 + 0.7 * TDM_ratio;
		if (arc_delay > arc_max_delay)
		{
			arc_max_delay = arc_delay;
			max_delay_arc = arc;
		}
	}

	for (int x = 0; x < numFPGA + 1; x++)
	{
		if (temp_fpga_weight[x] == FPGA_max_weight[x])
			reach_max_weight[x] = true;
		else
			reach_max_weight[x] = false;

		for (int y = 0; y < numFPGA + 1; y++)
		{
			if (nets_count_matrix[x][y] > arc_max_nets)
			{
				arc_max_nets = nets_count_matrix[x][y];
				max_nets_arc.clear();
				if (x < y) max_nets_arc.push_back({ x,y });
				else max_nets_arc.push_back({ x,y });
			}
			else if (nets_count_matrix[x][y] == arc_max_nets)
			{
				if (x < y) max_nets_arc.push_back({ x,y });
				else max_nets_arc.push_back({ x,y });
			}
		}
	}



	vector<vector<vector<pair<int, int>>>> available_nets;

	vector<vector<pair<int, int>>> paths_of_single_net;
	vector<pair<int, int>> path_in_arc;
	for (int x = 0; x < numNet; x++)
	{
		paths_of_single_net.clear();
		for (int y = 0; y < Current[x].sink_num; y++)
		{
			if ((int)Current[x].path[y].size() > 0)
			{
				auto path = Current[x].path[y];
				path_in_arc.clear();
				for (auto arc : path)
				{
					int a = arc.first; int b = arc.second;
					if (!reach_max_weight[a] && !reach_max_weight[b])
						path_in_arc.push_back(arc);
				}

				if ((int)path_in_arc.size() > 0)
					paths_of_single_net.push_back(path_in_arc);
			}
		}

		if ((int)paths_of_single_net.size() > 0)
			available_nets.push_back(paths_of_single_net);

	}


	//随机选择规则加通道
	added_count = 0;
	pair<int, int> added_arc = {};
	vector<int> available_rule = { 0,1,2,3,4 };

	double check_last_time = clock();
	int case_3_continue_count = 0;
	while (added_count < add_tunnel_num)
	{
		if ((int)available_rule.size() == 0) break;

		if (case_3_continue_count >= 40)
		{
			cout << "max trigger!" << endl;
			for (int x = 0; x < numFPGA + 1; x++)
			{
				if (temp_fpga_weight[x] == FPGA_max_weight[x])
					reach_max_weight[x] = true;
				else
					reach_max_weight[x] = false;
			}

			available_nets.clear();
			paths_of_single_net.clear();
			path_in_arc.clear();
			for (int x = 0; x < numNet; x++)
			{
				paths_of_single_net.clear();
				for (int y = 0; y < Current[x].sink_num; y++)
				{
					if ((int)Current[x].path[y].size() > 0)
					{
						auto path = Current[x].path[y];
						path_in_arc.clear();
						for (auto arc : path)
						{
							int a = arc.first; int b = arc.second;
							if (!reach_max_weight[a] && !reach_max_weight[b])
								path_in_arc.push_back(arc);
						}

						if ((int)path_in_arc.size() > 0)
							paths_of_single_net.push_back(path_in_arc);
					}
				}

				if ((int)paths_of_single_net.size() > 0)
					available_nets.push_back(paths_of_single_net);
			}
			case_3_continue_count = 0;

		}

		int rule_idx = rand() % (int)available_rule.size(); //随机选择某个规则执行
		int rule = available_rule[rule_idx];
		//cout << added_count << ' ' << add_tunnel_num << ' ' << rule << ' ' << (clock() - check_last_time) / CLOCKS_PER_SEC << endl;
		//check_last_time = clock();
		switch (rule)
		{
		case(0): //最差路径的最差边加通道
		{
			int a = max_delay_arc.first;
			int b = max_delay_arc.second;
			if (temp_fpga_weight[a] + 1 > FPGA_max_weight[a] || temp_fpga_weight[b] + 1 > FPGA_max_weight[b]) // 最大对外通道约束
			{
				// 移除规则
				auto it = std::remove(available_rule.begin(), available_rule.end(), 0);
				available_rule.erase(it, available_rule.end());

				// 移除最差边中的记录
				auto it2 = std::remove(max_delay_path.begin(), max_delay_path.end(), max_delay_arc);
				max_delay_path.erase(it2, max_delay_path.end());



				continue;
			}
			temp_delta_weight_matrix[a][b]++;
			temp_delta_weight_matrix[b][a]++;
			temp_fpga_weight[a]++;
			temp_fpga_weight[b]++;
			if (temp_fpga_weight[a] == FPGA_max_weight[a]) reach_max_weight[a] = true;
			if (temp_fpga_weight[b] == FPGA_max_weight[b]) reach_max_weight[b] = true;

			if (reach_max_weight[a] || reach_max_weight[b]) // 最差路径两节点通道达上限，移除不再使用
			{
				// 移除规则
				auto it = std::remove(available_rule.begin(), available_rule.end(), 0);
				available_rule.erase(it, available_rule.end());

				// 移除最差边中的记录
				auto it2 = std::remove(max_delay_path.begin(), max_delay_path.end(), max_delay_arc);
				max_delay_path.erase(it2, max_delay_path.end());
			}

			if (a < b) added_arc = { a,b };
			else added_arc = { b,a };


			arc_record.push_back(added_arc);

			break;
		}

		case(1): //整体跨net数量最多边加通道
		{
			int idx = rand() % (int)max_nets_arc.size();
			int a = max_nets_arc[idx].first;
			int b = max_nets_arc[idx].second;
			if (temp_fpga_weight[a] + 1 > FPGA_max_weight[a] || temp_fpga_weight[b] + 1 > FPGA_max_weight[b]) // 最大对外通道约束
			{
				auto it = std::remove(max_nets_arc.begin(), max_nets_arc.end(), max_nets_arc[idx]);
				max_nets_arc.erase(it, max_nets_arc.end());
				if ((int)max_nets_arc.size() == 0) // 整体跨net数量最多边的所有边通道均达到上限，移除
				{
					auto it = std::remove(available_rule.begin(), available_rule.end(), 1);
					available_rule.erase(it, available_rule.end());
				}

				continue;
			}
			temp_delta_weight_matrix[a][b]++;
			temp_delta_weight_matrix[b][a]++;
			temp_fpga_weight[a]++;
			temp_fpga_weight[b]++;
			if (temp_fpga_weight[a] == FPGA_max_weight[a]) reach_max_weight[a] = true;
			if (temp_fpga_weight[b] == FPGA_max_weight[b]) reach_max_weight[b] = true;

			if (reach_max_weight[a] || reach_max_weight[b]) //抽选的边任意一节点达到通道上限，即移除
			{
				auto it = std::remove(max_nets_arc.begin(), max_nets_arc.end(), max_nets_arc[idx]);
				max_nets_arc.erase(it, max_nets_arc.end());
			}

			if ((int)max_nets_arc.size() == 0) // 整体跨net数量最多边的所有边通道均达到上限，移除
			{
				auto it = std::remove(available_rule.begin(), available_rule.end(), 1);
				available_rule.erase(it, available_rule.end());
			}

			if (a < b) added_arc = { a,b };
			else added_arc = { b,a };
			arc_record.push_back(added_arc);

			break;
		}

		case(2): //最差路径中随机选边加通道
		{
			if ((int)max_delay_path.size() == 0)
			{
				auto it = std::remove(available_rule.begin(), available_rule.end(), 2);
				available_rule.erase(it, available_rule.end());

				continue;
			}
			int idx = rand() % (int)max_delay_path.size();
			auto chosen_arc = max_delay_path[idx];
			int a = chosen_arc.first;
			int b = chosen_arc.second;

			while (reach_max_weight[a] || reach_max_weight[b])
			{
				if (reach_max_weight[a] || reach_max_weight[b]) //判断某条边的两点是否满通道，并移除满通道的边
				{
					auto it = std::remove(max_delay_path.begin(), max_delay_path.end(), chosen_arc);
					max_delay_path.erase(it, max_delay_path.end());
				}

				if ((int)max_delay_path.size() == 0) //最差路径中所有边的两点通道都满
				{
					auto it = std::remove(available_rule.begin(), available_rule.end(), 2);
					available_rule.erase(it, available_rule.end());
					break;
				}

				idx = rand() % (int)max_delay_path.size();
				chosen_arc = max_delay_path[idx];
				a = chosen_arc.first;
				b = chosen_arc.second;
			}

			if (temp_fpga_weight[a] + 1 > FPGA_max_weight[a] || temp_fpga_weight[b] + 1 > FPGA_max_weight[b]) continue; // 最大对外通道约束
			temp_delta_weight_matrix[a][b]++;
			temp_delta_weight_matrix[b][a]++;
			temp_fpga_weight[a]++;
			temp_fpga_weight[b]++;
			if (temp_fpga_weight[a] == FPGA_max_weight[a]) reach_max_weight[a] = true;
			if (temp_fpga_weight[b] == FPGA_max_weight[b]) reach_max_weight[b] = true;

			if (reach_max_weight[a] || reach_max_weight[b]) //判断某条边的两点是否满通道，并移除满通道的边
			{
				auto it = std::remove(max_delay_path.begin(), max_delay_path.end(), chosen_arc);
				max_delay_path.erase(it, max_delay_path.end());
			}

			if ((int)max_delay_path.size() == 0) //最差路径中所有边的节点都达到上限，移除此规则
			{
				auto it = std::remove(available_rule.begin(), available_rule.end(), 2);
				available_rule.erase(it, available_rule.end());
			}

			if (a < b) added_arc = { a,b };
			else added_arc = { b,a };

			arc_record.push_back(added_arc);

			break;
		}

		case(3): //纯随机加通道
		{
			if (available_nets.empty()) // 判断是否所有net的通道都已经满
			{
				auto it = std::remove(available_rule.begin(), available_rule.end(), 3);
				available_rule.erase(it, available_rule.end());
				case_3_continue_count++;
				continue;
			}
			int net_idx = rand() % (int)available_nets.size();

			if (available_nets[net_idx].empty())
			{
				available_nets[net_idx].erase(available_nets[net_idx].begin() + net_idx);
				case_3_continue_count++;
				continue;
			}

			int path_idx = rand() % (int)available_nets[net_idx].size();

			if (available_nets[net_idx][path_idx].empty())
			{
				available_nets[net_idx][path_idx].erase(available_nets[net_idx][path_idx].begin() + path_idx);
				case_3_continue_count++;
				continue;
			}

			int arc_idx = rand() % (int)available_nets[net_idx][path_idx].size();
			auto chosen_arc = available_nets[net_idx][path_idx][arc_idx];

			case_3_continue_count = 0;

			int a = chosen_arc.first;
			int b = chosen_arc.second;
			if (temp_fpga_weight[a] + 1 > FPGA_max_weight[a] || temp_fpga_weight[b] + 1 > FPGA_max_weight[b]) // 最大对外通道约束
			{
				auto it = std::remove(available_nets[net_idx][path_idx].begin(), available_nets[net_idx][path_idx].end(), chosen_arc);
				available_nets[net_idx][path_idx].erase(it, available_nets[net_idx][path_idx].end());

				if ((int)available_nets[net_idx][path_idx].size() == 0) //判断某条路径中所有边的两点通道是否都满，并做移除
				{
					auto it2 = std::remove(available_nets[net_idx].begin(), available_nets[net_idx].end(), available_nets[net_idx][path_idx]);
					available_nets[net_idx].erase(it2, available_nets[net_idx].end());
				}
				//aaaaa
				if (available_nets[net_idx].empty())//判断某个net中所有路径的两边是否满通道
					available_nets.erase(available_nets.begin() + net_idx);

				if ((int)available_nets.size() == 0) // 判断是否所有net的通道都已经满
				{
					auto it = std::remove(available_rule.begin(), available_rule.end(), 3);
					available_rule.erase(it, available_rule.end());
				}


				continue;
			}
			temp_delta_weight_matrix[a][b]++;
			temp_delta_weight_matrix[b][a]++;
			temp_fpga_weight[a]++;
			temp_fpga_weight[b]++;
			if (temp_fpga_weight[a] == FPGA_max_weight[a]) reach_max_weight[a] = true;
			if (temp_fpga_weight[b] == FPGA_max_weight[b]) reach_max_weight[b] = true;

			if (a < b) added_arc = { a,b };
			else added_arc = { b,a };

			arc_record.push_back(added_arc);
			break;
		}

		case(4): //起点终点加通道
		{
			if (temp_fpga_weight[source_fpga] + 1 > FPGA_max_weight[source_fpga] || temp_fpga_weight[sink_fpga] + 1 > FPGA_max_weight[sink_fpga])
			{
				// 移除规则
				auto it = std::remove(available_rule.begin(), available_rule.end(), 4);
				available_rule.erase(it, available_rule.end());

				continue;
			}
			temp_delta_weight_matrix[source_fpga][sink_fpga]++;
			temp_delta_weight_matrix[sink_fpga][source_fpga]++;
			temp_fpga_weight[source_fpga]++;
			temp_fpga_weight[sink_fpga]++;
			if (temp_fpga_weight[source_fpga] == FPGA_max_weight[source_fpga]) reach_max_weight[source_fpga] = true;
			if (temp_fpga_weight[sink_fpga] == FPGA_max_weight[sink_fpga]) reach_max_weight[sink_fpga] = true;

			if (source_fpga < sink_fpga) added_arc = { source_fpga,sink_fpga };
			else added_arc = { sink_fpga,source_fpga };

			arc_record.push_back(added_arc);

			break;
		}

		default:
		{
			cout << "random chosen an undefined rule index!";
			exit(-1);
			break;
		}

		}
		added_count++;

		//去除通道数量已满的所有路径

		//double check_remove_start = clock();

		//cout << " remove records: " << (clock() - check_remove_start) / CLOCKS_PER_SEC << endl;
	}


	delete[] reach_max_weight;
}


void random_rule_add_tunnel(int net_index, int sink_index, int add_tunnel_num, int& added_count, int* temp_fpga_weight, int** temp_delta_weight_matrix, std::vector<std::pair<int, int>>& arc_record)
{
	// 基本信息
	const int source_node = Current[net_index].source_node;
	const int sink_node = Current[net_index].sink_nodes[sink_index];
	const int source_fpga = nodes_FPGA[source_node];
	const int sink_fpga = nodes_FPGA[sink_node];
	const auto& max_delay_path = Current[net_index].path[sink_index];

	// 当前是否已达上限
	std::vector<char> reach_max_weight(numFPGA + 1, 0);
	for (int v = 0; v <= numFPGA; ++v)
		reach_max_weight[v] = (temp_fpga_weight[v] >= FPGA_max_weight[v]);

	// ——最差路径最差边（按原口径）——
	std::pair<int, int> max_delay_arc = { 0,0 };
	double arc_max_delay = -1e100;
	for (auto arc : max_delay_path) {
		int u = arc.first, v = arc.second;
		int phys = weight_matrix[u][v] + delta_weight_matrix[u][v];
		if (phys <= 0) continue;
		double tdm = ceil8((double)nets_count_matrix[u][v] / (double)phys);
		if (tdm > R_max) continue;
		double d = 30.0 + 0.7 * tdm;
		if (d > arc_max_delay) { arc_max_delay = d; max_delay_arc = arc; }
	}

	// ——全局 nets_count 最大边集合——
	int arc_max_nets = 0;
	std::vector<std::pair<int, int>> max_nets_arc;
	for (int a = 0; a <= numFPGA; ++a) {
		for (int b = a + 1; b <= numFPGA; ++b) {
			if (nets_count_matrix[a][b] > arc_max_nets) {
				arc_max_nets = nets_count_matrix[a][b];
				max_nets_arc.clear();
				max_nets_arc.emplace_back(a, b);
			}
			else if (nets_count_matrix[a][b] == arc_max_nets) {
				max_nets_arc.emplace_back(a, b);
			}
		}
	}

	// ——构建“多重出现池”：每一次(边,net,sink,edge_idx)出现 = 一个候选位——
	struct Occur { int a, b, net_id, sink_id, edge_idx; };
	std::vector<Occur> pool;                pool.reserve(4096);
	std::vector<char>  alive;               alive.reserve(4096);
	std::vector<std::vector<int>> incident_indices(numFPGA + 1);

	// 活跃袋：只存仍可用出现的索引；pos_in_bag[idx] = 该出现当前在袋中的位置（-1 表示不在）
	std::vector<int> live_bag;              live_bag.reserve(4096);
	std::vector<int> pos_in_bag;            pos_in_bag.reserve(4096);

	auto push_live = [&](int idx) {
		pos_in_bag[idx] = (int)live_bag.size();
		live_bag.push_back(idx);
		};
	auto erase_live = [&](int idx) {
		int pos = pos_in_bag[idx];
		if (pos < 0) return; // 已不在袋中
		int last_idx = live_bag.back();
		live_bag[pos] = last_idx;
		pos_in_bag[last_idx] = pos;
		live_bag.pop_back();
		pos_in_bag[idx] = -1;
		};

	for (int ni = 0; ni < numNet; ++ni) {
		for (int sj = 0; sj < Current[ni].sink_num; ++sj) {
			const auto& path = Current[ni].path[sj];
			if (path.empty()) continue;
			for (int ei = 0; ei < (int)path.size(); ++ei) {
				int a = path[ei].first, b = path[ei].second;
				if (a == b) continue;
				// 初始就满的端点直接不入池
				if (reach_max_weight[a] || reach_max_weight[b]) continue;
				int idx = (int)pool.size();
				pool.push_back({ a,b,ni,sj,ei });
				alive.push_back(1);
				incident_indices[a].push_back(idx);
				incident_indices[b].push_back(idx);
				pos_in_bag.push_back(-1);
				push_live(idx); // 初始就是活跃候选
			}
		}
	}

	// 顶点达上限：批量失效该顶点的所有出现（O(度数) swap-remove）
	auto invalidate_by_vertex = [&](int v) {
		if (!reach_max_weight[v]) return;
		for (int idx : incident_indices[v]) {
			if ((unsigned)idx < alive.size() && alive[idx]) {
				alive[idx] = 0;
				erase_live(idx);
			}
		}
		};

	// 规则2用：最差路径边（一次出现，不去重）
	std::vector<std::pair<int, int>> worst_path_edges = max_delay_path;

	// 可用规则
	added_count = 0;
	std::vector<int> available_rule = { 0,1,2,3,4 };
	auto remove_rule = [&](int rid) {
		auto it = std::remove(available_rule.begin(), available_rule.end(), rid);
		available_rule.erase(it, available_rule.end());
		};

	// ——主循环——
	while (added_count < add_tunnel_num && !available_rule.empty()) {
		int rule = available_rule[rand() % available_rule.size()];
		int a = -1, b = -1;
		bool chose_ok = false;

		switch (rule) {
		case 0: { // 最差路径最差边
			a = max_delay_arc.first; b = max_delay_arc.second;
			if (a <= 0 || b <= 0 ||
				temp_fpga_weight[a] + 1 > FPGA_max_weight[a] ||
				temp_fpga_weight[b] + 1 > FPGA_max_weight[b]) {
				remove_rule(0);
			}
			else {
				chose_ok = true;
			}
			break;
		}
		case 1: { // 全局 nets_count 最大边集合
			while (!max_nets_arc.empty()) {
				auto cand = max_nets_arc[rand() % max_nets_arc.size()];
				a = cand.first; b = cand.second;
				if (temp_fpga_weight[a] + 1 <= FPGA_max_weight[a] &&
					temp_fpga_weight[b] + 1 <= FPGA_max_weight[b]) {
					chose_ok = true; break;
				}
				auto it = std::remove(max_nets_arc.begin(), max_nets_arc.end(), cand);
				max_nets_arc.erase(it, max_nets_arc.end());
			}
			if (!chose_ok) remove_rule(1);
			break;
		}
		case 2: { // 最差路径任一边（懒惰剔除）
			while (!worst_path_edges.empty()) {
				auto cand = worst_path_edges[rand() % worst_path_edges.size()];
				a = cand.first; b = cand.second;
				if (temp_fpga_weight[a] + 1 <= FPGA_max_weight[a] &&
					temp_fpga_weight[b] + 1 <= FPGA_max_weight[b] &&
					!reach_max_weight[a] && !reach_max_weight[b]) {
					chose_ok = true; break;
				}
				auto it = std::remove(worst_path_edges.begin(), worst_path_edges.end(), cand);
				worst_path_edges.erase(it, worst_path_edges.end());
			}
			if (!chose_ok) remove_rule(2);
			break;
		}
		case 3: { // 纯随机：对“所有活跃出现”均匀抽样（net/path 同样随机）
			if (live_bag.empty()) { remove_rule(3); break; }
			int idx = live_bag[rand() % live_bag.size()];
			const auto& oc = pool[idx];
			a = oc.a; b = oc.b;
			// 这里理论上 a,b 均未达上限（活跃袋保证），但仍做一次防御性检查
			if (!reach_max_weight[a] && !reach_max_weight[b] &&
				temp_fpga_weight[a] + 1 <= FPGA_max_weight[a] &&
				temp_fpga_weight[b] + 1 <= FPGA_max_weight[b]) {
				chose_ok = true;
			}
			else {
				// 若因并发更新导致刚刚不可用，立刻从活跃袋剔除后重试一次
				if (alive[idx]) { alive[idx] = 0; erase_live(idx); }
				if (!live_bag.empty()) {
					int idx2 = live_bag[rand() % live_bag.size()];
					const auto& oc2 = pool[idx2];
					a = oc2.a; b = oc2.b;
					if (!reach_max_weight[a] && !reach_max_weight[b] &&
						temp_fpga_weight[a] + 1 <= FPGA_max_weight[a] &&
						temp_fpga_weight[b] + 1 <= FPGA_max_weight[b]) {
						chose_ok = true;
					}
					else {
						if (alive[idx2]) { alive[idx2] = 0; erase_live(idx2); }
						if (live_bag.empty()) remove_rule(3);
					}
				}
				else {
					remove_rule(3);
				}
			}
			break;
		}
		case 4: { // 源-宿直接加
			a = source_fpga; b = sink_fpga;
			if (temp_fpga_weight[a] + 1 <= FPGA_max_weight[a] &&
				temp_fpga_weight[b] + 1 <= FPGA_max_weight[b]) {
				chose_ok = true;
			}
			else {
				remove_rule(4);
			}
			break;
		}
		default: break;
		}

		if (!chose_ok) continue;

		// ——落地更新：加通道 + 端点负载 + 可能的新达上限事件——
		temp_delta_weight_matrix[a][b]++; temp_delta_weight_matrix[b][a]++;
		temp_fpga_weight[a]++; temp_fpga_weight[b]++;

		if (temp_fpga_weight[a] >= FPGA_max_weight[a] && !reach_max_weight[a]) {
			reach_max_weight[a] = 1; invalidate_by_vertex(a);
		}
		if (temp_fpga_weight[b] >= FPGA_max_weight[b] && !reach_max_weight[b]) {
			reach_max_weight[b] = 1; invalidate_by_vertex(b);
		}

		// 记录新增无向边
		if (a > b) std::swap(a, b);
		arc_record.emplace_back(a, b);

		++added_count;
	}
}

void random_rule_add_tunnel_2(int net_index, int sink_index, int add_tunnel_num, int& added_count, int* temp_fpga_weight, int** temp_delta_weight_matrix, std::vector<std::pair<int, int>>& arc_record)
{
	// 基本信息
	const int source_node = Current[net_index].source_node;
	const int sink_node = Current[net_index].sink_nodes[sink_index];
	const int source_fpga = nodes_FPGA[source_node];
	const int sink_fpga = nodes_FPGA[sink_node];
	const auto& max_delay_path = Current[net_index].path[sink_index];

	// 端点是否已达上限（当前状态）
	std::vector<char> reach_max_weight(numFPGA + 1, 0);
	for (int v = 0; v <= numFPGA; ++v)
		reach_max_weight[v] = (temp_fpga_weight[v] >= FPGA_max_weight[v]);

	// ——最差路径最差边（维持原口径/风格）——
	std::pair<int, int> max_delay_arc = { 0,0 };
	double arc_max_delay = -1e100;
	for (auto arc : max_delay_path) {
		int u = arc.first, v = arc.second;
		int phys = weight_matrix[u][v] + delta_weight_matrix[u][v];
		if (phys <= 0) continue;
		double tdm = ceil8((double)nets_count_matrix[u][v] / (double)phys);
		if (tdm > R_max) continue;
		double d = 30.0 + 0.7 * tdm;
		if (d > arc_max_delay) { arc_max_delay = d; max_delay_arc = arc; }
	}

	// ——全局 nets_count 最大边集合——
	int arc_max_nets = 0;
	std::vector<std::pair<int, int>> max_nets_arc;
	for (int a = 0; a <= numFPGA; ++a) {
		for (int b = a + 1; b <= numFPGA; ++b) {
			if (nets_count_matrix[a][b] > arc_max_nets) {
				arc_max_nets = nets_count_matrix[a][b];
				max_nets_arc.clear();
				max_nets_arc.emplace_back(a, b);
			}
			else if (nets_count_matrix[a][b] == arc_max_nets) {
				max_nets_arc.emplace_back(a, b);
			}
		}
	}

	// 规则2用：最差路径边列表（懒惰剔除）
	std::vector<std::pair<int, int>> worst_path_edges = max_delay_path;

	// 工具：检查(a,b)当前是否可加1
	auto ok_add = [&](int a, int b)->bool {
		if (a < 0 || b < 0) return false;
		if (reach_max_weight[a] || reach_max_weight[b]) return false;
		if (temp_fpga_weight[a] + 1 > FPGA_max_weight[a]) return false;
		if (temp_fpga_weight[b] + 1 > FPGA_max_weight[b]) return false;
		return true;
		};

	// ——case(3) 两段式选择：先随机多次，失败再兜底扫描——
	auto choose_case3 = [&](int& out_a, int& out_b)->bool {
		const int MAX_TRIES = 64; // 可调：大样本下拒绝采样成功率高；小样本失败再扫一次
		// 第一段：快速随机
		for (int t = 0; t < MAX_TRIES; ++t) {
			if (numNet <= 0) break;
			int ni = std::rand() % numNet;
			if (Current[ni].sink_num <= 0) continue;
			int sj = std::rand() % Current[ni].sink_num;
			const auto& path = Current[ni].path[sj];
			if (path.empty()) continue;
			int ei = std::rand() % (int)path.size();
			int a = path[ei].first, b = path[ei].second;
			if (ok_add(a, b)) { out_a = a; out_b = b; return true; }
		}
		// 第二段：一次性收集所有当前可用的出现，再等概率抽 1 个
		std::vector<std::pair<int, int>> bucket;
		bucket.reserve(1024);
		for (int ni = 0; ni < numNet; ++ni) {
			int sn = Current[ni].sink_num;
			for (int sj = 0; sj < sn; ++sj) {
				const auto& path = Current[ni].path[sj];
				for (const auto& e : path) {
					int a = e.first, b = e.second;
					if (ok_add(a, b)) bucket.emplace_back(a, b);
				}
			}
		}
		if (bucket.empty()) return false;
		auto pick = bucket[std::rand() % bucket.size()];
		out_a = pick.first; out_b = pick.second;
		return true;
		};

	// 可用规则集
	added_count = 0;
	std::vector<int> available_rule = { 0,1,2,3,4 };
	auto remove_rule = [&](int rid) {
		auto it = std::remove(available_rule.begin(), available_rule.end(), rid);
		available_rule.erase(it, available_rule.end());
		};

	// ——主循环：尝试加到 add_tunnel_num——
	while (added_count < add_tunnel_num && !available_rule.empty()) {
		int rule = available_rule[std::rand() % available_rule.size()];
		int a = -1, b = -1;
		bool chose_ok = false;

		switch (rule) {
		case 0: { // 最差路径最差边
			a = max_delay_arc.first; b = max_delay_arc.second;
			if (ok_add(a, b)) chose_ok = true;
			else remove_rule(0);
			break;
		}
		case 1: { // 全局 nets_count 最大边集合
			while (!max_nets_arc.empty()) {
				auto cand = max_nets_arc[std::rand() % max_nets_arc.size()];
				a = cand.first; b = cand.second;
				if (ok_add(a, b)) { chose_ok = true; break; }
				auto it = std::remove(max_nets_arc.begin(), max_nets_arc.end(), cand);
				max_nets_arc.erase(it, max_nets_arc.end());
			}
			if (!chose_ok) remove_rule(1);
			break;
		}
		case 2: { // 最差路径任一边（懒惰剔除）
			while (!worst_path_edges.empty()) {
				auto cand = worst_path_edges[std::rand() % worst_path_edges.size()];
				a = cand.first; b = cand.second;
				if (ok_add(a, b)) { chose_ok = true; break; }
				auto it = std::remove(worst_path_edges.begin(), worst_path_edges.end(), cand);
				worst_path_edges.erase(it, worst_path_edges.end());
			}
			if (!chose_ok) remove_rule(2);
			break;
		}
		case 3: { // 简洁版：随机 net→path→edge；必要时兜底扫描
			if (choose_case3(a, b)) chose_ok = true;
			else remove_rule(3);
			break;
		}
		case 4: { // 源-宿直接加
			a = source_fpga; b = sink_fpga;
			if (ok_add(a, b)) chose_ok = true;
			else remove_rule(4);
			break;
		}
		default: break;
		}

		if (!chose_ok) continue;

		// ——落地更新：加通道 + 端点负载 + 可能的新达上限事件——
		temp_delta_weight_matrix[a][b]++; temp_delta_weight_matrix[b][a]++;
		temp_fpga_weight[a]++; temp_fpga_weight[b]++;

		if (temp_fpga_weight[a] >= FPGA_max_weight[a] && !reach_max_weight[a]) {
			reach_max_weight[a] = 1;
		}
		if (temp_fpga_weight[b] >= FPGA_max_weight[b] && !reach_max_weight[b]) {
			reach_max_weight[b] = 1;
		}

		// 记录新增无向边
		if (a > b) std::swap(a, b);
		arc_record.emplace_back(a, b);

		++added_count;
	}
}


void add_tunnel(int net_index, int sink_index) //邻域3
{
	// ---------- (0) 数据结构与参数设置  ----------
	//double adjust_ratio = 0.3;
	int add_tunnel_num = (numTunnel * 0.3) - numAddedTunnel; // 增加的通道最大数量
	int added_count = 0; //实际增加的通道数量

	int** temp_delta_weight_matrix = new int* [numFPGA + 1]; //变化的通道记录，用于搜索期间暂存
	int* temp_fpga_weight = new int[numFPGA + 1];

	vector<pair<int, int>> temp_added_arc_record;

	for (int x = 0; x < numFPGA + 1; x++)
	{
		temp_fpga_weight[x] = FPGA_weight[x];
		temp_delta_weight_matrix[x] = new int[numFPGA + 1];
		for (int y = 0; y < numFPGA + 1; y++)
			temp_delta_weight_matrix[x][y] = delta_weight_matrix[x][y];
	}

	for (auto each : added_arc_record)
		temp_added_arc_record.push_back(each);


	// ---------- (1) 增加通道阶段  ----------
	random_rule_add_tunnel(net_index, sink_index, add_tunnel_num, added_count, temp_fpga_weight, temp_delta_weight_matrix, temp_added_arc_record);
	//cout << "---------------------phase 1: add completed---------------------" << endl;



	// ---------- (2) 随机调整通道阶段  ----------
	int adjust_num = SDF_MAX(1, (added_count + numAddedTunnel) * ADJUSTRATIO);
	int adjust_count = 0;
	int actual_adjust = 0;

	unordered_set<pair<int, int>, PairHash> routed_used_added_arcs;
	for (int x = 0; x < numNet; x++)
		for (int y = 0; y < Current[x].sink_num; y++)
			for (auto arc : Current[x].path[y])
			{
				int a = arc.first; int b = arc.second;
				if (weight_matrix[a][b] == 0 && temp_delta_weight_matrix[a][b] > 0)
				{
					pair<int, int> record_arc = {};
					if (a < b) record_arc = { a,b };
					else record_arc = { b,a };
					routed_used_added_arcs.insert(record_arc);
				}
			}

	vector<bool> is_used;
	for (int x = 0; x < (int)temp_added_arc_record.size(); x++)
		is_used.push_back(false);


	while (adjust_count < adjust_num) //随机取消已增加的通道
	{
		auto it = std::find(is_used.begin(), is_used.end(), false);
		if (it == is_used.end()) break;
		int adjust_idx = rand() % (int)temp_added_arc_record.size();
		while (is_used[adjust_idx])
			adjust_idx = rand() % (int)temp_added_arc_record.size();
		auto adjust_arc = temp_added_arc_record[adjust_idx];
		int f_a = adjust_arc.first; int f_b = adjust_arc.second;

		if (temp_delta_weight_matrix[f_a][f_b] == 0 || temp_delta_weight_matrix[f_b][f_a] == 0)
		{
			cout << "arc has no tunnel but random seleced! (" << f_a << "," << f_b << ")" << endl;
			file_output();
			exit(-1);
		}

		pair<int, int> format_arc = {};
		if (f_a < f_b) format_arc = { f_a,f_b };
		else format_arc = { f_b,f_a };
		if (routed_used_added_arcs.find(format_arc) != routed_used_added_arcs.end())
		{
			is_used[adjust_idx] = true;
			continue;
		}


		temp_delta_weight_matrix[f_a][f_b]--;
		temp_delta_weight_matrix[f_b][f_a]--;
		temp_fpga_weight[f_a]--;
		temp_fpga_weight[f_b]--;

		temp_added_arc_record.erase(temp_added_arc_record.begin() + adjust_idx);

		adjust_count++;
	}

	if (adjust_count != adjust_num)adjust_num = 0;

	random_rule_add_tunnel(net_index, sink_index, adjust_count, actual_adjust, temp_fpga_weight, temp_delta_weight_matrix, temp_added_arc_record);

	//cout << "---------------------phase 2: change completed---------------------" << endl;


	int total_add = numAddedTunnel + added_count - adjust_num + actual_adjust;
	if (total_add > (numTunnel * 0.3))
	{
		cout << "add tunnel exceed max! actual: " << total_add << " ,max: " << (numTunnel * 0.3);
		exit(-1);
	}

	// ---------- (3) 模拟退火判断  ----------
	//double current_obj = 0;
	//for (int x = 0; x < numNet; x++)
	//	if (net_delay[x] > current_obj)
	//		current_obj = net_delay[x];
	double current_obj = net_delay[net_index];

	double new_obj = 0;
	calculate_temp_sol(Current, temp_delta_weight_matrix, new_obj);

	//cout << current_obj << ' ' << new_obj << endl;
	bool judge = SA_judge(current_obj, new_obj);
	//cout << "N3 judge: " << judge << endl;

	if (judge)
	{
		for (int x = 0; x < numFPGA + 1; x++)
		{
			FPGA_weight[x] = temp_fpga_weight[x];
			for (int y = 0; y < numFPGA + 1; y++)
				delta_weight_matrix[x][y] = temp_delta_weight_matrix[x][y];
		}
		numAddedTunnel = numAddedTunnel + added_count - adjust_num + actual_adjust;

		added_arc_record.clear();
		for (auto each : temp_added_arc_record)
			added_arc_record.push_back(each);
		calculate_su(Current);
	}



	//bool c = check_result(Current);
	//double check_max_delay = 0;
	//for (int x = 0; x < numNet; x++)
	//	if (net_delay[x] > check_max_delay)
	//		check_max_delay = net_delay[x];
	//if (abs(check_max_delay - new_obj) > 1e-3)
	//{
	//	cout << "unmatch in N3: reported " << current_obj << " actual " << check_max_delay;
	//	exit(-1);
	//}


	if (new_obj < Global_delay && abs(new_obj - Global_delay)>1e-3)
	{
		cout << "Global improved in N3: " << "from " << Global_delay << " to " << new_obj << " time: " << (clock() - beginTime) / CLOCKS_PER_SEC << endl;
		copy_solution(Global, Current, global_delta_weight_matrix, delta_weight_matrix);
		std::copy(net_delay, net_delay + numNet, Global_net_delay);
		Global_delay = new_obj;

		//bool check = check_result(Global, global_delta_weight_matrix);
		//if (!check) exit(-1);
	}


	/*check_result(Global);*/


	for (int x = 0; x < numFPGA + 1; x++)
		delete[] temp_delta_weight_matrix[x];
	delete[] temp_delta_weight_matrix;
	delete[] temp_fpga_weight;
}


void optimize_with_beam_search()
{
	//初始化函数
	initialize_solution();

	//迭代运行优化函数
	//主循环
	int iter = 0;
	while (((clock() - beginTime) / CLOCKS_PER_SEC) < maxRunTime)
	{
		for (int i = 0; i < 10; i++)
		{
			if (((clock() - beginTime) / CLOCKS_PER_SEC) > maxRunTime) break;
			int net_index = -1, sink_index = -1;
			choose_worst_path(Current, net_index, sink_index);

			int neighbor_choose = rand() % 3;
			//double n_search_begin = clock();
			switch (neighbor_choose)
			{
			case(0): { re_route_path(net_index, sink_index);   break; }//邻域1 cout <<i<< "N1: ";
			case(1): { neighbor_replan2(net_index, sink_index);  break; }//邻域2 cout<< i << " N2: ";
			case(2): { add_tunnel(net_index, sink_index);  break; }//邻域3 cout <<i<< " N3: ";
			default:break;
			}
			//cout << (clock() - n_search_begin) / CLOCKS_PER_SEC << endl;
			//cout << "outer check: \n";
			//bool check = check_result(Global, global_delta_weight_matrix);
			//if (!check) exit(-1);
		}
		T = T * 0.9;
	}


}




int main(int argc, char** argv)
{
	if (argc == 7) {
		/*命令行参数输入顺序*/
		caseName = argv[1]; //case文件夹名字
		designInfo = argv[2]; // design.info
		designNet = argv[3]; // design.net
		designTopo = argv[4]; //design.topo
		designFpgaOut = argv[5]; //design.fpga.out
		seed = atoi(argv[6]); // 随机数种子
		rep = argv[6];
	}
	else {
		cout << "Enter some parameters to run the program: " << endl;
		cout << "<instance file name>  <seed>" << endl;
		exit(0);
	}

	srand(seed);

	// 读取实例
	read_instance();

	// 运行时间限制
	maxRunTime = 600;

	//运行主算法
	beginTime = clock();
	optimize_with_beam_search();
	cout << "search is done!" << endl;
	cout << "run time: " << (clock() - beginTime) / CLOCKS_PER_SEC << endl;
	// 输出结果
	file_output();


	cout << endl;


	bool check = check_result(Global, global_delta_weight_matrix);
	if (!check) exit(-1);
	cout << "nets_count_matrix: " << endl;
	for (int i = 1; i <= numFPGA; i++)
	{
		for (int j = 1; j <= numFPGA; j++)
		{
			cout << nets_count_matrix[i][j] << ' ';
		}
		cout << endl;
	}


	delete[] FPGA_max_weight;
	delete[] FPGA_weight;
	delete[] net_delay;
	delete[] Global_net_delay;

	for (int x = 0; x < numFPGA + 1; x++)
	{
		delete[] weight_matrix[x];
		delete[] delta_weight_matrix[x];
		delete[] global_delta_weight_matrix[x];
		delete[] nets_count_matrix[x];
		delete[] FPGA_nodes[x];
	}
	delete[] weight_matrix;
	delete[] delta_weight_matrix;
	delete[] global_delta_weight_matrix;
	delete[] nets_count_matrix;

	delete[] FPGA_nodes;
	delete[] nodes_FPGA;
	delete[] length_FPGA_nodes;
}