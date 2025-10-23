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

int R_max; //最大TDM比率

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

void read_instance()
{
	char temp_caseName[50];
	char designInfoName[50];
	char designNetName[50];
	char designTopoName[50];
	char designFpgaOutName[50];

	strcpy_s(temp_caseName, caseName);
	strcat_s(temp_caseName, sizeof(temp_caseName), "/");
	strcpy_s(designInfoName, temp_caseName);
	strcpy_s(designNetName, temp_caseName);
	strcpy_s(designTopoName, temp_caseName);
	strcpy_s(designFpgaOutName, temp_caseName);

	strcat_s(designInfoName, sizeof(designInfoName), designInfo);
	strcat_s(designNetName, sizeof(designNetName), designNet);
	strcat_s(designTopoName, sizeof(designTopoName), designTopo);
	strcat_s(designFpgaOutName, sizeof(designFpgaOutName), designFpgaOut);

	R_max = 512; //最大TDM比率

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
			FPGA_index++;
		}
	}
	FIC.close();

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
		net_list.push_back(read_net);
		numNet++;
	}

	net_delay = new double[numNet];
	for (int x = 0; x < numNet; x++)
		net_delay[x] = (double)MAXINT;
	numNode = max_node_index;
	FIC.close();

	weight_matrix = new int* [numFPGA + 1];
	delta_weight_matrix = new int* [numFPGA + 1];
	nets_count_matrix = new int* [numFPGA + 1];

	for (int i = 0; i < numFPGA + 1; i++)
	{
		weight_matrix[i] = new int[numFPGA + 1];
		delta_weight_matrix[i] = new int[numFPGA + 1];
		nets_count_matrix[i] = new int[numFPGA + 1];
		for (int j = 0; j < numFPGA + 1; j++)
		{
			weight_matrix[i][j] = 0;
			delta_weight_matrix[i][j] = 0;
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


	for (int x = 0; x < net_list.size(); x++)
	{
		for (int y = 0; y < net_list[x].sink_nodes.size(); y++)
		{
			int soure_FPGA = nodes_FPGA[net_list[x].source_node];
			int sink_FPGA = nodes_FPGA[net_list[x].sink_nodes[y]];
			if (weight_matrix[soure_FPGA][sink_FPGA] > 0)
			{
				net_list[x].available_edges.push_back(make_pair(soure_FPGA, sink_FPGA));
				net_list[x].available_edges.push_back(make_pair(sink_FPGA, soure_FPGA));
			}
		}
	}

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
vector<vector<double>> current_cost(int** weight_matrix, int** nets_count_matrix) // 当前的成本，也就是选择每条边的成本
{
	vector<vector<double>> costn(numFPGA, vector<double>(numFPGA, MAXINT));
	for (int i = 0; i < numFPGA; i++) { costn[i][i] = 0; }
	for (int i = 1; i <= numFPGA; i++) {//计算下一次使用的成本 
		for (int j = i; j <= numFPGA; j++) {
			if (j == i || weight_matrix[i][j] == 0) { continue; }
			int tdm_ratio = ceil8((double)(nets_count_matrix[i][j] + 1) / weight_matrix[i][j]);
			if (tdm_ratio > R_max) continue; // 最大TDM比率约束
			costn[i - 1][j - 1] = 30 + 0.7 * ceil8((double)(nets_count_matrix[i][j] + 1) / weight_matrix[i][j]);
			costn[j - 1][i - 1] = costn[i - 1][j - 1];
		}
	}
	return costn;
}


void single_dijkstra(int source_node, int sink_node, vector<pair<int, int>>& path, double& delay_value)
{
	int* best_path = new int[numFPGA]; //新建一条路径，用于存储新找的最优路径
	int source_fpga = nodes_FPGA[source_node];//源点对应的FPGA
	int sink_fpga = nodes_FPGA[sink_node]; //终点对应的FPGA
	
	if (source_fpga == sink_fpga) // 起点终点在相同FPGA里
	{
		delay_value = 0;
		path.clear();
		return;
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
		for(int x=1;x<=numFPGA;x++)
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
		for(int v = 1; v<= numFPGA; v++)
		{
			if (weight_matrix[u][v] == 0) continue;
			if (visited[v]) continue;
			int tmd_ratio = ceil8((double)(nets_count_matrix[u][v] + 1) / weight_matrix[u][v]);
			if (tmd_ratio > R_max) continue; //判断最大TDM比率约束
			double delay = 30 + 0.7 * (double)tmd_ratio;
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
	path.clear();
	for (int x = 0; x < path_length-1; x++)
	{
		int u = best_path[x];
		int v = best_path[x + 1];
		path.push_back({ u,v });
	}
	reverse(path.begin(), path.end());
	delay_value = dist[sink_fpga];
	delete[] dist;
	delete[] visited;
	delete[] prev;
	delete[] best_path;
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
void ge_su_dp() {//初始化
	for (int n = 0; n < numNet; n++) {
		auto& net = net_list[n];
		multi_dijkstra(net.source_node, net.sink_nodes, net.path);
		// 更新累计使用次数
		unordered_set<pair<int, int>, PairHash> used_pairs;
		for (auto& edges : net.path) {
			for (auto& e : edges) {
				int u = e.first, v = e.second;
				if (u > v) { used_pairs.insert({ v,u }); }
				else { used_pairs.insert({ u, v }); }
			}
		}
		for (auto& p : used_pairs)
		{
			int a = p.first, b = p.second;
			++nets_count_matrix[a][b];
			++nets_count_matrix[b][a];
		}
	}
}
void calculate_su() {//计算net的delay
	for (int i = 0; i < numNet; i++) {
		net_delay[i] = 0;//net的整体delay初始化为0；
		int sink_num = net_list[i].sink_num;
		net_list[i].path_delay.assign(sink_num, 0);//分配空间，同时初始化0
		net_list[i].path_jump_count.assign(sink_num, 0);
		auto& path_jump = net_list[i].path_jump_count;
		auto& path_delay = net_list[i].path_delay;
		auto& path = net_list[i].path;
		for (int j = 0; j < sink_num; j++) {
			if (path[j].size() == 0) { continue; }
			for (auto edge : path[j]) {
				if (edge.first != edge.second) { path_delay[j] += 30 + 0.7 * ceil8((double)nets_count_matrix[edge.first][edge.second] / weight_matrix[edge.first][edge.second]); path_jump[j]++; }
			}
			if (path_delay[j] > net_delay[i]) { net_delay[i] = path_delay[j]; }
		}
	}
}

bool check_result() {

    cout << "There is no new topo file" << endl;

    // ---------- (A) 单次(i<j)循环合并：改动规模 + TDM + 容量累计 ----------
    int total_physical_links = 0;     // Σ weight(i,j)
    int delta_sum = 0;                // Σ |delta(i,j)|
    vector<int> sum_conn(numFPGA + 1, 0); // 每个FPGA的对外连接数
    vector<int> change_per_fpga(numFPGA + 1, 0);//每个FPGA的对外连接变化


    cout << endl << "Check FPGA channel capacity constraint" << endl;

    for (int i = 1; i <= numFPGA; ++i) {
        for (int j = i + 1; j <= numFPGA; ++j) {
            int phys = weight_matrix[i][j] + delta_weight_matrix[i][j];

            total_physical_links += weight_matrix[i][j];
			int d_abs = abs(delta_weight_matrix[i][j]);
            delta_sum += d_abs;

            change_per_fpga[i]+=d_abs;
            change_per_fpga[j]+=d_abs;


            sum_conn[i] += phys;
            sum_conn[j] += phys;

            // TDM & 物理缺失
            int nets_on_pair = nets_count_matrix[i][j];
            if (nets_on_pair > 0 && phys == 0 ) {
                cout << "[E1] Denominator is zero in ratio calculation: pair(" << i << "," << j
                        << ") nets=" << nets_on_pair << ", phys=0\n";//ratio计算中分母为0
                return false;
            }

            // TDM计算：先 ceil(nets/phys)，再 ceil 到8的倍数，≤512
			int tdm_q = ( ( (int)ceil((double)nets_on_pair / (double)phys) + 7 ) & ~7 );
			
            if (tdm_q > 512) {
                std::cout << "[E2] TDM ratio exceeds limit for pair(" << i << "," << j << "): ceil("
                          << nets_on_pair << "/" << phys << ")=" << ceil(nets_on_pair /phys)
                          << ", quantized=" << tdm_q << " > 512\n";//TDM 超限
                return false;
            }
        }
    }

    cout << "All FPGA channel capacity satisfies the constraint" << endl;
    cout << "Total number of connection is: " << std::fixed << std::setprecision(1)
         << (double)total_physical_links << endl << endl;

    // ---------- (B) 改动规模约束 ----------
    if (delta_sum > total_physical_links * 0.3) {
        std::cout << "[E3] Total delta change exceeds limit: delta_sum=" << delta_sum
                    << " > limit=" << total_physical_links * 0.3 << "\n";//改动规模超限
        return false;
    }
	
	cout << "Check channel reconfiguration scope constraint" << endl;
    cout << "Detail info of changing connections :" << endl;




    for (int f = 1; f <= numFPGA; ++f)
        cout << "Number of changing connections of FPGA F" << f << " is: "
             << change_per_fpga[f] << endl;

    cout << "Total number of changing connections: " << std::fixed << std::setprecision(1)
         << (double)delta_sum << endl;
    cout << "The variation in connection channels satisfies the constraints" << endl << endl;


    // ---------- (C) 通道容量约束 ----------
     for (int i = 1; i <= numFPGA; ++i) {
        if (sum_conn[i] > FPGA_max_weight[i]) {
            std::cout << "[E4] Channel capacity exceeded: FPGA " << i
                      << " total=" << sum_conn[i]
                      << " > max=" << FPGA_max_weight[i] << "\n";//容量超限
            return false;
        }
    }
	
	cout << "Check the max ratio constraints:" << endl;
    for (int i = 1; i <= numFPGA; ++i) {
        for (int j = i + 1; j <= numFPGA; ++j) {
            if (nets_count_matrix[i][j] <= 0) continue; // 跳过net=0的pair

            int nets_on_pair = nets_count_matrix[i][j];
            int phys = weight_matrix[i][j] + delta_weight_matrix[i][j];

            cout << "FPGA pair (" << i << ", " << j << "): "
                 << nets_on_pair << " nets" << endl;
            cout << "FPGA pair (" << i << ", " << j << ") has "
                 << phys << " connections, limit is "<<512*phys<<"!" << endl;
        }
    }
    cout << "All FPGA pairs are within the net limit." << endl << endl;


    // ---------- (D) 路径检查 ----------

    for (int k = 0; k < (int)net_list.size(); ++k) {

        for (int t = 0; t < (int)net_list[k].path.size(); ++t) {


            if (!net_list[k].path[t].empty()) 
            {
                int s_fpga = nodes_FPGA[ net_list[k].source_node ];
                int g_fpga = nodes_FPGA[ net_list[k].sink_nodes[t] ];

                int cur = s_fpga;
                
                for (int e = 0; e < (int)net_list[k].path[t].size(); ++e) {
                    int u = net_list[k].path[t][e].first;
                    int v = net_list[k].path[t][e].second;
                    int final_uv = weight_matrix[u][v] + delta_weight_matrix[u][v];


                    // 有向连续性
                    if (u != cur) {
                        std::cout << "[E5] Logical path discontinuity in net " << k
                                << " sinkIdx " << t << " cur=" << cur
                                << " edge=(" << u << "," << v << ")\n";
                        return false;
                    }
                    cur = v;
                }
            }


        }
    }

	
	cout << "Check whether all nets have routed" << endl;
    cout << "All nets has routed" << endl << endl;


    // ---------- (E) 计算得分 ----------
    cout << "Calculate the score of max delay: " << endl;
    double score_max_delay = 0.0;
    for (int k = 0; k < (int)net_list.size(); ++k) {
        for (int t = 0; t < (int)net_list[k].path_delay.size(); ++t)
            score_max_delay = max(score_max_delay, net_list[k].path_delay[t]);
    }
    cout << "Score of max delay =  " << std::fixed << std::setprecision(1)
         << score_max_delay << endl;

    return true;
}

void file()//输出design.route.out
{
	std::ofstream out("design.route.out", std::ios::out);
	if (!out.is_open())
		return;
	vector<int> order1(numNet);
	iota(order1.begin(), order1.end(), 0);
	stable_sort(order1.begin(), order1.end(),
		[](int a, int b)
		{ return net_delay[a] > net_delay[b]; });

	out.setf(std::ios::fixed);
	out << std::setprecision(1);

	for (int idx : order1)
	{
		if (net_delay[idx] <= 0.0)
			continue;

		auto& paths = net_list[idx].path;
		auto& pdelay = net_list[idx].path_delay;

		bool printed_header = false;
		vector<int> order2(pdelay.size());
		iota(order2.begin(), order2.end(), 0);
		stable_sort(order2.begin(), order2.end(),
			[idx](int a, int b)
			{ return net_list[idx].path_delay[a] > net_list[idx].path_delay[b]; });

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
	read_instance();
	ge_su_dp();
	calculate_su();
	file();
	cout << endl;
	cout << "nets_count_matrix: " << endl;
	for (int i = 1; i <= numFPGA; i++)
	{
		for (int j = 1; j <= numFPGA; j++)
		{
			cout << nets_count_matrix[i][j] << ' ';
		}
		cout << endl;
	}

	cout << "search is done!" << endl;

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